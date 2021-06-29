{-# LANGUAGE GADTs #-}

-- | A module dedicated to observability, that is reporting information about
-- what the program is doing in production to help us debugging it.
--
-- Specifically this module is dedicated to sending information that's already
-- been collected to external monitoring platforms, such as Bugsnag and
-- Honeycomb. To learn more about how this information is collected check out the
-- `Platform.Internal` module in the `nri-prelude` package. That module also
-- defines and documents the `TracingSpan` type, which is the data structure we use to
-- contain observability data we have collected.
module Observability
  ( report,
    Handler,
    Settings (..),
    Reporter (..),
    decoder,
    handler,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Environment
import qualified List
import qualified Platform
import qualified Reporter.Bugsnag as Bugsnag
import qualified Reporter.Dev as Dev
import qualified Reporter.File as File
import qualified Reporter.Honeycomb as Honeycomb
import qualified Set
import qualified Text
import Prelude (pure, traverse)
import qualified Prelude

-- | A handler for reporting to logging/monitoring/observability platforms.
--
-- The `report` function takes a span containing all the tracing information we
-- collected for a single request and then reports it to all platforms we
-- enabled when creating this handler.
newtype Handler = Handler {report :: Text -> Platform.TracingSpan -> Prelude.IO ()}
  deriving (Prelude.Semigroup, Prelude.Monoid)

-- | Function for creating an observability handler. The settings we pass in
-- determine which platforms we'll report information to.
handler :: Settings -> Conduit.Acquire Handler
handler settings = do
  case enabledReporters settings of
    [] -> Prelude.pure reportNothingHandler
    firstReporter : otherReporters -> do
      firstHandler <- toHandler reportNothingHandler settings firstReporter
      otherHandlers <- traverse (toHandler firstHandler settings) otherReporters
      Prelude.pure (Prelude.mconcat (firstHandler : otherHandlers))

reportNothingHandler :: Handler
reportNothingHandler = Handler (\_ _ -> Prelude.pure ())

-- | Initialize a reporter from settings, to get a handler for sending reports.
--
-- If a reporter fails with an exception here's what we'd like to be true:
-- - It doesn't affect other reporters (they don't get aborted or anything).
-- - The reporting error gets itself reported _somewhere_ in a way that's
--   relatively likely to succeed.
toHandler :: Handler -> Settings -> Reporter -> Conduit.Acquire Handler
toHandler backup settings Reporter {reporterName, reporterSettings, reporterHandler, reporterReport} = do
  handler' <-
    reporterSettings settings
      |> reporterHandler
      |> map (Handler << reporterReport)
  Prelude.pure
    ( Handler
        ( \requestId span ->
            report handler' requestId span
              -- If any error sync or async is thrown during reporting, report
              -- the failure to the 'backup reporter'.
              |> ( \io ->
                     Exception.withException
                       io
                       ( \err ->
                           err
                             |> reporterExceptionToTracingSpan reporterName span
                             |> report backup requestId
                       )
                 )
              -- If this reporter fails with a sync exception we do not want it
              -- to abort other reporters, so we catch those exceptions. We've
              -- already tried to report on the exception above so there's
              -- nothing more we could be doing with the exception anyway.
              |> Exception.handleAny (\_ -> Prelude.pure ())
        )
    )

-- | If a reporter fails with an exception we'd like to report that exception in
-- turn to a reporter. Because reporters take spans we turn the exception into a
-- span here.
reporterExceptionToTracingSpan :: Text -> Platform.TracingSpan -> Exception.SomeException -> Platform.TracingSpan
reporterExceptionToTracingSpan reporterName originalTracingSpan exceptionDuringReporting =
  Platform.emptyTracingSpan
    { Platform.name = "Failed to report span to " ++ reporterName,
      Platform.succeeded = Platform.FailedWith exceptionDuringReporting,
      Platform.started = Platform.finished originalTracingSpan,
      Platform.finished = Platform.finished originalTracingSpan,
      Platform.frame = Nothing,
      Platform.details =
        FailedToReportTracingSpan
          (Platform.name originalTracingSpan)
          (Platform.details originalTracingSpan)
          |> Platform.toTracingSpanDetails
          |> Just,
      Platform.allocated = 0,
      Platform.children = []
    }

-- | Some details about the span we were attempting to report when a reporting
-- failure happened. We're conservative with the information from the original
-- span we copy over here, in case some complexity somewhere in the original
-- span caused the report of it to fail. It's entirely likely the reporting
-- failure is unrelated to the reported span though. For example, reporting to
-- Bugsnag might fail if Bugsnag is down.
data FailedToReportTracingSpan = FailedToReportTracingSpan
  { originalTracingSpanName :: Text,
    originalTracingSpanDetails :: Maybe Platform.SomeTracingSpanDetails
  }
  deriving (Generic)

instance Aeson.ToJSON FailedToReportTracingSpan

instance Platform.TracingSpanDetails FailedToReportTracingSpan

-- | A helper type that combines all the different functions a specific reporter
-- must implement in a single record.
--
-- We've defined this as a GADT so we don't need to expose the `handler` and
-- `settings` parameters on the type, meaning this type is defined as `Reporter`
-- rather than `Reporter settings handler`. This allows us to combine multiple
-- reporters in a list.
data Reporter where
  Reporter ::
    { reporterName :: Text,
      -- | Pick the reporter-specific settings from the global `Settings` type
      -- defined in this module.
      reporterSettings :: Settings -> settings,
      -- | Create a handler for this reporter. This function will be called
      -- once when the application starts for each enabled reporter.
      reporterHandler :: settings -> Conduit.Acquire handler,
      -- | Report a span to this reporter.
      reporterReport :: handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
    } ->
    Reporter

-- | A list containing all the reporters we support. Reporters are ordered in
-- increasing chance of failure, so we can pick the safest one for reporting on
-- failures in other reporters.
supportedReporters :: [Reporter]
supportedReporters =
  [ Reporter "stdout" stdout fileHandler File.report,
    Reporter "stdout-pretty" dev devHandler Dev.report,
    Reporter "file" file fileHandler File.report,
    Reporter "bugsnag" bugsnag bugsnagHandler Bugsnag.report,
    Reporter "honeycomb" honeycomb honeycombHandler Honeycomb.report
  ]

fileHandler :: File.Settings -> Conduit.Acquire File.Handler
fileHandler settings = Conduit.mkAcquire (File.handler settings) File.cleanup

devHandler :: () -> Conduit.Acquire Dev.Handler
devHandler _ = Conduit.mkAcquire Dev.handler Dev.cleanup

bugsnagHandler :: Bugsnag.Settings -> Conduit.Acquire Bugsnag.Handler
bugsnagHandler settings = Conduit.liftIO (Bugsnag.handler settings)

honeycombHandler :: Honeycomb.Settings -> Conduit.Acquire Honeycomb.Handler
honeycombHandler settings = Conduit.liftIO (Honeycomb.handler settings)

-- | Settings for all supported reporters.
data Settings = Settings
  { -- | The reporters that we send debugging information to. The first
    -- reporter in the list is treated special: it will also report on
    -- failures that take place while running any of the other reporters.
    -- Because of this its best if the first reporter is one that has a
    -- high likelihood of succeeding, for example because it logs to stdout
    -- or a file.
    enabledReporters :: [Reporter],
    -- | Each supported reporter has a settings entry below. We parse
    -- settings even for reporters that aren't enabled. Because our
    -- environment parser insists all variables have default values it's not
    -- necessary to make up values for reporters we don't want enabled.
    file :: File.Settings,
    bugsnag :: Bugsnag.Settings,
    honeycomb :: Honeycomb.Settings,
    dev :: ()
  }

stdout :: Settings -> File.Settings
stdout settings = (file settings) {File.logFile = "/dev/stdout"}

decoder :: Environment.Decoder Settings
decoder =
  pure Settings
    |> andMap reportersDecoder
    |> andMap File.decoder
    |> andMap Bugsnag.decoder
    |> andMap Honeycomb.decoder
    |> andMap (Prelude.pure ())

reportersDecoder :: Environment.Decoder [Reporter]
reportersDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ENABLED_LOGGERS",
        Environment.description = "Comma-separated list of logging destinations.",
        Environment.defaultValue = "stdout-pretty"
      }
    (Environment.custom Environment.text reportersParser)

-- Parses reporters, maintaining the same order of reporters as the
-- `supportedReporters` list.
reportersParser :: Text -> Result Text [Reporter]
reportersParser reportersString = do
  names <-
    reportersString
      |> Text.split ","
      |> traverse parseLogger
      |> map Set.fromList
  supportedReporters
    |> List.filter (\reporter -> Set.member (reporterName reporter) names)
    |> Ok

parseLogger :: Text -> Result Text Text
parseLogger name =
  let normalizedName = Text.trim (Text.toLower name)
      supportedNames =
        supportedReporters
          |> map reporterName
          |> Set.fromList
   in if Set.member normalizedName supportedNames
        then Ok normalizedName
        else Err ("Unknown reporter: " ++ normalizedName)
