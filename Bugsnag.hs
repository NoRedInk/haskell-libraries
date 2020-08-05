module Observability.Bugsnag
  ( mkScribe,
    Settings,
    decoder,
  )
where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Environment
import qualified Health
import qualified Http
import qualified Katip
import qualified Language.Haskell.TH.Syntax as Syntax
import qualified List
import qualified Log
import qualified Maybe
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client
import qualified Platform
import qualified Text
import qualified Tuple
import Prelude (Either (Left, Right), IO, pure, putStrLn, readFile)

mkScribe :: Settings -> Http.Handler -> Conduit.Acquire (Health.Check, Katip.Scribe)
mkScribe settings http = do
  log <- liftIO <| Platform.silentHandler
  revision <- liftIO getRevision
  liftIO <| Http.withThirdPartyIO log http <| \manager -> do
    batcher <- Bugsnag.newBatcher manager (Log.unSecret (apiKey settings)) onError
    let scribe = Katip.Scribe (liPush batcher revision) (Bugsnag.flushBatcher batcher) (permitItem settings)
    pure (readiness settings manager, scribe)

onError :: Network.HTTP.Client.HttpException -> IO ()
onError err =
  -- When we're unsuccessful logging to Bugsnag we can't well log the error to
  -- Bugsnag. We write to stdout, at least that way we note the exception in the
  -- server output.
  putStrLn
    <| "Exception encountered when sending logs to Bugsnag: " ++ Control.Exception.displayException err

liPush :: Katip.LogItem a => Bugsnag.Batcher -> Text -> Katip.Item a -> IO ()
liPush batcher revision item =
  Bugsnag.queueSingleEvent batcher (prepare item revision)

permitItem :: Settings -> Katip.Item a -> IO Bool
permitItem settings item =
  pure <| Katip._itemSeverity item >= minimalSeverity settings

prepare :: Katip.LogItem a => Katip.Item a -> Text -> Bugsnag.Event
prepare item revision =
  Bugsnag.defaultEvent
    { Bugsnag.event_exceptions =
        [ Bugsnag.defaultException
            { Bugsnag.exception_errorClass = message,
              Bugsnag.exception_message = Just message,
              Bugsnag.exception_stacktrace = stackTrace,
              Bugsnag.exception_type = Nothing
            }
        ],
      -- We're currently not set up to collect breadcrumbs, but we might in
      -- the future. We could pass a bit of state around in our `Task` types
      -- that records logs and spans, so we can pass those on when we send
      -- an error to Bugsnag.
      Bugsnag.event_breadcrumbs = Nothing,
      -- There's request information we're collecting and storing in our
      -- `Contexts` type. Our scribe's don't have access to that type, only
      -- to a JSON representation of it. We could parse that back again, but
      -- that seems super inefficient.
      --
      -- For now we'll pass the request data into the event metadata, which
      -- means it will be visible in the Bugsnag UI, but not in the nicest
      -- place. If we want to do better, we should consider switching away
      -- from Katip.
      Bugsnag.event_request = Nothing,
      -- Haskell has light-weight threads. Reading the Bugsnag documentation
      -- it sounds like for the entry below it's more interested in os
      -- threads. Not sure if it makes sense to log thread information to
      -- Bugsnag, or if we can even access it. We're not Rollbaring it
      -- either at the moment, so going to leave it out.
      Bugsnag.event_threads = Nothing,
      Bugsnag.event_context = Just namespace,
      -- Haven't seen a reason yet to overwrite Bugsnag's grouping logic.
      Bugsnag.event_groupingHash = Nothing,
      -- We don't know if the error was unhandled or not at this point.
      Bugsnag.event_unhandled = Nothing,
      Bugsnag.event_severity = Just severity,
      Bugsnag.event_severityReason = Nothing,
      -- We're not automatically adding user information to the log contexts
      -- yet. We should add that!
      Bugsnag.event_user = Nothing,
      -- We're not automatically adding app information to the log contexts
      -- yet. We should add that!
      Bugsnag.event_app =
        Just
          Bugsnag.defaultApp
            { Bugsnag.app_id = appId,
              Bugsnag.app_version = appVersion,
              Bugsnag.app_releaseStage = Just environment,
              Bugsnag.app_type = Just "haskell"
            },
      Bugsnag.event_device =
        Just
          Bugsnag.defaultDevice
            { Bugsnag.device_hostname = Just hostname
            },
      -- We don't have any session data we could put here.
      Bugsnag.event_session = Nothing,
      Bugsnag.event_metaData = Just metaData
    }
  where
    message =
      Katip._itemMessage item
        |> Katip.unLogStr
        |> Data.Text.Lazy.Builder.toLazyText
        |> Data.Text.Lazy.toStrict
    namespace =
      Katip._itemNamespace item
        |> Katip.unNamespace
        |> Text.join ", "
    appId =
      Katip._itemNamespace item
        |> Katip.unNamespace
        |> List.head
    -- Same format as what bugsnag-build-notify uses for appVersion
    appVersion =
      Maybe.map (\justAppId -> Text.join "-" [justAppId, revision]) appId
    severity =
      case Katip._itemSeverity item of
        Katip.DebugS -> Bugsnag.infoSeverity
        Katip.InfoS -> Bugsnag.infoSeverity
        Katip.NoticeS -> Bugsnag.infoSeverity
        Katip.WarningS -> Bugsnag.warningSeverity
        Katip.ErrorS -> Bugsnag.errorSeverity
        Katip.CriticalS -> Bugsnag.errorSeverity
        Katip.AlertS -> Bugsnag.errorSeverity
        Katip.EmergencyS -> Bugsnag.errorSeverity
    hostname =
      Katip._itemHost item
        |> Data.Text.pack
    metaData =
      Katip._itemPayload item
        |> Katip.toObject
    environment =
      Katip._itemEnv item
        |> Katip.getEnvironment
    stackTrace =
      case Katip._itemLoc item of
        Nothing -> []
        Just loc ->
          -- We'd like to log a longer stack trace here, and we're collecting
          -- a frame for every `Log.withContext` that wraps the callsite.
          -- Unfortunately Katip doesn't offer a way to pass this information
          -- on to the Scribe.
          --
          -- We might want to consider switching to a different logging
          -- library that's more compatible with what we'd like to log.
          -- `co-log` looks promising.
          [ Bugsnag.defaultStackFrame
              { Bugsnag.stackFrame_file = Data.Text.pack (Syntax.loc_filename loc),
                Bugsnag.stackFrame_lineNumber = Tuple.first (Syntax.loc_start loc),
                Bugsnag.stackFrame_columnNumber = Just (Tuple.second (Syntax.loc_start loc)),
                -- The `Loc` type Bugsnag users doesn't record the method
                -- name. The call stacks we record internally in our
                -- `LogHandler` type do have this information, but we can't
                -- access it. See comment about potentially switching to
                -- another logging library above.
                Bugsnag.stackFrame_method = Data.Text.pack (Syntax.loc_package loc ++ ": " ++ Syntax.loc_module loc),
                Bugsnag.stackFrame_inProject = Just True,
                Bugsnag.stackFrame_code = Nothing
              }
          ]

data Settings
  = Settings
      { apiKey :: Log.Secret Bugsnag.ApiKey,
        minimalSeverity :: Katip.Severity
      }

decoder :: Environment.Decoder Settings
decoder =
  pure Settings
    |> andMap apiKeyDecoder
    |> andMap severityDecoder

apiKeyDecoder :: Environment.Decoder (Log.Secret Bugsnag.ApiKey)
apiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "BUGSNAG_API_KEY",
        Environment.description = "The API key of the Bugsnag project we should send items too.",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> map Bugsnag.apiKey |> Environment.secret)

severityDecoder :: Environment.Decoder Katip.Severity
severityDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "BUGSNAG_MINIMAL_SEVERITY",
        Environment.description = "The severity below which items will not be logged to Bugsnag.",
        Environment.defaultValue = "warning"
      }
    (Environment.custom Environment.text matchSeverity)

matchSeverity :: Text -> Result Text Katip.Severity
matchSeverity severity =
  case Text.toLower severity of
    "debug" -> Ok Katip.DebugS
    "info" -> Ok Katip.InfoS
    "warning" -> Ok Katip.WarningS
    "error" -> Ok Katip.ErrorS
    "alert" -> Ok Katip.AlertS
    _ -> Err ("Unknown severity: " ++ severity)

-- |
-- Check if Bugsnag is ready to receive requests.
readiness :: Settings -> Network.HTTP.Client.Manager -> Health.Check
readiness settings manager =
  Health.mkCheck "bugsnag" <| do
    result <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) []
    pure <| case result of
      Right () -> Health.Good
      Left err ->
        "HTTP request to Bugsnag failed: " ++ Control.Exception.displayException err
          |> Data.Text.pack
          |> Health.Bad

getRevision :: IO Text
getRevision = do
  eitherRevision <- Control.Exception.tryAny <| readFile "revision"
  case eitherRevision of
    Left _err ->
      pure "no revision file found"
    Right version -> pure <| Data.Text.pack version
