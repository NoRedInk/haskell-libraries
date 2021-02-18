-- | Reporting to a file.
--
-- This reporter logs debugging information about completed requests to a file
-- or `stdout` (in that case, pass in `/dev/stdout` as the file to log to).
--
-- Every line this reporter logs is a JSON string. This 'structured logging'
-- output is optimized for external logging platforms that display these logs in
-- a pretty UI.
--
-- This logger supports sampling of successful requests, to help us save money.
--
-- This reporter is based on Katip for historical reasons. Katip used to run all
-- of what is now called 'reporting' in our apps, not it's just the file logger.
-- We maybe be able to remove it entirely at this point.
module Observability.File
  ( report,
    handler,
    Handler,
    Settings (..),
    decoder,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Environment
import qualified GHC.Stack as Stack
import qualified Katip
import qualified Language.Haskell.TH as TH
import qualified Maybe
import Observability.Timer (Timer, toUTC)
import qualified Path
import qualified Path.IO
import qualified Platform
import qualified System.IO
import qualified System.Random as Random
import qualified Prelude

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' requestId span = do
  skip <- skipLogging handler' span
  if skip
    then Prelude.pure ()
    else
      logItemRecursively handler' Prelude.mempty requestId span
        |> Katip.runKatipT (logEnv handler')

logItemRecursively :: Handler -> Katip.Namespace -> Text -> Platform.TracingSpan -> Katip.KatipT Prelude.IO ()
logItemRecursively handler' namespace requestId span = do
  logItem handler' namespace requestId span
  Foldable.traverse_
    (logItemRecursively handler' (namespace ++ Katip.Namespace [Platform.name span]) requestId)
    (Platform.children span)

logItem :: Handler -> Katip.Namespace -> Text -> Platform.TracingSpan -> Katip.KatipT Prelude.IO ()
logItem (Handler env timer _) namespace requestId span =
  Katip.logKatipItem
    Katip.Item
      { Katip._itemApp = Katip._logEnvApp env,
        Katip._itemEnv = Katip._logEnvEnv env,
        Katip._itemSeverity = case Platform.succeeded span of
          Platform.Succeeded -> Katip.InfoS
          Platform.Failed -> Katip.ErrorS
          Platform.FailedWith _ -> Katip.AlertS,
        Katip._itemThread = Katip.ThreadIdText requestId,
        Katip._itemHost = Katip._logEnvHost env,
        Katip._itemProcess = Katip._logEnvPid env,
        Katip._itemPayload = LogItem span,
        Katip._itemMessage = Katip.logStr (Platform.name span),
        Katip._itemTime = toUTC timer (Platform.started span),
        Katip._itemNamespace = Katip._logEnvApp env ++ namespace,
        Katip._itemLoc = map srcLocToLoc (Platform.frame span)
      }

srcLocToLoc :: (Text, Stack.SrcLoc) -> TH.Loc
srcLocToLoc (_, srcLoc) =
  TH.Loc
    { TH.loc_filename = Stack.srcLocFile srcLoc,
      TH.loc_package = Stack.srcLocPackage srcLoc,
      TH.loc_module = Stack.srcLocModule srcLoc,
      TH.loc_start =
        ( Stack.srcLocStartLine srcLoc,
          Stack.srcLocStartCol srcLoc
        ),
      TH.loc_end =
        ( Stack.srcLocEndLine srcLoc,
          Stack.srcLocEndCol srcLoc
        )
    }

duration :: Platform.TracingSpan -> Platform.MonotonicTime
duration span = Platform.finished span - Platform.started span

-- We need this wrapper around `TracingSpan` so we can define some type class instances
-- for it that `Katip` needs, without having to define them as orphan instances
-- or defining them in `nri-prelude` (which would require it to take a
-- dependency on `katip` too).
newtype LogItem = LogItem Platform.TracingSpan

-- This instance defines how to turn a `TracingSpan` into a JSON object.
instance Katip.ToObject LogItem where
  toObject (LogItem span) =
    let genericFields =
          HashMap.fromList
            [ ("duration in us", Aeson.toJSON (Platform.inMicroseconds (duration span))),
              ( "exception",
                Aeson.toJSON
                  <| case Platform.succeeded span of
                    Platform.Succeeded -> Nothing
                    Platform.Failed -> Nothing
                    Platform.FailedWith err -> Just (Exception.displayException err)
              )
            ]
        detailFields =
          case Aeson.toJSON (Platform.details span) of
            Aeson.Object obj -> obj
            val -> HashMap.singleton "details" val
     in genericFields ++ detailFields
          |> HashMap.filter (Aeson.Null /=)

-- This instance would allow us to specify which fields of the JSON-ified `TracingSpan`
-- to log, depending on the log level and verbosity. In following the
-- 'observability' philosophy we don't make use of this and log all fields all
-- the time, because we cannot know upfront which information will help us
-- understand future problems. Instead we use sampling of of successfull
-- requests to keep logging volumes under control.
instance Katip.LogItem LogItem where
  payloadKeys _ _ = Katip.AllKeys

data Handler = Handler
  { -- | A bit of configuration that Katip needs to log.
    logEnv :: Katip.LogEnv,
    -- | A bit of state that can be used to turn the clock values attached
    -- to spans into real timestamps.
    timer :: Timer,
    -- | A function that determines for a particular span if it should be
    -- skipped in logging.
    skipLogging :: Platform.TracingSpan -> Prelude.IO Bool
  }

handler :: Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  let skipLogging span =
        case Platform.succeeded span of
          Platform.Succeeded -> do
            roll <- Random.randomRIO (0, 1)
            Prelude.pure (roll > fractionOfSuccessRequestsLogged settings)
          Platform.Failed -> Prelude.pure False
          Platform.FailedWith _ -> Prelude.pure False
  logEnv <- mkLogEnv settings
  Prelude.pure Handler {logEnv, timer, skipLogging}

mkLogEnv :: Settings -> Conduit.Acquire Katip.LogEnv
mkLogEnv settings = do
  handle <- fileHandle settings
  Conduit.mkAcquire
    ( do
        scribe' <-
          Katip.mkHandleScribeWithFormatter
            Katip.jsonFormat
            (Katip.ColorLog False)
            handle
            (Katip.permitItem Katip.DebugS)
            Katip.V3
        initLogEnv <- Katip.initLogEnv (appName settings) (appEnvironment settings)
        initLogEnv
          { Katip._logEnvHost =
              mockHostname settings
                |> map Text.toList
                |> Maybe.withDefault (Katip._logEnvHost initLogEnv),
            Katip._logEnvPid =
              mockPid settings
                |> map Prelude.fromIntegral
                |> Maybe.withDefault (Katip._logEnvPid initLogEnv)
          }
          |> Katip.registerScribe "file" scribe' Katip.defaultScribeSettings
    )
    (Katip.closeScribes >> map (\_ -> ()))

fileHandle :: Settings -> Conduit.Acquire System.IO.Handle
fileHandle settings =
  Conduit.mkAcquire
    ( do
        path <- resolvePath (logFile settings)
        System.IO.openFile (Path.toFilePath path) System.IO.AppendMode
    )
    System.IO.hClose

data Settings = Settings
  { logFile :: Prelude.FilePath,
    appName :: Katip.Namespace,
    appEnvironment :: Katip.Environment,
    fractionOfSuccessRequestsLogged :: Float,
    -- These mock values are useful in tests to ensure constant output.
    mockHostname :: Maybe Text,
    mockPid :: Maybe Int
  }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap logFileDecoder
    |> andMap namespaceDecoder
    |> andMap environmentDecoder
    |> andMap fractionOfSuccessRequestsLoggedDecoder
    -- We don't define decoders for the mock* fields used by tests. Tests can
    -- construct a Settings object directly without decoding it from env vars.
    |> andMap (Prelude.pure Nothing)
    |> andMap (Prelude.pure Nothing)

logFileDecoder :: Environment.Decoder Prelude.FilePath
logFileDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_FILE",
        Environment.description = "File to log too.",
        Environment.defaultValue = "app.log"
      }
    Environment.filePath

namespaceDecoder :: Environment.Decoder Katip.Namespace
namespaceDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ROOT_NAMESPACE",
        Environment.description = "Root of the log namespace. This should be the name of the application.",
        Environment.defaultValue = "your-application-name-here"
      }
    (map (Katip.Namespace << Prelude.pure) Environment.text)

environmentDecoder :: Environment.Decoder Katip.Environment
environmentDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ENVIRONMENT",
        Environment.description = "Environment to display in logs.",
        Environment.defaultValue = "development"
      }
    (map Katip.Environment Environment.text)

fractionOfSuccessRequestsLoggedDecoder :: Environment.Decoder Float
fractionOfSuccessRequestsLoggedDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "FRACTION_OF_SUCCESS_REQUESTS_LOGGED",
        Environment.description = "The fraction of successful requests logged. Defaults to logging all successful requests.",
        Environment.defaultValue = "1"
      }
    Environment.float

resolvePath :: Prelude.FilePath -> Prelude.IO (Path.Path Path.Abs Path.File)
resolvePath file =
  Exception.catch
    (Path.parseAbsFile file)
    (\(_ :: Path.PathException) -> Path.IO.resolveFile' file)
