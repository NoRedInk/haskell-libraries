module Reporter.File.Internal where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable
import qualified Environment
import qualified Network.HostName
import qualified Platform
import qualified Platform.ReporterHelpers as Helpers
import qualified Platform.Timer as Timer
import qualified System.IO
import qualified System.Random as Random
import qualified Prelude

-- | Log tracing information for a request to a file. Tracing information
-- contains nested spans but will appear flattend in the log. Each tracing span
-- will appear on its own line in the log, ordered by its start date.
--
-- Example usage:
--
-- > settings <- File.decode
-- > handler <- File.handler settings
-- > File.report handler "request-id" span
report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report Handler {skipLogging, writeQueue, logContext} requestId span = do
  skip <- skipLogging span
  if skip
    then Prelude.pure ()
    else
      logItemRecursively logContext {requestId} span []
        |> STM.writeTBQueue writeQueue
        |> (`STM.orElse` Prelude.pure ()) -- Drop the log if the queue is full.
        |> STM.atomically

logItemRecursively ::
  LogContext ->
  Platform.TracingSpan ->
  [ByteString.ByteString] ->
  [ByteString.ByteString]
logItemRecursively logContext span acc =
  logItemRecursivelyHelper logContext span acc
    |> List.reverse

logItemRecursivelyHelper ::
  LogContext ->
  Platform.TracingSpan ->
  [ByteString.ByteString] ->
  [ByteString.ByteString]
logItemRecursivelyHelper logContext span acc =
  List.foldr -- The right-most child is the oldest, so that's where we start
    ( logItemRecursivelyHelper
        logContext
          { namespace = Platform.name span : namespace logContext
          }
    )
    (logItem logContext span : acc)
    (Platform.children span)

logItem :: LogContext -> Platform.TracingSpan -> ByteString.ByteString
logItem LogContext {timer, namespace, environment, requestId, hostname} span =
  (Aeson..=) "name" (Platform.name span)
    ++ (Aeson..=) "start_utc" (Timer.toUTC timer (Platform.started span))
    ++ (Aeson..=) "duration_ms" (Prelude.fromIntegral (Timer.durationInUs span) / 1000)
    ++ (Aeson..=) "env" environment
    ++ (Aeson..=) "namespace" namespace
    ++ (Aeson..=) "request-id" requestId
    ++ (Aeson..=) "hostname" hostname
    ++ ( case Platform.frame span of
           Nothing -> Prelude.mempty
           Just (_, srcLoc) ->
             (Aeson..=) "src" (Helpers.srcString srcLoc)
       )
    ++ (Aeson..=) "allocated_mb" (toFloat (Platform.allocated span) / 1e6)
    ++ (Aeson..=) "details" (Platform.details span)
      |> Aeson.pairs
      |> Data.Aeson.Encoding.encodingToLazyByteString

-- | Contextual information this reporter needs to do its work. You can create
-- one using 'handler'.
data Handler = Handler
  { fileHandle :: System.IO.Handle,
    logContext :: LogContext,
    skipLogging :: Platform.TracingSpan -> Prelude.IO Bool,
    writeQueue :: STM.TBQueue [ByteString.ByteString],
    loggingThread :: Async.Async ()
  }

data LogContext = LogContext
  { timer :: Timer.Timer,
    namespace :: [Text],
    environment :: Text,
    requestId :: Text,
    hostname :: Text
  }

-- | Create a 'Handler' for a specified set of 'Settings'. Do this once when
-- your application starts and reuse the 'Handler' you get.
handler :: Settings -> Prelude.IO Handler
handler settings = do
  let skipLogging span =
        case Platform.succeeded span of
          Platform.Succeeded -> do
            roll <- Random.randomRIO (0.0, 1.0)
            Prelude.pure (roll > fractionOfSuccessRequestsLogged settings)
          Platform.Failed -> Prelude.pure False
          Platform.FailedWith _ -> Prelude.pure False
  fileHandle <- System.IO.openFile (logFile settings) System.IO.AppendMode
  hostname <- map Text.fromList Network.HostName.getHostName
  writeQueue <- STM.atomically (STM.newTBQueue 100)
  loggingThread <- Async.async (logLoop writeQueue fileHandle)
  timer <- Timer.mkTimer
  let logContext =
        LogContext
          { timer,
            hostname,
            requestId = "", -- This changes per request and so is set later.
            namespace = [appName settings],
            environment = appEnvironment settings
          }
  Prelude.pure
    Handler
      { fileHandle,
        skipLogging,
        writeQueue,
        loggingThread,
        logContext
      }

logLoop ::
  STM.TBQueue [ByteString.ByteString] ->
  System.IO.Handle ->
  Prelude.IO ()
logLoop writeQueue fileHandle = do
  lines <- STM.atomically (STM.readTBQueue writeQueue)
  Data.Foldable.for_ lines <| \line -> do
    ByteString.hPut fileHandle line
    ByteString.hPut fileHandle "\n"
  logLoop writeQueue fileHandle

-- | Clean up your handler after you're done with it. Call this before your
-- application shuts down.
cleanup :: Handler -> Prelude.IO ()
cleanup Handler {loggingThread, fileHandle} = do
  Async.cancel loggingThread
  System.IO.hClose fileHandle

-- | Configuration settings for this reporter. A value of this type can be read
-- from the environment using the 'decoder' function.
data Settings = Settings
  { -- | The file to log too. On unix systems you can set this to @/dev/stdout@
    -- in order to log to stdout.
    --
    -- [@environment variable@] LOG_FILE
    -- [@default value@] app.log
    logFile :: Prelude.FilePath,
    -- | The name of this application. This will be attached to all logs.
    --
    -- [@environment variable@] LOG_ROOT_NAMESPACE
    -- [@default value@] your-application-name-here
    appName :: Text,
    -- | The environment this application is running in. This will be attached
    -- to all logs.
    --
    -- [@environment variable@] ENVIRONMENT
    -- [@default value@] development
    appEnvironment :: Text,
    -- | The fraction of requests that should be logged. Set to 1 if you want to
    -- log everything, and a lower value to save money.
    --
    -- [@environment variable@] FRACTION_OF_SUCCESS_REQUESTS_LOGGED
    -- [@default value@] 1
    fractionOfSuccessRequestsLogged :: Float
  }

-- | Read 'Settings' from environment variables. Default variables will be used
-- in case no environment variable is set for an option.
decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap logFileDecoder
    |> andMap appNameDecoder
    |> andMap environmentDecoder
    |> andMap fractionOfSuccessRequestsLoggedDecoder

logFileDecoder :: Environment.Decoder Prelude.FilePath
logFileDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_FILE",
        Environment.description = "File to log too.",
        Environment.defaultValue = "app.log"
      }
    Environment.filePath

appNameDecoder :: Environment.Decoder Text
appNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ROOT_NAMESPACE",
        Environment.description = "Root of the log namespace. This should be the name of the application.",
        Environment.defaultValue = "your-application-name-here"
      }
    Environment.text

environmentDecoder :: Environment.Decoder Text
environmentDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ENVIRONMENT",
        Environment.description = "Environment to display in logs.",
        Environment.defaultValue = "development"
      }
    Environment.text

fractionOfSuccessRequestsLoggedDecoder :: Environment.Decoder Float
fractionOfSuccessRequestsLoggedDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "FRACTION_OF_SUCCESS_REQUESTS_LOGGED",
        Environment.description = "The fraction of successful requests logged. Defaults to logging all successful requests.",
        Environment.defaultValue = "1"
      }
    Environment.float
