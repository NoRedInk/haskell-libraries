module Observability.File (report, handler, Handler, Settings (..), decoder) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text
import qualified Data.Word as Word
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

report :: Handler -> Platform.Span -> Prelude.IO ()
report handler' span = do
  skip <- skipLogging handler' span
  if skip
    then Prelude.pure ()
    else
      logItemRecursively handler' Prelude.mempty span
        |> Katip.runKatipT (logEnv handler')

logItemRecursively :: Handler -> Katip.Namespace -> Platform.Span -> Katip.KatipT Prelude.IO ()
logItemRecursively handler' namespace span = do
  logItem handler' namespace span
  Foldable.traverse_
    (logItemRecursively handler' (namespace ++ Katip.Namespace [Platform.name span]))
    (Platform.children span)

logItem :: Handler -> Katip.Namespace -> Platform.Span -> Katip.KatipT Prelude.IO ()
logItem (Handler env timer _) namespace span =
  Katip.logKatipItem Katip.Item
    { Katip._itemApp = Katip._logEnvApp env,
      Katip._itemEnv = Katip._logEnvEnv env,
      Katip._itemSeverity = case Platform.succeeded span of
        Platform.Succeeded -> Katip.InfoS
        Platform.Failed -> Katip.ErrorS
        Platform.FailedWith _ -> Katip.AlertS,
      Katip._itemThread = Katip.ThreadIdText "",
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

duration :: Platform.Span -> Word.Word64
duration span = Platform.finished span - Platform.started span

newtype LogItem = LogItem Platform.Span

instance Katip.ToObject LogItem where
  toObject (LogItem span) =
    let genericFields =
          HashMap.fromList
            [ ("duration in ms", Aeson.toJSON (duration span `Prelude.div` 1000000)),
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

instance Katip.LogItem LogItem where
  payloadKeys _ _ = Katip.AllKeys

data Handler
  = Handler
      { -- | A bit of configuration that Katip needs to log.
        logEnv :: Katip.LogEnv,
        -- | A bit of state that can be used to turn the clock values attached
        -- to spans into real timestamps.
        timer :: Timer,
        -- | A function that determines for a particular span if it should be
        -- skipped in logging.
        skipLogging :: Platform.Span -> Prelude.IO Bool
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
        Katip.initLogEnv (appName settings) (appEnvironment settings)
          |> map
            ( \logEnv' ->
                logEnv'
                  { Katip._logEnvHost =
                      mockHostname settings
                        |> map Data.Text.unpack
                        |> Maybe.withDefault (Katip._logEnvHost logEnv'),
                    Katip._logEnvPid =
                      mockPid settings
                        |> map Prelude.fromIntegral
                        |> Maybe.withDefault (Katip._logEnvPid logEnv')
                  }
            )
          |> andThen (Katip.registerScribe "file" scribe' Katip.defaultScribeSettings)
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

data Settings
  = Settings
      { logFile :: Prelude.FilePath,
        appName :: Katip.Namespace,
        appEnvironment :: Katip.Environment,
        fractionOfSuccessRequestsLogged :: Float,
        -- These mock values are useful in tests to ensure the constant output.
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
