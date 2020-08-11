module Observability.File (reporter, handler, Handler, Settings, decoder) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Environment
import qualified GHC.Stack as Stack
import qualified Katip
import qualified Language.Haskell.TH as TH
import Observability.Timer (Timer, toUTC)
import qualified Path
import qualified Path.IO
import qualified Platform
import qualified System.IO
import qualified Prelude

reporter :: Handler -> Timer -> Platform.Span -> Prelude.IO ()
reporter handler' timer span =
  logItem handler' timer Prelude.mempty span
    |> Katip.runKatipT (logEnv handler')

logItem :: Handler -> Timer -> Katip.Namespace -> Platform.Span -> Katip.KatipT Prelude.IO ()
logItem (Handler env) timer namespace span =
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
      Katip._itemPayload = LogItem (Platform.details span),
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

newtype LogItem a = LogItem a deriving (Aeson.ToJSON)

instance Aeson.ToJSON a => Katip.ToObject (LogItem a)

instance Aeson.ToJSON a => Katip.LogItem (LogItem a) where
  payloadKeys _ _ = Katip.AllKeys

newtype Handler = Handler {logEnv :: Katip.LogEnv}

handler :: Settings -> Conduit.Acquire Handler
handler settings =
  scribe settings
    |> andThen
      ( \scribe' ->
          Katip.initLogEnv (appName settings) (appEnvironment settings)
            |> andThen (Katip.registerScribe "file" scribe' Katip.defaultScribeSettings)
            |> map Handler
            |> Control.Monad.IO.Class.liftIO
      )

scribe :: Settings -> Conduit.Acquire Katip.Scribe
scribe settings = do
  handle <- fileHandle settings
  Conduit.mkAcquire
    ( Katip.mkHandleScribeWithFormatter
        Katip.jsonFormat
        (Katip.ColorLog False)
        handle
        (Katip.permitItem Katip.DebugS)
        Katip.V3
    )
    Katip.scribeFinalizer

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
        appEnvironment :: Katip.Environment
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap logFileDecoder
    |> andMap namespaceDecoder
    |> andMap environmentDecoder

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

resolvePath :: Prelude.FilePath -> Prelude.IO (Path.Path Path.Abs Path.File)
resolvePath file =
  Exception.catch
    (Path.parseAbsFile file)
    (\(_ :: Path.PathException) -> Path.IO.resolveFile' file)
