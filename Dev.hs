module Observability.Dev (report, Handler, handler, Settings, decoder) where

import Cherry.Prelude
import qualified Conduit
import qualified Environment
import qualified Observability.Timer as Timer
import qualified Platform
import qualified System.IO
import qualified Prelude

report :: Handler -> Platform.Span -> Prelude.IO ()
report = Prelude.undefined

data Handler
  = Handler
      { _timer :: Timer.Timer,
        _handle :: System.IO.Handle
      }

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer' settings =
  Conduit.mkAcquire
    (System.IO.openFile (logFile settings) System.IO.AppendMode)
    System.IO.hClose
    |> map (Handler timer')

newtype Settings
  = Settings
      { logFile :: Prelude.FilePath
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap logFileDecoder

logFileDecoder :: Environment.Decoder Prelude.FilePath
logFileDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_FILE",
        Environment.description = "File to log too.",
        Environment.defaultValue = "app.log"
      }
    Environment.filePath
