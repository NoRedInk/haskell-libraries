module Observability.File (Settings (..), decoder, resolvePath) where

import Cherry.Prelude
import Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Environment
import qualified Path
import qualified Path.IO
import Prelude (FilePath, IO)

newtype Settings
  = Settings
      { logFile :: FilePath
      }

decoder :: Environment.Decoder Settings
decoder =
  map Settings logFileDecoder

logFileDecoder :: Environment.Decoder FilePath
logFileDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_FILE",
        Environment.description = "File to log too.",
        Environment.defaultValue = "app.log"
      }
    Environment.filePath

resolvePath :: FilePath -> IO (Path.Path Path.Abs Path.File)
resolvePath file =
  liftIO
    ( Path.parseAbsFile file `Exception.catch` \(_ :: Path.PathException) ->
        Path.IO.resolveFile' file
    )
