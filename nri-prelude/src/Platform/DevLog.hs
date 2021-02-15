module Platform.DevLog (writeSpanToDevLog) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Time as Time
import qualified Platform.Internal
import qualified System.IO
import qualified Prelude

-- | Write a tracing span to the development log, where it can be found by
-- `log-explorer` for closer inspection.
writeSpanToDevLog :: Platform.Internal.TracingSpan -> Prelude.IO ()
writeSpanToDevLog span = do
  now <- Time.getCurrentTime
  let logFile = "/tmp/nri-prelude-logs"
  System.IO.withFile
    logFile
    System.IO.AppendMode
    ( \handle -> do
        Data.ByteString.Lazy.hPut handle (Aeson.encode (now, span))
        Data.ByteString.Lazy.hPut handle "\n"
    )
