{-# OPTIONS_GHC -fno-cse #-}

module Platform.DevLog
  ( writeSpanToDevLog,
  )
where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Time as Time
import NriPrelude
import qualified Platform.Internal
import qualified System.IO
import qualified System.IO.Unsafe
import qualified Prelude

-- | Write a tracing span to the development log, where it can be found by
-- `log-explorer` for closer inspection.
writeSpanToDevLog :: Platform.Internal.TracingSpan -> Prelude.IO ()
writeSpanToDevLog span = do
  now <- Time.getCurrentTime
  let logFile = "/tmp/nri-prelude-logs-test"
  MVar.withMVar writeLock <| \_ ->
    System.IO.withFile
      logFile
      System.IO.AppendMode
      ( \handle -> do
          Data.ByteString.Lazy.hPut handle (Aeson.encode (now, span))
          Data.ByteString.Lazy.hPut handle "\n"
      )

-- A lock used to ensure writing spans to the dev log are atomic, processes will
-- take turns to fully write their spans to the log to prevent interleaving.
{-# NOINLINE writeLock #-}
writeLock :: MVar.MVar ()
writeLock =
  MVar.newMVar ()
    |> System.IO.Unsafe.unsafePerformIO
