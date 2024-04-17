{-# OPTIONS_GHC -fno-cse #-}

module Platform.DevLog
  ( writeSpanToDevLog,
  )
where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Time as Time
import NriPrelude
import qualified Platform.Internal
import qualified System.Environment.Blank as Environment
import qualified System.IO
import qualified System.IO.Unsafe
import qualified System.Posix.Files as Files
import qualified Prelude

-- | Write a tracing span to the development log, where it can be found by
-- `log-explorer` for closer inspection.
writeSpanToDevLog :: Platform.Internal.TracingSpan -> Prelude.IO ()
writeSpanToDevLog span = do
  now <- Time.getCurrentTime
  logFile <- Environment.getEnvDefault "NRI_DEV_LOG" "/tmp/nri-prelude-logs"
  MVar.withMVar writeLock <| \_ ->
    System.IO.withFile
      logFile
      System.IO.AppendMode
      ( \handle -> do
          fileStatus <- Files.getFileStatus logFile
          let fileMode = Files.fileMode fileStatus
          let fileAccessModes = Files.intersectFileModes fileMode Files.accessModes
          Control.Monad.unless (fileAccessModes == Files.stdFileMode)
            <| Files.setFileMode logFile Files.stdFileMode
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
