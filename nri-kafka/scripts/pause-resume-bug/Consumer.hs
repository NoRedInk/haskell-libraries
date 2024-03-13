module Consumer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (void)
import qualified Environment
import qualified Kafka.Worker as Kafka
import Message
import System.Environment (getEnv, setEnv)
import System.IO (Handle, hPutStrLn, stdout)
import Prelude (IO, String, mod, show, fromIntegral, pure)

main :: IO ()
main = do
  -- Disable console logging to make it easier to spot the bug
  setEnv "LOG_ENABLED_LOGGERS" "file"
  setEnv "LOG_FILE" "/dev/null"

  -- Reduce buffer and batch sizes to make it fail faster
  setEnv "KAFKA_MAX_MSGS_PER_PARTITION_BUFFERED_LOCALLY" "1"
  setEnv "KAFKA_POLL_BATCH_SIZE" "1"

  settings <- Environment.decode Kafka.decoder
  doAnythingHandler <- Platform.doAnythingHandler

  lock <- newMVar ()

  let processMsg (msg :: Message) =
        ( do
            let msgId = id msg
            let msgIdStr = "ID(" ++ show msgId ++ ")"
            printAtomic lock stdout ("✅ " ++ msgIdStr ++ " Done")
            threadDelay (10 * 1000000)
        )
          |> fmap Ok
          |> Platform.doAnything doAnythingHandler
  let subscription = Kafka.subscription "pause-resume-bug" processMsg

  Kafka.process settings "pause-resume-bug-consumer" subscription

printAtomic :: MVar () -> Handle -> String -> IO ()
printAtomic lock handle msg = do
  (\_ -> hPutStrLn handle msg)
    |> withMVar lock
    |> forkIO
    |> void

readIntEnvVar :: String -> Int -> IO Int
readIntEnvVar name defaultVal = do
  valueStr <- getEnv name
  valueStr 
    |> Text.fromList 
    |> Text.toInt 
    |> Maybe.withDefault defaultVal
    |> pure
