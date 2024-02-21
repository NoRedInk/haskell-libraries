module Consumer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (void, when)
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
  setEnv "KAFKA_MAX_MSGS_PER_PARTITION_BUFFERED_LOCALLY" "20"
  setEnv "KAFKA_POLL_BATCH_SIZE" "5"

  fireDelay <- readIntEnvVar "FIRE_DELAY"
  fireModulo <- readIntEnvVar "FIRE_MODULO"

  settings <- Environment.decode Kafka.decoder
  doAnythingHandler <- Platform.doAnythingHandler

  lock <- newMVar ()

  let processMsg (msg :: Message) =
        ( do
            let msgId = id msg
            let msgIdStr = "ID(" ++ show msgId ++ ")"
            when
              (msgId `mod` fireModulo == 0)
              ( do
                  printAtomic lock stdout (msgIdStr ++ " Pausing consumer (simulating stuck MySQL)")
                  threadDelay (fromIntegral fireDelay * 1000000)
              )
            printAtomic lock stdout (msgIdStr ++ " Done")
            threadDelay 2000
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

readIntEnvVar :: String -> IO Int
readIntEnvVar name = do
  valueStr <- getEnv name
  valueStr 
    |> Text.fromList 
    |> Text.toInt 
    |> Maybe.withDefault (Debug.todo (Text.fromList name ++ " must be a number"))
    |> pure
