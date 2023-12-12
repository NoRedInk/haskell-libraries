module Consumer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, tryTakeMVar, withMVar)
import Control.Monad (void)
import qualified Environment
import qualified Kafka.Worker as Kafka
import Message
import System.Environment (setEnv)
import System.IO (Handle, hPutStrLn, stderr, stdout)
import Prelude (IO, String, show)

main :: IO ()
main = do
  -- Disable console logging to make it easier to spot the bug
  setEnv "LOG_ENABLED_LOGGERS" "file"
  setEnv "LOG_FILE" "/dev/null"

  -- Reduce buffer and batch sizes to make it fail faster
  setEnv "KAFKA_MAX_MSGS_PER_PARTITION_BUFFERED_LOCALLY" "20"
  setEnv "KAFKA_POLL_BATCH_SIZE" "5"

  settings <- Environment.decode Kafka.decoder
  doAnythingHandler <- Platform.doAnythingHandler
  lastId <- newEmptyMVar

  lock <- newMVar ()

  let processMsg (msg :: Message) =
        ( do
            let msgId = ("ID(" ++ show (id msg) ++ ")")
            prevId <- tryTakeMVar lastId

            case (prevId, id msg) of
              (Nothing, _) ->
                printAtomic lock stdout (msgId ++ " First message has been received")
              (_, 1) ->
                printAtomic lock stdout (msgId ++ " Producer has been restarted")
              (Just prev, curr)
                | prev + 1 == curr ->
                    -- This is the expected behavior
                    printAtomic lock stdout (msgId ++ " OK")
              (Just prev, curr) ->
                -- This is the bug
                printAtomic
                  lock
                  stderr
                  ( "ERROR: Expected ID "
                      ++ show (prev + 1)
                      ++ " but got "
                      ++ show curr
                  )

            putMVar lastId (id msg)
            threadDelay 200000
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
