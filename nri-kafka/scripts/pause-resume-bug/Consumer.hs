module Consumer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Environment
import qualified Kafka.Worker as Kafka
import Message
import System.Environment (setEnv)
import Prelude (IO, pure, putStrLn, show)

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

  let processMsg (msg :: Message) =
        ( do
            putStrLn ("ID " ++ show (id msg))
            prevId <- tryTakeMVar lastId

            case (prevId, id msg) of
              (Nothing, _) ->
                -- First message has been received
                pure ()
              (_, 1) ->
                -- Producer has been restarted
                pure ()
              (Just prev, curr)
                | prev + 1 == curr ->
                  -- This is the expected behavior
                  pure ()
              (Just prev, curr) ->
                -- This is the bug
                putStrLn
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
