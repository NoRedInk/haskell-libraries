{-# LANGUAGE DisambiguateRecordFields #-}

module Producer where

import Conduit
import qualified Environment
import qualified Kafka
import Message
import System.Environment (setEnv)
import Prelude (IO, error, pure, putStrLn)

main :: IO ()
main = do
  setEnv "KAFKA_COMPRESSION_CODEC" "Gzip"
  settings <- Environment.decode Kafka.decoder
  logHandler <- Platform.silentHandler

  putStrLn "Sending messages..."

  Conduit.withAcquire (Kafka.handler settings) <| \handler -> do
    [1 .. 300]
      |> List.map
        ( \id -> do
            let msg =
                  Kafka.emptyMsg "pause-resume-bug"
                    |> Kafka.addPayload Message {id}
            Kafka.sendSync handler msg
        )
      |> Task.sequence
      |> Task.attempt logHandler
      |> andThen fromResult

fromResult :: Result Text a -> IO ()
fromResult (Ok _) = pure ()
fromResult (Err err) = error (Text.toList err)
