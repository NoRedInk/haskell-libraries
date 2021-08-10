module Kafka.Worker.Stopping
  ( init,
    stopReason,
    stopTakingRequests,
    runUnlessStopping,
    Stopping,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Prelude

newtype Stopping = Stopping (MVar.MVar Text)

init :: Prelude.IO Stopping
init = MVar.newEmptyMVar |> map Stopping

stopReason :: Stopping -> Prelude.IO (Maybe Text)
stopReason (Stopping stopping) =
  MVar.tryReadMVar stopping

stopTakingRequests :: Stopping -> Text -> Prelude.IO ()
stopTakingRequests (Stopping stopping) reason = do
  Prelude.putStrLn "Gracefully shutting down..."
  MVar.tryPutMVar stopping reason
    |> map (\_ -> ())

runUnlessStopping :: Stopping -> a -> Prelude.IO a -> Prelude.IO a
runUnlessStopping (Stopping stopping) stoppingVal action =
  Async.race
    (MVar.readMVar stopping |> map (\_ -> ()))
    action
    |> map
      ( \either ->
          case either of
            Prelude.Left () -> stoppingVal
            Prelude.Right r -> r
      )
