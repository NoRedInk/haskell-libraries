module Kafka.Worker.Stopping
  ( init,
    stopTakingRequests,
    runUnlessStopping,
    Stopping,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Prelude

newtype Stopping = Stopping (MVar.MVar ())

init :: Prelude.IO Stopping
init = MVar.newEmptyMVar |> map Stopping

stopTakingRequests :: Stopping -> Prelude.IO ()
stopTakingRequests (Stopping stopping) = do
  Prelude.putStrLn "Gracefully shutting down..."
  MVar.tryPutMVar stopping ()
    |> map (\_ -> ())

runUnlessStopping :: Stopping -> a -> Prelude.IO a -> Prelude.IO a
runUnlessStopping (Stopping stopping) stoppingVal action =
  Async.race
    (MVar.readMVar stopping)
    action
    |> map
      ( \either ->
          case either of
            Prelude.Left () -> stoppingVal
            Prelude.Right r -> r
      )
