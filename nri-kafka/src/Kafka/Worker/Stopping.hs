module Kafka.Worker.Stopping
  ( init,
    stopReason,
    stopTakingRequests,
    runUnlessStopping,
    waitUntilStopping,
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
runUnlessStopping stopping stoppingVal action =
  Async.race
    (waitUntilStopping stopping)
    action
    |> map
      ( \either ->
          case either of
            Prelude.Left () -> stoppingVal
            Prelude.Right r -> r
      )

waitUntilStopping :: Stopping -> Prelude.IO ()
waitUntilStopping (Stopping stopping) =
  MVar.readMVar stopping |> map (\_ -> ())
