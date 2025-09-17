{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where


import Control.Monad (void)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (threadDelay)
import qualified Conduit
import qualified Environment
import qualified Observability
import qualified Process
import qualified Platform
import Prelude (IO, show, putStrLn)

main :: IO ()
main = do
  settings' <- Environment.decode Observability.decoder
  putStrLn (show settings'.enabledReporters)
  Conduit.withAcquire (Observability.handler settings') <| \handler -> do
    [0..300_000]
      |> List.map Text.fromInt
      |> mapConcurrently (\requestId -> do
          Platform.rootTracingSpanIO
              requestId
              (Observability.report handler requestId)
              ("Running task" ++ requestId)
              ( \log -> do
                  Task.perform log (do
                    Process.sleep 5
                    Task.succeed ())
              )
      )
      |> void
  -- give async threads 1s to finish
  -- threadDelay 5_000_000
