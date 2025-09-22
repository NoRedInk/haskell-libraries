{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where


import Control.Monad (void, sequence, forM_)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (threadDelay)
import qualified Conduit
import qualified Environment
import qualified Observability
import qualified Process
import qualified Platform
import Data.List (splitAt)
import Prelude (IO, show, putStrLn, fromIntegral, pure, mapM_, div)
import GHC.Conc (numCapabilities)

threads :: Int
threads = fromIntegral numCapabilities

main :: IO ()
main = do
  settings' <- Environment.decode Observability.decoder
  putStrLn (show settings'.enabledReporters)
  let ids = [1..12] |> List.map Text.fromInt
  Conduit.withAcquire (Observability.handler settings') <| \handler -> do
    forM_ [1..(floor (1_000_000 / fromIntegral threads))] <| \n -> do
      runRequests handler ids

runRequests :: Observability.Handler -> [Text] -> IO ()
runRequests handler =
  mapConcurrently_ (\requestId -> do
            Platform.rootTracingSpanIO
                requestId
                (handler.report requestId)
                ("Running task" ++ requestId)
                ( \log -> do
                    Task.perform log (do
                      Process.sleep 5
                      Task.succeed ())
                )
          )

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt (fromIntegral n) xs
    in  ys : chunks n zs