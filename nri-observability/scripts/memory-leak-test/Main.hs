{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forM_, sequence, void)
import Data.List (splitAt)
import qualified Environment
import GHC.Conc (numCapabilities)
import qualified Observability
import qualified Platform
import qualified Process
import Prelude (IO, div, fromIntegral, mapM_, pure, putStrLn, show)

threads :: Int
threads = fromIntegral numCapabilities

main :: IO ()
main = do
  settings' <- Environment.decode Observability.decoder
  putStrLn (show settings'.enabledReporters)
  let ids = [1 .. 12] |> List.map Text.fromInt
  Conduit.withAcquire (Observability.handler settings') <| \handler -> do
    forM_ [1 .. (floor (1_000_000 / fromIntegral threads))] <| \n -> do
      runRequests handler ids

runRequests :: Observability.Handler -> [Text] -> IO ()
runRequests handler =
  mapConcurrently_
    ( \requestId -> do
        Platform.rootTracingSpanIO
          requestId
          Platform.silentTrack
          (handler.report requestId)
          ("Running task" ++ requestId)
          ( \log -> do
              Task.perform
                log
                ( do
                    Process.sleep 5
                    Task.succeed ()
                )
          )
    )

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt (fromIntegral n) xs
   in ys : chunks n zs
