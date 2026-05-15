{-# LANGUAGE DisambiguateRecordFields #-}

-- | Benchmark for `Kafka.sendSync` per-message latency.
--
-- Measures wall time of N sequential synchronous produces against the broker
-- configured by 'KAFKA_BROKER_ADDRESSES'. Run with the same env on master
-- (baseline) and on the fix branch (after) to compare the polling-induced
-- latency floor.
--
-- > cabal run sync-write-benchmark -fsync-write-benchmark
--
-- Tunables:
--   BENCHMARK_TOPIC          (default: nri-kafka-sync-benchmark)
--   BENCHMARK_MESSAGE_COUNT  (default: 1000)
--   BENCHMARK_WARMUP         (default: 50)  -- excluded from stats
module Main where

import qualified Conduit
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List
import qualified Data.Word
import qualified Environment
import GHC.Clock (getMonotonicTimeNSec)
import qualified Kafka
import qualified System.Environment
import Text.Printf (printf)
import Prelude (IO, String, error, fromIntegral, pure, putStrLn)
import qualified Prelude

newtype Sample = Sample {idx :: Int}
  deriving (Generic)

instance FromJSON Sample

instance ToJSON Sample

main :: IO ()
main = do
  topic <- envText "BENCHMARK_TOPIC" "nri-kafka-sync-benchmark"
  count <- envInt "BENCHMARK_MESSAGE_COUNT" 1000
  warmup <- envInt "BENCHMARK_WARMUP" 50

  settings <- Environment.decode Kafka.decoder
  logHandler <- Platform.silentHandler

  putStrLn
    ( printf
        "Benchmarking sync writes: warmup=%d count=%d topic=%s"
        warmup
        count
        (Text.toList topic)
    )

  Conduit.withAcquire (Kafka.handler settings) <| \handler -> do
    putStrLn "Warming up..."
    Prelude.mapM_ (sendOne logHandler handler topic) [1 .. warmup]

    putStrLn (printf "Measuring %d sends..." count)
    samples <-
      Prelude.traverse
        (timeOne logHandler handler topic)
        [warmup + 1 .. warmup + count]

    putStrLn (formatStats samples)

envText :: String -> Text -> IO Text
envText name def = do
  v <- System.Environment.lookupEnv name
  pure
    ( case v of
        Prelude.Just s -> Text.fromList s
        Prelude.Nothing -> def
    )

envInt :: String -> Int -> IO Int
envInt name def = do
  v <- System.Environment.lookupEnv name
  pure
    ( case v of
        Prelude.Just s -> case Prelude.reads s of
          [(i, "")] -> i
          _ -> def
        Prelude.Nothing -> def
    )

mkMsg :: Text -> Int -> Kafka.Msg
mkMsg topic i =
  Kafka.emptyMsg topic
    |> Kafka.addPayload (Sample {idx = i})
    -- Pin all messages to one partition so partition variability doesn't
    -- pollute the latency distribution.
    |> Kafka.addKey "benchmark"

sendOne :: Platform.LogHandler -> Kafka.Handler -> Text -> Int -> IO ()
sendOne logHandler handler topic i = do
  result <- Task.attempt logHandler (Kafka.sendSync handler (mkMsg topic i))
  case result of
    Ok _ -> pure ()
    Err err -> error (Text.toList err)

timeOne :: Platform.LogHandler -> Kafka.Handler -> Text -> Int -> IO Data.Word.Word64
timeOne logHandler handler topic i = do
  let msg = mkMsg topic i
  t0 <- getMonotonicTimeNSec
  result <- Task.attempt logHandler (Kafka.sendSync handler msg)
  t1 <- getMonotonicTimeNSec
  case result of
    Ok _ -> pure (t1 - t0)
    Err err -> error (Text.toList err)

formatStats :: [Data.Word.Word64] -> String
formatStats samples =
  let sorted = Data.List.sort samples
      n = Prelude.length sorted
      total = Prelude.sum sorted
      avg = nsToMs (total `Prelude.div` fromIntegral n)
      pct :: Prelude.Double -> Prelude.Double
      pct p =
        let i = Prelude.min (n - 1) (Prelude.floor (Prelude.fromIntegral n Prelude.* p / 100))
         in nsToMs (sorted Prelude.!! i)
   in printf
        "count=%d  min=%.1fms  avg=%.1fms  p50=%.1fms  p95=%.1fms  p99=%.1fms  max=%.1fms"
        n
        (nsToMs (Prelude.head sorted))
        avg
        (pct 50)
        (pct 95)
        (pct 99)
        (nsToMs (Prelude.last sorted))

nsToMs :: Data.Word.Word64 -> Prelude.Double
nsToMs ns = fromIntegral ns / 1_000_000
