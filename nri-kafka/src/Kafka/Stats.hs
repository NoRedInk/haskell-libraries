-- | Kafka is a module for _writing_ to Kafka
--
-- See Kafka.Worker for the basic building blocks of a CLI app that will poll &
-- process kafka messages
module Kafka.Stats
  ( StatsCallback,
    Stats (..),
    Broker (..),
    Rtt (..),
    allStats,
  )
where

import qualified Data.Aeson as Aeson
import Dict (Dict)
import Set (Set)
import qualified Set

type StatsCallback = (Stats -> Task Text ())

-- | See https://github.com/edenhill/librdkafka/blob/0261c86228e910cc84c4c7ab74e563c121f50696/STATISTICS.md
data Stats = Stats
  { name :: Text,
    client_id :: Text,
    type_ :: Text,
    ts :: Int,
    time :: Int,
    brokers :: Dict Text Broker
  }
  deriving (Generic)

instance Aeson.FromJSON Stats

data Broker = Broker
  { brokerName :: Text,
    brokerRtt :: Rtt
  }
  deriving (Generic)

instance Aeson.FromJSON Broker where
  parseJSON = Aeson.genericParseJSON (removePrefix "broker")

removePrefix :: Text -> Aeson.Options
removePrefix prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier =
        Aeson.camelTo2 '_' << Text.toList
          << (if Text.isEmpty prefix then identity else Text.replace prefix "")
          << Text.fromList
    }

data Rtt = Rtt
  { min :: Int,
    max :: Int,
    avg :: Int,
    sum :: Int,
    stddev :: Int,
    p50 :: Int,
    p75 :: Int,
    p90 :: Int,
    p95 :: Int,
    p99 :: Int,
    p99_99 :: Int,
    outofrange :: Int,
    hdrsize :: Int,
    cnt :: Int
  }
  deriving (Generic)

instance Aeson.FromJSON Rtt

allStats :: Set Text
allStats =
  Set.fromList
    [ "name",
      "client_id",
      "type",
      "ts",
      "time",
      "brokers.name",
      "brokers.rtt.min",
      "brokers.rtt.max",
      "brokers.rtt.avg",
      "brokers.rtt.sum",
      "brokers.rtt.stddev",
      "brokers.rtt.p50",
      "brokers.rtt.p75",
      "brokers.rtt.p90",
      "brokers.rtt.p95",
      "brokers.rtt.p99",
      "brokers.rtt.p99_99",
      "brokers.rtt.outofrange",
      "brokers.rtt.hdrsize",
      "brokers.rtt.cnt"
    ]
