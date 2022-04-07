-- | Kafka is a module for _writing_ to Kafka
-- See https://github.com/edenhill/librdkafka/blob/0261c86228e910cc84c4c7ab74e563c121f50696/STATISTICS.md
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

removePrefix :: Text -> Aeson.Options
removePrefix prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier =
        Aeson.camelTo2 '_' << Text.toList
          << (if Text.isEmpty prefix then identity else Text.replace prefix "")
          << Text.fromList
    }

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

-- | Currengly suggested strucure
data Value = IntValue Int | TextValue Text | BoolValue Bool
type Stats = Dict Key Value
data Key = Key (List Text)

-- | Type exposed for clients
-- TODO statsDict :: Text Aeson.Value
