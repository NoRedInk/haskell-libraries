-- | Kafka is a module for _writing_ to Kafka
-- See https://github.com/edenhill/librdkafka/blob/0261c86228e910cc84c4c7ab74e563c121f50696/STATISTICS.md
--
-- See Kafka.Worker for the basic building blocks of a CLI app that will poll &
-- process kafka messages
module Kafka.Stats (StatsCallback, Stats, decode, Path, pathToText) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (Path, pathToText)
import qualified Data.Aeson.Extra as Aeson.Extra
import Data.ByteString (ByteString)
import Dict (Dict)

type Stats = Dict Aeson.Extra.Path Aeson.Value

type StatsCallback = (Result Text Stats -> Task Text ())

decode :: ByteString -> Result Text Stats
decode raw = Aeson.Extra.decodeIntoFlatDict raw
