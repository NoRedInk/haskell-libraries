-- | Kafka is a module for _writing_ to Kafka
-- See https://github.com/edenhill/librdkafka/blob/0261c86228e910cc84c4c7ab74e563c121f50696/STATISTICS.md
--
-- See Kafka.Worker for the basic building blocks of a CLI app that will poll &
-- process kafka messages
module Kafka.Stats (StatsCallback, Stats, decode, Path, Segment(..), Metric (..)) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (Path, Segment(..))
import qualified Data.Aeson.Extra as Aeson.Extra
import Data.ByteString (ByteString)
import qualified Data.Text
import Dict (Dict)
import qualified Dict
import qualified Tuple
import qualified Prelude

type Stats = Dict Path Metric

type StatsCallback = (Result Text Stats -> Task Text ())

decode :: ByteString -> Result Text Stats
decode raw =
  case Aeson.Extra.decodeIntoFlatDict raw of
    Err err -> Err err
    Ok stats ->
      stats
        |> Dict.toList
        |> List.map
          ( \(path, value) ->
              toMetric path value
                |> Result.map (Tuple.pair path)
          )
        |> Prelude.sequence
        |> Result.map Dict.fromList

data Metric = StringMetric Text | IntMetric Int | IntGauge Int | BoolMetric Bool
  deriving (Show)

toMetric :: Path -> Aeson.Value -> Result Text Metric
toMetric path value =
  case List.reverse path of
    (Aeson.Extra.Key last) : _ ->
      case Dict.get last allMetrics of
        Nothing ->
          -- Get second to last segment of path for `req` case.
          case List.tail (List.reverse path) of
            Just ((Aeson.Extra.Key secondToLast) : _) ->
              case Dict.get secondToLast allMetrics of
                Just metricType -> metricTypeToMetric metricType value path
                Nothing -> Err ("Unknown metric type: " ++ Data.Text.pack (Prelude.show path))
            _ ->
              Err ("Unknown metric: " ++ Data.Text.pack (Prelude.show path))
        Just metricType -> metricTypeToMetric metricType value path
    _ -> Err "Empty path"

metricTypeToMetric :: MetricType -> Aeson.Value -> Path -> Result Text Metric
metricTypeToMetric metricType value path =
  case (metricType, value) of
    (StringType, Aeson.String str) -> Ok (StringMetric str)
    (IntType, Aeson.Number num) -> Ok (IntMetric (Prelude.floor num))
    (IntGaugeType, Aeson.Number num) -> Ok (IntGauge (Prelude.floor num))
    (BoolType, Aeson.Bool bool) -> Ok (BoolMetric bool)
    _ -> Err ("Metric type mismatch: " ++ Data.Text.pack (Prelude.show path))

data MetricType = StringType | IntType | IntGaugeType | BoolType

allMetrics :: Dict Text MetricType
allMetrics =
  List.foldl
    Dict.union
    Dict.empty
    [ topLevel,
      brokers,
      windowStats,
      brokersToppars,
      topics,
      partitions,
      cgrp,
      eos
    ]

topLevel :: Dict Text MetricType
topLevel =
  Dict.fromList
    [ ("name", StringType),
      ("client_id", StringType),
      ("type", StringType),
      ("ts", IntType),
      ("time", IntType),
      ("age", IntType),
      ("replyq", IntGaugeType),
      ("msg_cnt", IntGaugeType),
      ("msg_size", IntGaugeType),
      ("msg_max", IntType),
      ("msg_size_max", IntType),
      ("tx", IntType),
      ("tx_bytes", IntType),
      ("rx", IntType),
      ("rx_bytes", IntType),
      ("txmsgs", IntType),
      ("txmsg_bytes", IntType),
      ("rxmsgs", IntType),
      ("rxmsg_bytes", IntType),
      ("simple_cnt", IntGaugeType),
      ("metadata_cache_cnt", IntGaugeType)
    ]

brokers :: Dict Text MetricType
brokers =
  Dict.fromList
    [ ("name", StringType),
      ("nodeid", IntType),
      ("nodename", StringType),
      ("source", StringType),
      ("state", StringType),
      ("stateage", IntGaugeType),
      ("outbuf_cnt", IntGaugeType),
      ("outbuf_msg_cnt", IntGaugeType),
      ("waitresp_cnt", IntGaugeType),
      ("waitresp_msg_cnt", IntGaugeType),
      ("tx", IntType),
      ("txbytes", IntType),
      ("txerrs", IntType),
      ("txretries", IntType),
      ("txidle", IntType),
      ("req_timeouts", IntType),
      ("rx", IntType),
      ("rxbytes", IntType),
      ("rxerrs", IntType),
      ("rxcorriderrs", IntType),
      ("rxpartial", IntType),
      ("rxidle", IntType),
      ("zbuf_grow", IntType),
      ("buf_grow", IntType),
      ("wakeups", IntType),
      ("connects", IntType),
      ("disconnects", IntType),
      -- This is an object that has custom entries. We are special casing it in metricTypeToMetric
      ("req", IntType)
    ]

windowStats :: Dict Text MetricType
windowStats =
  Dict.fromList
    [ ("min", IntGaugeType),
      ("max", IntGaugeType),
      ("avg", IntGaugeType),
      ("sum", IntGaugeType),
      ("cnt", IntGaugeType),
      ("stddev", IntGaugeType),
      ("hdrsize", IntGaugeType),
      ("p50", IntGaugeType),
      ("p75", IntGaugeType),
      ("p90", IntGaugeType),
      ("p95", IntGaugeType),
      ("p99", IntGaugeType),
      ("p99_99", IntGaugeType),
      ("outofrange", IntGaugeType)
    ]

brokersToppars :: Dict Text MetricType
brokersToppars =
  Dict.fromList
    [ ("topic", StringType),
      ("partition", IntType)
    ]

topics :: Dict Text MetricType
topics =
  Dict.fromList
    [ ("topic", StringType),
      ("age  ", IntGaugeType),
      ("metadata_age", IntGaugeType)
    ]

partitions :: Dict Text MetricType
partitions =
  Dict.fromList
    [ ("partition", IntType),
      ("broker", IntType),
      ("leader", IntType),
      ("desired", BoolType),
      ("unknown", BoolType),
      ("msgq_cnt", IntGaugeType),
      ("msgq_bytes", IntGaugeType),
      ("xmit_msgq_cnt", IntGaugeType),
      ("xmit_msgq_bytes", IntGaugeType),
      ("fetchq_cnt", IntGaugeType),
      ("fetchq_size", IntGaugeType),
      ("fetch_state", StringType),
      ("query_offset", IntGaugeType),
      ("next_offset", IntGaugeType),
      ("app_offset", IntGaugeType),
      ("stored_offset", IntGaugeType),
      ("committed_offset", IntGaugeType),
      ("commited_offset", IntGaugeType),
      ("eof_offset", IntGaugeType),
      ("lo_offset", IntGaugeType),
      ("hi_offset", IntGaugeType),
      ("ls_offset", IntGaugeType),
      ("consumer_lag", IntGaugeType),
      ("consumer_lag_stored", IntGaugeType),
      ("txmsgs", IntType),
      ("txbytes", IntType),
      ("rxmsgs", IntType),
      ("rxbytes", IntType),
      ("msgs", IntType),
      ("rx_ver_drops", IntType),
      ("msgs_inflight", IntGaugeType),
      ("next_ack_seq", IntGaugeType),
      ("next_err_seq", IntGaugeType),
      ("acked_msgid", IntType)
    ]

cgrp :: Dict Text MetricType
cgrp =
  Dict.fromList
    [ ("state", StringType),
      ("stateage", IntGaugeType),
      ("join_state", StringType),
      ("rebalance_age", IntGaugeType),
      ("rebalance_cnt", IntType),
      ("rebalance_reason", StringType),
      ("assignment_size", IntGaugeType)
    ]

eos :: Dict Text MetricType
eos =
  Dict.fromList
    [ ("idemp_state", StringType),
      ("idemp_stateage", IntGaugeType),
      ("txn_state", StringType),
      ("txn_stateage", IntGaugeType),
      ("txn_may_enq", BoolType),
      ("producer_id", IntGaugeType),
      ("producer_epoch", IntGaugeType),
      ("epoch_cnt", IntType)
    ]
