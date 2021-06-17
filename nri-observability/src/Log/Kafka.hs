{-# LANGUAGE GADTs #-}

-- | A module for creating great logs in code using Kafka.
module Log.Kafka (Consumer (..), Encodable (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Clock
import qualified Platform

-- | A type describing a kafka message being processed by a consumer.
data Consumer = Consumer
  { -- | The topic name of the message.
    topic :: Text,
    -- | The partition id of the message.
    partitionId :: Int,
    -- | The key of the message (if it has one). If a key is provided by a
    -- message producer it is used to determine the partition id, in such a way
    -- that messages with the same key are guaranteed to end up in the same
    -- partition.
    key :: Maybe Text,
    -- | The contents of the message.
    --
    -- This library is currently assuming all message payloads use JSON. That's
    -- not a limitation of Kafka itself though.
    contents :: Encodable,
    -- | The time at which this message was created by a producer.
    -- Whether this property is available for a message depends on the
    -- `log.message.timestamp.type` configuration option.
    -- More context: https://github.com/edenhill/librdkafka/blob/8bacbc0b4c357193288c81277bfcc815633126ea/INTRODUCTION.md#latency-measurement
    createTime :: Maybe Clock.UTCTime,
    -- | The time at which this message was added to a log by a broker.
    -- Whether this property is available for a message depends on the
    -- `log.message.timestamp.type` configuration option.
    -- More context: https://github.com/edenhill/librdkafka/blob/8bacbc0b4c357193288c81277bfcc815633126ea/INTRODUCTION.md#latency-measurement
    logAppendTime :: Maybe Clock.UTCTime,
    -- | Zero-based counter indicating the how-manyth time it is we're attemping
    -- to process this message.
    processAttempt :: Int,
    -- | The amount of partitions for this topic the consumer is responsible
    -- for.
    assignedPartitions :: Int,
    -- | The amount of partitions this consumer currently has paused, because
    -- it's behing processing this partition.
    pausedPartitions :: Int,
    -- | Time since last rebalance in s
    timeSinceLastRebalance :: Float,
    -- | The request id of the http request that resulted in the enqueueing of
    -- the message that is now being processed by a worker.
    requestId :: Maybe Text
  }
  deriving (Generic)

instance Aeson.ToJSON Consumer where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_',
      Aeson.omitNothingFields = True
    }

instance Platform.TracingSpanDetails Consumer

-- | Defering encoding to json to later.
data Encodable where
  Encodable :: (Aeson.ToJSON a) => a -> Encodable

instance Aeson.ToJSON Encodable where
  toJSON (Encodable x) = Aeson.toJSON x
  toEncoding (Encodable x) = Aeson.toEncoding x
