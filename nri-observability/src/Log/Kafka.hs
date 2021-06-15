{-# LANGUAGE GADTs #-}

module Log.Kafka (Consumer (..), Encodable (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Clock
import qualified Platform

data Consumer = Consumer
  { topic :: Text,
    partitionId :: Int,
    key :: Maybe Text,
    contents :: Encodable,
    -- | The meaning of this is depending on `log.message.timestamp.type`.
    -- Fortunately, `Consumer.Timestamp` is a union and contains the
    -- information about this.
    -- More context: https://github.com/edenhill/librdkafka/blob/8bacbc0b4c357193288c81277bfcc815633126ea/INTRODUCTION.md#latency-measurement
    createTime :: Maybe Clock.UTCTime,
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

data Encodable where
  Encodable :: (Aeson.ToJSON a) => a -> Encodable

instance Aeson.ToJSON Encodable where
  toJSON (Encodable x) = Aeson.toJSON x
  toEncoding (Encodable x) = Aeson.toEncoding x
