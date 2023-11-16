{-# LANGUAGE GADTs #-}

-- | A module for creating great logs in code using Kafka.
module Log.Kafka
  ( emptyDetails,
    Details,
    topic,
    partitionId,
    key,
    offset,
    contents,
    createTime,
    logAppendTime,
    processAttempt,
    assignedPartitions,
    pausedPartitions,
    timeSinceLastRebalance,
    requestId,
    mkContents,
    Contents,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Clock
import qualified Platform

-- | A type describing a kafka message being processed by a consumer.
--
-- > emptyDetails
-- >   { topic = Just "kafka-topic"
-- >   , partitionId = Just 1
-- >   , contents = Just (mkContents "This message is a JSON string!")
-- >   }
data Details = Details
  { -- | The topic name of the message.
    topic :: Maybe Text,
    -- | The partition id of the message.
    partitionId :: Maybe Int,
    -- | The key of the message (if it has one). If a key is provided by a
    -- message producer it is used to determine the partition id, in such a way
    -- that messages with the same key are guaranteed to end up in the same
    -- partition.
    key :: Maybe Text,
    -- | The message offset into the partition
    offset :: Maybe Int,
    -- | The contents of the message.
    contents :: Maybe Contents,
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
    processAttempt :: Maybe Int,
    -- | The amount of partitions for this topic the consumer is responsible
    -- for.
    assignedPartitions :: Maybe Int,
    -- | The amount of partitions this consumer currently has paused, because
    -- it's behing processing this partition.
    pausedPartitions :: Maybe Int,
    -- | Time since last rebalance in s
    timeSinceLastRebalance :: Maybe Float,
    -- | The request id of the http request that resulted in the enqueueing of
    -- the message that is now being processed by a worker.
    requestId :: Maybe Text
  }
  deriving (Generic)

-- | An empty details value to be modified by you.
emptyDetails :: Details
emptyDetails =
  Details
    { topic = Nothing,
      partitionId = Nothing,
      key = Nothing,
      offset = Nothing,
      contents = Nothing,
      createTime = Nothing,
      logAppendTime = Nothing,
      processAttempt = Nothing,
      assignedPartitions = Nothing,
      pausedPartitions = Nothing,
      timeSinceLastRebalance = Nothing,
      requestId = Nothing
    }

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_',
      Aeson.omitNothingFields = True
    }

instance Platform.TracingSpanDetails Details

-- | The contents of a Kafka message. Use 'mkContents' to create one of these.
data Contents where
  Contents :: (Aeson.ToJSON a) => a -> Contents

instance Aeson.ToJSON Contents where
  toJSON (Contents x) = Aeson.toJSON x
  toEncoding (Contents x) = Aeson.toEncoding x

-- | Create a 'Contents' value.
--
-- The type wrapped needs to have an Aeson.ToJSON instance, so we can present it
-- nicely in observability tools.
--
-- > data MyMessagePayload { counter :: Int } deriving (Generic)
-- > instance Aeson.ToJSON MyMessagePayload
-- >
-- > contents = mkContents MyMessagePayload { counter = 5 }
mkContents :: Aeson.ToJSON a => a -> Contents
mkContents = Contents
