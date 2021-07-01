module Kafka.Worker.Settings
  ( Settings (..),
    decoder,
    MaxMsgsPerSecondPerPartition (..),
    MaxMsgsPerPartitionBufferedLocally (..),
    MaxPollIntervalMs (..),
    SkipOrNot (..),
  )
where

import qualified Environment
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Settings.Internal as Internal
import qualified Observability
import qualified Prelude

-- | Settings required to process kafka messages
data Settings = Settings
  { -- | broker addresses. See hw-kafka's documentation for more info
    brokerAddresses :: [Consumer.BrokerAddress],
    -- | Worker will poll Kafka for new messages. This is the timeout
    pollingTimeout :: Consumer.Timeout,
    -- | Used for throttling. Turn this down to give Kafka a speed limit.
    maxMsgsPerSecondPerPartition :: MaxMsgsPerSecondPerPartition,
    logLevel :: Internal.KafkaLogLevel,
    observability :: Observability.Settings,
    -- | Provides backpressure from message-workers to the queue-reader worker.
    -- Ensures that the thread responsible for pulling messages off of kafka
    -- doesn't race ahead / steal resources from the threads executing messages.
    maxMsgsPerPartitionBufferedLocally :: MaxMsgsPerPartitionBufferedLocally,
    pollBatchSize :: Consumer.BatchSize,
    -- | Time between polling
    maxPollIntervalMs :: MaxPollIntervalMs,
    -- | This option provides us the possibility to skip messages on failure.
    -- Useful for testing Kafka worker. DoNotSkip is a reasonable default!
    onProcessMessageSkip :: SkipOrNot,
    -- | The consumer group this worker is part of. Workers sharing the same
    -- consumer group id will collaborate, each processing messages for part
    -- of the partitions of a topic.
    groupId :: Consumer.ConsumerGroupId
  }

-- | This option provides us the possibility to skip messages on failure.
-- Useful for testing Kafka worker. DoNotSkip is a reasonable default!
data SkipOrNot = Skip | DoNotSkip

-- | Used for throttling. Turn this down to give Kafka a speed limit.
data MaxMsgsPerSecondPerPartition = ThrottleAt Int | DontThrottle

-- | Provides backpressure from message-workers to the queue-reader worker.
-- Ensures that the thread responsible for pulling messages off of kafka
-- doesn't race ahead / steal resources from the threads executing messages.
newtype MaxMsgsPerPartitionBufferedLocally = MaxMsgsPerPartitionBufferedLocally {unMaxMsgsPerPartitionBufferedLocally :: Int}

-- | Time between polling
newtype MaxPollIntervalMs = MaxPollIntervalMs {unMaxPollIntervalMs :: Int}

-- | decodes Settings from environmental variables
-- Also consumes Observability env variables (see nri-observability)
-- KAFKA_BROKER_ADDRESSES=localhost:9092 # comma delimeted list
-- KAFKA_LOG_LEVEL=Debug
-- KAFKA_POLLING_TIMEOUT=1000
-- KAFKA_MAX_MESSAGES_PER_SECOND_PER_PARTITION=0 (disabled)
-- KAFKA_MAX_POLL_INTERVAL_MS=300000
-- KAFKA_MAX_MSGS_PER_PARTITION_BUFFERED_LOCALLY=100
-- POLL_BATCH_SIZE=100
-- SKIP_ON_PROCESS_MESSAGE_FAILURE=0
-- GROUP_ID=0
decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap Internal.decoderBrokerAddresses
    |> andMap decoderPollingTimeout
    |> andMap decoderMaxMessagesPerSecondPerPartition
    |> andMap Internal.decoderKafkaLogLevel
    |> andMap Observability.decoder
    |> andMap decoderMaxMsgsPerPartitionBufferedLocally
    |> andMap decoderPollBatchSize
    |> andMap decoderMaxPollIntervalMs
    |> andMap decoderOnProcessMessageFailure
    |> andMap decoderGroupId

decoderPollingTimeout :: Environment.Decoder Consumer.Timeout
decoderPollingTimeout =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_POLLING_TIMEOUT",
        Environment.description = "Polling timout for consumers",
        Environment.defaultValue = "1000"
      }
    (map Consumer.Timeout Environment.int)

decoderMaxMessagesPerSecondPerPartition :: Environment.Decoder MaxMsgsPerSecondPerPartition
decoderMaxMessagesPerSecondPerPartition =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_MAX_MESSAGES_PER_SECOND_PER_PARTITION",
        Environment.description = "This is how we throttle workers. Sets the maximum amount of messages this worker should process per second per partition. 0 is disabled.",
        Environment.defaultValue = "0"
      }
    ( map
        ( \maxPerSecond ->
            ( if maxPerSecond == 0
                then DontThrottle
                else ThrottleAt maxPerSecond
            )
        )
        Environment.int
    )

decoderMaxPollIntervalMs :: Environment.Decoder MaxPollIntervalMs
decoderMaxPollIntervalMs =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_MAX_POLL_INTERVAL_MS",
        Environment.description = "This is used to set max.poll.interval.ms",
        Environment.defaultValue = "300000"
      }
    (map MaxPollIntervalMs Environment.int)

decoderMaxMsgsPerPartitionBufferedLocally :: Environment.Decoder MaxMsgsPerPartitionBufferedLocally
decoderMaxMsgsPerPartitionBufferedLocally =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_MAX_MSGS_PER_PARTITION_BUFFERED_LOCALLY",
        Environment.description = "Pausing reading from kafka when we have this many messages queued up but not yet processed",
        Environment.defaultValue = "100"
      }
    (map MaxMsgsPerPartitionBufferedLocally Environment.int)

decoderPollBatchSize :: Environment.Decoder Consumer.BatchSize
decoderPollBatchSize =
  Environment.variable
    Environment.Variable
      { Environment.name = "POLL_BATCH_SIZE",
        Environment.description = "The amount of messages we request in a single poll request to Kafka",
        Environment.defaultValue = "100"
      }
    (map Consumer.BatchSize Environment.int)

decoderOnProcessMessageFailure :: Environment.Decoder SkipOrNot
decoderOnProcessMessageFailure =
  Environment.variable
    Environment.Variable
      { Environment.name = "SKIP_ON_PROCESS_MESSAGE_FAILURE",
        Environment.description = "Whether to skip message that are failing processing. 1 means on, 0 means off.",
        Environment.defaultValue = "0"
      }
    ( Environment.custom
        Environment.int
        ( \int ->
            if int >= 1
              then Ok Skip
              else Ok DoNotSkip
        )
    )

decoderGroupId :: Environment.Decoder Consumer.ConsumerGroupId
decoderGroupId =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_GROUP_ID",
        Environment.description = "Identifies the consumer group this worker is part off",
        Environment.defaultValue = "procrastinators"
      }
    (map Consumer.ConsumerGroupId Environment.text)
