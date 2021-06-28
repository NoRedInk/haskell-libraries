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

data Settings = Settings
  { brokerAddresses :: [Consumer.BrokerAddress],
    pollingTimeout :: Consumer.Timeout,
    -- used for throttling
    maxMsgsPerSecondPerPartition :: MaxMsgsPerSecondPerPartition,
    logLevel :: Internal.KafkaLogLevel,
    observability :: Observability.Settings,
    -- Provides backpressure from message-workers to the queue-reader worker.
    -- Ensures that the thread responsible for pulling messages off of kafka
    -- doesn't race ahead / steal resources from the threads executing messages.
    maxMsgsPerPartitionBufferedLocally :: MaxMsgsPerPartitionBufferedLocally,
    pollBatchSize :: Consumer.BatchSize,
    maxPollIntervalMs :: MaxPollIntervalMs,
    -- This option provides us the possibility to skip messages on failure. We
    -- won't be able to do this once we're properly life with quiz-engine:
    -- failures will need to be handled. But it's useful for testing things
    -- now.
    onProcessMessageSkip :: SkipOrNot
  }

data SkipOrNot = Skip | DoNotSkip

data MaxMsgsPerSecondPerPartition = ThrottleAt Int | DontThrottle

newtype MaxMsgsPerPartitionBufferedLocally = MaxMsgsPerPartitionBufferedLocally {unMaxMsgsPerPartitionBufferedLocally :: Int}

newtype MaxPollIntervalMs = MaxPollIntervalMs {unMaxPollIntervalMs :: Int}

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
        Environment.defaultValue = "1"
      }
    ( Environment.custom
        Environment.int
        ( \int ->
            if int >= 1
              then Ok Skip
              else Ok DoNotSkip
        )
    )
