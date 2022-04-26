-- | Kafka.Settings
module Kafka.Settings
  ( Settings (..),
    decoder,
    BatchNumMessages,
    unBatchNumMessages,
    exampleBatchNumMessages,
    StatisticsIntervalMs (..),
  )
where

import qualified Environment
import qualified Kafka.Producer
import qualified Kafka.Settings.Internal as Internal
import qualified Prelude

-- | Settings required to write to Kafka
data Settings = Settings
  { -- | broker addresses. See hw-kafka's documentation for more info
    brokerAddresses :: [Kafka.Producer.BrokerAddress],
    -- | client log level. See hw-kafka's documentation for more info
    logLevel :: Internal.KafkaLogLevel,
    -- | Message delivery timeout. See hw-kafka's documentation for more info
    deliveryTimeout :: Kafka.Producer.Timeout,
    -- | Number of messages to batch together before sending to Kafka.
    batchNumMessages :: BatchNumMessages,
    -- | librdkafka statistics emit interval. The application also needs to
    -- register a stats callback using rd_kafka_conf_set_stats_cb(). The
    -- granularity is 1000ms. A value of 0 disables statistics.
    statisticsIntervalMs :: StatisticsIntervalMs
  }

-- | Number of messages to batch together before sending to Kafka.
newtype BatchNumMessages = BatchNumMessages {unBatchNumMessages :: Int}

newtype StatisticsIntervalMs = StatisticsIntervalMs {unStatisticsIntervalMs :: Int}

-- |  example BatchNumMessages to use in tests
exampleBatchNumMessages :: BatchNumMessages
exampleBatchNumMessages = BatchNumMessages 1

-- | decodes Settings from environmental variables
-- KAFKA_BROKER_ADDRESSES=localhost:9092 # comma delimeted list
-- KAFKA_LOG_LEVEL=Debug
-- KAFKA_DELIVERY_TIMEOUT=120000
-- KAFKA_BATCH_SIZE=10000
decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap Internal.decoderBrokerAddresses
    |> andMap Internal.decoderKafkaLogLevel
    |> andMap decoderDeliveryTimeout
    |> andMap decoderBatchNumMessages
    |> andMap decoderStatisticsIntervalMs

decoderDeliveryTimeout :: Environment.Decoder Kafka.Producer.Timeout
decoderDeliveryTimeout =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_DELIVERY_TIMEOUT",
        Environment.description = "Delivery timout for producer. Aka 'delivery.timeout.ms'",
        Environment.defaultValue = "120000"
      }
    (map Kafka.Producer.Timeout Environment.int)

decoderBatchNumMessages :: Environment.Decoder BatchNumMessages
decoderBatchNumMessages =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_BATCH_SIZE",
        Environment.description = "Kafka Producer 'batch.num.messages'",
        Environment.defaultValue = "10000"
      }
    (map BatchNumMessages Environment.int)

decoderStatisticsIntervalMs :: Environment.Decoder StatisticsIntervalMs
decoderStatisticsIntervalMs =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_STATISTICS_INTERVAL_MS",
        Environment.description = "librdkafka statistics emit interval. The application also needs to register a stats callback using rd_kafka_conf_set_stats_cb(). The granularity is 1000ms. A value of 0 disables statistics.",
        Environment.defaultValue = "0"
      }
    (map StatisticsIntervalMs Environment.int)
