-- | Kafka.Settings
module Kafka.Settings
  ( Settings (..),
    decoder,
    BatchNumMessages,
    unBatchNumMessages,
    exampleBatchNumMessages,
  )
where

import qualified Environment
import qualified Kafka.Producer
import qualified Kafka.Settings.Internal as Internal
import Kafka.Types (KafkaCompressionCodec (..))

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
    -- | Compression codec used for topics
    compressionCodec :: KafkaCompressionCodec
  }

-- | Number of messages to batch together before sending to Kafka.
newtype BatchNumMessages = BatchNumMessages {unBatchNumMessages :: Int}

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
  map5
    Settings
    Internal.decoderBrokerAddresses
    Internal.decoderKafkaLogLevel
    decoderDeliveryTimeout
    decoderBatchNumMessages
    decodeCompressionCodec

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

decodeCompressionCodec :: Environment.Decoder KafkaCompressionCodec
decodeCompressionCodec =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_COMPRESSION_CODEC",
        Environment.description = "Compression codec used for topics. Supported values are: NoCopmression, Gzip, Snappy and Lz4",
        Environment.defaultValue = "Snappy"
      }
    ( Environment.custom
        Environment.text
        ( \text ->
            case text of
              "NoCompression" -> Ok NoCompression
              "Gzip" -> Ok Gzip
              "Snappy" -> Ok Snappy
              "Lz4" -> Ok Lz4
              _ -> Err ("Unrecognized compression codec: " ++ text)
        )
    )
