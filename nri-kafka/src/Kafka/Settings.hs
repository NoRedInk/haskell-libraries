module Kafka.Settings
  ( Settings (..),
    decoder,
    BatchNumMessages (..),
  )
where

import qualified Environment
import qualified Kafka.Producer
import qualified Kafka.Settings.Internal as Internal

data Settings = Settings
  { brokerAddresses :: [Kafka.Producer.BrokerAddress],
    logLevel :: Internal.KafkaLogLevel,
    deliveryTimeout :: Kafka.Producer.Timeout,
    batchNumMessages :: BatchNumMessages
  }

newtype BatchNumMessages = BatchNumMessages {unBatchNumMessages :: Int}

decoder :: Environment.Decoder Settings
decoder =
  map4
    Settings
    Internal.decoderBrokerAddresses
    Internal.decoderKafkaLogLevel
    decoderDeliveryTimeout
    decoderBatchNumMessages

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
