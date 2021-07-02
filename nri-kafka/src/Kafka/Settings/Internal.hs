module Kafka.Settings.Internal
  ( Kafka.Types.KafkaLogLevel (..),
    decoderBrokerAddresses,
    decoderKafkaLogLevel,
  )
where

import qualified Environment
import qualified Kafka.Types

decoderBrokerAddresses :: Environment.Decoder [Kafka.Types.BrokerAddress]
decoderBrokerAddresses =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_BROKER_ADDRESSES",
        Environment.description = "A comma-separated list of broker addresses",
        Environment.defaultValue = "localhost:9092"
      }
    (map (List.map Kafka.Types.BrokerAddress << Text.split ",") Environment.text)

decoderKafkaLogLevel :: Environment.Decoder Kafka.Types.KafkaLogLevel
decoderKafkaLogLevel =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_LOG_LEVEL",
        Environment.description = "Kafka log level",
        Environment.defaultValue = "Debug"
      }
    (map kafkaLogLevelFromText Environment.text)

kafkaLogLevelFromText :: Text -> Kafka.Types.KafkaLogLevel
kafkaLogLevelFromText text =
  case text of
    "Emerg" -> Kafka.Types.KafkaLogEmerg
    "Alert" -> Kafka.Types.KafkaLogAlert
    "Crit" -> Kafka.Types.KafkaLogCrit
    "Err" -> Kafka.Types.KafkaLogErr
    "Warning" -> Kafka.Types.KafkaLogWarning
    "Notice" -> Kafka.Types.KafkaLogNotice
    "Info" -> Kafka.Types.KafkaLogInfo
    "Debug" -> Kafka.Types.KafkaLogDebug
    _ -> Kafka.Types.KafkaLogDebug
