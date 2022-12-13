module Kafka.Settings.Internal
  ( Kafka.Types.KafkaLogLevel (..),
    Kafka.Types.KafkaCompressionCodec (..),
    decoderBrokerAddresses,
    decoderKafkaLogLevel,
    decoderCompressionCodec,
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
    ( Environment.enum
        [ ("Emerg", Kafka.Types.KafkaLogEmerg),
          ("Alert", Kafka.Types.KafkaLogAlert),
          ("Crit", Kafka.Types.KafkaLogCrit),
          ("Err", Kafka.Types.KafkaLogErr),
          ("Warning", Kafka.Types.KafkaLogWarning),
          ("Notice", Kafka.Types.KafkaLogNotice),
          ("Info", Kafka.Types.KafkaLogInfo),
          ("Debug", Kafka.Types.KafkaLogDebug)
        ]
    )

decoderCompressionCodec :: Environment.Decoder Kafka.Types.KafkaCompressionCodec
decoderCompressionCodec =
  Environment.variable
    Environment.Variable
      { Environment.name = "KAFKA_COMPRESSION_CODEC",
        Environment.description = "Compression codec used for topics. Supported values are: NoCopmression, Gzip, Snappy and Lz4",
        Environment.defaultValue = "Snappy"
      }
    ( Environment.enum
        [ ("NoCompression", Kafka.Types.NoCompression),
          ("Gzip", Kafka.Types.Gzip),
          ("Snappy", Kafka.Types.Snappy),
          ("Lz4", Kafka.Types.Lz4)
        ]
    )
