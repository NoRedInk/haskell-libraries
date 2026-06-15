{-# LANGUAGE GADTs #-}

module Kafka.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Kafka.Producer as Producer
import qualified Prelude

-- | A handler for writing to Kafka
data Handler = Handler
  { -- | sends messages asynchronously with to Kafka
    --
    -- This is the recommended approach for high throughput. The C++ library
    -- behind hte scenes, librdkafka, will batch messages together.
    sendAsync :: Task Never () -> Msg -> Task Text (),
    -- | sends messages synchronously with to Kafka
    --
    -- This can have a large negative impact on throughput. Use sparingly!
    sendSync :: Msg -> Task Text ()
  }

-- | A message that can be written to Kafka
data Msg = Msg
  { topic :: Topic,
    key :: Maybe Key,
    payload :: Maybe Encodable
  }
  deriving (Generic, Show)

instance Aeson.ToJSON Msg

data Encodable where
  Encodable :: (Aeson.FromJSON a, Aeson.ToJSON a) => a -> Encodable

instance Aeson.ToJSON Encodable where
  toJSON (Encodable x) = Aeson.toJSON x
  toEncoding (Encodable x) = Aeson.toEncoding x

instance Aeson.FromJSON Encodable where
  parseJSON x = do
    val <- Aeson.parseJSON x
    Prelude.pure (Encodable (val :: Aeson.Value))

instance Show Encodable where
  show (Encodable x) = Prelude.show (Aeson.toJSON x)

-- | Errors.
-- If you experience an 'Uncaught' exception, please wrap it here type here!
data Error
  = -- | A message could not be enqueued for sending. This is a pre-flight
    -- failure surfaced synchronously by librdkafka (e.g. the local producer
    -- queue is full, or the message exceeds the configured maximum size); the
    -- message was never handed to the broker.
    SendingFailed (Producer.ProducerRecord, Producer.KafkaError)
  | -- | A message was enqueued and handed to the broker, but delivery
    -- ultimately failed (e.g. @delivery.timeout.ms@ exceeded, retries
    -- exhausted, a non-retriable broker error, or no available partition
    -- leader). This is reported asynchronously through the delivery callback,
    -- after a successful enqueue.
    DeliveryFailed (Producer.ProducerRecord, Producer.KafkaError)
  | -- | librdkafka invoked the delivery callback to report a failure but did
    -- not attach the original message, so there is no 'Producer.ProducerRecord'
    -- to report — only the 'Producer.KafkaError'. In hw-kafka-client this is
    -- the @NoMessageError@ delivery report, which is produced when the C
    -- delivery callback fires with a null message pointer and the error code is
    -- read from @errno@. It is an exceptional, library-level condition rather
    -- than a normal per-message broker rejection (those arrive as
    -- 'DeliveryFailed', which carries the record).
    NoMessageDelivered Producer.KafkaError
  | Uncaught Exception.SomeException
  deriving (Show)

errorToText :: Error -> Text
errorToText err = Text.fromList (Prelude.show err)

-- | Translate a librdkafka 'Producer.DeliveryReport' into a 'Result' the
-- caller can act on: a success carries no payload, while the two failure
-- reports map to the corresponding 'Error' constructors. Kept pure (no
-- 'Producer.KafkaProducer', no IO) so the dispatch can be unit-tested without
-- a running broker.
deliveryReportToResult :: Producer.DeliveryReport -> Result Error ()
deliveryReportToResult deliveryReport =
  case deliveryReport of
    Producer.DeliverySuccess _record _offset -> Ok ()
    Producer.DeliveryFailure record kafkaError -> Err (DeliveryFailed (record, kafkaError))
    Producer.NoMessageError kafkaError -> Err (NoMessageDelivered kafkaError)

-- | A kafka topic
newtype Topic = Topic {unTopic :: Text} deriving (Aeson.ToJSON, Show)

-- | A kafka key
newtype Key = Key {unKey :: Text} deriving (Show, Aeson.ToJSON, Eq, Ord)

data MsgWithMetaData = MsgWithMetaData
  { metaData :: MetaData,
    value :: Encodable
  }
  deriving (Generic)

instance Aeson.ToJSON MsgWithMetaData

instance Aeson.FromJSON MsgWithMetaData

newtype MetaData = MetaData
  { requestId :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON MetaData

instance Aeson.FromJSON MetaData

newtype Offset = Offset Int
