{-# LANGUAGE GADTs #-}

module Kafka.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
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
  = SendingFailed (Producer.ProducerRecord, Producer.KafkaError)
  | Uncaught Exception.SomeException
  deriving (Show)

errorToText :: Error -> Text
errorToText err = Text.fromList (Prelude.show err)

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

type StatsCallback = (ByteString -> Task Text ())
