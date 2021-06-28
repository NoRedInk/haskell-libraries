{-# LANGUAGE GADTs #-}

module Kafka.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Kafka.Producer as Producer
import qualified Prelude

data Handler = Handler
  { sendAsync :: OnDelivery -> Msg -> Task Error (),
    sendSync :: Msg -> Task Error ()
  }

type OnDelivery = Task Never ()

data Msg = Msg
  { topic :: Topic,
    key :: Key,
    payload :: Encodable
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

data Error
  = SendingFailed (Producer.ProducerRecord, Producer.KafkaError)
  | Uncaught Exception.SomeException
  deriving (Show)

newtype Topic = Topic {unTopic :: Text} deriving (Aeson.ToJSON, Show)

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
