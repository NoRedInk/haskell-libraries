{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Kafka is a module for _writing_ to Kafka
--
-- See Kafka.Worker for the basic building blocks of a CLI app that will poll &
-- process kafka messages
module Kafka
  ( Handler,
    Error (..),
    Topic (..),
    Key (..),
    Msg,
    Settings.Settings,
    Settings.decoder,
    handler,
    msg,
    sendAsync,
    sendSync,
  )
where

import qualified Conduit
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Dict
import Kafka.Internal as Internal
import qualified Kafka.Producer as Producer
import qualified Kafka.Settings as Settings
import qualified Platform
import qualified Prelude

data Details = Details
  { detailsBrokers :: List Text,
    detailsMsg :: Msg
  }
  deriving (Generic, Show)

instance Aeson.ToJSON Details

instance Platform.TracingSpanDetails Details

newtype DeliveryReportDetails = DeliveryReportDetails
  { deliveryReportProducerRecord :: Text
  }
  deriving (Generic, Show)

instance Aeson.ToJSON DeliveryReportDetails

instance Platform.TracingSpanDetails DeliveryReportDetails

-- | Creates a Kafka-writable message from a topic, key, and JSON-encodable
-- payload
--
-- We ask for JSON decodability to ensure the Kafka worker can later read the message
msg :: (Aeson.FromJSON a, Aeson.ToJSON a) => Topic -> Key -> a -> Msg
msg topic key contents = Msg topic key (Encodable contents)

record :: Msg -> Task e Producer.ProducerRecord
record Msg {topic, key, payload} = do
  requestId <- Platform.requestId
  Task.succeed
    Producer.ProducerRecord
      { Producer.prTopic = Producer.TopicName (unTopic topic),
        Producer.prPartition = Producer.UnassignedPartition,
        Producer.prKey = Just (Data.Text.Encoding.encodeUtf8 (unKey key)),
        Producer.prValue =
          Internal.MsgWithMetaData
            { Internal.metaData =
                Internal.MetaData
                  { Internal.requestId
                  },
              Internal.value = payload
            }
            |> Aeson.encode
            |> ByteString.Lazy.toStrict
            |> Just
      }

-- | Function for creating a Kafka handler.
--
-- See 'Kafka.Settings' for potential customizations.
handler :: Settings.Settings -> Conduit.Acquire Handler
handler settings = do
  producer <- Conduit.mkAcquire (mkProducer settings) Producer.closeProducer
  _ <- Conduit.mkAcquire (startPollEventLoop producer) (\terminator -> STM.atomically (TMVar.putTMVar terminator Terminate))
  liftIO (mkHandler settings producer)

data Terminate = Terminate

-- | By default events only get polled right before sending a record to kafka.
-- This means that the deliveryCallback only gets fired on the next call to produceMessage'.
-- We want to be informed about delivery status as soon as possible though.
startPollEventLoop :: Producer.KafkaProducer -> Prelude.IO (TMVar.TMVar b)
startPollEventLoop producer = do
  terminator <- STM.atomically TMVar.newEmptyTMVar
  _ <-
    Async.race_
      (pollEvents producer)
      (STM.atomically <| TMVar.readTMVar terminator)
      |> Async.async
  Prelude.pure terminator

-- | We use a little trick here to poll events, by sending an empty message batch.
-- This will call the internal pollEvent function in hw-kafka-client.
pollEvents :: Producer.KafkaProducer -> Prelude.IO ()
pollEvents producer = do
  Producer.produceMessageBatch producer []
    |> map (\_ -> ())
  Control.Concurrent.threadDelay 100_000 {- 100ms -}
  pollEvents producer

-- |
mkHandler :: Settings.Settings -> Producer.KafkaProducer -> Prelude.IO Handler
mkHandler settings producer = do
  doAnything <- Platform.doAnythingHandler
  Prelude.pure
    Handler
      { sendAsync = \onDeliveryCallback msg' ->
          Platform.tracingSpan "Async send Kafka messages" <| do
            let details = Details (List.map Producer.unBrokerAddress (Settings.brokerAddresses settings)) msg'
            Platform.setTracingSpanDetails details
            sendHelperAsync producer doAnything onDeliveryCallback msg',
        sendSync = \msg' ->
          Platform.tracingSpan "Sync send Kafka messages" <| do
            let details = Details (List.map Producer.unBrokerAddress (Settings.brokerAddresses settings)) msg'
            Platform.setTracingSpanDetails details
            terminator <- doSTM doAnything TMVar.newEmptyTMVar
            let onDeliveryCallback = doSTM doAnything (TMVar.putTMVar terminator Terminate)
            sendHelperAsync producer doAnything onDeliveryCallback msg'
            Terminate <- doSTM doAnything (TMVar.readTMVar terminator)
            Task.succeed ()
      }

doSTM :: Platform.DoAnythingHandler -> STM.STM a -> Task e a
doSTM doAnything stm =
  STM.atomically stm
    |> map Ok
    |> Platform.doAnything doAnything

mkProducer :: Settings.Settings -> Prelude.IO Producer.KafkaProducer
mkProducer Settings.Settings {Settings.brokerAddresses, Settings.deliveryTimeout, Settings.logLevel, Settings.batchNumMessages} = do
  let properties =
        Producer.brokersList brokerAddresses
          ++ Producer.sendTimeout deliveryTimeout
          ++ Producer.logLevel logLevel
          ++ Producer.compression Producer.Snappy
          ++ Producer.extraProps
            ( Dict.fromList
                [ ( "batch.num.messages",
                    batchNumMessages
                      |> Settings.unBatchNumMessages
                      |> Text.fromInt
                  ),
                  -- Enable idemptent producers
                  -- See https://www.cloudkarafka.com/blog/apache-kafka-idempotent-producer-avoiding-message-duplication.html for reference
                  ("enable.idempotence", "true"),
                  ("acks", "all")
                ]
            )
  eitherProducer <- Producer.newProducer properties
  case eitherProducer of
    Prelude.Left err ->
      -- We create the handler as part of starting the application. Throwing
      -- means that if there's a problem with the settings the application will
      -- fail immediately upon start. It won't result in runtime errors during
      -- operation.
      Exception.throwIO err
    Prelude.Right producer ->
      Prelude.pure producer

sendHelperAsync :: Producer.KafkaProducer -> Platform.DoAnythingHandler -> Task Never () -> Msg -> Task Error ()
sendHelperAsync producer doAnything onDeliveryCallback msg' = do
  record' <- record msg'
  Exception.handleAny
    (\exception -> Prelude.pure (Err (Uncaught exception)))
    ( do
        maybeFailedMessages <-
          Producer.produceMessage'
            producer
            record'
            ( \deliveryReport -> do
                log <- Platform.silentHandler
                Task.perform log
                  <| case deliveryReport of
                    Producer.DeliverySuccess _producerRecord _offset -> onDeliveryCallback
                    _ -> Task.succeed ()
            )
        Prelude.pure <| case maybeFailedMessages of
          Prelude.Right _ -> Ok ()
          Prelude.Left (Producer.ImmediateError failure) -> Err (SendingFailed (record', failure))
    )
    |> Platform.doAnything doAnything
