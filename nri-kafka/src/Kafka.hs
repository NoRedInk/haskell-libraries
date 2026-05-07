{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Kafka is a module for _writing_ to Kafka
--
-- See Kafka.Worker for the basic building blocks of a CLI app that will poll &
-- process kafka messages
module Kafka
  ( -- * Setup
    Internal.Handler,
    Settings.Settings,
    Settings.decoder,
    handler,

    -- * Creating messages
    Internal.Msg,
    emptyMsg,
    addPayload,
    addKey,

    -- * Sending messags
    Internal.sendAsync,
    Internal.sendSync,

    -- * Reading messages
    topic,
    payload,
    key,
  )
where

import qualified Conduit
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Dict
import qualified Kafka.Internal as Internal
import qualified Kafka.Producer as Producer
import qualified Kafka.Settings as Settings
import qualified Platform
import qualified Prelude

data Details = Details
  { detailsBrokers :: List Text,
    detailsMsg :: Internal.Msg
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

-- | Creates a Kafka-writable message for a topic.
--
-- > msg =
-- >   emptyMsg "groceries"
-- >     |> addPayload "broccoli"
-- >     |> addKey "vegetables"
emptyMsg :: Text -> Internal.Msg
emptyMsg topic' =
  Internal.Msg
    { Internal.topic = Internal.Topic topic',
      Internal.payload = Nothing,
      Internal.key = Nothing
    }

-- Add a payload to a message.
--
-- Message payloads aren't mandatory in Kafka, so using this function really is
-- optional. A counter is an example of an application that doesn't require
-- message payloads. Just knowing an increment event took place would be enough
-- for it to work.
--
-- We ask for JSON decodability to ensure the Kafka worker can later read the message
addPayload :: (Aeson.FromJSON a, Aeson.ToJSON a) => a -> Internal.Msg -> Internal.Msg
addPayload contents msg =
  msg {Internal.payload = (Just (Internal.Encodable contents))}

-- Add a key to a message.
--
-- Kafka divides messages in a topic in different partitions. Kafka workers can
-- collaborate on a topic by each processing messages from a couple of the
-- topic's partitions. Within a partition messages will never overtake each
-- other.
--
-- By default each message is assigned to a random partition. Setting a key on
-- the message gives you more control over this process. Messages with the same
-- key are guaranteed to end up in the same partition.
--
-- Example: if each message is related to a single user and you need to ensure
-- messagse for a user don't overtake each other, you can set the key to be the
-- user's id.
addKey :: Text -> Internal.Msg -> Internal.Msg
addKey key' msg = msg {Internal.key = Just (Internal.Key key')}

record :: Internal.Msg -> Task e Producer.ProducerRecord
record msg = do
  requestId <- Platform.requestId
  Task.succeed
    Producer.ProducerRecord
      { Producer.prTopic =
          Internal.topic msg
            |> Internal.unTopic
            |> Producer.TopicName,
        Producer.prPartition = Producer.UnassignedPartition,
        Producer.prKey =
          Maybe.map
            (Data.Text.Encoding.encodeUtf8 << Internal.unKey)
            (Internal.key msg),
        Producer.prValue =
          Maybe.map
            ( \payload' ->
                Internal.MsgWithMetaData
                  { Internal.metaData =
                      Internal.MetaData
                        { Internal.requestId
                        },
                    Internal.value = payload'
                  }
                  |> Aeson.encode
                  |> ByteString.Lazy.toStrict
            )
            (Internal.payload msg)
      }

-- | The topic of a message. This function might sometimes be useful in tests.
topic :: Internal.Msg -> Text
topic msg = Internal.unTopic (Internal.topic msg)

-- | The payload of a message. This function might sometimes be useful in tests.
payload :: (Aeson.FromJSON a) => Internal.Msg -> Maybe a
payload msg =
  Internal.payload msg
    |> Maybe.andThen (Aeson.decode << Aeson.encode)

-- | The key of a message. This function might sometimes be useful in tests.
key :: Internal.Msg -> Maybe Text
key msg = Maybe.map Internal.unKey (Internal.key msg)

-- | Function for creating a Kafka handler.
--
-- See 'Kafka.Settings' for potential customizations.
handler :: Settings.Settings -> Conduit.Acquire Internal.Handler
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

-- | Drains librdkafka's event queue (delivery reports, errors, stats) so
-- per-message delivery callbacks fire promptly. 'Producer.flushProducer' is
-- internally a loop over @rd_kafka_poll(rk, 100)@ that exits when the
-- outbound queue is empty; each underlying poll blocks the OS thread up to
-- 100ms but returns immediately when an event arrives, so latency-sensitive
-- 'sendSync' callers wake on event rather than on a fixed clock tick. The
-- previous implementation used a 100ms threadDelay between non-blocking
-- drains, which added up to a 100ms (~50ms mean) wait per sync send.
--
-- We can't import 'Kafka.Internal.Shared.pollEvents' directly to pass our own
-- timeout because that module is hidden in hw-kafka-client 4.x.
-- 'flushProducer' is the closest exposed alternative; despite its name and
-- the @closeProducer = flushProducer@ alias, it does not transition the
-- producer to a closed state — it just flushes. It is safe to call
-- repeatedly on a live producer.
--
-- Shutdown: this loop is cancelled by 'Async.race_' when the terminator
-- TMVar is signalled. GHC can't deliver the async exception while the
-- thread is parked in 'safe' FFI, but 'flushProducer' is not a single FFI
-- call — it's a Haskell loop calling @rd_kafka_poll(rk, 100)@. Each poll
-- returns within 100ms (when idle) or immediately (on event), and control
-- briefly returns to Haskell between iterations, where any pending async
-- exception is delivered and kills the thread. So shutdown is bounded by
-- ~100ms even in the worst case (idle producer, no events flowing). The
-- 'Async.race_' cleanup itself does not wait for the loser to finish dying,
-- so we don't deadlock the release chain.
pollEvents :: Producer.KafkaProducer -> Prelude.IO ()
pollEvents producer = do
  Producer.flushProducer producer
  pollEvents producer

mkHandler :: Settings.Settings -> Producer.KafkaProducer -> Prelude.IO Internal.Handler
mkHandler settings producer = do
  doAnything <- Platform.doAnythingHandler
  Prelude.pure
    Internal.Handler
      { Internal.sendAsync = \onDeliveryCallback msg' ->
          Platform.tracingSpan "Async send Kafka messages" <| do
            let details = Details (List.map Producer.unBrokerAddress (Settings.brokerAddresses settings)) msg'
            Platform.setTracingSpanDetails details
            sendHelperAsync producer doAnything onDeliveryCallback msg'
              |> Task.mapError Internal.errorToText,
        Internal.sendSync = \msg' ->
          Platform.tracingSpan "Sync send Kafka messages" <| do
            let details = Details (List.map Producer.unBrokerAddress (Settings.brokerAddresses settings)) msg'
            Platform.setTracingSpanDetails details
            terminator <- doSTM doAnything TMVar.newEmptyTMVar
            let onDeliveryCallback = doSTM doAnything (TMVar.putTMVar terminator Terminate)
            sendHelperAsync producer doAnything onDeliveryCallback msg'
              |> Task.mapError Internal.errorToText
            Terminate <- doSTM doAnything (TMVar.readTMVar terminator)
            Task.succeed ()
      }

doSTM :: Platform.DoAnythingHandler -> STM.STM a -> Task e a
doSTM doAnything stm =
  STM.atomically stm
    |> map Ok
    |> Platform.doAnything doAnything

mkProducer :: Settings.Settings -> Prelude.IO Producer.KafkaProducer
mkProducer Settings.Settings {Settings.brokerAddresses, Settings.deliveryTimeout, Settings.logLevel, Settings.batchNumMessages, Settings.compressionCodec} = do
  let properties =
        Producer.brokersList brokerAddresses
          ++ Producer.sendTimeout deliveryTimeout
          ++ Producer.logLevel logLevel
          ++ Producer.compression compressionCodec
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

sendHelperAsync ::
  Producer.KafkaProducer ->
  Platform.DoAnythingHandler ->
  Task Never () ->
  Internal.Msg ->
  Task Internal.Error ()
sendHelperAsync producer doAnything onDeliveryCallback msg' = do
  record' <- record msg'
  Exception.handleAny
    (\exception -> Prelude.pure (Err (Internal.Uncaught exception)))
    ( do
        maybeFailedMessages <-
          Producer.produceMessage'
            producer
            record'
            ( \deliveryReport -> do
                log <- Platform.silentHandler
                Task.perform log <|
                  case deliveryReport of
                    Producer.DeliverySuccess _producerRecord _offset -> onDeliveryCallback
                    _ -> Task.succeed ()
            )
        Prelude.pure <| case maybeFailedMessages of
          Prelude.Right _ -> Ok ()
          Prelude.Left (Producer.ImmediateError failure) ->
            Err (Internal.SendingFailed (record', failure))
    )
    |> Platform.doAnything doAnything
