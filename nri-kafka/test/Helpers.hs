-- | Tests, exposed so we can run them against GHCID
module Helpers
  ( spawnWorker,
    stopWorker,
    test,
    sendSync,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Dict
import qualified Environment
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Internal as Internal
import qualified Kafka.Producer as Producer
import qualified Kafka.Settings as Settings
import qualified Kafka.Worker as Worker
import qualified Kafka.Worker.Internal
import qualified Kafka.Worker.Settings as Worker.Settings
import qualified Platform
import qualified Test
import qualified Prelude

-- | A reference to a Kafka worker. In practice, each worker will be running
-- in its own container. For testing purposes, we sometimes launch multiple
-- workers in a single test thread.
newtype Worker = Worker (Async.Async ())

data TestHandler = TestHandler
  { producer :: Producer.KafkaProducer,
    terminator :: MVar.MVar (),
    doAnything :: Platform.DoAnythingHandler
  }

returnWhenTerminating :: TestHandler -> Prelude.IO ()
returnWhenTerminating TestHandler {terminator} = MVar.readMVar terminator

terminate :: TestHandler -> Prelude.IO ()
terminate TestHandler {terminator} = MVar.putMVar terminator ()

-- | Spawns a worker, guarded by the terminator 🦾
spawnWorker ::
  (Aeson.ToJSON msg, Aeson.FromJSON msg) =>
  TestHandler ->
  Internal.Topic ->
  (msg -> STM.STM ()) ->
  Expect.Expectation' Worker
spawnWorker handler' topic callback =
  Expect.fromIO <| do
    settings <-
      case Environment.decodeDefaults Worker.Settings.decoder of
        Ok settings' -> Prelude.pure settings'
        Err err -> Prelude.fail (Text.toList err)
    async <-
      Kafka.Worker.Internal.processWithoutShutdownEnsurance
        settings
        (Consumer.ConsumerGroupId "group")
        ( Worker.subscription
            (Internal.unTopic topic)
            ( \msg -> do
                callback msg
                  |> STM.atomically
                  |> map Ok
                  |> Platform.doAnything (doAnything handler')
            )
        )
        |> Async.race_ (returnWhenTerminating handler')
        |> Async.async
    Async.link async
    Prelude.pure (Worker async)

-- | Stops a single worker
stopWorker :: Worker -> Expect.Expectation
stopWorker (Worker async) =
  Async.cancel async
    |> Expect.fromIO

-- | creates a test handler
testHandler :: Settings.Settings -> Prelude.IO TestHandler
testHandler Settings.Settings {Settings.brokerAddresses, Settings.deliveryTimeout, Settings.logLevel, Settings.batchNumMessages} = do
  doAnything <- Platform.doAnythingHandler
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
    Prelude.Right producer -> do
      terminator <- MVar.newEmptyMVar
      Prelude.pure TestHandler {producer, doAnything, terminator}

-- | puts a message synchronously onto a topic-partition
sendSync :: Aeson.ToJSON a => TestHandler -> Internal.Topic -> Int -> a -> Expect.Expectation
sendSync handler topicName partitionId msg' =
  Platform.tracingSpan
    "Sync send Kafka messages"
    ( sendHelperSync
        (producer handler)
        (doAnything handler)
        topicName
        partitionId
        (Aeson.toJSON msg')
    )
    |> Expect.succeeds

sendHelperSync ::
  Producer.KafkaProducer ->
  Platform.DoAnythingHandler ->
  Internal.Topic ->
  Int ->
  Aeson.Value ->
  Task Text ()
sendHelperSync producer doAnything topicName partitionId msg' =
  Exception.handleAny
    (\exception -> Prelude.pure (Err (Debug.toString exception)))
    ( do
        res <- Producer.produceMessage producer (record topicName partitionId msg')
        case res of
          Nothing -> Prelude.pure ()
          Just err -> Exception.throwIO err
        -- by flushing the producer immediately after producing a message,
        -- we make this function synchronous. Without flush it's by default asynchronous.
        Producer.flushProducer producer
        Prelude.pure (Ok ())
    )
    |> Platform.doAnything doAnything

record :: Internal.Topic -> Int -> Aeson.Value -> Producer.ProducerRecord
record topicName partitionId val =
  Producer.ProducerRecord
    { Producer.prTopic = Producer.TopicName (Internal.unTopic topicName),
      Producer.prPartition = Producer.SpecifiedPartition (Prelude.fromIntegral partitionId),
      Producer.prKey = Nothing,
      Producer.prValue =
        Internal.MsgWithMetaData
          { Internal.metaData =
              Internal.MetaData
                { Internal.requestId = "test-request"
                },
            Internal.value = Internal.Encodable val
          }
          |> Aeson.encode
          |> Data.ByteString.Lazy.toStrict
          |> Just
    }

-- | test helper, that yields a new @Kafka.Topic@ and @TestHandler@
test ::
  Stack.HasCallStack =>
  Text ->
  ((Internal.Topic, TestHandler) -> Expect.Expectation) ->
  Test.Test
test description body =
  Stack.withFrozenCallStack Test.test description <| \_ -> do
    doAnything <- Expect.fromIO Platform.doAnythingHandler
    Expect.around
      ( \task' ->
          Platform.bracketWithError
            ( -- create handler
              Platform.doAnything doAnything
                <| case Environment.decodeDefaults Settings.decoder of
                  Ok settings ->
                    map
                      Ok
                      ( testHandler
                          settings
                            { Settings.batchNumMessages = Settings.exampleBatchNumMessages
                            }
                      )
                  Err err -> Debug.todo ("Failed to to decode worker settings" ++ err)
            )
            ( -- terminate all workers hanging around after the test is over
              \_maybeErr handler' ->
                terminate handler'
                  |> map Ok
                  |> Platform.doAnything doAnything
            )
            ( -- yield the test
              \handler' -> do
                uuid <-
                  map Data.UUID.toText Data.UUID.V4.nextRandom
                    |> map Ok
                    |> Platform.doAnything doAnything
                let topic = Internal.Topic uuid
                task' (topic, handler')
            )
      )
      body
