{-# LANGUAGE GADTs #-}

module Kafka.Worker.Internal where

import qualified Conduit
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Dict
import qualified GHC.Clock
import qualified Kafka
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Metadata
import qualified Kafka.Worker.Analytics as Analytics
import qualified Kafka.Worker.Fetcher as Fetcher
import qualified Kafka.Worker.Partition as Partition
import qualified Kafka.Worker.Settings as Settings
import qualified Kafka.Worker.Stopping as Stopping
import qualified Observability
import qualified Set
import qualified System.Environment
import qualified System.Exit
import qualified System.Posix.Process
import qualified System.Posix.Signals as Signals
import qualified Prelude

-- | Alias for a TopicName and PartitionId, something every message will have
type PartitionKey = (Consumer.TopicName, Consumer.PartitionId)

type AllPartitions = TVar.TVar (Dict.Dict PartitionKey Partition.Partition)

data Rebalance = Assign | Revoking | Revoked deriving (Show)

type RebalanceInfo = TVar.TVar (Dict.Dict PartitionKey (Rebalance, Float))

data State = State
  { partitions :: AllPartitions,
    -- | When we receive a shutdown signal this variable will change. Threads
    -- should stop processing new messages to allow the process to shut down.
    stopping :: Stopping.Stopping,
    analytics :: Analytics.Analytics,
    rebalanceInfo :: RebalanceInfo
  }

-- | The topics this worker should subscribe too. At the moment this library
-- only supports subscribing to a single topic.
data TopicSubscription = TopicSubscription
  { topic :: Kafka.Topic,
    onMessage :: Partition.MessageCallback,
    offsetSource :: OffsetSource
  }

-- | Params needed to write / read offsets to another data store
data OffsetParams = OffsetParams
  { -- | The name of the topic this message is from.
    topicName :: Text,
    -- | The topic partition this message is from.
    partitionId :: Int
  }

-- | Create a subscription for a topic.
--
-- > main :: IO ()
-- > main = do
-- >   settings <- Environment.decode decoder
-- >   let subscription =
-- >         subscription
-- >           "the-topic"
-- >           (\msg -> Debug.todo "Process your message here!")
-- >   process settings subscription
subscription ::
  (Aeson.FromJSON msg, Aeson.ToJSON msg) =>
  Text ->
  (msg -> Task Text ()) ->
  TopicSubscription
subscription topic callback =
  TopicSubscription
    { topic = Kafka.Topic topic,
      onMessage =
        Partition.MessageCallback
          ( \_ msg -> do
              callback msg
              Task.succeed Partition.NoSeek
          ),
      offsetSource = InKafka
    }

-- | Create a subscription for a topic and manage offsets for that topic
-- yourself.
--
-- You'll need to tell Kafka where it can read starting offsets. When passed
-- a message you can also tell Kafka to seek to a different offset.
--
-- > main :: IO ()
-- > main = do
-- >   settings <- Environment.decode decoder
-- >   let subscription =
-- >         subscriptionManageOwnOffsets
-- >           "the-topic"
-- >           (sql "SELECT partition_id, offset FROM offsets")
-- >           (\msg -> Debug.todo "Process your message here!")
-- >   process settings subscription
subscriptionManageOwnOffsets ::
  (Aeson.FromJSON msg, Aeson.ToJSON msg) =>
  Text ->
  ([OffsetParams] -> Task Text (Dict.Dict OffsetParams Int)) ->
  (OffsetParams -> Int -> msg -> Task Text Partition.SeekCmd) ->
  TopicSubscription
subscriptionManageOwnOffsets topic fetchOffsets callback =
  TopicSubscription
    { topic = Kafka.Topic topic,
      onMessage =
        Partition.MessageCallback
          ( \record msg -> do
              let offsetParams =
                    OffsetParams
                      { topicName =
                          Consumer.crTopic record
                            |> Consumer.unTopicName,
                        partitionId =
                          Consumer.crPartition record
                            |> Consumer.unPartitionId
                            |> Prelude.fromIntegral
                      }
              callback offsetParams (Consumer.unOffset (Consumer.crOffset record)) msg
          ),
      offsetSource =
        Elsewhere
          ( \partitionKeys -> do
              offsets <- fetchOffsets (List.map toOffsetParams partitionKeys)
              Dict.toList offsets
                |> List.map (Tuple.mapFirst toPartitionKey)
                |> Task.succeed
          )
    }
  where
    toOffsetParams :: PartitionKey -> OffsetParams
    toOffsetParams (Consumer.TopicName topicName, Consumer.PartitionId partitionId) =
      OffsetParams topicName (Prelude.fromIntegral partitionId)

    toPartitionKey :: OffsetParams -> PartitionKey
    toPartitionKey (OffsetParams topicName partitionId) =
      (Consumer.TopicName topicName, Consumer.PartitionId (Prelude.fromIntegral partitionId))

-- | This determines how a worker that was just assigned a partition should
-- decide at which message offset to continue processing.
data OffsetSource where
  -- | Use Kafka's own offset storage mechanism.
  InKafka :: OffsetSource
  -- | Store offsets somewhere else, in which case you need to provide a
  -- function that the worker can use to load initial offsets. Storing offsets
  -- outside Kafka can be used to implement exactly-once-delivery schemes.
  -- Using this requires the message itself to commit the offset.
  Elsewhere ::
    ([PartitionKey] -> Task Text [(PartitionKey, Int)]) ->
    OffsetSource

-- | Starts the kafka worker handling messages.
process :: Settings.Settings -> Text -> TopicSubscription -> Prelude.IO ()
process settings groupIdText topicSubscriptions = do
  processWithoutShutdownEnsurance settings (Consumer.ConsumerGroupId groupIdText) topicSubscriptions
  -- Start an ensurance policy to make sure we exit in 5 seconds. We've seen
  -- cases where our graceful shutdown seems to hang, resulting in a worker
  -- that's not doing anything. We should try to fix those failures, but for the
  -- ones that remain this is our fallback.
  --
  -- Running it using `Async.async` makes it so we won't wait for this thread to
  -- complete. If the regular shutdown completes before this thread is done we
  -- will exit early.
  _ <-
    Async.async <| do
      Control.Concurrent.threadDelay 5_000_000 {- 5 seconds -}
      Prelude.putStrLn "Something is holding up shutdown. Going to die ungracefully now."
      System.Posix.Process.exitImmediately (System.Exit.ExitFailure 1)
  Prelude.pure ()

-- | Like `process`, but doesn't exit the current process by itself. This risks
-- leaving zombie processes when used in production but is safer in tests, where
-- the worker shares the OS process with other test code and the test runner.
processWithoutShutdownEnsurance :: Settings.Settings -> Consumer.ConsumerGroupId -> TopicSubscription -> Prelude.IO ()
processWithoutShutdownEnsurance settings groupId topicSubscriptions = do
  let TopicSubscription {onMessage, topic, offsetSource} = topicSubscriptions
  state <- initState
  onQuitSignal (Stopping.stopTakingRequests (stopping state))
  Conduit.withAcquire (Observability.handler (Settings.observability settings)) <| \observabilityHandler -> do
    Exception.bracketWithError
      (createConsumer settings groupId observabilityHandler offsetSource onMessage topic state)
      (cleanUp observabilityHandler (rebalanceInfo state) (stopping state))
      (runThreads settings state)

initState :: Prelude.IO State
initState = do
  stopping <- Stopping.init
  partitions <- TVar.newTVarIO Dict.empty
  analytics <- Analytics.init (map Dict.size (TVar.readTVarIO partitions))
  rebalanceInfo <- TVar.newTVarIO Dict.empty
  Prelude.pure
    State
      { partitions,
        stopping,
        analytics,
        rebalanceInfo
      }

-- | Goes to Kafka and registers a consumer on the Topic
-- Kafka whould give the consumer some number of partitions to be responsible for
createConsumer ::
  Settings.Settings ->
  Consumer.ConsumerGroupId ->
  Observability.Handler ->
  OffsetSource ->
  Partition.MessageCallback ->
  Kafka.Topic ->
  State ->
  Prelude.IO Consumer.KafkaConsumer
createConsumer
  Settings.Settings
    { Settings.brokerAddresses,
      Settings.logLevel,
      Settings.maxPollIntervalMs,
      Settings.onProcessMessageSkip
    }
  groupId
  observability
  offsetSource
  callback
  topic
  state = do
    let rebalance =
          rebalanceCallback
            onProcessMessageSkip
            observability
            callback
            offsetSource
            state
    let properties =
          Consumer.brokersList
            brokerAddresses
            ++ Consumer.groupId groupId
            ++ Consumer.noAutoCommit
            ++ Consumer.logLevel logLevel
            ++ Consumer.setCallback (Consumer.rebalanceCallback rebalance)
            ++ Consumer.compression Consumer.Snappy
            ++ Consumer.extraProps
              ( Dict.fromList
                  [("max.poll.interval.ms", Text.fromInt (Settings.unMaxPollIntervalMs maxPollIntervalMs))]
              )
    let subscription' =
          Consumer.topics [Consumer.TopicName (Kafka.unTopic topic)]
            ++ Consumer.offsetReset Consumer.Earliest
    eitherConsumer <- Consumer.newConsumer properties subscription'
    case eitherConsumer of
      Prelude.Left err ->
        -- We create the worker as part of starting the application. Throwing
        -- means that if there's a problem with the settings the application will
        -- fail immediately upon start. It won't result in runtime errors during
        -- operation.
        Exception.throwIO err
      Prelude.Right consumer ->
        Prelude.pure consumer

-- | Triggered when a rebalance happens, due to workers going offline or coming
-- online. This typically happens when we're scaling up or down, or when a
-- worker dies because of an unexpected exception.
--
-- The following provides reasonable documentation for these events. It's Java
-- documentation, but it should apply to what's happening here fairly well (the
-- `hw-kafka-client` library we use is a wrapper over librdkafka).
-- https://docs.confluent.io/2.0.0/clients/librdkafka/classRdKafka_1_1RebalanceCb.html#a490a91c52724382a72380af621958741
rebalanceCallback ::
  Settings.SkipOrNot ->
  Observability.Handler ->
  Partition.MessageCallback ->
  OffsetSource ->
  State ->
  Consumer.KafkaConsumer ->
  Consumer.RebalanceEvent ->
  Prelude.IO ()
rebalanceCallback skipOrNot observability callback offsetSource state consumer rebalanceEvent = do
  now <- GHC.Clock.getMonotonicTime
  Analytics.updateTimeOfLastRebalance now (analytics state)
  case rebalanceEvent of
    Consumer.RebalanceBeforeAssign newPartitions -> do
      keysWithOffsets <-
        case offsetSource of
          InKafka ->
            Prelude.pure <| List.map (\partitionKey -> (partitionKey, Partition.ToKafka)) newPartitions
          Elsewhere fetch -> do
            log <- Platform.silentHandler
            fetchResult <- Task.attempt log (fetch newPartitions)
            case fetchResult of
              Err err -> Exception.throwString (Text.toList err)
              Ok fetched -> do
                let storedOffsets =
                      List.map (Tuple.mapSecond Partition.Elsewhere) fetched
                        |> Dict.fromList
                let storedKeys =
                      fetched
                        |> List.map Tuple.first
                        |> Set.fromList
                let missingPartitions = Set.diff (Set.fromList newPartitions) storedKeys
                -- NOTE: we fallback to the end of the queue so we can reset
                -- offsets in staging.
                -- in production, we should never hit this code. Perhaps we
                -- should explicitly guard against it?
                waterMarkInfo :: Prelude.Either Consumer.KafkaError (List Kafka.Metadata.WatermarkOffsets) <-
                  missingPartitions
                    |> Set.toList
                    |> Prelude.traverse
                      ( \(topicName, partitionId) ->
                          Kafka.Metadata.partitionWatermarkOffsets
                            consumer
                            (Consumer.Timeout 5000 {- 5 seconds -})
                            topicName
                            partitionId
                      )
                    |> map Prelude.sequence
                case waterMarkInfo of
                  Prelude.Left err -> Exception.throwIO err
                  Prelude.Right waterMarks ->
                    let fallbackOffsets =
                          waterMarks
                            |> List.map
                              ( \Kafka.Metadata.WatermarkOffsets {Kafka.Metadata.woTopicName, Kafka.Metadata.woPartitionId, Kafka.Metadata.woHighWatermark} ->
                                  ((woTopicName, woPartitionId), Partition.Elsewhere (Consumer.unOffset woHighWatermark))
                              )
                            |> Dict.fromList
                     in Dict.union storedOffsets fallbackOffsets
                          |> Dict.toList
                          |> Prelude.pure
      -- We are being assigned new partitions. Lets prepare threads for the new
      -- messages we're about to receive. Lib-rdkafka hasn't yet started to send
      -- messages, so the queues exist but should be idle
      keysWithOffsets
        |> Prelude.traverse
          ( \(partitionKey, offset) -> do
              initPartition
                skipOrNot
                offset
                observability
                consumer
                callback
                state
                partitionKey
              STM.atomically
                <| TVar.modifyTVar' (rebalanceInfo state) (Dict.insert partitionKey (Assign, now))
          )
        |> map (\_ -> ())
    Consumer.RebalanceAssign _ -> Prelude.pure ()
    Consumer.RebalanceBeforeRevoke revokedPartitions -> do
      -- This callback is intended to allow us to finish committing any
      -- ongoing work before rebalancing. This avoids processing messages twice.
      --
      -- When the callback returns, the rebalancing occurs.
      --
      -- We will tell workers to wrap up current work, and wait for them to
      -- complete before rebalancing.
      --
      -- First: mark partitions as Stopping. Worker threads will stop.
      _ <-
        revokedPartitions
          |> Prelude.traverse
            ( \partitionKey -> do
                STM.atomically <| do
                  partitions <- TVar.readTVar (partitions state)
                  case Dict.get partitionKey partitions of
                    Nothing -> Prelude.pure ()
                    Just partition -> Partition.revoke partition
                STM.atomically <| TVar.modifyTVar' (rebalanceInfo state) (Dict.insert partitionKey (Revoking, now))
            )
      -- Second: Wait for the workers to stop working.
      -- (They will only remove themselves from the partitions when done processing
      -- any ongoing message, so checking non-existence should suffice.)
      _ <-
        revokedPartitions
          |> Prelude.traverse
            ( \partitionKey -> do
                STM.atomically <| do
                  partitions <- TVar.readTVar (partitions state)
                  case Dict.get partitionKey partitions of
                    Nothing -> Prelude.pure ()
                    Just _ -> do
                      -- we cannot block on work happening in the FETCHER
                      -- but here, we're blocking work happening in the WORKER
                      -- which is fine!
                      STM.retry
                STM.atomically <| TVar.modifyTVar' (rebalanceInfo state) (Dict.insert partitionKey (Revoked, now))
            )
      -- Now workers have stopped and it's safe to rebalance.
      -- Returning from this callback starts the rebalance.
      Prelude.pure ()
    Consumer.RebalanceRevoke _revokedPartitions -> do
      Prelude.pure ()

-- | Disconnects our Consumer / yields back partitions on quit / node shutdown
cleanUp :: Observability.Handler -> RebalanceInfo -> Stopping.Stopping -> Maybe Exception.SomeException -> Consumer.KafkaConsumer -> Prelude.IO ()
cleanUp observabilityHandler rebalanceInfo stopping maybeException consumer = do
  Prelude.putStrLn "Cleaning up"
  _ <- Consumer.closeConsumer consumer
  -- ensure workers shut down
  Stopping.stopTakingRequests stopping
  requestId <- map Data.UUID.toText Data.UUID.V4.nextRandom
  -- at some point, k8s should report system crashes. In the mean time, we'll do it.
  Platform.rootTracingSpanIO
    requestId
    (Observability.report observabilityHandler requestId)
    "Kafka consumer shutting down"
    <| \log -> do
      case maybeException of
        Nothing -> Prelude.pure ()
        Just exception -> do
          rebalanceInfo' <- TVar.readTVarIO rebalanceInfo
          Log.error
            "Kafka consumer crashed"
            [ Log.context "triage" ("The consumer should automatically restart. If we see lots of these in a short period of time we should try to figure out what's wrong" :: Text),
              Log.context "error" (Debug.toString exception),
              Log.context "rebalance info" (Debug.toString rebalanceInfo')
            ]
            |> Task.perform log

  writeCrashLogOnError maybeException
  Prelude.putStrLn "Bye!"

-- | Handle crash logging
writeCrashLogOnError :: Maybe Exception.SomeException -> Prelude.IO ()
writeCrashLogOnError maybeException = do
  -- Not using the nri-env-parser lib for this configuration option because it would
  -- require us to run code to make it available. If that code failed it
  -- wouldn't end up in the crash log! Using only `base` functionality allows us
  -- to put the crashlog reporting in the very root of the application.
  crashLogPath <- System.Environment.lookupEnv "CRASHLOG_PATH"
  let crashLog =
        case maybeException of
          Nothing -> "System exited in response to signal"
          Just exception -> Exception.displayException exception
  case crashLogPath of
    Nothing -> Prelude.pure ()
    Just "" -> Prelude.pure ()
    Just path -> Prelude.writeFile path crashLog

-- Adds a partition to our partitions dict. These partitions will be idle until
-- lib-rdkafka actually starts sending us new messages
-- (after the Consumer.rebalanceassign event occurs)
initPartition ::
  Settings.SkipOrNot ->
  Partition.CommitOffsets ->
  Observability.Handler ->
  Consumer.KafkaConsumer ->
  Partition.MessageCallback ->
  State ->
  PartitionKey ->
  Prelude.IO ()
initPartition skipOrNot commitOffset observabilityHandler consumer callback state key = do
  -- # Start worker thread for handling messages in partition.
  Partition.spawnWorkerThread
    skipOrNot
    commitOffset
    observabilityHandler
    (analytics state)
    (stopping state)
    consumer
    callback
    -- startup function
    ( Partition.OnStartup
        ( \partition ->
            STM.atomically <| do
              queues <- TVar.readTVar (partitions state)
              case Dict.get key queues of
                Just _ ->
                  -- We never expect to be asked to create a partition that already exists.
                  -- We expect that revoke has completely removed the partition before
                  -- re-assign.
                  --
                  -- If it happens anyway, it seems safer to crash (and restart) than to
                  -- try continue into the unknown.
                  STM.throwSTM (AskedToInitPartitionThatAlreadyExists key)
                Nothing -> do
                  TVar.writeTVar (partitions state) (Dict.insert key partition queues)
        )
    )
    -- cleanup function
    ( Partition.OnCleanup
        ( do
            -- Remove the partition from the dict to clean up memory
            STM.atomically <| TVar.modifyTVar' (partitions state) (Dict.remove key)
            Prelude.putStrLn ("Stop processing messages for partition: " ++ Prelude.show key)
        )
    )

runThreads ::
  Settings.Settings ->
  State ->
  Consumer.KafkaConsumer ->
  Prelude.IO ()
runThreads settings state consumer = do
  Stopping.runUnlessStopping
    (stopping state)
    ()
    ( Async.race
        (pauseAndAnalyticsLoop (Settings.maxMsgsPerPartitionBufferedLocally settings) consumer state Set.empty)
        (Fetcher.pollingLoop settings (enqueueRecord (partitions state)) (analytics state) consumer)
        |> map (\_ -> ())
    )

data RuntimeExceptions
  = ReceivedMsgNotInAssignedPartitions (Consumer.TopicName, Consumer.PartitionId)
  | AskedToInitPartitionThatAlreadyExists (Consumer.TopicName, Consumer.PartitionId)
  deriving (Show)

instance Exception.Exception RuntimeExceptions

enqueueRecord ::
  AllPartitions ->
  Partition.ConsumerRecord ->
  Prelude.IO Partition.SeekCmd
enqueueRecord partitions record =
  STM.atomically <| do
    let key = (Consumer.crTopic record, Consumer.crPartition record)
    partitions' <- TVar.readTVar partitions
    let maybePartition = Dict.get key partitions'
    case maybePartition of
      Nothing -> STM.throwSTM (ReceivedMsgNotInAssignedPartitions key)
      Just partition -> Partition.append record partition

-- | Intermittently updates
-- - paused partitions to reflect desired state.
-- - analytics, so that the worker node can report up-to-date data to honeycomb
pauseAndAnalyticsLoop ::
  Settings.MaxMsgsPerPartitionBufferedLocally ->
  Consumer.KafkaConsumer ->
  State ->
  Set.Set PartitionKey ->
  Prelude.IO ()
pauseAndAnalyticsLoop maxBufferSize consumer state pausedPartitions = do
  desiredPausedPartitions <- pausedPartitionKeys maxBufferSize (partitions state)
  Analytics.updatePaused (Set.size desiredPausedPartitions) (analytics state)
  let newlyPaused = Set.diff desiredPausedPartitions pausedPartitions
  _ <- Consumer.pausePartitions consumer (Set.toList newlyPaused)
  let newlyResumed = Set.diff pausedPartitions desiredPausedPartitions
  _ <- Consumer.resumePartitions consumer (Set.toList newlyResumed)
  Control.Concurrent.threadDelay 1_000_000 {- 1 second -}
  pauseAndAnalyticsLoop maxBufferSize consumer state desiredPausedPartitions

pausedPartitionKeys :: Settings.MaxMsgsPerPartitionBufferedLocally -> AllPartitions -> Prelude.IO (Set.Set PartitionKey)
pausedPartitionKeys (Settings.MaxMsgsPerPartitionBufferedLocally maxBufferSize) partitions = do
  partitions' <- TVar.readTVarIO partitions
  partitions'
    |> Dict.toList
    |> Prelude.traverse
      ( \(key, partition) -> do
          maybeLen <- Partition.length partition
          Prelude.pure
            <| case maybeLen of
              Nothing -> Nothing
              Just length ->
                if length > maxBufferSize
                  then Just key
                  else Nothing
      )
    |> map (List.filterMap identity >> Set.fromList)

quitSignals :: [Signals.Signal]
quitSignals =
  [ Signals.sigINT, -- ctrl-c
    Signals.sigQUIT, -- ctrl-\ ???
    Signals.sigTERM
  ]

onQuitSignal :: Prelude.IO () -> Prelude.IO ()
onQuitSignal release = do
  let handleQuit signal =
        Signals.installHandler
          signal
          (Signals.Catch release)
          Nothing
  _ <- Prelude.traverse handleQuit quitSignals
  Prelude.pure ()
