{-# LANGUAGE GADTs #-}

module Kafka.Worker.Partition
  ( spawnWorkerThread,
    append,
    length,
    revoke,
    ConsumerRecord,
    Partition,
    MessageCallback (..),
    ProcessResult (..),
    CommitOffsets (..),
    -- just exported for tests
    microSecondsDelayForAttempt,
    OnStartup (OnStartup),
    OnCleanup (OnCleanup),
  )
where

import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified GHC.Clock
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Internal as Internal
import qualified Kafka.Worker.Analytics as Analytics
import qualified Kafka.Worker.Settings as Settings
import qualified Kafka.Worker.Stopping as Stopping
import qualified Log.Kafka
import qualified Observability
import qualified Platform
import qualified Prelude

data WorkerError e
  = WorkerCallbackFailed e
  | WorkerCallbackThrew Exception.SomeException
  | MsgDecodingFailed Text
  | SeekFailed Consumer.KafkaError
  deriving (Show)

data State = State
  { analytics :: Analytics.Analytics,
    stopping :: Stopping.Stopping,
    partition :: Partition
  }

type ConsumerRecord = Consumer.ConsumerRecord (Maybe ByteString.ByteString) (Maybe ByteString.ByteString)

newtype ProcessAttemptsCount = ProcessAttemptsCount Int
  deriving (Num)

newtype Partition = Partition (TVar.TVar Backlog)

data ProcessResult
  = Success
  | ExpectedOffset Int

-- for each partition, we keep a local backlog of messages we've read from Kafka
-- that are not yet processed
data Backlog
  = AwaitingSeekTo Int
  | -- | Use a `Sequence` type to store records. It's a bit like a list, but unlike
    -- lists it has O(1) appends to both ends, and an O(1) length function as well.
    -- We use this a lot considering we're appending new messages on one end, and
    -- processes messages from the other.
    Assigned (Seq.Seq (ProcessAttemptsCount, ConsumerRecord))
  | -- we set the Backlog to this to tell the partition's thread to stop
    Stopping

-- using these types as a proxy for named parameters
-- this is a function that runs with the partition once it's started
newtype OnStartup = OnStartup (Partition -> Prelude.IO ())

-- using this types as a proxy for named parameters
-- this is a function that runs with the partition when it's done to cleanup
newtype OnCleanup = OnCleanup (Prelude.IO ())

data MessageCallback where
  MessageCallback ::
    (Show e, Aeson.ToJSON msg, Aeson.FromJSON msg) =>
    (Consumer.ConsumerRecord () () -> msg -> Task e ProcessResult) ->
    MessageCallback

data CommitOffsets
  = ToKafka
  | -- | Commit offsets elsewhere. Takes the offset of the last committed
    -- message so we can skip old messages.
    Elsewhere Int

-- | A thread that processes messages for a particular partition. Cleans itself
-- up if it ever runs out.
--
-- When processing a message fails we have a couple of non-appealing options:
--
-- - We could throw an error. That would kill the worker process, preventing it
--   from working on unrelated partitions that might be fine.
-- - We can skip the message. This might be okay for some domains, but we cannot
--   generally assume ignorning messages is fine.
-- - We can park the message in a separate topic for processing later (a dead
--   letter queue) This might be fine for some domains, but we cannot generally
--   assume handling messages out of order is fine.
-- - We can retry until it works. If we're lucky this will get the message
--   unstuck, but some logic errors (such as JSON decoding errors) won't go
--   away by themselves and will require new code to be deployed. Until a retry
--   succeeds we'll be blocked from handling any messages on the same partition.
--
-- The retrying solution is unsatisfying, but at least is simple to implement,
-- and will not attempt to fix matters in a way that might also make them worse.
--
-- In domains where we skipping messages or resubmitting them out of order is
-- okay we already have the option of passing in a callback function that
-- catches and handles errors itself and always succeeds.
--
-- In case message order needs to be maintainted, there might be strategies we
-- can devise where being blocked on one key would allow processing on other
-- keys within the same partition to continue. It's not clear however how often
-- a logic error will affect only some keys in a partition and not others, and
-- so whether it's worth it to expend the effort and complexity to implement
-- such a scheme. As we run this code we'll gather data that can help us decide.
spawnWorkerThread ::
  Settings.SkipOrNot ->
  CommitOffsets ->
  Observability.Handler ->
  Analytics.Analytics ->
  Stopping.Stopping ->
  Consumer.KafkaConsumer ->
  MessageCallback ->
  OnStartup ->
  OnCleanup ->
  Prelude.IO ()
spawnWorkerThread skipOrNot commitOffsets observabilityHandler analytics stopping consumer callback (OnStartup onStartup) (OnCleanup onCleanup) = do
  -- Synchronously create the queue that will come to contain messages for the
  -- partition. This way we'll be able to start receiving messages for this
  -- partition as soon as this function returns, even if the processing thread
  -- we start below still needs boot.
  partition <-
    map Partition <| TVar.newTVarIO
      <| case commitOffsets of
        ToKafka -> Assigned Seq.empty
        Elsewhere offset -> AwaitingSeekTo offset
  onStartup partition
  Exception.finally
    (processMsgLoop skipOrNot commitOffsets observabilityHandler State {analytics, stopping, partition} consumer callback)
    onCleanup
    |> Async.async
    -- If the async process spawned here throws an exception, rethrow it
    -- in the main thread so we bring down the worker.
    --
    -- We take care to prevent exceptions from the user-provided callback
    -- to bring down the processing thread. Should an exception slip
    -- through somehow, or should the code in this module produce one,
    -- that could result in a partition thread being killed without us
    -- knowing, and without it being restarted. This would result in no
    -- messages for this partition being processed.
    --
    -- Linking the partition processing threads to the main one will
    -- ensure that if one thread goes down, they all go down. We'll get a
    -- loud crash, which isn't nice, but at least we'll know something bad
    -- happened.
    |> andThen Async.link

processMsgLoop ::
  Settings.SkipOrNot ->
  CommitOffsets ->
  Observability.Handler ->
  State ->
  Consumer.KafkaConsumer ->
  MessageCallback ->
  Prelude.IO ()
processMsgLoop skipOrNot commitOffsets observabilityHandler state consumer callback@(MessageCallback runCallback) = do
  -- # Get the next message from the queue.
  peekResponse <- peekRecord state
  case peekResponse of
    StopThread ->
      Prelude.pure ()
    (NextMsg processAttempts record) -> do
      case processAttempts of
        (ProcessAttemptsCount 0) -> Prelude.pure ()
        (ProcessAttemptsCount attempts) ->
          -- Wait a bit if this is a retry, to prevent putting a lot of retry
          -- stress on downstream systems or generating huge numbers of error
          -- messages.
          microSecondsDelayForAttempt attempts
            |> Prelude.fromIntegral
            |> Control.Concurrent.threadDelay

      doAnything <- Platform.doAnythingHandler
      let commit processResult =
            case processResult of
              ExpectedOffset offset ->
                awaitingSeekTo (partition state) offset
                  |> map Ok
                  |> Platform.doAnything doAnything
              Success -> do
                -- Still around? That means things must have gone well. Let's mark
                -- this message as succesfully processed.
                case commitOffsets of
                  ToKafka ->
                    commitRecord doAnything consumer record
                  Elsewhere _ ->
                    -- The user of the Kafka module is responsible for
                    -- comitting the offsets. To help them do so we pass
                    -- them the record in the callback function.
                    --
                    -- It's important the module user can determine when to
                    -- commit, for example to allow them to commit as part
                    -- of a larger database transaction as part of an
                    -- exactly-once-delivery scheme.
                    Prelude.pure ()

                -- finally, let's remove it from the queue
                dequeueRecord (partition state) record
                  |> map Ok
                  |> Platform.doAnything doAnything

      -- # Process message.
      (RequestId requestId, details) <- getTracingDetails (analytics state) processAttempts record
      Platform.rootTracingSpanIO
        requestId
        (Observability.report observabilityHandler requestId)
        "Assigned Kafka message"
        ( \log -> do
            -- Setting the tracing details first. If anything goes wrong below
            -- at least we'll have nice context in logs!
            Platform.setTracingSpanDetailsIO log details
            handleFailures log <| do
              msg <- decodeMessage record
              runCallback record {Consumer.crKey = (), Consumer.crValue = ()} msg
                |> Task.mapError WorkerCallbackFailed
                |> Task.onError
                  ( \err -> do
                      case skipOrNot of
                        Settings.Skip -> Task.succeed Success
                        Settings.DoNotSkip -> Task.fail err
                  )
                |> Task.andThen commit
        )

      -- # Loop for the next message
      processMsgLoop
        skipOrNot
        commitOffsets
        observabilityHandler
        state
        consumer
        callback

microSecondsDelayForAttempt :: Int -> Int
microSecondsDelayForAttempt attempts =
  min
    3_600_000_000 {- 1 hour in microseconds -}
    ((10 Prelude.^ attempts) * 1000_000 {- 1 second in microseconds -})

handleFailures ::
  Show e =>
  Platform.LogHandler ->
  Task (WorkerError e) a ->
  Prelude.IO ()
handleFailures logHandler task = do
  result <-
    -- Catch any synchronous exceptions the callback might have thrown, to
    -- prevent them from propagating further and killing the entire worker
    -- process.
    Exception.handleAny
      (Prelude.pure << Err << WorkerCallbackThrew)
      (Task.attempt logHandler task)
  case result of
    Ok _ -> Prelude.pure ()
    Err err -> do
      Log.error
        "Assigned Kafka message failed"
        [ Log.context "triage" ("We'll automatically attempt to retry processing of the message. Until a retry succeeds no messages for the same topic partition as the failing message can be processed." :: Text),
          Log.context "error" (Debug.toString err)
        ]
        |> Task.perform logHandler

newtype RequestId = RequestId Text

getTracingDetails ::
  Analytics.Analytics ->
  ProcessAttemptsCount ->
  ConsumerRecord ->
  Prelude.IO (RequestId, Log.Kafka.Details)
getTracingDetails analytics (ProcessAttemptsCount processAttempt) record = do
  let (createTime, logAppendTime) =
        case Consumer.crTimestamp record of
          Consumer.CreateTime millis -> (Just (millisToSecs millis), Nothing)
          Consumer.LogAppendTime millis -> (Nothing, Just (millisToSecs millis))
          Consumer.NoTimestamp -> (Nothing, Nothing)
  let eitherMsg =
        Consumer.crValue record
          -- We'll accept the absence of a message if the worker expects a message
          -- of type `()`. The default JSON encoding for `()` is "[]".
          |> Maybe.withDefault "[]"
          |> Aeson.eitherDecodeStrict
  let (requestId, contents) = case eitherMsg of
        Prelude.Right Internal.MsgWithMetaData {Internal.metaData, Internal.value} ->
          (Just (Internal.requestId metaData), Log.Kafka.mkContents value)
        Prelude.Left _ ->
          ( Nothing,
            Consumer.crValue record
              |> Maybe.andThen (Data.Text.Encoding.decodeUtf8' >> Prelude.either (\_ -> Nothing) Just)
              |> Log.Kafka.mkContents
          )
  ( Analytics.PausedPartitions pausedPartitions,
    Analytics.AssignedPartitions assignedPartitions,
    Analytics.TimeOfLastRebalance timeOfLastRebalance
    ) <-
    Analytics.read analytics
  now <- GHC.Clock.getMonotonicTime
  let timeSinceLastRebalance = now - timeOfLastRebalance
  requestIdForReturn <-
    case requestId of
      Nothing ->
        -- if the message doens't contain a request id, create a new one
        map Data.UUID.toText Data.UUID.V4.nextRandom
      Just requestId' -> Prelude.pure requestId'
      |> map RequestId
  Prelude.pure
    ( requestIdForReturn,
      Log.Kafka.emptyDetails
        { Log.Kafka.topic = Just (Consumer.unTopicName (Consumer.crTopic record)),
          Log.Kafka.partitionId = Just (Prelude.fromIntegral (Consumer.unPartitionId (Consumer.crPartition record))),
          Log.Kafka.key =
            Consumer.crKey record
              |> Maybe.andThen
                ( \keyBytes ->
                    case Data.Text.Encoding.decodeUtf8' keyBytes of
                      Prelude.Left _ -> Nothing
                      Prelude.Right keyText -> Just keyText
                ),
          Log.Kafka.contents = Just contents,
          Log.Kafka.processAttempt = Just processAttempt,
          Log.Kafka.createTime,
          Log.Kafka.assignedPartitions = Just assignedPartitions,
          Log.Kafka.pausedPartitions = Just pausedPartitions,
          Log.Kafka.timeSinceLastRebalance = Just timeSinceLastRebalance,
          Log.Kafka.logAppendTime,
          Log.Kafka.requestId
        }
    )

millisToSecs :: Consumer.Millis -> Clock.UTCTime
millisToSecs (Consumer.Millis millis) = fromPosix (millis // 1000)

decodeMessage :: (Aeson.FromJSON msg) => ConsumerRecord -> Task (WorkerError e) msg
decodeMessage record = do
  let eitherMsg =
        Consumer.crValue record
          -- We'll accept the absence of a message if the worker expects a message
          -- of type `()`. The default JSON encoding for `()` is "[]".
          |> Maybe.withDefault "[]"
          |> Aeson.eitherDecodeStrict
  case eitherMsg of
    Prelude.Left err ->
      Task.fail (MsgDecodingFailed (Text.fromList err))
    Prelude.Right msgWithMetaData ->
      case Internal.value msgWithMetaData of
        (Internal.Encodable value) ->
          case Aeson.fromJSON (Aeson.toJSON value) of
            Aeson.Error err ->
              Task.fail (MsgDecodingFailed (Text.fromList err))
            Aeson.Success msg ->
              Task.succeed msg

commitRecord ::
  Platform.DoAnythingHandler ->
  Consumer.KafkaConsumer ->
  ConsumerRecord ->
  Task e ()
commitRecord doAnything consumer record = do
  commitResult <-
    Consumer.commitOffsetMessage Consumer.OffsetCommit consumer record
      |> map Ok
      |> Platform.doAnything doAnything
  case commitResult of
    Just err ->
      Log.error
        "Failed to commit offset to Kafka after succesfully processing message."
        [ Log.context "err" (Debug.toString err),
          Log.context "context" ("We failed to commit progress on the message, which means there is a risk of us processing it again. If the message is not idempotent this will be a problem. If we see a lot of these errors it might mean no commits are happening at all, in which cases our queues are not making forward progress." :: Text)
        ]
    Nothing -> Task.succeed ()

data PollResponse
  = NextMsg ProcessAttemptsCount ConsumerRecord
  | StopThread

-- | Read the next message for a particular partition, but keep it on the local
-- queue. We should only remove the message if we finish processing it.
peekRecord ::
  State ->
  Prelude.IO PollResponse
peekRecord state =
  Stopping.runUnlessStopping
    (stopping state)
    StopThread
    ( STM.atomically
        <| do
          let (Partition partition') = partition state
          backlog' <- TVar.readTVar partition'
          case backlog' of
            AwaitingSeekTo _ ->
              STM.retry
            Stopping -> do
              Prelude.pure StopThread
            Assigned Seq.Empty ->
              STM.retry
            Assigned ((processAttemptsCount, first) Seq.:<| rest) -> do
              -- Bump the retry count so that the next time we read this message, we
              -- know we've read it before.
              TVar.writeTVar
                partition'
                (Assigned ((processAttemptsCount + 1, first) Seq.:<| rest))
              Prelude.pure (NextMsg processAttemptsCount first)
    )

awaitingSeekTo :: Partition -> Int -> Prelude.IO ()
awaitingSeekTo (Partition partition) offset =
  STM.atomically (TVar.writeTVar partition (AwaitingSeekTo offset))

-- | removes the record from the Backlog if it's still assigned
-- if not assigned, doesn't matter, the current thread will die in the next
-- loop
dequeueRecord ::
  Partition ->
  ConsumerRecord ->
  Prelude.IO ()
dequeueRecord (Partition partition) record =
  STM.atomically <| do
    backlog' <- TVar.readTVar partition
    case backlog' of
      AwaitingSeekTo _ ->
        Prelude.pure ()
      Stopping ->
        Prelude.pure ()
      Assigned Seq.Empty ->
        Prelude.pure ()
      Assigned ((_, first) Seq.:<| rest) -> do
        if Consumer.crOffset first == Consumer.crOffset record
          then TVar.writeTVar partition (Assigned rest)
          else -- why would this ever be the case??? should we log here?
            Prelude.pure ()

append :: ConsumerRecord -> Partition -> STM.STM ProcessResult
append item (Partition partition) =
  TVar.stateTVar
    partition
    ( \queue' ->
        case queue' of
          AwaitingSeekTo offset ->
            if offset == Consumer.unOffset (Consumer.crOffset item)
              then
                ( Success,
                  Assigned (Seq.singleton (ProcessAttemptsCount 0, item))
                )
              else
                ( ExpectedOffset offset,
                  AwaitingSeekTo offset
                )
          Stopping -> (Success, Stopping)
          Assigned queue ->
            ( Success,
              Assigned (queue Seq.:|> (ProcessAttemptsCount 0, item))
            )
    )

length :: Partition -> Prelude.IO (Maybe Int)
length (Partition partition) = do
  backlog <- TVar.readTVarIO partition
  Prelude.pure
    <| case backlog of
      AwaitingSeekTo _ -> Nothing
      Stopping -> Nothing
      Assigned queue ->
        Just (Prelude.fromIntegral (Seq.length queue))

revoke :: Partition -> STM.STM ()
revoke (Partition partition) = TVar.writeTVar partition Stopping

-- | Create a time from a posix timestamp, a number of seconds since the Linux
-- epoch. This provides us a way to create constant timetamps for tests.
fromPosix :: Int -> Clock.UTCTime
fromPosix secondsSinceEpoch =
  secondsSinceEpoch
    |> Prelude.fromIntegral
    |> Clock.POSIX.posixSecondsToUTCTime
