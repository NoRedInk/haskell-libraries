module Kafka.Worker.Fetcher (pollingLoop) where

import qualified Control.Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString as ByteString
import qualified Dict
import qualified GHC.Clock
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Worker.Analytics as Analytics
import qualified Kafka.Worker.Partition as Partition
import qualified Kafka.Worker.Settings as Settings
import qualified Prelude
import qualified Data.Either

type EnqueueRecord = (ConsumerRecord -> Prelude.IO Partition.SeekCmd)

-- | pollingLoop
-- our long-running event loop that
-- - polls for new messages
-- - for each message, spawns a thread for its partition if it doesn't yet exist
-- - appends the message to an in-memory queue that's being worked on by a partition-specific thread
pollingLoop ::
  Settings.Settings ->
  EnqueueRecord ->
  Analytics.Analytics ->
  Consumer.KafkaConsumer ->
  MVar.MVar () ->
  Prelude.IO ()
pollingLoop settings enqueueRecord analytics consumer consumerLock = do
  now <- nextPollingTimestamp
  pollingLoop' settings enqueueRecord analytics consumer consumerLock (pollTimeIsOld now)

newtype LastPollingTimestamp = LastPollingTimestamp Float

newtype NextPollingTimestamp = NextPollingTimestamp Float

pollTimeIsOld :: NextPollingTimestamp -> LastPollingTimestamp
pollTimeIsOld (NextPollingTimestamp time) = LastPollingTimestamp time

nextPollingTimestamp :: Prelude.IO NextPollingTimestamp
nextPollingTimestamp = do
  map NextPollingTimestamp GHC.Clock.getMonotonicTime

type ConsumerRecord = Consumer.ConsumerRecord (Maybe ByteString.ByteString) (Maybe ByteString.ByteString)

pollingLoop' ::
  Settings.Settings ->
  EnqueueRecord ->
  Analytics.Analytics ->
  Consumer.KafkaConsumer ->
  MVar.MVar () ->
  LastPollingTimestamp ->
  Prelude.IO ()
pollingLoop'
  settings@Settings.Settings
    { Settings.pollingTimeout,
      Settings.pollBatchSize,
      Settings.maxMsgsPerSecondPerPartition,
      Settings.maxPollIntervalMs
    }
  enqueueRecord
  analytics
  consumer
  consumerLock
  lastPollTimestamp = do
    -- we block here if we're actively revoking
    -- Check whether we need to shut down while long-polling for new messages.
    eitherMsgs <-
      -- We use a lock to prevent running this concurrently with pause/resume calls, due to bugs in
      -- librdkafka, fixed in 2.1.0, while hw-kafka is on 1.6. Search Worker/Internal.hs for
      -- consumerLock for the other side of this.
      --
      -- The symptom is messages being skipped every once in a while in a slow consumer that has its
      -- buffer filled up and had to pause/resume all the time.
      --
      -- See https://github.com/confluentinc/librdkafka/blob/c282ba2423b2694052393c8edb0399a5ef471b3f/CHANGELOG.md?plain=1#L90-L95
      --
      -- We have a small app to reproduce the bug. Check out scripts/pause-resume-bug/README.md
      MVar.withMVar consumerLock <| \_ -> do
          Prelude.putStrLn "Polling for messages..."
          em <- Consumer.pollMessageBatch consumer pollingTimeout pollBatchSize
          let digits = List.filterMap recordContents em |> ByteString.intercalate ", "
          Prelude.putStrLn <| "Polling done. Found messages: " ++ Prelude.show digits
          Prelude.pure em
    msgs <- Prelude.traverse handleKafkaError eitherMsgs
    assignment <-
      Consumer.assignment consumer
        |> andThen handleKafkaError
    appendResults <-
      msgs
        -- We occasionally get a message here for a partition that based on
        -- internal state we believed to be revoked. We feel uneasy just
        -- dropping those messages, for what if our internal state is wrong? We
        -- might be dropping messages we really should be processing.
        -- So instead we ask librdkafka to tell us what our current assignment
        -- is. If we receive messages for partitions outside of that
        -- assignment, then we can confidently drop them.
        |> List.filter (msgIsForAssignedPartition assignment)
        -- Enqueue messages in per-partition queues.
        |> Prelude.traverse enqueueRecord
    List.map2 (,) (List.map getPartitionKey msgs) appendResults
      |> groupDictAndMap identity
      |> Dict.toList
      |> List.filterMap toSeekPartition
      |> seek consumer
    now <- nextPollingTimestamp
    throttle maxMsgsPerSecondPerPartition maxPollIntervalMs (List.length appendResults) analytics now lastPollTimestamp
    pollingLoop' settings enqueueRecord analytics consumer consumerLock (pollTimeIsOld now)

recordContents :: Data.Either.Either x ConsumerRecord -> Maybe ByteString.ByteString
recordContents (Data.Either.Left _) = Nothing
recordContents (Data.Either.Right record) = do
  val <- Consumer.crValue record
  let digits = ByteString.filter (\c -> c >= 48 && c <= 57) val
  Just digits

getPartitionKey :: Consumer.ConsumerRecord k v -> (Consumer.TopicName, Consumer.PartitionId)
getPartitionKey record =
  ( Consumer.crTopic record,
    Consumer.crPartition record
  )

toSeekPartition ::
  ( (Consumer.TopicName, Consumer.PartitionId),
    List Partition.SeekCmd
  ) ->
  Maybe Consumer.TopicPartition
toSeekPartition ((topicName, partitionId), appendResults) =
  -- Among they last batch of fetched messages might have been multiple messages
  -- for this partition, which we subsequently tried to enqueue. It's possible
  -- the first message might have had an offset smaller than the one that we
  -- were looking for, but that somewhere in the middle of the series we caught
  -- up. That's why we consider only the last message appended to the patition
  -- in this batch. If that one was succesfull then there's nothing for us to
  -- do. If it had an unexpected offset then we should seek.
  case last appendResults of
    Nothing -> Nothing
    Just Partition.NoSeek -> Nothing
    Just (Partition.SeekToOffset offset) ->
      Just
        Consumer.TopicPartition
          { Consumer.tpTopicName = topicName,
            Consumer.tpPartition = partitionId,
            Consumer.tpOffset = Consumer.PartitionOffset offset
          }

last :: List a -> Maybe a
last list = List.head (List.reverse list)

seek :: Consumer.KafkaConsumer -> List Consumer.TopicPartition -> Prelude.IO ()
seek consumer partitions = do
  let goSeek = Consumer.seek consumer (Consumer.Timeout 5000 {- 5 seconds -}) partitions
  maybeSeekError <- goSeek
  case maybeSeekError of
    Nothing -> Prelude.pure ()
    Just _ -> do
      -- Retry once, after a delay, because we're seeing reports
      -- that attempting to `seek` after just having called
      -- `assign` (which hw-kafka-client does for us before running
      -- this callback) might result in seek failing. See:
      -- https://github.com/confluentinc/confluent-kafka-dotnet/issues/1303
      Control.Concurrent.threadDelay 5_000_000 {- 5 seconds -}
      maybeSeekError2 <- goSeek
      case maybeSeekError2 of
        Nothing -> Prelude.pure ()
        Just seekError2 -> Exception.throwIO seekError2

msgIsForAssignedPartition ::
  Dict.Dict Consumer.TopicName [Consumer.PartitionId] ->
  ConsumerRecord ->
  Bool
msgIsForAssignedPartition assignment msg =
  case Dict.get (Consumer.crTopic msg) assignment of
    Nothing -> False
    Just partitionIds ->
      List.member (Consumer.crPartition msg) partitionIds

handleKafkaError :: Prelude.Either Consumer.KafkaError a -> Prelude.IO a
handleKafkaError eitherMsg = do
  case eitherMsg of
    Prelude.Left err ->
      -- Kill the worker process if polling for messages results in an error.
      -- Every individual message of a batch can contain an error. It's unclear
      -- what the implications of that are. Specifically: could such errors
      -- result in holes in the stream of messages for a partition? That would
      -- be bad, it could result in some messages being ignored.
      --
      -- Crashing the worker seems a "safe" option at least. If such crashes
      -- are very rare this solution might be good enough. If it happens
      -- regularly we should figure out a better solution.
      Exception.throwIO err
    Prelude.Right record ->
      Prelude.pure record

-- | Call on the poll thread after fetching a new batch of messages. If we're
-- ahead of our quotum this function will sleep for a bit, delaying the fetch of
-- the next batch.
throttle ::
  Settings.MaxMsgsPerSecondPerPartition ->
  Settings.MaxPollIntervalMs ->
  Int ->
  Analytics.Analytics ->
  NextPollingTimestamp ->
  LastPollingTimestamp ->
  Prelude.IO ()
throttle Settings.DontThrottle _ _ _ _ _ = Prelude.pure ()
throttle (Settings.ThrottleAt maxMsgsPerSecondPerPartition) maxPollIntervalMs newPolledMessages analytics (NextPollingTimestamp now) (LastPollingTimestamp lastPollTimestamp) = do
  (_, Analytics.AssignedPartitions numPartitions, _) <- Analytics.read analytics
  let timeDiff = Prelude.floor (now - lastPollTimestamp)
  let quotumPerSecond = maxMsgsPerSecondPerPartition * numPartitions
  let quotum = timeDiff * quotumPerSecond
  let overQuotum = newPolledMessages - quotum
  let secondsToSleep =
        Prelude.fromIntegral overQuotum / Prelude.fromIntegral quotumPerSecond
  let microSecondsToSleep =
        Prelude.floor (secondsToSleep * 1e6)
          |> min (Prelude.fromIntegral <| Settings.unMaxPollIntervalMs maxPollIntervalMs - 100) -- -100ms so that it has time to loop.
  if microSecondsToSleep > 0
    then Control.Concurrent.threadDelay microSecondsToSleep
    else Prelude.pure ()

groupDictAndMap :: Ord b => (a -> (b, c)) -> List a -> Dict.Dict b (List c)
groupDictAndMap f =
  List.foldr
    ( \x ->
        Dict.update (Tuple.first (f x)) <| \val ->
          case val of
            Nothing -> Just [Tuple.second (f x)]
            Just y -> Just (Tuple.second (f x) : y)
    )
    Dict.empty
