{-# LANGUAGE GADTs #-}

-- The Kafka worker has the following concurrent workflows:
-- 1. The main thread which handles
--   - gracefully quitting
--   - a single-thread `pollingLoop` that reads new messages from kafka
-- 2. The Kafka `rebalanceCallback` which handles rebalancing partitions, in turn
--    turning on and off worker threads.
-- 3. A single-thread `pauseAndAnalyticsLoop` that tells the kafka library to pause & resume
--    sending us messages from specific partitions (based on a number of factors)
-- 4. Multiple worker-threads (`Partition.processMsgLoop`), one per
--    (topic,partition)

-- | Kafka.Worker is a module for processing a Kafka log.
-- It can be used to build a CLI that will consume and process a user-defined message type
module Kafka.Worker
  ( Internal.process,
    Internal.TopicSubscription (..),
    Partition.MessageCallback (Partition.MessageCallback),
    Internal.OffsetSource (..),
    Consumer.ConsumerGroupId (..),
    Internal.PartitionKey,
    Consumer.TopicName (..),
    Consumer.ConsumerRecord (..),
    Partition.SeekCmd (..),
    Consumer.PartitionId (..),
    Consumer.unOffset,
    Settings.Settings,
    Settings.decoder,
  )
where

import qualified Kafka.Consumer as Consumer
import qualified Kafka.Worker.Internal as Internal
import qualified Kafka.Worker.Partition as Partition
import qualified Kafka.Worker.Settings as Settings
