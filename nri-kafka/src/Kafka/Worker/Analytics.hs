module Kafka.Worker.Analytics
  ( Analytics,
    AssignedPartitions (AssignedPartitions),
    PausedPartitions (PausedPartitions),
    TimeOfLastRebalance (TimeOfLastRebalance),
    init,
    read,
    updatePaused,
    updateTimeOfLastRebalance,
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Prelude

newtype PausedPartitions = PausedPartitions Int

newtype AssignedPartitions = AssignedPartitions Int

newtype TimeOfLastRebalance = TimeOfLastRebalance Float

data Analytics = Analytics
  { pausedPartitions :: TVar.TVar PausedPartitions,
    timeOfLastRebalance :: TVar.TVar TimeOfLastRebalance,
    assignedPartitions :: Prelude.IO AssignedPartitions
  }

init :: Prelude.IO Int -> Prelude.IO Analytics
init assignedPartitions' = do
  pausedPartitions <- TVar.newTVarIO (PausedPartitions 0)
  timeOfLastRebalance <- TVar.newTVarIO (TimeOfLastRebalance 0)
  Prelude.pure
    ( Analytics
        { pausedPartitions,
          timeOfLastRebalance,
          assignedPartitions = map AssignedPartitions assignedPartitions'
        }
    )

read :: Analytics -> Prelude.IO (PausedPartitions, AssignedPartitions, TimeOfLastRebalance)
read Analytics {pausedPartitions, assignedPartitions, timeOfLastRebalance} = do
  analyticsPausedPartitions <- TVar.readTVarIO pausedPartitions
  analyticsTimeOfLastRebalance <- TVar.readTVarIO timeOfLastRebalance
  analyticsAssignedPartitions <- assignedPartitions
  Prelude.pure (analyticsPausedPartitions, analyticsAssignedPartitions, analyticsTimeOfLastRebalance)

updatePaused :: Int -> Analytics -> Prelude.IO ()
updatePaused numPaused Analytics {pausedPartitions} =
  STM.atomically <| TVar.writeTVar pausedPartitions (PausedPartitions numPaused)

updateTimeOfLastRebalance :: Float -> Analytics -> Prelude.IO ()
updateTimeOfLastRebalance now Analytics {timeOfLastRebalance} = do
  STM.atomically <| TVar.writeTVar timeOfLastRebalance (TimeOfLastRebalance now)
