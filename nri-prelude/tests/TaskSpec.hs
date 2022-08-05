module TaskSpec (tests) where

import qualified Expect
import qualified List
import NriPrelude
import qualified Platform.Internal as Internal
import Prelude ((*>), pure)
import qualified Process
import qualified Result
import qualified System.TimeIt
import Test (Test, describe, test)
import Test.Internal (Failure)
import qualified Task
import qualified Tuple

tests :: Test
tests =
  describe "Task"
    [ describe "parallel"
      [ test "returns the right values and executes in parallel" <| \() -> do
        let results = [1, 2, 3]
        let tasks = List.map afterDelay results

        (parallelExecutionTime, parallelResults) <-
          Expect.succeeds <| timeIt <| Task.parallel tasks
        (sequentialExecutionTime, _) <-
          Expect.succeeds <| timeIt <| Task.sequence tasks

        Expect.equal results parallelResults

        parallelExecutionTime |>
          Expect.lessThan sequentialExecutionTime
      ]
    , describe "concurrently"
      [ test "returns the right values and executes concurrently" <| \() -> do
        let results = (1, ("two", 3.0 :: Float))
        let (taskA, (taskB, taskC)) = Tuple.mapBoth afterDelay (Tuple.mapBoth afterDelay afterDelay) results

        (concurrentExecutionTime, concurrentResults) <-
          Expect.succeeds <| timeIt <| Task.concurrently taskA <| Task.concurrently taskB taskC
        (sequentialExecutionTime, _) <-
          Expect.succeeds <| timeIt <| Task.andThen (\_ -> taskB) taskA

        Expect.equal results concurrentResults

        concurrentExecutionTime |>
          Expect.lessThan sequentialExecutionTime
      ]
    ]

afterDelay :: a -> Task Failure a
afterDelay a =
  Process.sleep waitMilliseconds *> Task.succeed a

waitMilliseconds :: Float
waitMilliseconds = 100

timeIt :: Task e a -> Task e (Float, a)
timeIt task =
  Internal.Task <| \handler -> do
    (seconds, res) <- System.TimeIt.timeItT <| Internal._run task handler
    let milliseconds = seconds * 1000
    pure <| Result.map ((,) milliseconds) res