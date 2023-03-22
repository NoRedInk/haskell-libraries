module TaskSpec (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Expect
import qualified List
import NriPrelude
import qualified Platform
import qualified Process
import qualified Task
import Test (Test, describe, test)
import Test.Internal (Failure)
import qualified Tuple
import Prelude ((*>))
import qualified Prelude

tests :: Test
tests =
  describe
    "Task"
    [ describe
        "sequence"
        [ test "actually runs in sequence" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            let shouldNeverRun = \x -> Platform.doAnything doAnything (Exception.throwString x)
            failure <-
              [Task.succeed 1, Task.fail "a", shouldNeverRun "b"]
                |> Task.sequence
                |> Expect.fails
            Expect.equal "a" failure
            -- Prelude.sequence also runs things in sequence.
            failure2 <-
              [Task.succeed 1, Task.fail "a", shouldNeverRun "b"]
                |> Prelude.sequence
                |> Expect.fails
            Expect.equal "a" failure2
        ],
      describe
        "map2"
        [ test "only runs first task if that fails" <| \() -> do
            doAnything <- Expect.fromIO Platform.doAnythingHandler
            let shouldNeverRun = \x -> Platform.doAnything doAnything (Exception.throwString x)
            failure <-
              Task.map2
                Tuple.pair
                (Task.fail "a")
                (shouldNeverRun "b")
                |> Expect.fails
            Expect.equal "a" failure
        ],
      describe
        "parallel"
        [ test "returns the right values" <| \() -> do
            let results = [1, 2, 3]
            let tasks = List.map afterDelay results

            parallelResults <- Expect.succeeds <| Task.parallel tasks

            Expect.equal results parallelResults
        ],
      describe
        "concurrently"
        [ test "returns the right values" <| \() -> do
            let results = (1, ("two", 3.0 :: Float))
            let (taskA, (taskB, taskC)) = Tuple.mapBoth afterDelay (Tuple.mapBoth afterDelay afterDelay) results

            concurrentResults <-
              Expect.succeeds <| Task.concurrently taskA <| Task.concurrently taskB taskC

            Expect.equal results concurrentResults
        ]
    ]

afterDelay :: a -> Task Failure a
afterDelay a =
  Process.sleep waitMilliseconds *> Task.succeed a

waitMilliseconds :: Float
waitMilliseconds = 100
