module TaskSpec (tests) where

import qualified Expect
import qualified List
import NriPrelude
import Prelude ((*>))
import qualified Process
import qualified Result
import Test (Test, describe, test)
import qualified Task
import qualified Tuple

tests :: Test
tests =
  describe
    "Task"
    [ describe "parallel"
        [ test "returns the right values" <| \() -> do
            let results = [1, 2, 3]
            let tasks = List.map waitForIt results
            Task.parallel tasks
              |> Expect.andCheck (Expect.equal results)
        ]
    , describe "concurrently"
        [ test "returns the right values" <| \() -> do
            let results = (1, 2)
            let (taskA, taskB) = Tuple.mapBoth waitForIt waitForIt results
            Task.concurrently taskA taskB
              |> Expect.andCheck (Expect.equal results)
        ]
    ]

waitForIt :: a -> Task x a
waitForIt a =
    Process.sleep waitMilliseconds *> Task.succeed a

waitMilliseconds :: Float
waitMilliseconds = 100
