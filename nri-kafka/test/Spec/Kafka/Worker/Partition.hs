module Spec.Kafka.Worker.Partition (tests) where

import qualified Expect
import qualified Kafka.Worker.Partition as Partition
import qualified Test

tests :: Test.Test
tests =
  Test.describe
    "Worker"
    [ Test.describe
        "microSecondsDelayForAttempt"
        [ Test.test "1 attempt" <| \() ->
            Partition.microSecondsDelayForAttempt 1
              |> Expect.equal 2_000_000,
          Test.test "2 attempts" <| \() ->
            Partition.microSecondsDelayForAttempt 2
              |> Expect.equal 4_000_000,
          Test.test "3 attempts" <| \() ->
            Partition.microSecondsDelayForAttempt 3
              |> Expect.equal 8_000_000,
          Test.test "4 attempts" <| \() ->
            Partition.microSecondsDelayForAttempt 4
              |> Expect.equal 16_000_000,
          Test.test "100 attempts" <| \() ->
            Partition.microSecondsDelayForAttempt 100
              |> Expect.equal 1_024_000_000
        ]
    ]
