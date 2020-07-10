module Internal.TimeSpec (tests) where

import Cherry.Prelude
import qualified Expect
import qualified Internal.Time as Time
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Internal.Time"
    [ test "fromSeconds" <| \_ ->
        Time.fromSeconds 2.5
          |> Time.microseconds
          |> Expect.equal 2500000,
      test "seconds" <| \_ ->
        Time.fromMicroseconds 2000000
          |> Time.seconds
          |> round
          |> Expect.equal 2,
      test "fromMilliseconds" <| \_ ->
        Time.fromMilliseconds 2.5
          |> Time.microseconds
          |> Expect.equal 2500,
      test "milliseconds" <| \_ ->
        Time.fromMicroseconds 2000
          |> Time.milliseconds
          |> round
          |> Expect.equal 2,
      test "fromMicroseconds" <| \_ ->
        Time.fromMicroseconds 2
          |> Time.microseconds
          |> Expect.equal 2,
      test "microseconds" <| \_ ->
        Time.fromMicroseconds 2
          |> Time.microseconds
          |> Expect.equal 2
    ]
