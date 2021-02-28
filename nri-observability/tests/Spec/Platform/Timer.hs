module Spec.Platform.Timer (tests) where

import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Expect
import qualified GHC.Clock
import qualified Platform.Timer as Timer
import qualified Test
import qualified Prelude

tests :: Test.Test
tests =
  Test.describe
    "Observability.Timer"
    [ Test.test "can reproduce the current time" <| \_ -> do
        timer <- Expect.fromIO <| Timer.mkTimer
        clock <-
          GHC.Clock.getMonotonicTimeNSec
            |> map (\t -> Prelude.fromIntegral (t `Prelude.div` 1000))
            |> Expect.fromIO
        expectedNow <- Expect.fromIO <| Clock.POSIX.getPOSIXTime
        let actualNow = Timer.toPosix timer clock
        actualNow
          |> Expect.all
            [ Expect.atLeast (expectedNow - 1),
              Expect.atMost (expectedNow + 1)
            ]
    ]
