module Internal.GenericDBSpec (tests) where

import qualified Control.Concurrent
import qualified Control.Exception
import qualified Expect
import Internal.GenericDb
import Nri.Prelude
import Test (Test, describe, test)
import Prelude (Either (Left, Right), IO, pure)

tests :: Test
tests =
  describe
    "Internal.GenericDB"
    [ withTimeoutTests
    ]

withTimeoutTests :: Test
withTimeoutTests =
  describe
    "withTimeout"
    [ test "IO that completes before the timeout is unaffected" <| \_ ->
        Expect.withIO
          (Expect.equal False)
          (withTimeout 1000000 (pure ()) |> throwsException),
      test "IO that runs longer than the timeout is killed" <| \_ ->
        Expect.withIO
          (Expect.equal True)
          (withTimeout 0 (Control.Concurrent.threadDelay 1000000) |> throwsException)
      -- This suite intentionally only tests the extreme cases (either the timeout
      -- is zero or the operation completes immediately), because the risk of
      -- time-based tests being inherently flaky.
    ]

throwsException :: IO a -> IO Bool
throwsException io = do
  result <- Control.Exception.try io
  case result of
    Left (_ :: QueryTimeoutException) -> pure True
    Right _ -> pure False
