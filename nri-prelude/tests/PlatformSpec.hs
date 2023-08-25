module PlatformSpec (tests) where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch (catchAll)
import Data.Aeson as Aeson
import qualified Expect
import qualified List
import qualified Log
import NriPrelude
import qualified Platform
import Task
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "Platform"
    [ test "can recover custom span details from TracingSpanDetails" <| \_ ->
        CustomTracingSpanDetails "Hi!"
          |> Platform.toTracingSpanDetails
          |> Platform.fromTracingSpanDetails
          |> Expect.equal (Just (CustomTracingSpanDetails "Hi!")),
      test "parent span marked as failed when exception is thrown" <| \_ -> do
        span <-
          runTaskAndExpectTacingSpan <| Platform.tracingSpan "throw" <| Platform.unsafeThrowException "error"

        Expect.false (isSucceeded span)
        Expect.true (Platform.containsFailures span),
      test "parent span not marked as failed when error is logged" <| \_ -> do
        span <-
          runTaskAndExpectTacingSpan <| Log.error "error" []

        Expect.true (isSucceeded span)
        Expect.true (Platform.containsFailures span)
    ]

newtype CustomTracingSpanDetails = CustomTracingSpanDetails Text
  deriving (Aeson.ToJSON, Show, Eq)

instance Platform.TracingSpanDetails CustomTracingSpanDetails

runTaskAndExpectTacingSpan :: Task e () -> Expect.Expectation' Platform.TracingSpan
runTaskAndExpectTacingSpan task =
  Expect.fromIO <| do
    spanVar <- MVar.newEmptyMVar

    _ <-
      catchAll
        ( Platform.rootTracingSpanIO
            ""
            (MVar.putMVar spanVar)
            "test"
            (\log -> Task.attempt log task)
        )
        (\_ -> Prelude.pure <| NriPrelude.Ok ())

    MVar.takeMVar spanVar

isSucceeded :: Platform.TracingSpan -> Bool
isSucceeded span =
  case Platform.succeeded span of
    Platform.Succeeded -> True
    _ -> False
