module PlatformSpec (tests) where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch (catchAll)
import Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Expect
import qualified Log
import NriPrelude
import qualified Platform
import qualified Platform.Internal
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
        Expect.true (Platform.containsFailures span),
      test "trackAnalyticsEventIO threaded by rootTracingSpanIO is invoked from a child span" <| \_ -> do
        ref <- Expect.fromIO (IORef.newIORef [])
        let track v = IORef.atomicModifyIORef' ref (\xs -> (v : xs, ()))
        Expect.fromIO
          <| Platform.rootTracingSpanIO "test-req" track (\_ -> Prelude.pure ()) "root"
          <| \log -> do
            child <- Platform.Internal.startChildTracingSpan log "child-span"
            Platform.Internal.trackAnalyticsEventIO child (Aeson.toJSON ("hello" :: Text))
        observed <- Expect.fromIO (IORef.readIORef ref)
        observed |> Expect.equal [Aeson.toJSON ("hello" :: Text)],
      test "nullHandler.trackAnalyticsEventIO is a silent no-op" <| \_ ->
        Expect.fromIO
          <| Platform.Internal.trackAnalyticsEventIO Platform.Internal.nullHandler Aeson.Null
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
            Platform.silentTrack
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
