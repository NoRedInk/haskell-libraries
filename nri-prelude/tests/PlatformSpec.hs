module PlatformSpec (tests) where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch (catchAll)
import Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Expect
import qualified Log
import NriPrelude
import qualified Platform
import qualified Platform.Analytics.Internal
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
      test "trackAnalyticsEvent threaded by rootTracingSpanIO is invoked from a child span" <| \_ -> do
        ref <- Expect.fromIO (IORef.newIORef [])
        let track v =
              Platform.Internal.Task
                ( \_ -> do
                    IORef.atomicModifyIORef' ref (\xs -> (v : xs, ()))
                    Prelude.pure (Ok ())
                )
        Expect.fromIO
          <| Platform.rootTracingSpanIO "test-req" track (\_ -> Prelude.pure ()) "root"
          <| \log -> do
            child <- Platform.Internal.startChildTracingSpan log "child-span"
            let Platform.Internal.Task runTrack =
                  Platform.Internal.trackAnalyticsEvent child (Aeson.toJSON ("hello" :: Text))
            _ <- runTrack child
            Prelude.pure ()
        observed <- Expect.fromIO (IORef.readIORef ref)
        observed |> Expect.equal [Aeson.toJSON ("hello" :: Text)],
      test "nullHandler.trackAnalyticsEvent is a silent no-op" <| \_ -> do
        let Platform.Internal.Task runTrack =
              Platform.Internal.trackAnalyticsEvent Platform.Internal.nullHandler Aeson.Null
        _ <- Expect.fromIO (runTrack Platform.Internal.nullHandler)
        Expect.pass,
      test "Platform.Analytics.Internal.trackEvent invokes the current LogHandler's analytics callback with toJSON of the event" <| \_ -> do
        ref <- Expect.fromIO (IORef.newIORef [])
        let track v =
              Platform.Internal.Task
                ( \_ -> do
                    IORef.atomicModifyIORef' ref (\xs -> (v : xs, ()))
                    Prelude.pure (Ok ())
                )
        let event = Aeson.object ["kind" Aeson..= ("LessonStarted" :: Text)]
        result <-
          Expect.fromIO
            <| Platform.rootTracingSpanIO "test-req" track (\_ -> Prelude.pure ()) "root"
            <| \log -> Task.attempt log (Platform.Analytics.Internal.trackEvent event)
        case result of
          Ok () -> Expect.pass
          Err _ -> Expect.fail "trackEvent task failed"
        observed <- Expect.fromIO (IORef.readIORef ref)
        observed |> Expect.equal [event],
      test "Platform.Analytics.Internal.trackEvent opens an analytics.track child span carrying the JSON payload as details" <| \_ -> do
        spanVar <- Expect.fromIO MVar.newEmptyMVar
        let event = Aeson.object ["kind" Aeson..= ("LessonStarted" :: Text)]
        _ <-
          Expect.fromIO
            <| Platform.rootTracingSpanIO "test-req" Platform.silentTrack (MVar.putMVar spanVar) "root"
            <| \log -> Task.attempt log (Platform.Analytics.Internal.trackEvent event)
        root <- Expect.fromIO (MVar.takeMVar spanVar)
        case Prelude.filter (\s -> Platform.Internal.name s == "analytics.track") (Platform.Internal.children root) of
          [child] -> do
            Platform.Internal.name child |> Expect.equal "analytics.track"
            NriPrelude.map Aeson.toJSON (Platform.Internal.details child) |> Expect.equal (Just event)
          [] -> Expect.fail "expected an analytics.track child span; found none"
          _ -> Expect.fail "expected exactly one analytics.track child span"
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
