module PlatformSpec (tests) where

import qualified Control.Concurrent
import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch (catchAll)
import Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Expect
import qualified List
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
        Expect.fromIO <|
          Platform.rootTracingSpanIO "test-req" track (\_ -> Prelude.pure ()) "root" <|
            \log -> do
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
          Expect.fromIO <|
            Platform.rootTracingSpanIO "test-req" track (\_ -> Prelude.pure ()) "root" <|
              \log -> Task.attempt log (Platform.Analytics.Internal.trackEvent event)
        case result of
          Ok () -> Expect.pass
          Err _ -> Expect.fail "trackEvent task failed"
        observed <- Expect.fromIO (IORef.readIORef ref)
        observed |> Expect.equal [event],
      test "Platform.Analytics.Internal.trackEvent opens an analytics.track child span carrying the JSON payload as details" <| \_ -> do
        spanVar <- Expect.fromIO MVar.newEmptyMVar
        let event = Aeson.object ["kind" Aeson..= ("LessonStarted" :: Text)]
        _ <-
          Expect.fromIO <|
            Platform.rootTracingSpanIO "test-req" Platform.silentTrack (MVar.putMVar spanVar) "root" <|
              \log -> Task.attempt log (Platform.Analytics.Internal.trackEvent event)
        root <- Expect.fromIO (MVar.takeMVar spanVar)
        case Prelude.filter (\s -> Platform.Internal.name s == "analytics.track") (Platform.Internal.children root) of
          [child] -> do
            Platform.Internal.name child |> Expect.equal "analytics.track"
            NriPrelude.map Aeson.toJSON (Platform.Internal.details child) |> Expect.equal (Just event)
          [] -> Expect.fail "expected an analytics.track child span; found none"
          _ -> Expect.fail "expected exactly one analytics.track child span",
      describe "late span reparenting" lateSpanReparentingTests
    ]

lateSpanReparentingTests :: List Test
lateSpanReparentingTests =
  [ test "child finishing before its parent nests normally" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          parent <- Platform.Internal.startChildTracingSpan root "parent"
          child <- Platform.Internal.startChildTracingSpan parent "child"
          Platform.Internal.finishTracingSpan child Nothing
          Platform.Internal.finishTracingSpan parent Nothing
      case reported of
        [rootSpan] -> do
          childNames rootSpan |> Expect.equal ["parent"]
          case Platform.Internal.children rootSpan of
            [parentSpan] -> childNames parentSpan |> Expect.equal ["child"]
            _ -> Expect.fail "expected exactly one child under the root"
        _ -> Expect.fail "expected exactly one reported root span",
    test "child created after its parent finished is reparented to the root" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          parent <- Platform.Internal.startChildTracingSpan root "parent"
          Platform.Internal.finishTracingSpan parent Nothing
          child <- Platform.Internal.startChildTracingSpan parent "child"
          Platform.Internal.finishTracingSpan child Nothing
      case reported of
        [rootSpan] -> do
          childNames rootSpan |> Expect.equal ["child", "parent"]
          rootSpan
            |> Platform.Internal.children
            |> List.concatMap Platform.Internal.children
            |> List.map Platform.Internal.name
            |> Expect.equal []
        _ -> Expect.fail "expected exactly one reported root span",
    test "child started before its parent finished but finishing after is reparented to the root" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          parent <- Platform.Internal.startChildTracingSpan root "parent"
          child <- Platform.Internal.startChildTracingSpan parent "child"
          Platform.Internal.finishTracingSpan parent Nothing
          Platform.Internal.finishTracingSpan child Nothing
      case reported of
        [rootSpan] -> childNames rootSpan |> Expect.equal ["child", "parent"]
        _ -> Expect.fail "expected exactly one reported root span",
    test "racing parent and child finalization never loses or duplicates the child" <| \_ -> do
      counts <-
        Expect.fromIO <|
          Prelude.mapM
            ( \_ -> do
                reported <-
                  reportedSpansForIO <| \root -> do
                    parent <- Platform.Internal.startChildTracingSpan root "parent"
                    child <- Platform.Internal.startChildTracingSpan parent "child"
                    parentDone <- MVar.newEmptyMVar
                    childDone <- MVar.newEmptyMVar
                    _ <-
                      Control.Concurrent.forkIO <| do
                        Platform.Internal.finishTracingSpan parent Nothing
                        MVar.putMVar parentDone ()
                    _ <-
                      Control.Concurrent.forkIO <| do
                        Platform.Internal.finishTracingSpan child Nothing
                        MVar.putMVar childDone ()
                    MVar.takeMVar parentDone
                    MVar.takeMVar childDone
                reported
                  |> List.concatMap (descendantsNamed "child")
                  |> List.length
                  |> Prelude.pure
            )
            (List.range 1 200)
      counts |> Expect.equal (List.repeat 200 1),
    test "failed reparented child marks the root as containing failures" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          parent <- Platform.Internal.startChildTracingSpan root "parent"
          Platform.Internal.finishTracingSpan parent Nothing
          child <- Platform.Internal.startChildTracingSpan parent "child"
          Platform.Internal.markTracingSpanFailedIO child
          Platform.Internal.finishTracingSpan child Nothing
      case reported of
        [rootSpan] -> do
          childNames rootSpan |> Expect.equal ["child", "parent"]
          Expect.true (Platform.containsFailures rootSpan)
        _ -> Expect.fail "expected exactly one reported root span",
    test "child finishing after the root closed is reported as a separate root" <| \_ -> do
      reported <-
        Expect.fromIO <| do
          reportedRef <- IORef.newIORef []
          childVar <- MVar.newEmptyMVar
          Platform.rootTracingSpanIO
            "test-request"
            Platform.silentTrack
            (\span -> IORef.modifyIORef' reportedRef (\spans -> spans ++ [span]))
            "root"
            ( \root -> do
                child <- Platform.Internal.startChildTracingSpan root "late-child"
                MVar.putMVar childVar child
            )
          child <- MVar.takeMVar childVar
          Platform.Internal.finishTracingSpan child Nothing
          IORef.readIORef reportedRef
      List.map Platform.Internal.name reported |> Expect.equal ["root", "late-child"]
      case reported of
        [rootSpan, _] -> childNames rootSpan |> Expect.equal []
        _ -> Expect.fail "expected exactly two reported root spans",
    test "repeated finalization attaches the span only once" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          parent <- Platform.Internal.startChildTracingSpan root "parent"
          Platform.Internal.finishTracingSpan parent Nothing
          Platform.Internal.finishTracingSpan parent Nothing
      case reported of
        [rootSpan] -> childNames rootSpan |> Expect.equal ["parent"]
        _ -> Expect.fail "expected exactly one reported root span",
    test "late children inside Platform.newRoot reparent to the new root, not the surrounding trace" <| \_ -> do
      reported <-
        reportedSpansFor <| \root -> do
          newRootHandler <- Platform.Internal.startNewRoot root "new-root"
          parent <- Platform.Internal.startChildTracingSpan newRootHandler "inner-parent"
          Platform.Internal.finishTracingSpan parent Nothing
          child <- Platform.Internal.startChildTracingSpan parent "late-child"
          Platform.Internal.finishTracingSpan child Nothing
          Platform.Internal.finishTracingSpan newRootHandler Nothing
      List.map Platform.Internal.name reported |> Expect.equal ["new-root", "root"]
      case reported of
        [newRootSpan, rootSpan] -> do
          childNames newRootSpan |> Expect.equal ["inner-parent", "late-child"]
          childNames rootSpan |> Expect.equal []
        _ -> Expect.fail "expected exactly two reported root spans",
    test "silent handler remains a no-op for late children" <| \_ -> do
      Expect.fromIO <| do
        child <- Platform.Internal.startChildTracingSpan Platform.Internal.nullHandler "child"
        Platform.Internal.setTracingSpanSummaryIO child "summary"
        Platform.Internal.finishTracingSpan child Nothing
      Expect.pass
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

-- | Run some IO against a root span's handler and collect every span the root
-- reporter receives, in reporting order.
reportedSpansForIO :: (Platform.Internal.LogHandler -> Prelude.IO ()) -> Prelude.IO [Platform.TracingSpan]
reportedSpansForIO run = do
  reportedRef <- IORef.newIORef []
  Platform.rootTracingSpanIO
    "test-request"
    Platform.silentTrack
    (\span -> IORef.modifyIORef' reportedRef (\spans -> spans ++ [span]))
    "root"
    run
  IORef.readIORef reportedRef

reportedSpansFor :: (Platform.Internal.LogHandler -> Prelude.IO ()) -> Expect.Expectation' [Platform.TracingSpan]
reportedSpansFor run = Expect.fromIO (reportedSpansForIO run)

-- | The names of a span's direct children, sorted for order-independent
-- assertions (children are stored new-to-old).
childNames :: Platform.TracingSpan -> List Text
childNames span =
  Platform.Internal.children span
    |> List.map Platform.Internal.name
    |> List.sort

descendantsNamed :: Text -> Platform.TracingSpan -> List Platform.TracingSpan
descendantsNamed name span =
  let rest = List.concatMap (descendantsNamed name) (Platform.Internal.children span)
   in if Platform.Internal.name span == name
        then span : rest
        else rest
