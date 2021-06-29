module Spec.Observability (tests) where

import qualified Conduit
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Data.IORef as IORef
import qualified Environment
import qualified Expect
import qualified Observability
import qualified Platform
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "Observability"
    [ test "When creating a handler for two reports when the handler is reporting a span both reporters are ran" <| \_ -> do
        settings' <- Expect.fromIO <| Environment.decode Observability.decoder
        log <- Expect.fromIO <| IORef.newIORef []
        let reporter1 = fakeReporter (\_ -> appendLog "reporter1" log)
        let reporter2 = fakeReporter (\_ -> appendLog "reporter2" log)
        let settings = settings' {Observability.enabledReporters = [reporter1, reporter2]}
        reports <-
          Expect.fromIO
            ( Conduit.withAcquire (Observability.handler settings) <| \handler -> do
                Observability.report handler "request-id-1234" emptyTracingSpan
                IORef.readIORef log
            )
        reports
          |> Expect.equal ["reporter1", "reporter2"],
      test "The first reporter reports sync exceptions thrown by the other reporters" <| \_ -> do
        settings' <- Expect.fromIO <| Environment.decode Observability.decoder
        log <- Expect.fromIO <| IORef.newIORef []
        let reporter1 = fakeReporter (\span -> appendLog ("reporter1: " ++ Platform.name span) log)
        let reporter2 = fakeReporter (\_ -> Exception.throwString "failed!")
        let settings = settings' {Observability.enabledReporters = [reporter1, reporter2]}
        reports <-
          Expect.fromIO
            ( Conduit.withAcquire (Observability.handler settings) <| \handler -> do
                Observability.report handler "request-id-1234" emptyTracingSpan
                IORef.readIORef log
            )
        reports
          |> Expect.equal ["reporter1: example", "reporter1: Failed to report span to fake"],
      test "The first reporter reports async exceptions thrown by the other reporters" <| \_ -> do
        settings' <- Expect.fromIO <| Environment.decode Observability.decoder
        log <- Expect.fromIO <| IORef.newIORef []
        let reporter1 = fakeReporter (\span -> appendLog ("reporter1: " ++ Platform.name span) log)
        let reporter2 =
              fakeReporter
                ( \_ -> do
                    threadId <- Control.Concurrent.myThreadId
                    Exception.throwTo threadId TestException
                )
        let settings = settings' {Observability.enabledReporters = [reporter1, reporter2]}
        reports <-
          Expect.fromIO <| Conduit.withAcquire (Observability.handler settings) <| \handler -> do
            Observability.report handler "request-id-1234" emptyTracingSpan
              |> Exception.handleAsync (\(Exception.AsyncExceptionWrapper _) -> Prelude.pure ())
            IORef.readIORef log
        reports
          |> Expect.equal ["reporter1: example", "reporter1: Failed to report span to fake"],
      test "A reporter that fails with a sync exception does not prevent other reporters from completing" <| \_ -> do
        settings' <- Expect.fromIO <| Environment.decode Observability.decoder
        log <- Expect.fromIO <| IORef.newIORef []
        let reporter1 = fakeReporter (\span -> appendLog ("reporter1: " ++ Platform.name span) log)
        let reporter2 = fakeReporter (\_ -> Exception.throwString "failed!")
        let reporter3 = fakeReporter (\span -> appendLog ("reporter3: " ++ Platform.name span) log)
        let settings = settings' {Observability.enabledReporters = [reporter1, reporter2, reporter3]}
        reports <-
          Expect.fromIO
            ( Conduit.withAcquire (Observability.handler settings) <| \handler -> do
                Observability.report handler "request-id-1234" emptyTracingSpan
                IORef.readIORef log
            )
        reports
          |> Expect.equal ["reporter1: example", "reporter1: Failed to report span to fake", "reporter3: example"]
    ]

data TestException = TestException deriving (Show)

instance Exception.Exception TestException

appendLog :: Text -> IORef.IORef [Text] -> Prelude.IO ()
appendLog line log =
  IORef.atomicModifyIORef'
    log
    (\prev -> (prev ++ [line], ()))

fakeReporter :: (Platform.TracingSpan -> Prelude.IO ()) -> Observability.Reporter
fakeReporter report =
  Observability.Reporter
    { Observability.reporterName = "fake",
      Observability.reporterSettings = \_ -> (),
      Observability.reporterHandler = \_ -> Prelude.pure (),
      Observability.reporterReport = \_ _ span -> report span
    }

emptyTracingSpan :: Platform.TracingSpan
emptyTracingSpan =
  Platform.emptyTracingSpan
    { Platform.name = "example",
      Platform.started = 0,
      Platform.finished = 0,
      Platform.frame = Nothing,
      Platform.details = Nothing,
      Platform.succeeded = Platform.Succeeded,
      Platform.allocated = 0,
      Platform.children = []
    }
