-- | Helpers for testing code that sends messages to Kafka.
module Kafka.Test
  ( stub,
  )
where

import qualified Data.IORef
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Kafka
import qualified Kafka.Internal as Internal
import qualified Platform

-- | Can be used to test your Kafka writer.
-- yields a mock Kafka handler, and returns an expectation wrapping a list of
-- messages that would have been written if the handler was real
stub ::
  Stack.HasCallStack =>
  (Internal.Handler -> Expect.Expectation) ->
  Expect.Expectation' (List Kafka.Msg)
stub stubbed = do
  logRef <- Expect.fromIO (Data.IORef.newIORef [])
  doAnything <- Expect.fromIO Platform.doAnythingHandler
  let sendStub = \msg' -> do
        Data.IORef.modifyIORef' logRef (\prev -> msg' : prev)
          |> map Ok
          |> Platform.doAnything doAnything
  let mockHandler =
        Internal.Handler
          { Internal.sendAsync = \_ -> sendStub,
            Internal.sendSync = sendStub
          }
  Expect.around (\f -> f mockHandler) (Stack.withFrozenCallStack stubbed)
  Expect.fromIO (Data.IORef.readIORef logRef)
    |> map List.reverse
