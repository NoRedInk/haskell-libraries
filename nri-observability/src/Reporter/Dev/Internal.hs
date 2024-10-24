{-# LANGUAGE NumericUnderscores #-}

module Reporter.Dev.Internal where

import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Log.HttpRequest as HttpRequest
import qualified Platform
import qualified Platform.Timer as Timer
import qualified Prelude

-- | Print basic information about requests to stdout and make more detailed
-- information available to the log-explorer tool.
--
-- Example usage:
-- > handler <- Dev.handler
-- > Dev.report handler "request-id" span
report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span = do
  Platform.writeSpanToDevLog span
  MVar.putMVar (writeLock handler') (mkLog (timer handler') span)

mkLog :: Timer.Timer -> Platform.TracingSpan -> Builder.Builder
mkLog timer span =
  time timer span
    ++ " "
    ++ name span
    ++ " "
    ++ failed span

time :: Timer.Timer -> Platform.TracingSpan -> Builder.Builder
time timer span =
  Timer.toLocal timer (Platform.started span)
    |> Prelude.show
    |> Builder.fromString

name :: Platform.TracingSpan -> Builder.Builder
name span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer nameIncomingRequest
          ]
      )
    |> Maybe.withDefault (Builder.fromText (Platform.name span))

nameIncomingRequest :: HttpRequest.Incoming -> Builder.Builder
nameIncomingRequest (HttpRequest.Incoming details) =
  HttpRequest.endpoint details
    |> Maybe.withDefault "Incoming HTTP request"
    |> Builder.fromText

failed :: Platform.TracingSpan -> Builder.Builder
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> ""
    Platform.Failed -> "❌"
    Platform.FailedWith err ->
      "❌\n"
        ++ Builder.fromString (Exception.displayException err)

-- | Contextual information this reporter needs to do its work. You can create
-- one using 'handler'.
data Handler = Handler
  { timer :: Timer.Timer,
    -- If we let each request log to stdout directly the result will be lots
    -- of unreadable interleaved output from requests that are handled
    -- concurrently. To prevent this we use an MVar as a lock.
    --
    -- After a request is done it can write it's log to the MVar. If the
    -- MVar already contains a log this operation will block until the MVar
    -- is empty. We have a logging thread running separately that takes logs
    -- from the MVar and prints them to stdout one at a time.
    writeLock :: MVar.MVar Builder.Builder,
    loggingThread :: Async.Async ()
  }

-- | Create a 'Handler'. Do this once when your application starts and reuse
-- the 'Handler' you get.
handler :: Prelude.IO Handler
handler = do
  writeLock <- MVar.newEmptyMVar
  counter <- MVar.newMVar 0
  loggingThread <- Async.async (logLoop counter writeLock)
  timer <- Timer.mkTimer
  Prelude.pure Handler {timer, writeLock, loggingThread}

-- | Clean up your handler after you're done with it. Call this before your
-- application shuts down.
cleanup :: Handler -> Prelude.IO ()
cleanup = Async.cancel << loggingThread

-- | Waits for a log message to become available in the MVar, logs it, then
-- waits for the next one. This is intended to be ran on a separate thread.
logLoop :: MVar.MVar Int -> MVar.MVar Builder.Builder -> Prelude.IO ()
logLoop counter lock = do
  line <- MVar.takeMVar lock
  Builder.toLazyText line
    |> Data.Text.Lazy.toStrict
    |> putTextLn
  ownCount <- MVar.modifyMVar counter (\n -> Prelude.pure (n + 1, n + 1))
  Async.concurrently_
    (logLoop counter lock)
    ( do
        -- After a few seconds of inactivity, advertise for log-explorer.
        Control.Concurrent.threadDelay 3_000_000 {- 3 seconds -}
        currentCount <- MVar.readMVar counter
        if ownCount == currentCount
          then putTextLn "🕵️ Need more detail? Try running the `log-explorer` command!\n"
          else Prelude.pure ()
    )
