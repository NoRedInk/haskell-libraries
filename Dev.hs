{-# LANGUAGE NumericUnderscores #-}

-- | Reporting for development
--
-- This reporter logs information about requests in a human-readable format, for
-- use in development.
module Observability.Dev
  ( report,
    Handler,
    handler,
    Settings,
    decoder,

    -- * Exported for tests
    mkLog,
  )
where

import qualified Conduit
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Internal as Doc.Internal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal
import qualified Data.Time.Format as Format
import qualified Environment
import qualified List
import qualified Maybe
import qualified Monitoring
import NriPrelude
import qualified Observability.Helpers
import qualified Observability.Timer as Timer
import qualified Platform
import qualified System.IO
import qualified Text
import qualified Prelude

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span = do
  Platform.writeSpanToDevLog span
  MVar.putMVar (writeLock handler') (mkLog (timer handler') span)

type Doc = Doc.Doc Terminal.AnsiStyle

mkLog :: Timer.Timer -> Platform.TracingSpan -> Doc
mkLog timer' span =
  vsep
    [ hsep
        [ time timer' (Platform.started span),
          name span,
          failed span
        ],
      exception span,
      duration span,
      details span
    ]
    |> Doc.hang 2

label :: Doc -> Doc
label text =
  Doc.annotate (Terminal.colorDull Terminal.White) text

details :: Platform.TracingSpan -> Doc
details span =
  Platform.details span
    |> Maybe.map flattenDetails
    |> Maybe.withDefault Doc.emptyDoc

flattenDetails :: Platform.SomeTracingSpanDetails -> Doc
flattenDetails details' =
  details'
    |> Platform.renderTracingSpanDetails
      -- Some spans contain information that isn't as useful in a development
      -- setting. We have the opportunity below to pick useful data for specific
      -- span types.
      [ Platform.Renderer incomingRequestToDetails
      ]
    |> Maybe.withDefault (Observability.Helpers.toHashMap details')
    |> HashMap.toList
    |> Prelude.map (\(key, val) -> hsep [label (Doc.pretty key ++ ":"), Doc.pretty val])
    |> vsep

incomingRequestToDetails :: Monitoring.RequestDetails -> HashMap.HashMap Text Text
incomingRequestToDetails info =
  HashMap.fromList
    [ ("path", Monitoring.method info ++ " " ++ Monitoring.path info ++ Monitoring.queryString info),
      ("endpoint", Monitoring.endpoint info),
      ("status", Monitoring.responseStatus info |> Text.fromInt)
    ]

exception :: Platform.TracingSpan -> Doc
exception span =
  case Platform.succeeded span of
    Platform.Succeeded -> Doc.emptyDoc
    Platform.Failed -> Doc.emptyDoc
    Platform.FailedWith err ->
      hsep
        [ label "exception:",
          Exception.displayException err
            |> Doc.pretty
            |> Doc.annotate (Terminal.color Terminal.Red)
        ]

name :: Platform.TracingSpan -> Doc
name = Doc.pretty << Platform.name

time :: Timer.Timer -> Platform.MonotonicTime -> Doc
time timer' time' =
  Timer.toLocal timer' time'
    |> Format.formatTime Format.defaultTimeLocale "%T"
    |> Doc.pretty

duration :: Platform.TracingSpan -> Doc
duration span =
  let milliseconds =
        Platform.finished span - Platform.started span
          |> Platform.inMicroseconds
          |> Doc.pretty
   in label "duration: " ++ milliseconds ++ "us"

failed :: Platform.TracingSpan -> Doc
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> Doc.emptyDoc
    Platform.Failed ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)
    Platform.FailedWith _ ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)

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
    writeLock :: MVar.MVar Doc,
    loggingThread :: Async.Async ()
  }

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer Settings =
  Conduit.mkAcquire
    ( do
        writeLock <- MVar.newEmptyMVar
        counter <- MVar.newMVar 0
        loggingThread <- Async.async (logLoop counter writeLock)
        Prelude.pure Handler {timer, writeLock, loggingThread}
    )
    (Async.cancel << loggingThread)

-- | Waits for a log message to become available in the MVar, logs it, then
-- waits for the next one. This is intended to be ran on a separate thread.
logLoop :: MVar.MVar Int -> MVar.MVar Doc -> Prelude.IO ()
logLoop counter lock = do
  doc <- MVar.takeMVar lock
  Terminal.hPutDoc System.IO.stdout doc
  System.IO.putStrLn "\n"
  ownCount <- MVar.modifyMVar counter (\n -> Prelude.pure (n + 1, n + 1))
  Async.concurrently_
    (logLoop counter lock)
    ( do
        -- After a few seconds of inactivity, advertise for log-explorer.
        Control.Concurrent.threadDelay 3_000_000 {- 3 seconds -}
        currentCount <- MVar.readMVar counter
        if ownCount == currentCount
          then Prelude.putStrLn "🕵️ Need more detail? Try running the `log-explorer` command!\n"
          else Prelude.pure ()
    )

data Settings = Settings

decoder :: Environment.Decoder Settings
decoder = Prelude.pure Settings

--
-- Pretty print helpers for combining lists of docs.
-- These versions of functions of the prettyprinter library remove empty
-- elements from lists to prevent whitespace from being added around empty
-- elements.
--

vsep :: [Doc] -> Doc
vsep = removeEmpties >> Doc.vsep

hsep :: [Doc] -> Doc
hsep = removeEmpties >> Doc.hsep

removeEmpties :: [Doc] -> [Doc]
removeEmpties = List.filter (not << isEmpty)

isEmpty :: Doc -> Bool
isEmpty Doc.Internal.Empty = True
isEmpty _ = False
