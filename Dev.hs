-- | Reporting for development
--
-- This reporter logs information about requests in a human-readable format, for
-- use in development.
module Observability.Dev
  ( report,
    logSpanRecursively,
    Handler,
    handler,
    Settings,
    decoder,
  )
where

import Cherry.Prelude
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
import qualified GHC.Stack as Stack
import qualified List
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified MySQL
import qualified Observability.Helpers
import qualified Observability.Timer as Timer
import qualified Platform
import qualified Postgres
import qualified System.IO
import qualified Text
import qualified Prelude

report :: Handler -> Platform.Span -> Prelude.IO ()
report handler' span = do
  Control.Concurrent.threadDelay 10000000
  MVar.putMVar (writeLock handler') (logSpanRecursively (timer handler') span)

type Doc = Doc.Doc Terminal.AnsiStyle

logSpanRecursively :: Timer.Timer -> Platform.Span -> Doc
logSpanRecursively timer' span =
  let (beforeChildren, afterChildren) = logSingleSpan timer' span
   in vcat
        [ Doc.hang 2 beforeChildren,
          Platform.children span
            |> List.reverse
            |> Prelude.map (logSpanRecursively timer')
            |> vcat,
          case afterChildren of
            Nothing -> Doc.emptyDoc
            Just after -> Doc.hang 2 after
        ]

logSingleSpan :: Timer.Timer -> Platform.Span -> (Doc, Maybe Doc)
logSingleSpan timer' span =
  if List.isEmpty (Platform.children span)
    then
      ( vsep
          [ hsep
              [ time timer' (Platform.started span),
                name span,
                failed span
              ],
            trace span,
            exception span,
            duration span,
            details span
          ],
        Nothing
      )
    else
      ( vsep
          [ hsep
              [ time timer' (Platform.started span),
                label "Started:",
                name span
              ],
            trace span,
            details span
          ],
        vsep
          [ hsep
              [ time timer' (Platform.finished span),
                label "Finished:",
                name span,
                failed span
              ],
            exception span,
            duration span
          ]
          |> Just
      )

label :: Doc -> Doc
label text =
  Doc.annotate (Terminal.colorDull Terminal.White) text

details :: Platform.Span -> Doc
details span =
  Platform.details span
    |> Maybe.map flattenDetails
    |> Maybe.withDefault Doc.emptyDoc

flattenDetails :: Platform.SomeSpanDetails -> Doc
flattenDetails details' =
  details'
    |> Platform.renderSpanDetails
      -- Some spans contain information that isn't as useful in a development
      -- setting. We have the opportunity below to pick useful data for specific
      -- span types.
      [ Platform.Renderer mysqlQueryToDetails,
        Platform.Renderer postgresQueryToDetails,
        Platform.Renderer incomingRequestToDetails
      ]
    |> Maybe.withDefault (Observability.Helpers.toHashMap details')
    |> HashMap.toList
    |> Prelude.map (\(key, val) -> hsep [label (Doc.pretty key ++ ":"), Doc.pretty val])
    |> vsep

mysqlQueryToDetails :: MySQL.Info -> HashMap.HashMap Text Text
mysqlQueryToDetails info =
  HashMap.fromList
    [("query", MySQL.infoQuery info)]

postgresQueryToDetails :: Postgres.Info -> HashMap.HashMap Text Text
postgresQueryToDetails info =
  HashMap.fromList
    [ ( "query",
        -- This is a development logger, and so we're not concerned about
        -- logging sensitive data here.
        Log.unSecret (Postgres.infoQuery info)
      )
    ]

incomingRequestToDetails :: Monitoring.RequestDetails -> HashMap.HashMap Text Text
incomingRequestToDetails info =
  HashMap.fromList
    [ ("path", Monitoring.method info ++ " " ++ Monitoring.path info ++ Monitoring.queryString info),
      ("endpoint", Monitoring.endpoint info),
      ("status", Monitoring.responseStatus info |> Text.fromInt)
    ]

exception :: Platform.Span -> Doc
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

trace :: Platform.Span -> Doc
trace span =
  case Platform.frame span of
    Nothing -> Doc.emptyDoc
    Just (_, frame) ->
      hsep
        [ label "source:",
          Doc.pretty (Stack.srcLocFile frame),
          label "line:",
          Doc.pretty (Stack.srcLocStartLine frame),
          label "column:",
          Doc.pretty (Stack.srcLocStartCol frame)
        ]

name :: Platform.Span -> Doc
name = Doc.pretty << Platform.name

time :: Timer.Timer -> Platform.MonotonicTime -> Doc
time timer' time' =
  Timer.toLocal timer' time'
    |> Format.formatTime Format.defaultTimeLocale "%T"
    |> Doc.pretty

duration :: Platform.Span -> Doc
duration span =
  let milliseconds =
        Platform.finished span - Platform.started span
          |> Platform.inMilliseconds
          |> Doc.pretty
   in label "duration: " ++ milliseconds ++ "ms"

failed :: Platform.Span -> Doc
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> Doc.emptyDoc
    Platform.Failed ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)
    Platform.FailedWith _ ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)

data Handler
  = Handler
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
        loggingThread <- Async.async (logLoop writeLock)
        Prelude.pure Handler {timer, writeLock, loggingThread}
    )
    (Async.cancel << loggingThread)

-- | Waits for a log message to become available in the MVar, logs it, then
-- waits for the next one. This is intended to be ran on a separate thread.
logLoop :: MVar.MVar Doc -> Prelude.IO ()
logLoop lock = do
  doc <- MVar.takeMVar lock
  Terminal.hPutDoc System.IO.stdout doc
  System.IO.putStrLn "\n"
  logLoop lock

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

vcat :: [Doc] -> Doc
vcat = removeEmpties >> Doc.vcat

removeEmpties :: [Doc] -> [Doc]
removeEmpties = List.filter (not << isEmpty)

isEmpty :: Doc -> Bool
isEmpty Doc.Internal.Empty = True
isEmpty _ = False
