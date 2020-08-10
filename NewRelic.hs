module Observability.NewRelic (reporter) where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Foldable as Foldable
import qualified Http
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified MySQL
import Observability.Timer (Timer, toWord64)
import qualified Platform
import qualified Postgres
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

-- TODO:
-- [ ] Ignore transaction if reporting on any part of it fails.
-- [ ] Strategy for dealing with exceptions that reach the reporter.
-- [ ] Figure out which attributes to report.

reporter :: Timer -> NewRelic.App -> Platform.Span -> Prelude.IO ()
reporter timer app span =
  case Platform.details span |> andThen Platform.fromSpanDetails of
    Nothing -> Prelude.pure ()
    Just details -> reportWebTransaction timer app span details

reportWebTransaction :: Timer -> NewRelic.App -> Platform.Span -> Monitoring.RequestDetails -> Prelude.IO ()
reportWebTransaction timer app span details =
  Exception.bracket
    (startWebTransaction app (Monitoring.endpoint details))
    endTransaction
    ( \tx -> do
        setTransactionTiming tx (startTime timer span) (toDuration span)
        reportSpan timer tx Nothing span
    )

reportSpan ::
  Timer ->
  NewRelic.Transaction ->
  Maybe NewRelic.Segment ->
  Platform.Span ->
  Prelude.IO ()
reportSpan timer tx maybeParent span =
  Exception.bracket
    (startSegment tx (Platform.name span) (category span))
    endSegment
    ( \segment -> do
        setSegmentTiming segment (startTime timer span) (toDuration span)
        case maybeParent of
          Just parent -> setSegmentParent segment parent
          Nothing -> setSegmentParentRoot segment
        Platform.children span
          |> Foldable.traverse_ (reportSpan timer tx (Just segment))
    )

startTime :: Timer -> Platform.Span -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> toWord64 timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

toDuration :: Platform.Span -> NewRelic.DurationUs
toDuration span =
  Platform.finished span - Platform.started span `Prelude.div` 1000
    |> NewRelic.DurationUs

category :: Platform.Span -> Text
category span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderSpanDetails
          [ Platform.Renderer (\(_ :: MySQL.Info) -> "mysql"),
            Platform.Renderer (\(_ :: Postgres.Info) -> "postgres"),
            Platform.Renderer (\(_ :: Http.Info) -> "http"),
            Platform.Renderer (\(_ :: Monitoring.RequestDetails) -> "request"),
            Platform.Renderer (\(_ :: Log.LogContexts) -> "haskell")
          ]
      )
    |> Maybe.withDefault "unknown"

--
-- NewRelic API
--
-- Wrappers around the functions from `tracing-newrelic` that throw in case of
-- failure.
--

startWebTransaction :: NewRelic.App -> Text -> Prelude.IO NewRelic.Transaction
startWebTransaction app name = do
  maybeTx <- NewRelic.startWebTransaction app name
  case maybeTx of
    Nothing -> Exception.throwIO NewRelicFailure
    Just tx -> Prelude.pure tx

endTransaction :: NewRelic.Transaction -> Prelude.IO ()
endTransaction tx =
  NewRelic.endTransaction tx
    |> andThen expectSuccess

setTransactionTiming ::
  NewRelic.Transaction ->
  NewRelic.StartTimeUsSinceUnixEpoch ->
  NewRelic.DurationUs ->
  Prelude.IO ()
setTransactionTiming tx start duration =
  NewRelic.setTransactionTiming tx start duration
    |> andThen expectSuccess

startSegment ::
  NewRelic.Transaction ->
  Text ->
  Text ->
  Prelude.IO NewRelic.Segment
startSegment tx name cat = do
  maybeSegment <- NewRelic.startSegment tx (Just name) (Just cat)
  case maybeSegment of
    Nothing -> Exception.throwIO NewRelicFailure
    Just segment -> Prelude.pure segment

endSegment :: NewRelic.Segment -> Prelude.IO ()
endSegment segment =
  NewRelic.endSegment segment
    |> andThen expectSuccess

setSegmentTiming ::
  NewRelic.Segment ->
  NewRelic.StartTimeUsSinceUnixEpoch ->
  NewRelic.DurationUs ->
  Prelude.IO ()
setSegmentTiming tx start duration =
  NewRelic.setSegmentTiming tx start duration
    |> andThen expectSuccess

setSegmentParent :: NewRelic.Segment -> NewRelic.Segment -> Prelude.IO ()
setSegmentParent segment parent =
  NewRelic.setSegmentParent segment parent
    |> andThen expectSuccess

setSegmentParentRoot :: NewRelic.Segment -> Prelude.IO ()
setSegmentParentRoot segment =
  NewRelic.setSegmentParentRoot segment
    |> andThen expectSuccess

expectSuccess :: Bool -> Prelude.IO ()
expectSuccess success =
  if success
    then Prelude.pure ()
    else Exception.throwIO NewRelicFailure

data NewRelicFailure = NewRelicFailure deriving (Show)

instance Exception.Exception NewRelicFailure
