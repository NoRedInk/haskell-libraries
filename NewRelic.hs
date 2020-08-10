module Observability.NewRelic (reporter) where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Monitoring
import Observability.Timer (Timer, toWord64)
import qualified Platform
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

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
        Prelude.undefined
    )

-- IDEAS:
-- - Record of NewRelic helpers to support tests
-- - Abstract failure handling of new relic functions
-- - Ignore transaction if reporting on any part of it fails

startTime :: Timer -> Platform.Span -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> toWord64 timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

toDuration :: Platform.Span -> NewRelic.DurationUs
toDuration span =
  Platform.finished span - Platform.started span `Prelude.div` 1000
    |> NewRelic.DurationUs

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

expectSuccess :: Bool -> Prelude.IO ()
expectSuccess success =
  if success
    then Prelude.pure ()
    else Exception.throwIO NewRelicFailure

data NewRelicFailure = NewRelicFailure deriving (Show)

instance Exception.Exception NewRelicFailure
