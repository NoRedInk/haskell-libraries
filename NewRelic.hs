module Observability.NewRelic (reporter) where

import Cherry.Prelude
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
reportWebTransaction timer app span details = do
  maybeTx <- NewRelic.startWebTransaction app (Monitoring.endpoint details)
  case maybeTx of
    Nothing -> Prelude.pure ()
    Just tx -> do
      _ <- NewRelic.setTransactionTiming tx (startTime timer span) (duration span)
      Prelude.undefined

-- IDEAS:
-- - Record of NewRelic helpers to support tests
-- - Abstract failure handling of new relic functions
-- - Ignore transaction if reporting on any part of it fails

startTime :: Timer -> Platform.Span -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> toWord64 timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

duration :: Platform.Span -> NewRelic.DurationUs
duration span =
  Platform.finished span - Platform.started span `Prelude.div` 1000
    |> NewRelic.DurationUs
