module Observability.NewRelic (reporter) where

import Cherry.Prelude
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.Word
import qualified GHC.Clock
import qualified Monitoring
import qualified Platform
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

reporter :: NewRelic.App -> Platform.Span -> Prelude.IO ()
reporter app span = do
  timer <- mkTimer
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
-- - Share Timer type between reporters
-- - Ignore transaction if reporting on any part of it fails

-- | Our spans' timestamps are produced by the `GHC.Clock` module and consist of
-- the amount of nanoseconds passed since some arbitrary (but constant) moment
-- in the past. This is the faster and more accurate way to measure precisely
-- what the running time of spans is.
--
-- Now we want to turn these times into regular dates. The `Timer` type is a
-- wrapper around a function that allows us to do that.
newtype Timer = Timer (Data.Word.Word64 -> NewRelic.StartTimeUsSinceUnixEpoch)

mkTimer :: Prelude.IO Timer
mkTimer = do
  -- 'Sync our clocks', to find our how monotonic time and actual time relate.
  nowTime <- Clock.POSIX.getPOSIXTime
  nowClock <- GHC.Clock.getMonotonicTimeNSec
  let nowInNs = Prelude.round (1e9 * Prelude.realToFrac nowTime :: Float)
  let t0InNs = nowInNs - nowClock
  Prelude.pure
    <| Timer
      ( \thenClock ->
          (t0InNs + thenClock) `Prelude.div` 1000
            |> NewRelic.StartTimeUsSinceUnixEpoch
      )

startTime :: Timer -> Platform.Span -> NewRelic.StartTimeUsSinceUnixEpoch
startTime (Timer toTime) span =
  toTime (Platform.started span)

duration :: Platform.Span -> NewRelic.DurationUs
duration span =
  Platform.finished span - Platform.started span `Prelude.div` 1000
    |> NewRelic.DurationUs
