-- | Helper functions for converting the timestamp format used in our `Span`
-- data to the time formats used by the various platforms we report to.
module Observability.Timer
  ( Timer (Timer),
    mkTimer,
    toUTC,
    toLocal,
    toPosix,
    toPosixMicroseconds,
    toISO8601,
    difference,
  )
where

import qualified Data.Text
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Word as Word
import qualified GHC.Clock
import NriPrelude
import qualified Platform
import qualified Prelude

-- | Our spans' timestamps are produced by the `GHC.Clock` module and consist of
-- the amount of time passed since some arbitrary (but constant) moment in the
-- past. This is the faster and more accurate way to measure precisely what the
-- running time of spans is. This type helpers convert these times into regular
-- dates.
data Timer
  = Timer
      { -- | The POSIX time in microseconds that corresponds with t=0 according
        -- to `GHC.Clock`. We can use this to calculate other `GHC.Clock`
        -- values.
        tzero :: Word.Word64,
        -- | The timezone of the machine this code is running on. Useful for
        -- printing local times in development reporters.
        timezone :: LocalTime.TimeZone
      }

mkTimer :: Prelude.IO Timer
mkTimer = do
  -- 'Sync our clocks', to find our how monotonic time and actual time relate.
  nowTime <- Clock.POSIX.getPOSIXTime
  nowClock <- GHC.Clock.getMonotonicTimeNSec
  timezone <- LocalTime.getCurrentTimeZone
  let tzero = Prelude.floor (1e6 * nowTime) - (nowClock `Prelude.div` 1000)
  Prelude.pure Timer {tzero, timezone}

toUTC :: Timer -> Platform.MonotonicTime -> Clock.UTCTime
toUTC timer clock =
  toPosix timer clock
    |> Clock.POSIX.posixSecondsToUTCTime

toLocal :: Timer -> Platform.MonotonicTime -> LocalTime.LocalTime
toLocal timer clock =
  toUTC timer clock
    |> LocalTime.utcToLocalTime (timezone timer)

toPosixMicroseconds :: Timer -> Platform.MonotonicTime -> Word.Word64
toPosixMicroseconds timer clock = tzero timer + Platform.inMicroseconds clock

toPosix :: Timer -> Platform.MonotonicTime -> Clock.POSIX.POSIXTime
toPosix timer clock =
  toPosixMicroseconds timer clock
    |> Prelude.fromIntegral
    |> (*) 1e-6

toISO8601 :: Timer -> Platform.MonotonicTime -> Text
toISO8601 timer clock =
  toUTC timer clock
    |> Format.formatTime Format.defaultTimeLocale "%FT%T%QZ"
    |> Data.Text.pack

-- | We have to be careful when calculating the difference between two times.
-- Because they are unsigned (don't allow negative numbers), subtracting times
-- in the wrong order is going to result in very large numbers:
--
--     ghci> import GHC.Word
--     ghci> 5 - 2 :: Word64
--     3
--     ghci> 2 - 5 :: Word64
--     18446744073709551613
--
-- The span data we get from Platform should ensure end times always come
-- before start times. If they're not though one of these extremely long span
-- durations can have a major effect on request duration statistics.
--
-- This function performs some defensive programming to prevent flukes from
-- doing major damage.
difference :: Platform.MonotonicTime -> Platform.MonotonicTime -> Platform.MonotonicTime
difference start end =
  if end > start
    then end - start
    else 0
