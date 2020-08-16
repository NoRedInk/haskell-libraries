module Observability.Timer (Timer (Timer), mkTimer, toUTC, toPosix, toPosixMilliseconds, toISO8601) where

import Cherry.Prelude
import qualified Data.Text
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.Time.Format as Format
import qualified Data.Word as Word
import qualified GHC.Clock
import qualified Platform
import qualified Prelude

-- | Our spans' timestamps are produced by the `GHC.Clock` module and consist of
-- the amount of time passed since some arbitrary (but constant) moment in the
-- past. This is the faster and more accurate way to measure precisely what the
-- running time of spans is.
--
-- Now we want to turn these times into regular dates. The `Timer` type stores
-- the POSIX time in milliseconds that corresponds with t=0 according to
-- `GHC.Clock`. We can use this to calculate other `GHC.Clock` values.
newtype Timer = Timer Word.Word64

mkTimer :: Prelude.IO Timer
mkTimer = do
  -- 'Sync our clocks', to find our how monotonic time and actual time relate.
  nowTime <- Clock.POSIX.getPOSIXTime
  nowClock <- GHC.Clock.getMonotonicTimeNSec
  Prelude.floor (1e3 * nowTime) - (nowClock `Prelude.div` 1000000)
    |> Timer
    |> Prelude.pure

toUTC :: Timer -> Platform.MonotonicTime -> Clock.UTCTime
toUTC timer clock =
  toPosix timer clock
    |> Clock.POSIX.posixSecondsToUTCTime

toPosixMilliseconds :: Timer -> Platform.MonotonicTime -> Word.Word64
toPosixMilliseconds (Timer t0) clock = t0 + Platform.inMilliseconds clock

toPosix :: Timer -> Platform.MonotonicTime -> Clock.POSIX.POSIXTime
toPosix timer clock =
  toPosixMilliseconds timer clock
    |> Prelude.fromIntegral
    |> (*) 1e-3

toISO8601 :: Timer -> Platform.MonotonicTime -> Text
toISO8601 timer clock =
  toUTC timer clock
    |> Format.formatTime Format.defaultTimeLocale "%FT%T%QZ"
    |> Data.Text.pack
