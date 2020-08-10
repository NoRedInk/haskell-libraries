module Observability.Timer (Timer (Timer), mkTimer, toUTC, toPOSIX, toWord64, toISO8601) where

import Cherry.Prelude
import qualified Data.Text
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.Time.Format as Format
import qualified Data.Word as Word
import qualified GHC.Clock
import qualified Prelude

-- | Our spans' timestamps are produced by the `GHC.Clock` module and consist of
-- the amount of nanoseconds passed since some arbitrary (but constant) moment
-- in the past. This is the faster and more accurate way to measure precisely
-- what the running time of spans is.
--
-- Now we want to turn these times into regular dates. The `Timer` type stores
-- the POSIX time in nanoseconds that corresponds with t=0 according to
-- `GHC.Clock`. We can use this to calculate other `GHC.Clock` values.
newtype Timer = Timer Word.Word64

mkTimer :: Prelude.IO Timer
mkTimer = do
  -- 'Sync our clocks', to find our how monotonic time and actual time relate.
  nowTime <- Clock.POSIX.getPOSIXTime
  nowClock <- GHC.Clock.getMonotonicTimeNSec
  Prelude.floor (1e9 * nowTime) - nowClock
    |> Timer
    |> Prelude.pure

toUTC :: Timer -> Word.Word64 -> Clock.UTCTime
toUTC timer clock =
  toPOSIX timer clock
    |> Clock.POSIX.posixSecondsToUTCTime

toWord64 :: Timer -> Word.Word64 -> Word.Word64
toWord64 (Timer t0) clock = t0 + clock

toPOSIX :: Timer -> Word.Word64 -> Clock.POSIX.POSIXTime
toPOSIX timer clock =
  toWord64 timer clock
    |> Prelude.fromIntegral
    |> (*) 1e-9

toISO8601 :: Timer -> Word.Word64 -> Text
toISO8601 timer clock =
  toUTC timer clock
    |> Format.formatTime Format.defaultTimeLocale "%FT%T%QZ"
    |> Data.Text.pack
