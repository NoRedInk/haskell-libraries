module Internal.Time
  ( Interval,
    fromMicroseconds,
    fromMilliseconds,
    fromSeconds,
    microseconds,
    milliseconds,
    seconds,
  )
where

import Nri.Prelude

-- | A type representing a time interval.
newtype Interval
  = Interval
      { -- | Get the duration of an interval in microseconds.
        microseconds :: Int
      }
  deriving (Eq, Show)

-- | Create an `Interval` lasting a certain number of microseconds.
fromMicroseconds :: Int -> Interval
fromMicroseconds = Interval

-- | Get the duration of an interval in seconds.
seconds :: Interval -> Float
seconds interval = 1e-6 * toFloat (microseconds interval)

-- | Create an `Interval` lasting a certain number of seconds.
fromSeconds :: Float -> Interval
fromSeconds duration = fromMicroseconds (round (1e6 * duration))

-- | Get the duration of an interval in milliseconds.
milliseconds :: Interval -> Float
milliseconds interval = 1e-3 * toFloat (microseconds interval)

-- | Create an `Interval` lasting a certain number of milliseconds.
fromMilliseconds :: Float -> Interval
fromMilliseconds duration = fromMicroseconds (round (1e3 * duration))
