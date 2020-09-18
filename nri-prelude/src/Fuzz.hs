module Fuzz
  ( Fuzzer,
    int,
    intRange,
    list,
  )
where

import NriPrelude
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type Fuzzer = Hedgehog.Gen

-- | A fuzzer for int values. It will never produce `NaN`, `Infinity`, or `-Infinity`.
-- It's possible for this fuzzer to generate any 32-bit integer, signed or unsigned, but it favors
-- numbers between -50 and 50 and especially zero.
int :: Fuzzer Int
int =
  Gen.frequency
    [ (30, intRange (-50) 50),
      (2, Gen.constant 0),
      (10, intRange 0 (maxInt - minInt)),
      (10, intRange (minInt - maxInt) 0)
    ]

-- | A fuzzer for int values between a given minimum and maximum value,
-- inclusive. Shrunken values will also be within the range.
-- Remember that [Random.maxInt](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#maxInt)
-- is the maximum possible int value, so you can do `intRange x Random.maxInt` to get all
-- the ints x or bigger.
intRange :: Int -> Int -> Fuzzer Int
intRange min_ max_ = Gen.integral (Range.linear min_ max_)

list :: Fuzzer a -> Fuzzer (List a)
list = Gen.list (Range.linear 0 10)

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648
