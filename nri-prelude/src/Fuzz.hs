-- | This is a library of fuzzers you can use to supply values to your fuzz tests. You can typically pick out which ones you need according to their types.
-- A @Fuzzer@ a knows how to create values of type @a@ in two different ways. It can create them randomly, so that your test's expectations are run against many values. Fuzzers will often generate edge cases likely to find bugs. If the fuzzer can make your test fail, it also knows how to "shrink" that failing input into more minimal examples, some of which might also cause the tests to fail. In this way, fuzzers can usually find the smallest or simplest input that reproduces a bug.
module Fuzz
  ( -- * Common Fuzzers
    int,
    intRange,
    list,

    -- * Working with Fuzzers
    Fuzzer,
  )
where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import NriPrelude

type Fuzzer = Hedgehog.Gen

-- | A fuzzer for int values. It will never produce @NaN@, @Infinity@, or @-Infinity@.
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
-- is the maximum possible int value, so you can do @intRange x Random.maxInt@ to get all
-- the ints x or bigger.
intRange :: Int -> Int -> Fuzzer Int
intRange min_ max_ = Gen.integral (Range.linear min_ max_)

-- | Given a fuzzer of a type, create a fuzzer of a list of that type. Generates random lists of varying length, favoring shorter lists.
list :: Fuzzer a -> Fuzzer (List a)
list = Gen.list (Range.linear 0 10)

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648
