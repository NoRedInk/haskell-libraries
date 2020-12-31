-- | This is a library of fuzzers you can use to supply values to your fuzz
-- tests. You can typically pick out which ones you need according to their
-- types.
--
-- A @Fuzzer a@ knows how to create values of type @a@ in two different ways.
-- It can create them randomly, so that your test's expectations are run
-- against many values. Fuzzers will often generate edge cases likely to find
-- bugs. If the fuzzer can make your test fail, it also knows how to "shrink"
-- that failing input into more minimal examples, some of which might also
-- cause the tests to fail. In this way, fuzzers can usually find the smallest
-- or simplest input that reproduces a bug.
module Fuzz
  ( -- * Common Fuzzers
    int,
    intRange,
    float,
    floatRange,
    percentage,
    text,
    bool,
    maybe,
    result,
    list,
    array,

    -- * Working with Fuzzers
    Fuzzer,
    oneOf,
    constant,
    frequency,

    -- * Tuple Fuzzers
    tuple,
    tuple3,

    -- * Uncommon Fuzzers
    char,
    unit,
    order,
  )
where

import qualified Array
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import NriPrelude
import Test.Internal (Fuzzer (Fuzzer, unFuzzer))
import qualified Prelude

-- | A fuzzer for int values. It will never produce @NaN@, @Infinity@, or
-- @-Infinity@. It's possible for this fuzzer to generate any 64-bit integer,
-- signed or unsigned, but it favors smaller numbers.
int :: Fuzzer Int
int =
  Gen.integral Range.exponentialBounded
    |> Fuzzer

-- | A fuzzer for int values between a given minimum and maximum value,
-- inclusive.
intRange :: Int -> Int -> Fuzzer Int
intRange min_ max_ =
  Gen.integral (Range.linearFrom (clamp min_ max_ 0) min_ max_)
    |> Fuzzer

-- | A fuzzer for float values. It will never produce NaN, Infinity, or
-- -Infinity.
--
-- It's possible for this fuzzer to generate other floating-point value, but it
-- favors smaller numbers.
float :: Fuzzer Float
float =
  Gen.double (Range.exponentialFloatFrom 0 (-1e100) 1e100)
    |> Fuzzer

-- | A fuzzer for float values within between a given minimum and maximum
-- value, inclusive.
floatRange :: Float -> Float -> Fuzzer Float
floatRange min_ max_ =
  Gen.double (Range.linearFracFrom (clamp min_ max_ 0) min_ max_)
    |> Fuzzer

-- | A fuzzer for percentage values. Generates random floats between 0.0 and
-- 1.0.
percentage :: Fuzzer Float
percentage =
  Gen.double (Range.linearFrac 0 1)
    |> Fuzzer

-- | Generates random printable ASCII strings of up to 1000 characters.
--
-- Shorter strings are more common, especially the empty string.
text :: Fuzzer Text
text =
  Gen.text (Range.exponential 0 1000) Gen.ascii
    |> Fuzzer

-- | A fuzzer for boolean values. It's useful when building up fuzzers of
-- complex types that contain a boolean somewhere.

-- We recommend against writing tests fuzzing over booleans. Write a unit test
-- for the true and false cases explicitly.
bool :: Fuzzer Bool
bool =
  Gen.bool
    |> Fuzzer

-- | Given a fuzzer of a type, create a fuzzer of a maybe for that type.
maybe :: Fuzzer a -> Fuzzer (Maybe a)
maybe (Fuzzer gen) =
  Gen.maybe gen
    |> Fuzzer

-- | Given fuzzers for an error type and a success type, create a fuzzer for a
-- result.
result :: Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result errorFuzzer valueFuzzer =
  oneOf
    [ map Err errorFuzzer,
      map Ok valueFuzzer
    ]

-- | Given a fuzzer of a type, create a fuzzer of a list of that type.
-- Generates random lists of varying length, favoring shorter lists.
list :: Fuzzer a -> Fuzzer (List a)
list (Fuzzer gen) =
  Gen.list (Range.exponential 0 100) gen
    |> Fuzzer

-- | Given a fuzzer of a type, create a fuzzer of an array of that type.
-- Generates random arrays of varying length, favoring shorter arrays.
array :: Fuzzer a -> Fuzzer (Array.Array a)
array itemFuzzer =
  list itemFuzzer
    |> map Array.fromList

-- | Choose one of the given fuzzers at random. Each fuzzer has an equal chance
-- of being chosen; to customize the probabilities, use 'frequency'.
oneOf :: List (Fuzzer a) -> Fuzzer a
oneOf optionFuzzers =
  map unFuzzer optionFuzzers
    |> Gen.choice
    |> Fuzzer

-- | Create a fuzzer that only and always returns the value provided, and
-- performs no shrinking. This is hardly random, and so this function is best
-- used as a helper when creating more complicated fuzzers.
constant :: a -> Fuzzer a
constant x =
  Gen.constant x
    |> Fuzzer

-- | Create a new Fuzzer by providing a list of probabilistic weights to use
-- with other fuzzers. For example, to create a Fuzzer that has a 1/4 chance of
-- generating an int between -1 and -100, and a 3/4 chance of generating one
-- between 1 and 100, you could do this:
--
-- > Fuzz.frequency
-- >     [ ( 1, Fuzz.intRange -100 -1 )
-- >     , ( 3, Fuzz.intRange 1 100 )
-- >     ]
--
-- There are a few circumstances in which this function will return an invalid
-- fuzzer, which causes it to fail any test that uses it:
--
-- - If you provide an empty list of frequencies
-- - If any of the weights are less than 0
-- - If the weights sum to 0
-- - Be careful recursively using this fuzzer in its arguments. Often using map
--   is a better way to do what you want. If you are fuzzing a tree-like data
--   structure, you should include a depth limit so to avoid infinite recursion,
--   like so:
--
-- > data Tree
-- >       = Leaf
-- >       | Branch Tree Tree
-- >
-- > tree :: Int -> Fuzzer Tree
-- > tree i =
-- >     if i <= 0 then
-- >         Fuzz.constant Leaf
-- >
-- >     else
-- >         Fuzz.frequency
-- >             [ ( 1, Fuzz.constant Leaf )
-- >             , ( 2, Fuzz.map2 Branch (tree (i - 1)) (tree (i - 1)) )
-- >             ]
frequency :: List (Float, Fuzzer a) -> Fuzzer a
frequency optionFuzzers =
  optionFuzzers
    |> map (\(float', (Fuzzer gen)) -> (Prelude.round (float' * 1000000), gen))
    |> Gen.frequency
    |> Fuzzer

-- | Turn a tuple of fuzzers into a fuzzer of tuples.
tuple :: (Fuzzer a, Fuzzer b) -> Fuzzer (a, b)
tuple (fuzzerA, fuzzerB) =
  map2 (,) fuzzerA fuzzerB

-- | Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.
tuple3 :: (Fuzzer a, Fuzzer b, Fuzzer c) -> Fuzzer (a, b, c)
tuple3 (fuzzerA, fuzzerB, fuzzerC) =
  map3 (,,) fuzzerA fuzzerB fuzzerC

-- | A fuzzer for char values. Generates random ascii chars disregarding the
-- control characters and the extended character set.
char :: Fuzzer Char
char =
  Gen.ascii
    |> Fuzzer

-- | A fuzzer for the unit value. Unit is a type with only one value, commonly
-- used as a placeholder.
unit :: Fuzzer ()
unit = constant ()

-- | A fuzzer for order values.
order :: Fuzzer Ordering
order = oneOf [constant EQ, constant LT, constant GT]
