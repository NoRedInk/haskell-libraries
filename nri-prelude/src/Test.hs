-- | A module containing functions for creating and managing tests.
--
-- @docs Test, test
--
-- Organizing Tests
-- @docs describe, concat, todo, skip, only
--
-- Fuzz Testing
-- @docs fuzz, fuzz2, fuzz3, fuzzWith, FuzzOptions
module Test
  ( Test,
    test,
    describe,
    concat,
    skip,
    only,
    todo,
    fuzz,
    fuzz2,
    fuzz3,
    fromTestTree,
    task,
  )
where

import NriPrelude
import qualified Data.Text
import qualified Expect
import Fuzz (Fuzzer)
import qualified Internal.Expectation
import qualified Internal.Test
import Internal.TestResult (TestFailure)
import qualified Platform
import qualified Task
import Test.Tasty (TestName, TestTree)
import Prelude (Show)

-- | A test which has yet to be evaluated. When evaluated, it produces one
-- or more [`Expectation`](../Expect#Expectation)s.
-- See [`test`](#test) and [`fuzz`](#fuzz) for some ways to create a `Test`.
type Test = Internal.Test.Test

-- | Apply a description to a list of tests.
--
--    import Test (describe, test, fuzz)
--    import Fuzz (int)
--    import Expect
--
--    describe "List"
--        [ describe "reverse"
--            [ test "has no effect on an empty list" <|
--                \_ ->
--                    List.reverse []
--                        |> Expect.equal []
--            , fuzz int "has no effect on a one-item list" <|
--                \num ->
--                     List.reverse [ num ]
--                        |> Expect.equal [ num ]
--            ]
--        ]
--
-- Passing an empty list will result in a failing test, because you either made a
-- mistake or are creating a placeholder.
describe :: Text -> List Test -> Test
describe =
  Internal.Test.Describe

-- | Run each of the given tests.
--
--    concat [ testDecoder, testSorting ]
concat :: List Test -> Test
concat =
  Internal.Test.Describe ""

-- | Return a [`Test`](#Test) that evaluates a single
-- [`Expectation`](../Expect#Expectation).
--
--    import Test (fuzz)
--    import Expect
--    test "the empty list has 0 length" <|
--        \_ ->
--            List.length []
--                |> Expect.equal 0
test :: Text -> (() -> Expect.Expectation) -> Test
test =
  Internal.Test.Test

-- | Returns a [`Test`](#Test) that gets skipped.
-- Calls to `skip` aren't meant to be committed to version control. Instead, use
-- it when you want to focus on getting a particular subset of your tests to
-- pass. If you use `skip`, your entire test suite will fail, even if
-- each of the individual tests pass. This is to help avoid accidentally
-- committing a `skip` to version control.
-- See also [`only`](#only). Note that `skip` takes precedence over `only`;
-- if you use a `skip` inside an `only`, it will still get skipped, and if you use
-- an `only` inside a `skip`, it will also get skipped.
--
--    describe "List"
--        [ skip <|
--            describe "reverse"
--                [ test "has no effect on an empty list" <|
--                    \_ ->
--                        List.reverse []
--                            |> Expect.equal []
--                , fuzz int "has no effect on a one-item list" <|
--                    \num ->
--                        List.reverse [ num ]
--                            |> Expect.equal [ num ]
--                ]
--        , test "This is the only test that will get run; the other was skipped!" <|
--            \_ ->
--                List.length []
--                    |> Expect.equal 0
--        ]
skip :: Test -> Test
skip = Internal.Test.Skip

-- | Returns a [`Test`](#Test) that causes other tests to be skipped, and
-- only runs the given one.
-- Calls to `only` aren't meant to be committed to version control. Instead, use
-- them when you want to focus on getting a particular subset of your tests to pass.
-- If you use `only`, your entire test suite will fail, even if
-- each of the individual tests pass. This is to help avoid accidentally
-- committing a `only` to version control.
-- If you you use `only` on multiple tests, only those tests will run. If you
-- put a `only` inside another `only`, only the outermost `only`
-- will affect which tests gets run.
-- See also [`skip`](#skip). Note that `skip` takes precedence over `only`;
-- if you use a `skip` inside an `only`, it will still get skipped, and if you use
-- an `only` inside a `skip`, it will also get skipped.
--
--    describe "List"
--        [ only <|
--            describe "reverse"
--                [ test "has no effect on an empty list" <|
--                    \_ ->
--                        List.reverse []
--                            |> Expect.equal []
--                , fuzz int "has no effect on a one-item list" <|
--                    \num ->
--                        List.reverse [ num ]
--                            |> Expect.equal [ num ]
--                ]
--        , test "This will not get run, because of the `only` above!" <|
--            \_ ->
--                List.length []
--                    |> Expect.equal 0
--        ]
only :: Test -> Test
only = Internal.Test.Only

-- | Returns a [`Test`](#Test) that is "todo" (not yet implemented). These tests
-- always fail.
-- These tests aren't meant to be committed to version control. Instead, use them
-- when you're brainstorming lots of tests you'd like to write, but you can't
-- implement them all at once. When you replace `todo` with a real test, you'll be
-- able to see if it fails without clutter from tests still not implemented. But,
-- unlike leaving yourself comments, you'll be prompted to implement these tests
-- because your suite will fail.
--
--    describe "a new thing"
--        [ todo "does what is expected in the common case"
--        , todo "correctly handles an edge case I just thought of"
--        ]
--
-- This functionality is similar to "pending" tests in other frameworks, except
-- that a todo test is considered failing but a pending test often is not.
todo :: Text -> Test
todo = Internal.Test.Todo

-- | Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
-- from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Fuzz) each time. This allows you to
-- test that a property that should always be true is indeed true under a wide variety of conditions. The function also
-- takes a string describing the test.
-- These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
-- You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
-- [generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
-- [QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).
--
--    import Test (fuzz)
--    import Fuzz (list, int)
--    import Expect
--
--    fuzz (list int) "List.length should always be positive" <|
--        -- This anonymous function will be run 100 times, each time with a
--        -- randomly-generated fuzzList value.
--        \fuzzList ->
--            fuzzList
--                |> List.length
--                |> Expect.atLeast 0
--
-- NOTE: You can use any Hedgehog.Gen for Fuzzer.
fuzz :: Show a => Fuzzer a -> Text -> (a -> Expect.Expectation) -> Test
fuzz a name cb = Internal.Test.Fuzz (Internal.Test.Fuzzer1 a cb) name

-- | Run a [fuzz test](#fuzz) using two random inputs.
-- This is a convenience function that lets you skip calling [`Fuzz.tuple`](Fuzz#tuple).
-- See [`fuzzWith`](#fuzzWith) for an example of writing this in tuple style.
--
--    import Test (fuzz2)
--    import Fuzz (list, int)
--
--    fuzz2 (list int) int "List.reverse never influences List.member" <|
--        \nums target ->
--            List.member target (List.reverse nums)
--                |> Expect.equal (List.member target nums)
fuzz2 :: (Show a, Show b) => Fuzzer a -> Fuzzer b -> Text -> (a -> b -> Expect.Expectation) -> Test
fuzz2 a b name cb = Internal.Test.Fuzz (Internal.Test.Fuzzer2 a b cb) name

-- | Run a [fuzz test](#fuzz) using three random inputs.
-- This is a convenience function that lets you skip calling [`Fuzz.tuple3`](Fuzz#tuple3).
fuzz3 :: (Show a, Show b, Show c) => Fuzzer a -> Fuzzer b -> Fuzzer c -> Text -> (a -> b -> c -> Expect.Expectation) -> Test
fuzz3 a b c name cb = Internal.Test.Fuzz (Internal.Test.Fuzzer3 a b c cb) name

fromTestTree :: Text -> (TestName -> TestTree) -> Test
fromTestTree n f =
  Internal.Test.FromTestTree n (f (Data.Text.unpack n))

task :: Text -> Task TestFailure a -> Test
task n expectation =
  Internal.Test.Test n (\() -> runTask expectation)

runTask :: Task TestFailure a -> Expect.Expectation
runTask t =
  Expect.withIO
    ( \res ->
        case res of
          Ok _ -> Expect.pass
          Err message -> Internal.Expectation.fromResult message
    )
    <| do
      noLogger <- Platform.silentHandler
      Task.attempt noLogger t
