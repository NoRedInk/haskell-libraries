-- | A module containing functions for creating and managing tests.
module Test
  ( -- * Organizing Tests
    Test,
    test,
    describe,
    skip,
    only,
    todo,

    -- * Fuzz Testing
    fuzz,
    fuzz2,
    fuzz3,

    -- * Task Testing
    task,

    -- * Running test
    run,
  )
where

import qualified Expect
import NriPrelude
import qualified Platform
import qualified Task
import qualified Test.Internal as Internal
import Test.Internal (Fuzzer, Test)
import qualified Test.Reporter.ExitCode
import qualified Test.Reporter.Stdout
import qualified Prelude

-- | Apply a description to a list of tests.
--
-- > import Test (describe, test, fuzz)
-- > import Fuzz (int)
-- > import Expect
-- >
-- > describe "List"
-- >     [ describe "reverse"
-- >         [ test "has no effect on an empty list" <|
-- >             \_ ->
-- >                 List.reverse []
-- >                     |> Expect.equal []
-- >         , fuzz int "has no effect on a one-item list" <|
-- >             \num ->
-- >                  List.reverse [ num ]
-- >                     |> Expect.equal [ num ]
-- >         ]
-- >     ]
--
-- Passing an empty list will result in a failing test, because you either made a
-- mistake or are creating a placeholder.
describe :: Text -> List Test -> Test
describe =
  Internal.describe

-- | Return a 'Test' that evaluates a single
-- 'Expect.Expectation'
--
-- > import Test (fuzz)
-- > import Expect
-- > test "the empty list has 0 length" <|
-- >     \_ ->
-- >         List.length []
-- >             |> Expect.equal 0
test :: Text -> (() -> Expect.Expectation) -> Test
test =
  Internal.test

-- | Returns a 'Test' that gets skipped.
--
-- Calls to @skip@ aren't meant to be committed to version control. Instead,
-- use it when you want to focus on getting a particular subset of your tests
-- to pass. If you use @skip@, your entire test suite will fail, even if each
-- of the individual tests pass. This is to help avoid accidentally committing
-- a @skip@ to version control.
--
-- See also 'only'. Note that @skip@ takes precedence over @only@; if you use a
-- @skip@ inside an @only@, it will still get skipped, and if you use an @only@
-- inside a @skip@, it will also get skipped.
--
-- > describe "List"
-- >     [ skip <|
-- >         describe "reverse"
-- >             [ test "has no effect on an empty list" <|
-- >                 \_ ->
-- >                     List.reverse []
-- >                         |> Expect.equal []
-- >             , fuzz int "has no effect on a one-item list" <|
-- >                 \num ->
-- >                     List.reverse [ num ]
-- >                         |> Expect.equal [ num ]
-- >             ]
-- >     , test "This is the only test that will get run; the other was skipped!" <|
-- >         \_ ->
-- >             List.length []
-- >                 |> Expect.equal 0
-- >     ]
skip :: Test -> Test
skip = Internal.skip

-- | Returns a 'Test' that causes other tests to be skipped, and only runs the given one.
--
-- Calls to @only@ aren't meant to be committed to version control. Instead,
-- use them when you want to focus on getting a particular subset of your tests
-- to pass.  If you use @only@, your entire test suite will fail, even if each
-- of the individual tests pass. This is to help avoid accidentally committing
-- a @only@ to version control.
--
-- If you you use @only@ on multiple tests, only those tests will run. If you
-- put a @only@ inside another @only@, only the outermost @only@ will affect
-- which tests gets run. See also 'skip'. Note that @skip@ takes precedence
-- over @only@; if you use a @skip@ inside an @only@, it will still get
-- skipped, and if you use an @only@ inside a @skip@, it will also get skipped.
--
-- > describe "List"
-- >     [ only <|
-- >         describe "reverse"
-- >             [ test "has no effect on an empty list" <|
-- >                 \_ ->
-- >                     List.reverse []
-- >                         |> Expect.equal []
-- >             , fuzz int "has no effect on a one-item list" <|
-- >                 \num ->
-- >                     List.reverse [ num ]
-- >                         |> Expect.equal [ num ]
-- >             ]
-- >     , test "This will not get run, because of the @only@ above!" <|
-- >         \_ ->
-- >             List.length []
-- >                 |> Expect.equal 0
-- >     ]
only :: Test -> Test
only = Internal.only

-- | Returns a 'Test' that is "todo" (not yet implemented). These tests always
-- fail.
--
-- These tests aren't meant to be committed to version control. Instead, use
-- them when you're brainstorming lots of tests you'd like to write, but you
-- can't implement them all at once. When you replace @todo@ with a real test,
-- you'll be able to see if it fails without clutter from tests still not
-- implemented. But, unlike leaving yourself comments, you'll be prompted to
-- implement these tests because your suite will fail.
--
-- > describe "a new thing"
-- >     [ todo "does what is expected in the common case"
-- >     , todo "correctly handles an edge case I just thought of"
-- >     ]
--
-- This functionality is similar to "pending" tests in other frameworks, except
-- that a todo test is considered failing but a pending test often is not.
todo :: Text -> Test
todo = Internal.todo

-- | Take a function that produces a test, and calls it several (usually 100)
-- times, using a randomly-generated input from a 'Fuzzer' each time. This
-- allows you to test that a property that should always be true is indeed true
-- under a wide variety of conditions. The function also takes a string
-- describing the test.
--
-- These are called "fuzz tests" because of the randomness. You may find them
-- elsewhere called property-based tests, generative tests, or QuickCheck-style
-- tests.
fuzz :: Fuzzer a -> Text -> (a -> Expect.Expectation) -> Test
fuzz = Internal.fuzz

-- | Run a fuzz test using two random inputs.
fuzz2 :: Fuzzer a -> Fuzzer b -> Text -> (a -> b -> Expect.Expectation) -> Test
fuzz2 = Internal.fuzz2

-- | Run a fuzz test using three random inputs.
fuzz3 :: Fuzzer a -> Fuzzer b -> Fuzzer c -> Text -> (a -> b -> c -> Expect.Expectation) -> Test
fuzz3 = Internal.fuzz3

-- | Run a test that executes a task. The test passes if the task returns a
-- success value.
task :: Text -> Task Internal.Failure a -> Test
task = Internal.task

-- | Turn a test suite into a program that can be executed in Haskell. Use like
-- this:
--
-- > module Main (main) where
-- >
-- > import qualified Test
-- >
-- > main :: IO ()
-- > main = Test.run (Test.todo "write your tests here!")
run :: Test -> Prelude.IO ()
run suite = do
  log <- Platform.silentHandler
  results <- Task.perform log (Internal.run suite)
  Test.Reporter.Stdout.report results
  Test.Reporter.ExitCode.report results
