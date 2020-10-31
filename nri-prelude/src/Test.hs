-- | A module containing functions for creating and managing tests.
module Test
  ( -- * Organizing Tests
    Test,
    test,
    describe,
    skip,
    only,
    todo,

    -- * Task Testing
    task,
  )
where

import qualified Expect
import NriPrelude
import qualified Test.Internal as Internal

-- | A test which has yet to be evaluated. When evaluated, it produces one
-- or more 'Expect.Expectation's.
-- See 'test' and 'fuzz' for some ways to create a @Test@.
type Test = Internal.Test

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
  Test.describe

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
  Test.test

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
skip = Test.skip

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
only = Test.only

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
todo = Test.todo

-- | Run a test that executes a task. The test passes if the task returns a
-- success value.
task :: Text -> Task Internal.Failure a -> Test
task = Internal.task
