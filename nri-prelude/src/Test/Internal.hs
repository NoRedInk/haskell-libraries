{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
-- GHC wants us to remove `Err never` branches from case statements, because it
-- knows we'll never end up in those branches. We like them though, because
-- missing such a branch in a case statement looks like a problem and so is
-- distracting.
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Test.Internal where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.IO.Class
import qualified Data.Either
import qualified Data.IORef as IORef
import Data.List (isSuffixOf)
import qualified Dict
import qualified GHC.Stack as Stack
import qualified Hedgehog
import qualified Hedgehog.Internal.Property
import qualified Hedgehog.Internal.Report
import qualified Hedgehog.Internal.Runner
import qualified Hedgehog.Internal.Seed
import qualified List
import qualified Maybe
import NriPrelude
import Platform (TracingSpan)
import qualified Platform
import qualified Platform.Internal
import qualified Set
import System.FilePath (FilePath)
import qualified Task
import qualified Tuple
import qualified Prelude

data Request = All | Some [SubsetOfTests]
  deriving (Eq, Show)

data SubsetOfTests = SubsetOfTests {requestedPath :: FilePath, lineOfCode :: Maybe Int}
  deriving (Eq, Show)

data SingleTest a = SingleTest
  { describes :: [Text],
    name :: Text,
    label :: Label,
    -- This is used for serializing execution of grouped tests (e.g. database tests)
    group :: Group,
    loc :: Stack.SrcLoc,
    body :: a
  }
  deriving (Show, Prelude.Functor)

data Label = None | Skip | Only | Todo
  deriving (Show, Eq, Ord)

data Group = Grouped (Set.Set GroupKey) | Ungrouped
  deriving (Show, Eq, Ord)

newtype GroupKey = GroupKey {unGroupkey :: Text}
  deriving (Show, Eq, Ord)

data TestResult
  = Succeeded
  | Failed Failure

data Failure
  = FailedAssertion Text Stack.SrcLoc
  | ThrewException Exception.SomeException
  | TookTooLong
  | TestRunnerMessedUp Text
  deriving (Show)

instance Exception.Exception Failure

data SuiteResult
  = AllPassed [SingleTest TracingSpan]
  | OnlysPassed [SingleTest TracingSpan] [SingleTest NotRan]
  | PassedWithSkipped [SingleTest TracingSpan] [SingleTest NotRan]
  | TestsFailed [SingleTest TracingSpan] [SingleTest NotRan] [SingleTest FailedSpan]
  | NoTestsInSuite
  deriving (Show)

data NotRan = NotRan
  deriving (Show)

data FailedSpan = FailedSpan TracingSpan Failure

instance Show FailedSpan where
  show (FailedSpan span failure) = Prelude.show failure ++ ": " ++ Prelude.show span

-- | A test which has yet to be evaluated. When evaluated, it produces one
-- or more 'Expect.Expectation's.
-- See 'test' and 'fuzz' for some ways to create a @Test@.
newtype Test = Test {unTest :: [SingleTest Expectation]}

-- | The result of a single test run: either a 'pass' or a 'fail'.
type Expectation = Expectation' ()

-- | The type of a test that runs some script with multiple expectations in
-- between.
newtype Expectation' a = Expectation {unExpectation :: Task Failure a}
  deriving (Prelude.Functor, Prelude.Applicative, Prelude.Monad)

-- | A @Fuzzer a@ knows how to produce random values of @a@ and how to "shrink"
-- a value of @a@, that is turn a value into another that is slightly simpler.
newtype Fuzzer a = Fuzzer {unFuzzer :: Hedgehog.Gen a}
  deriving (Prelude.Functor, Prelude.Applicative)

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
describe :: Text -> [Test] -> Test
describe description tests =
  tests
    |> List.concatMap unTest
    |> List.map (\test' -> test' {describes = description : describes test'})
    |> Test

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
todo :: Stack.HasCallStack => Text -> Test
todo name =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame name,
          group = Ungrouped,
          label = Todo,
          body = Expectation (Task.succeed ())
        }
    ]

-- | Return a 'Test' that evaluates a single
-- 'Expect.Expectation'
--
-- > import Test (fuzz)
-- > import Expect
-- > test "the empty list has 0 length" <|
-- >     \_ ->
-- >         List.length []
-- >             |> Expect.equal 0
test :: Stack.HasCallStack => Text -> (() -> Expectation) -> Test
test name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame name,
          group = Ungrouped,
          label = None,
          body = handleUnexpectedErrors (expectation ())
        }
    ]

-- | Serializes the execution of 'Test' based on a certain grouping
--
-- > serialize "mysql" <| todo "some db stuff!"
serialize :: Text -> Test -> Test
serialize groupKey (Test tests) =
  tests
    |> List.map
      ( \singleTest ->
          let groupKeys = case group singleTest of
                Ungrouped -> Set.empty
                Grouped keys -> keys
           in singleTest {group = Grouped (Set.insert (GroupKey groupKey) groupKeys)}
      )
    |> Test

-- | Take a function that produces a test, and calls it several (usually 100)
-- times, using a randomly-generated input from a 'Fuzzer' each time. This
-- allows you to test that a property that should always be true is indeed true
-- under a wide variety of conditions. The function also takes a string
-- describing the test.
--
-- These are called "fuzz tests" because of the randomness. You may find them
-- elsewhere called property-based tests, generative tests, or QuickCheck-style
-- tests.
fuzz :: (Stack.HasCallStack, Show a) => Fuzzer a -> Text -> (a -> Expectation) -> Test
fuzz fuzzer name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame name,
          group = Ungrouped,
          label = None,
          body = fuzzBody fuzzer expectation
        }
    ]

-- | Run a fuzz test using two random inputs.
fuzz2 :: (Stack.HasCallStack, Show a, Show b) => Fuzzer a -> Fuzzer b -> Text -> (a -> b -> Expectation) -> Test
fuzz2 (Fuzzer genA) (Fuzzer genB) name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame name,
          group = Ungrouped,
          label = None,
          body =
            fuzzBody
              (Fuzzer (map2 (,) genA genB))
              (\(a, b) -> expectation a b)
        }
    ]

-- | Run a fuzz test using three random inputs.
fuzz3 :: (Stack.HasCallStack, Show a, Show b, Show c) => Fuzzer a -> Fuzzer b -> Fuzzer c -> Text -> (a -> b -> c -> Expectation) -> Test
fuzz3 (Fuzzer genA) (Fuzzer genB) (Fuzzer genC) name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame name,
          group = Ungrouped,
          label = None,
          body =
            fuzzBody
              (Fuzzer (map3 (,,) genA genB genC))
              (\(a, b, c) -> expectation a b c)
        }
    ]

fuzzBody :: Show a => Fuzzer a -> (a -> Expectation) -> Expectation
fuzzBody (Fuzzer gen) expectation = do
  Expectation
    <| Platform.Internal.Task
      ( \_log -> do
          -- For the moment we're not recording traces in fuzz tests. Because
          -- the test body runs a great many times, we'd record a ton of data
          -- that's not all that useful.
          --
          -- Ideally we'd only keep the recording of the most significant run,
          -- but we have to figure out how to do that first.
          silentLog <- Platform.silentHandler
          seed <- Hedgehog.Internal.Seed.random
          failureRef <- IORef.newIORef Nothing
          hedgehogResult <-
            Hedgehog.Internal.Runner.checkReport
              Hedgehog.Internal.Property.defaultConfig
              0 -- Same value used as in Hedgehog internals.
              seed
              ( do
                  generated <- Hedgehog.forAll gen
                  result <-
                    expectation generated
                      |> handleUnexpectedErrors
                      |> unExpectation
                      |> Task.map Ok
                      |> Task.onError (Task.succeed << Err)
                      |> Task.perform silentLog
                      |> Control.Monad.IO.Class.liftIO
                  case result of
                    Ok () -> Prelude.pure ()
                    Err failure -> do
                      IORef.writeIORef failureRef (Just failure)
                        |> Control.Monad.IO.Class.liftIO
                      Hedgehog.failure
              )
              (\_ -> Prelude.pure ())
          case Hedgehog.Internal.Report.reportStatus hedgehogResult of
            Hedgehog.Internal.Report.Failed _ -> do
              maybeFailure <- IORef.readIORef failureRef
              case maybeFailure of
                Nothing ->
                  TestRunnerMessedUp "I lost the error report of a failed fuzz test test."
                    |> Err
                    |> Prelude.pure
                Just failure ->
                  Err failure
                    |> Prelude.pure
            Hedgehog.Internal.Report.GaveUp ->
              TestRunnerMessedUp "I couldn't generate any values for a fuzz test."
                |> Err
                |> Prelude.pure
            Hedgehog.Internal.Report.OK ->
              Ok ()
                |> Prelude.pure
      )

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
skip (Test tests) =
  Test <| List.map (\test' -> test' {label = Skip}) tests

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
only (Test tests) =
  Test <| List.map (\test' -> test' {label = Only}) tests

run :: Request -> Test -> Task e SuiteResult
run request (Test all) = do
  let grouped = groupBy label all
  let skipped = Dict.get Skip grouped |> Maybe.withDefault []
  let todos = Dict.get Todo grouped |> Maybe.withDefault []
  let containsOnlys =
        case Dict.get Only grouped |> Maybe.withDefault [] of
          [] -> False
          _ -> True
  let doRun label =
        if containsOnlys
          then label == Only
          else label == None
  let (toRun, notToRun') =
        Dict.toList grouped
          |> List.partition (doRun << Tuple.first)
          |> Tuple.mapBoth (List.concatMap Tuple.second) (List.concatMap Tuple.second)
  let notToRun = List.map (\test' -> test' {body = NotRan}) notToRun'
  results <-
    ( case request of
        All -> toRun
        Some tests -> List.filter (subset tests) toRun
      )
      |> groupBy runStrategy
      |> Dict.toList
      |> List.map runGroup
      |> Task.parallel
      |> Task.map List.concat
  let (failed, passed) =
        results
          |> List.map
            ( \test' ->
                case body test' of
                  (tracingSpan, Failed failure) ->
                    Prelude.Left test' {body = FailedSpan tracingSpan failure}
                  (tracingSpan, Succeeded) ->
                    Prelude.Right test' {body = tracingSpan}
            )
          |> Data.Either.partitionEithers
  let summary =
        Summary
          { noTests = List.isEmpty all,
            allPassed = List.isEmpty failed,
            anyOnlys = containsOnlys,
            noneSkipped = List.isEmpty (skipped ++ todos)
          }
  Task.succeed <| case summary of
    Summary {noTests = True} -> NoTestsInSuite
    Summary {allPassed = False} -> TestsFailed passed notToRun failed
    Summary {anyOnlys = True} -> OnlysPassed passed notToRun
    Summary {noneSkipped = False} -> PassedWithSkipped passed notToRun
    Summary {} -> AllPassed passed

data RunStrategy = Parallel | Sequence deriving (Eq, Ord)

runStrategy :: SingleTest exp -> RunStrategy
runStrategy singleTest =
  case group singleTest of
    Grouped _ -> Sequence
    Ungrouped -> Parallel

subset :: List SubsetOfTests -> SingleTest expectation -> Bool
subset subsets singleTest =
  case (subsets, loc singleTest) of
    ([], _) -> False -- Should never happen, we should have a NonEmpty SubsetOfTests tbh
    (SubsetOfTests {requestedPath, lineOfCode} : rest, Stack.SrcLoc {Stack.srcLocFile, Stack.srcLocStartLine, Stack.srcLocEndLine}) ->
      -- isSuffixOf allows us to write --files=quiz-engine-http/spec/Smth/DerpSpec.hs
      if srcLocFile `isSuffixOf` requestedPath
        then case lineOfCode of
          Nothing -> True
          Just requestedLoc' ->
            let requestedLoc = Prelude.fromIntegral requestedLoc'
             in if srcLocStartLine <= requestedLoc && requestedLoc <= srcLocEndLine
                  then True
                  else subset rest singleTest
        else subset rest singleTest

data Summary = Summary
  { noTests :: Bool,
    allPassed :: Bool,
    anyOnlys :: Bool,
    noneSkipped :: Bool
  }

handleUnexpectedErrors :: Expectation -> Expectation
handleUnexpectedErrors (Expectation task') =
  task'
    |> onException (Task.fail << ThrewException)
    |> Task.timeout 10_000 TookTooLong
    |> Task.onError Task.fail
    |> Expectation

runGroup :: (RunStrategy, List (SingleTest Expectation)) -> Task e (List (SingleTest (TracingSpan, TestResult)))
runGroup (groupped, tests) =
  List.map runSingle tests
    |> ( case groupped of
           Sequence -> Task.sequence
           Parallel -> Task.parallel
       )

runSingle :: SingleTest Expectation -> Task e (SingleTest (TracingSpan, TestResult))
runSingle test' =
  Platform.Internal.Task
    ( \_ -> do
        spanVar <- MVar.newEmptyMVar
        res <-
          Platform.Internal.rootTracingSpanIO
            ""
            (MVar.putMVar spanVar)
            "test"
            ( \log ->
                body test'
                  |> unExpectation
                  |> Task.map Ok
                  |> Task.onError (Task.succeed << Err)
                  |> Task.perform log
            )
        let testRest =
              case res of
                Ok () -> Succeeded
                -- If you remove this branch, consider also removing the
                -- -fno-warn-overlapping-patterns warning above.
                Err err -> Failed err
        span' <- MVar.takeMVar spanVar
        let span =
              span'
                { Platform.Internal.summary = Just (name test'),
                  Platform.Internal.frame = Just ("", loc test'),
                  Platform.Internal.succeeded = case testRest of
                    Succeeded -> Platform.Internal.Succeeded
                    Failed failure ->
                      Exception.toException failure
                        |> Platform.Internal.FailedWith
                }
        test' {body = (span, testRest)}
          |> Ok
          |> Prelude.pure
    )

ioToTask :: Prelude.IO a -> Task Exception.SomeException a
ioToTask io =
  Platform.Internal.Task <| \_ ->
    Exception.handleAny (Prelude.pure << Err) (map Ok io)

onException :: (Exception.SomeException -> Task e a) -> Task e a -> Task e a
onException f (Platform.Internal.Task run') =
  Platform.Internal.Task
    ( \log ->
        run' log
          |> Exception.handleAny (Task.attempt log << f)
    )

getFrame :: Stack.HasCallStack => Text -> Stack.SrcLoc
getFrame testName =
  case Stack.callStack |> Stack.getCallStack |> List.head of
    Just (_, srcLoc) ->
      srcLoc
    Nothing ->
      ( "Oops! We can't find the source location for this test: " ++ testName ++ "\n"
          ++ "This indicates a bug in our Test module in nri-prelude.\n"
      )
        |> TestRunnerMessedUp
        |> Exception.impureThrow

groupBy :: Ord key => (a -> key) -> [a] -> Dict.Dict key [a]
groupBy key xs =
  List.foldr
    ( \x acc ->
        Dict.update
          (key x)
          ( \val ->
              Just
                <| case val of
                  Nothing -> [x]
                  Just ys -> x : ys
          )
          acc
    )
    Dict.empty
    xs

append :: Expectation -> Expectation -> Expectation
append (Expectation task1) (Expectation task2) =
  Expectation <| do
    task1
    task2

-- Assertion constructors
-- All exposed assertion functions should call these functions internally and
-- never each other, to ensure a single unnested 'expectation' entry from
-- appearing in log-explorer traces.

pass :: Stack.HasCallStack => Text -> a -> Expectation' a
pass name a = Stack.withFrozenCallStack traceExpectation name (Task.succeed a)

failAssertion :: Stack.HasCallStack => Text -> Text -> Expectation' a
failAssertion name err =
  FailedAssertion err (Stack.withFrozenCallStack getFrame name)
    |> Task.fail
    |> Stack.withFrozenCallStack traceExpectation name

traceExpectation :: Stack.HasCallStack => Text -> Task Failure a -> Expectation' a
traceExpectation name task =
  Stack.withFrozenCallStack
    Platform.tracingSpan
    name
    task
    |> Expectation
