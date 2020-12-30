{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.IO.Class
import qualified Data.IORef as IORef
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
import qualified Platform.Internal
import qualified Task
import qualified Tuple
import qualified Prelude

data SingleTest a
  = SingleTest
      { describes :: [Text],
        name :: Text,
        label :: Label,
        loc :: Maybe Stack.SrcLoc,
        body :: a
      }

data Label = None | Skip | Only | Todo
  deriving (Eq, Ord)

data TestResult
  = Succeeded
  | Failed Failure

data Failure
  = FailedAssertion Text
  | ThrewException Exception.SomeException
  | TookTooLong
  | TestRunnerMessedUp Text
  deriving (Show)

data SuiteResult
  = AllPassed
  | OnlysPassed
  | PassedWithSkipped [SingleTest ()]
  | TestsFailed [SingleTest Failure]
  | NoTestsInSuite

-- | A test which has yet to be evaluated. When evaluated, it produces one
-- or more 'Expect.Expectation's.
-- See 'test' and 'fuzz' for some ways to create a @Test@.
newtype Test = Test {unTest :: [SingleTest Expectation]}

-- |  The result of a single test run: either a 'pass' or a 'fail'.
newtype Expectation = Expectation {unExpectation :: Task Never TestResult}

-- | A @Fuzzer a@ knows how to produce random values of @a@ and how to "shrink"
-- a value of @a@, that is turn a value into another that is slightly simpler.
newtype Fuzzer a = Fuzzer (Hedgehog.Gen a)

describe :: Text -> [Test] -> Test
describe description tests =
  tests
    |> List.concatMap unTest
    |> List.map (\test' -> test' {describes = description : describes test'})
    |> Test

todo :: Stack.HasCallStack => Text -> Test
todo name =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = Todo,
          body = Expectation (Task.succeed Succeeded)
        }
    ]

test :: Stack.HasCallStack => Text -> (() -> Expectation) -> Test
test name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = None,
          body = handleUnexpectedErrors (expectation ())
        }
    ]

fuzz :: Show a => Fuzzer a -> Text -> (a -> Expectation) -> Test
fuzz fuzzer name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = None,
          body = fuzzBody fuzzer expectation
        }
    ]

fuzz2 :: (Show a, Show b) => Fuzzer a -> Fuzzer b -> Text -> (a -> b -> Expectation) -> Test
fuzz2 (Fuzzer genA) (Fuzzer genB) name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = None,
          body =
            fuzzBody
              (Fuzzer (map2 (,) genA genB))
              (\(a, b) -> expectation a b)
        }
    ]

fuzz3 :: (Show a, Show b, Show c) => Fuzzer a -> Fuzzer b -> Fuzzer c -> Text -> (a -> b -> c -> Expectation) -> Test
fuzz3 (Fuzzer genA) (Fuzzer genB) (Fuzzer genC) name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = None,
          body =
            fuzzBody
              (Fuzzer (map3 (,,) genA genB genC))
              (\(a, b, c) -> expectation a b c)
        }
    ]

fuzzBody :: Show a => Fuzzer a -> (a -> Expectation) -> Expectation
fuzzBody (Fuzzer gen) expectation =
  handleUnexpectedErrors
    <| Expectation
    <| Platform.Internal.Task
      ( \log -> do
          seed <- Hedgehog.Internal.Seed.random
          resultRef <- IORef.newIORef Nothing
          hedgehogResult <-
            Hedgehog.Internal.Runner.checkReport
              Hedgehog.Internal.Property.defaultConfig
              0 -- Same value used as in Hedgehog internals.
              seed
              ( do
                  generated <- Hedgehog.forAll gen
                  result <-
                    expectation generated
                      |> unExpectation
                      |> Task.perform log
                      |> Control.Monad.IO.Class.liftIO
                  IORef.writeIORef resultRef (Just result)
                    |> Control.Monad.IO.Class.liftIO
                  case result of
                    Succeeded -> Prelude.pure ()
                    Failed failure -> Prelude.fail (Prelude.show failure)
              )
              (\_ -> Prelude.pure ())
          case Hedgehog.Internal.Report.reportStatus hedgehogResult of
            Hedgehog.Internal.Report.Failed _ -> do
              maybeResult <- IORef.readIORef resultRef
              case maybeResult of
                Nothing ->
                  TestRunnerMessedUp "I lost the error report of a failed fuzz test test."
                    |> Failed
                    |> Ok
                    |> Prelude.pure
                Just result ->
                  Ok result
                    |> Prelude.pure
            Hedgehog.Internal.Report.GaveUp ->
              TestRunnerMessedUp "I couldn't generate any values for a fuzz test."
                |> Failed
                |> Ok
                |> Prelude.pure
            Hedgehog.Internal.Report.OK ->
              Ok Succeeded
                |> Prelude.pure
      )

skip :: Test -> Test
skip (Test tests) =
  Test <| List.map (\test' -> test' {label = Skip}) tests

only :: Test -> Test
only (Test tests) =
  Test <| List.map (\test' -> test' {label = Skip}) tests

task :: Stack.HasCallStack => Text -> Task Failure a -> Test
task name expectation =
  Test
    [ SingleTest
        { describes = [],
          name = name,
          loc = Stack.withFrozenCallStack getFrame,
          label = None,
          body =
            expectation
              |> Task.map (\_ -> Succeeded)
              |> Task.onError (Task.succeed << Failed)
              |> Expectation
              |> handleUnexpectedErrors
        }
    ]

run :: Test -> Task e SuiteResult
run (Test all) = do
  let grouped = groupBy label all
  let onlys = Dict.get Only grouped |> Maybe.withDefault []
  let regulars = Dict.get None grouped |> Maybe.withDefault []
  let skipped = Dict.get Skip grouped |> Maybe.withDefault []
  let todos = Dict.get Todo grouped |> Maybe.withDefault []
  let skippedAndTodos = List.map (\test' -> test' {body = ()}) (skipped ++ todos)
  let toRun = if List.isEmpty onlys then regulars else onlys
  results <- Task.parallel (List.map runSingle toRun)
  let failed = List.filterMap justFailure results
  let summary =
        Summary
          { noTests = List.isEmpty all,
            allPassed = List.isEmpty failed,
            noOnlys = List.isEmpty onlys,
            noneSkipped = List.isEmpty skippedAndTodos
          }
  Task.succeed <| case summary of
    Summary {noTests = True} -> NoTestsInSuite
    Summary {allPassed = False} -> TestsFailed failed
    Summary {noOnlys = False} -> OnlysPassed
    Summary {noneSkipped = False} -> PassedWithSkipped skippedAndTodos
    Summary {} -> AllPassed

data Summary
  = Summary
      { noTests :: Bool,
        allPassed :: Bool,
        noOnlys :: Bool,
        noneSkipped :: Bool
      }

justFailure :: SingleTest TestResult -> Maybe (SingleTest Failure)
justFailure test' =
  case body test' of
    Succeeded -> Nothing
    Failed failure -> Just test' {body = failure}

handleUnexpectedErrors :: Expectation -> Expectation
handleUnexpectedErrors (Expectation task') =
  task'
    |> Task.mapError never
    |> onException (Task.succeed << Failed << ThrewException)
    |> Task.timeout 10_000 TookTooLong
    |> Task.onError (Task.succeed << Failed)
    |> Expectation

runSingle :: SingleTest Expectation -> Task e (SingleTest TestResult)
runSingle test' = do
  body test'
    |> unExpectation
    |> Task.mapError never
    |> map (\res -> test' {body = res})

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

getFrame :: Stack.HasCallStack => Maybe Stack.SrcLoc
getFrame =
  Stack.callStack
    |> Stack.getCallStack
    |> List.head
    |> map Tuple.second

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
  task1
    |> andThen
      ( \result1 ->
          case result1 of
            Succeeded -> task2
            Failed _ -> task1
      )
    |> Expectation
