{-# LANGUAGE NumericUnderscores #-}

module Test.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Dict
import qualified GHC.Stack as Stack
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

data SuiteResult
  = AllPassed
  | OnlysPassed
  | PassedWithSkipped [SingleTest ()]
  | TestsFailed [SingleTest Failure]
  | NoTestsInSuite

newtype Test = Test {unTest :: [SingleTest Expectation]}

newtype Expectation = Expectation {unExpectation :: Task Never TestResult}

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
