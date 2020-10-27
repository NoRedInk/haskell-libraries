{-# LANGUAGE NumericUnderscores #-}

module TestNew
  ( -- * Organizing tests
    Test,
    test,
    describe,
    skip,
    only,
    todo,

    -- * Task testing
    task,

    -- * Internals
    run,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import qualified Dict
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Internal.Expectation as Expectation
import qualified Internal.TestResult as TestResult
import qualified List
import qualified Maybe
import NriPrelude
import qualified Platform
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

newtype Test = Test {unTest :: [SingleTest (Prelude.IO TestResult)]}

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
          body = Prelude.pure Succeeded
        }
    ]

test :: Stack.HasCallStack => Text -> (() -> Expect.Expectation) -> Test
test name expectation =
  let body :: Prelude.IO TestResult
      body = do
        log <- Platform.silentHandler
        Expectation.toResult (expectation ())
          |> ioToTask
          |> Task.mapError ThrewException
          |> Task.timeout 10_000 TookTooLong
          |> Task.attempt log
          |> map
            ( \res ->
                -- TODO: remove duplicate TestResult type.
                case res of
                  Ok (TestResult.Passed) -> Succeeded
                  Ok (TestResult.Skipped) -> Succeeded
                  Ok (TestResult.Failed (TestResult.TestFailure message)) ->
                    Failed (FailedAssertion message)
                  Err failure -> Failed failure
            )
   in Test
        [ SingleTest
            { describes = [],
              name = name,
              loc = Stack.withFrozenCallStack getFrame,
              label = None,
              body = body
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
  let body :: Prelude.IO TestResult
      body = do
        log <- Platform.silentHandler
        expectation
          |> Task.timeout 10_000 TookTooLong
          |> Task.attempt log
          |> map
            ( \res ->
                case res of
                  Ok _ -> Succeeded
                  Err failure -> Failed failure
            )
          |> Exception.handle (Prelude.pure << Failed << ThrewException)
   in Test
        [ SingleTest
            { describes = [],
              name = name,
              loc = Stack.withFrozenCallStack getFrame,
              label = None,
              body = body
            }
        ]

run :: Test -> Prelude.IO SuiteResult
run (Test all) = do
  let grouped = groupBy label all
  let onlys = Dict.get Only grouped |> Maybe.withDefault []
  let regulars = Dict.get None grouped |> Maybe.withDefault []
  let skipped = Dict.get Skip grouped |> Maybe.withDefault []
  let todos = Dict.get Todo grouped |> Maybe.withDefault []
  let skippedAndTodos = List.map (\test' -> test' {body = ()}) (skipped ++ todos)
  let toRun = if List.isEmpty onlys then regulars else onlys
  results <- Async.mapConcurrently runSingle toRun
  let failed = List.filterMap justFailure results
  let summary =
        Summary
          { noTests = List.isEmpty all,
            allPassed = List.isEmpty failed,
            noOnlys = List.isEmpty onlys,
            noneSkipped = List.isEmpty skippedAndTodos
          }
  Prelude.pure <| case summary of
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

runSingle :: SingleTest (Prelude.IO TestResult) -> Prelude.IO (SingleTest TestResult)
runSingle test' = do
  body test'
    |> map (\res -> test' {body = res})

ioToTask :: Prelude.IO a -> Task Exception.SomeException a
ioToTask io =
  Platform.Internal.Task <| \_ ->
    Exception.handleAny (Prelude.pure << Err) (map Ok io)

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
