-- | A module containing functions for creating and managing tests.
module Test
  ( -- * Organizing Tests
    Internal.Test,
    Internal.test,
    Internal.describe,
    Internal.skip,
    Internal.only,
    Internal.todo,

    -- * Fuzz Testing
    Internal.fuzz,
    Internal.fuzz2,
    Internal.fuzz3,

    -- * Task Testing
    Internal.task,

    -- * Running test
    run,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform
import qualified Platform.DevLog
import qualified System.Environment
import qualified System.IO
import qualified Task
import qualified Test.Internal as Internal
import qualified Test.Reporter.ExitCode
import qualified Test.Reporter.Junit
import qualified Test.Reporter.Logfile
import qualified Test.Reporter.Stdout
import qualified Prelude

-- | Turn a test suite into a program that can be executed in Haskell. Use like
-- this:
--
-- > module Main (main) where
-- >
-- > import qualified Test
-- >
-- > main :: IO ()
-- > main = Test.run (Test.todo "write your tests here!")
run :: Stack.HasCallStack => Internal.Test -> Prelude.IO ()
run suite = do
  log <- Platform.silentHandler
  results <- Task.perform log (Internal.run suite)
  Async.mapConcurrently_
    identity
    [ reportStdout results,
      Stack.withFrozenCallStack reportLogfile results,
      reportJunit results
    ]
  Test.Reporter.ExitCode.report results

reportStdout :: Internal.SuiteResult -> Prelude.IO ()
reportStdout results =
  Test.Reporter.Stdout.report System.IO.stdout results

reportLogfile :: Stack.HasCallStack => Internal.SuiteResult -> Prelude.IO ()
reportLogfile results =
  Stack.withFrozenCallStack
    Test.Reporter.Logfile.report
    Platform.DevLog.writeSpanToDevLog
    results

reportJunit :: Internal.SuiteResult -> Prelude.IO ()
reportJunit results =
  do
    args <- System.Environment.getArgs
    case getPath args of
      Nothing -> Prelude.pure ()
      Just path -> Test.Reporter.Junit.report path results

getPath :: [Prelude.String] -> Maybe Prelude.String
getPath args =
  case args of
    [] -> Nothing
    "--xml" : path : _ -> Just path
    _ : rest -> getPath rest
