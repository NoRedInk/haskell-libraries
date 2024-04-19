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

    -- * Serialize test execution
    Internal.serialize,

    -- * Running test
    run,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified GHC.IO.Encoding
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform
import qualified Platform.DevLog
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import System.IO (hPutStrLn, stderr)
import qualified System.IO
import qualified Task
import qualified Test.CliParser as CliParser
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
-- > main = Test.run  (Test.todo "write your tests here!")
run :: Stack.HasCallStack => Internal.Test -> Prelude.IO ()
run suite = do
  -- Work around `hGetContents: invalid argument (invalid byte sequence)` bug on
  -- Nix: https://github.com/dhall-lang/dhall-haskell/issues/865
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  log <- Platform.silentHandler
  args <- System.Environment.getArgs
  let requestOrError = CliParser.parseArgs args
  request <- case requestOrError of
    Err errs -> do
      let error = ("Invalid arguments:\n" ++ errs)
      hPutStrLn stderr error
      System.Exit.exitFailure
    Ok request ->
      Prelude.pure request
  (results, logExplorerAvailable) <-
    Async.concurrently
      (Task.perform log (Internal.run request suite))
      isLogExplorerAvailable
  Async.mapConcurrently_
    identity
    [ reportStdout results,
      Stack.withFrozenCallStack reportLogfile results,
      reportJunit args results
    ]
  if logExplorerAvailable
    then putTextLn "\nRun log-explorer in your shell to inspect logs collected during this test run."
    else putTextLn "\nInstall the log-explorer tool to inspect logs collected during test runs. Find it at github.com/NoRedInk/haskell-libraries."
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

reportJunit :: [Prelude.String] -> Internal.SuiteResult -> Prelude.IO ()
reportJunit args results =
  case getPath args of
    Nothing -> Prelude.pure ()
    Just path -> Test.Reporter.Junit.report path results

getPath :: [Prelude.String] -> Maybe Prelude.String
getPath args =
  case args of
    [] -> Nothing
    "--xml" : path : _ -> Just path
    _ : rest -> getPath rest

isLogExplorerAvailable :: Prelude.IO Bool
isLogExplorerAvailable = do
  System.Directory.findExecutable "log-explorer"
    |> map (/= Nothing)
