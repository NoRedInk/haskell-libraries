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
    Internal.hedgehog,

    -- * Serialize test execution
    Internal.serialize,

    -- * Running test
    run,
    runWithSettings,
    TestSettings (..),
    defaultTestSettings,
  )
where

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

data TestSettings = TestSettings
  { output :: Maybe System.IO.Handle,
    junitPath :: Maybe Prelude.String,
    writeDevLog :: Bool
  }

defaultTestSettings :: TestSettings
defaultTestSettings =
  TestSettings
    { output = Just System.IO.stdout,
      junitPath = Nothing,
      writeDevLog = True
    }

-- | Turn a test suite into a program that can be executed in Haskell. Use like
-- this:
--
-- > module Main (main) where
-- >
-- > import qualified Test
-- >
-- > main :: IO ()
-- > main = Test.run  (Test.todo "write your tests here!")
run :: (Stack.HasCallStack) => Internal.Test -> Prelude.IO ()
run suite = do
  args <- System.Environment.getArgs
  let settings = defaultTestSettings {junitPath = getJunitPath args}
  runWithSettings settings suite

runWithSettings :: (Stack.HasCallStack) => TestSettings -> Internal.Test -> Prelude.IO ()
runWithSettings settings suite = do
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

  results <- Task.perform log (Internal.run request suite)

  case output settings of
    Just outputHandle -> reportConsole outputHandle results
    Nothing -> Prelude.pure ()

  case junitPath settings of
    Nothing -> Prelude.pure ()
    Just path -> reportJunit path results

  if writeDevLog settings
    then do
      logExplorerAvailable <- isLogExplorerAvailable
      if logExplorerAvailable
        then putTextLn "\nRun log-explorer in your shell to inspect logs collected during this test run."
        else putTextLn "\nInstall the log-explorer tool to inspect logs collected during test runs. Find it at github.com/NoRedInk/haskell-libraries."
      Stack.withFrozenCallStack reportLogfile results
    else Prelude.pure ()

  Test.Reporter.ExitCode.report results

reportConsole :: System.IO.Handle -> Internal.SuiteResult -> Prelude.IO ()
reportConsole outputHandle results =
  Test.Reporter.Stdout.report outputHandle results

reportLogfile :: (Stack.HasCallStack) => Internal.SuiteResult -> Prelude.IO ()
reportLogfile results =
  Stack.withFrozenCallStack
    Test.Reporter.Logfile.report
    Platform.DevLog.writeSpanToDevLog
    results

reportJunit :: Prelude.String -> Internal.SuiteResult -> Prelude.IO ()
reportJunit path results =
  Test.Reporter.Junit.report path results

getJunitPath :: [Prelude.String] -> Maybe Prelude.String
getJunitPath args =
  case args of
    [] -> Nothing
    "--xml" : path : _ -> Just path
    _ : rest -> getJunitPath rest

isLogExplorerAvailable :: Prelude.IO Bool
isLogExplorerAvailable = do
  System.Directory.findExecutable "log-explorer"
    |> map (/= Nothing)
