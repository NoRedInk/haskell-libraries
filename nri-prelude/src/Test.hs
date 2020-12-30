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

import qualified Platform
import qualified Task
import qualified Test.Internal as Internal
import qualified Test.Reporter.ExitCode
import qualified Test.Reporter.Junit
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
run :: Internal.Test -> Prelude.IO ()
run suite = do
  log <- Platform.silentHandler
  results <- Task.perform log (Internal.run suite)
  Test.Reporter.Stdout.report results
  Test.Reporter.Junit.report results
  Test.Reporter.ExitCode.report results
