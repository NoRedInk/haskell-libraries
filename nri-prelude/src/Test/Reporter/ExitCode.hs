module Test.Reporter.ExitCode (report) where

import qualified System.Exit
import qualified Test.Internal as Internal
import qualified Prelude

report :: Internal.SuiteResult -> Prelude.IO ()
report results =
  case results of
    Internal.AllPassed _ -> System.Exit.exitSuccess
    _ -> System.Exit.exitFailure
