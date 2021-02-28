module Main (main) where

import qualified Spec.Platform.Timer
import qualified Spec.Reporter.Bugsnag
import qualified Spec.Reporter.Dev
import qualified Spec.Reporter.File
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test.Test
tests =
  Test.describe
    "nri-observability"
    [ Spec.Platform.Timer.tests,
      Spec.Reporter.Bugsnag.tests,
      Spec.Reporter.Dev.tests,
      Spec.Reporter.File.tests
    ]
