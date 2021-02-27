module Main (main) where

import qualified Spec.Reporter.Bugsnag
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test.Test
tests =
  Test.describe
    "nri-observability"
    [ Spec.Reporter.Bugsnag.tests
    ]
