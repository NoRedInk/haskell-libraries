module Main (main) where

import qualified Spec.Analytics
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test.Test
tests =
  Test.describe
    "nri-analytics"
    [ Spec.Analytics.tests
    ]
