module Main (main) where

import qualified DatabaseSpec
import Nri.Prelude (IO)
import qualified Test.Runner.Tasty

main :: IO ()
main = Test.Runner.Tasty.main DatabaseSpec.tests
