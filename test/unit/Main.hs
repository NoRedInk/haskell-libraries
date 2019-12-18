module Main (main) where

import qualified DatabaseSpec
import qualified Test.Runner.Tasty
import Prelude (IO)

main :: IO ()
main = Test.Runner.Tasty.main DatabaseSpec.tests
