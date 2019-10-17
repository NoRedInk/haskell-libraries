module Main (main) where

import Nri.Prelude (IO)
import qualified PostgresSpecs
import qualified Test.Runner.Tasty

main :: IO ()
main = Test.Runner.Tasty.main PostgresSpecs.tests
