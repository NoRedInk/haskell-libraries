module Main (main) where

import Test (Test, describe)
import qualified Test.Runner.Tasty
import Prelude (IO)

main :: IO ()
main = Test.Runner.Tasty.main tests

tests :: Test
tests =
  describe
    "Http"
    []
