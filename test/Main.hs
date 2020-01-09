module Main (main) where

import qualified Http
import Test (Test, describe, todo)
import qualified Test.Runner.Tasty
import Prelude (IO)

main :: IO ()
main = Test.Runner.Tasty.main tests

tests :: Test
tests =
  describe
    "Http"
    [ todo "Test a thing"
    ]
