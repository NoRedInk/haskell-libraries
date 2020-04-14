module Main (main) where

import Cherry.Prelude
import qualified Expect
import Test
import qualified Test.Runner.Tasty
import Prelude (IO)

specs :: Test
specs =
  describe
    "Redis"
    [test "blah" <| \() -> Expect.equal 'A' 'B']

main :: IO ()
main = do
  Test.Runner.Tasty.main specs
