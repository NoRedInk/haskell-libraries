module Main
  ( main,
  )
where

import qualified ArraySpec
import qualified BitwiseSpec
import qualified DebugSpec
import qualified DictSpec
import qualified LogSpec
import qualified PlatformSpec
import qualified SetSpec
import Test (Test, describe, run)
import qualified TextSpec
import Prelude (IO)

main :: IO ()
main = run tests

tests :: Test
tests =
  describe
    "NriPrelude"
    [ ArraySpec.tests,
      BitwiseSpec.tests,
      DictSpec.tests,
      SetSpec.tests,
      TextSpec.tests,
      LogSpec.tests,
      PlatformSpec.tests,
      DebugSpec.tests
    ]
