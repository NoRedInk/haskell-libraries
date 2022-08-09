module Main
  ( main,
  )
where

import qualified ArraySpec
import qualified BitwiseSpec
import qualified DebugSpec
import qualified DictSpec
import qualified GHC.IO.Encoding
import qualified LogSpec
import qualified PlatformSpec
import qualified SetSpec
import qualified System.IO
import Test (Test, describe, run)
import qualified TaskSpec
import qualified TestSpec
import qualified TextSpec
import qualified Prelude

main :: Prelude.IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  run tests

tests :: Test
tests =
  describe
    "NriPrelude"
    [ ArraySpec.tests,
      BitwiseSpec.tests,
      DictSpec.tests,
      SetSpec.tests,
      TaskSpec.tests,
      TestSpec.tests,
      TextSpec.tests,
      LogSpec.tests,
      PlatformSpec.tests,
      DebugSpec.tests
    ]
