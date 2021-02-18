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
import qualified Platform
import qualified Platform.Internal
import qualified PlatformSpec
import qualified SetSpec
import qualified System.IO
import Test (Test, describe, run)
import qualified TestSpec
import qualified TextSpec
import qualified Prelude

main :: Prelude.IO ()
main = do
  log <- Platform.silentHandler
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  run (tests log)

tests :: Platform.Internal.LogHandler -> Test
tests log =
  describe
    "NriPrelude"
    [ ArraySpec.tests,
      BitwiseSpec.tests,
      DictSpec.tests,
      SetSpec.tests,
      TestSpec.tests log,
      TextSpec.tests,
      LogSpec.tests,
      PlatformSpec.tests,
      DebugSpec.tests
    ]
