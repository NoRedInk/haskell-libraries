-- | Top-level specs module.
--
-- This collects and exposes this package's tests/specs, so that internal specs can
-- be written for internal modules without exposing either publicly.
--
-- The specs themselves don't live in here.
module DatabaseSpec
  ( main,
  )
where

import qualified Internal.Query.ParserSpec
import qualified Internal.TimeSpec
import qualified MySQL.InternalSpec
import qualified MySQLSpec
import qualified MySQLTransactionSpec
import qualified Postgres.SettingsSpec
import Test (Test, describe)
import qualified Test.Runner.Tasty
import qualified Prelude

main :: Prelude.IO ()
main = do
  -- `Test.Runner.Tasty.main` exits after finishing, so run other tests first.
  MySQLTransactionSpec.main
  Test.Runner.Tasty.main DatabaseSpec.tests

tests :: Test
tests =
  describe
    "lib/database"
    [ Postgres.SettingsSpec.tests,
      MySQL.InternalSpec.tests,
      Internal.Query.ParserSpec.tests,
      Internal.TimeSpec.tests,
      MySQLSpec.tests
    ]
