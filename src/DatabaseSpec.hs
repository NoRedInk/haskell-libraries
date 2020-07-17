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
import qualified Postgres.SettingsSpec
import Test (Test, describe)
import qualified Test.Runner.Tasty
import qualified Prelude

-- `Test.Runner.Tasty.main` exits after finishing, so run other tests first.
--
-- We're not running this spec: MySQLTransactionSpec.main
-- The spec runs a fuzz test against the database. Reasonbly fast locally but
-- not in CI. It's not using our standard test framework so it doesn't include
-- Junit output when it fails either.
--
-- The test was useful for debugging a problem and so we keep it around, but
-- it's not essential as a regression test.
main :: Prelude.IO ()
main =
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
