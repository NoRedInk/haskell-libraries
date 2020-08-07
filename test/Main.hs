module Main
  ( main,
  )
where

import qualified MySQLInternalSpec
import qualified MySQLQuerySpec
import qualified MySQLSpec
import qualified PostgresSettingsSpec
import qualified QueryParserSpec
import Test (Test, describe)
import qualified Test.Runner.Tasty
import qualified TimeSpec
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
  Test.Runner.Tasty.main tests

tests :: Test
tests =
  describe
    "lib/database"
    [ PostgresSettingsSpec.tests,
      MySQLInternalSpec.tests,
      QueryParserSpec.tests,
      TimeSpec.tests,
      MySQLQuerySpec.tests,
      MySQLSpec.tests
    ]
