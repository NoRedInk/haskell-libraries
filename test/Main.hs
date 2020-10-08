module Main
  ( main,
  )
where

import qualified Data.Acquire as Acquire
import qualified Environment
import qualified MySQL
import qualified MySQLInternalSpec
import qualified MySQLQuerySpec
import qualified MySQLSpec
import Nri.Prelude
import qualified ObservabilitySpec
import qualified Postgres
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
main = do
  mysqlSettings <- Environment.decode MySQL.decoder
  postgresSettings <- Environment.decode Postgres.decoder
  Acquire.withAcquire
    ( do
        mysql <- MySQL.connection mysqlSettings
        postgres <- Postgres.connection postgresSettings
        Prelude.pure (mysql, postgres)
    )
    (Test.Runner.Tasty.main << tests)

tests :: (MySQL.Connection, Postgres.Connection) -> Test
tests (mysql, postgres) =
  describe
    "lib/database"
    [ PostgresSettingsSpec.tests,
      MySQLInternalSpec.tests,
      QueryParserSpec.tests,
      TimeSpec.tests,
      MySQLQuerySpec.tests,
      ObservabilitySpec.tests mysql postgres,
      MySQLSpec.tests mysql
    ]
