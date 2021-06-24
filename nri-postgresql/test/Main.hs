module Main
  ( main,
  )
where

import qualified Data.Acquire as Acquire
import qualified Environment
import qualified ObservabilitySpec
import qualified Postgres
import qualified PostgresSettingsSpec
import qualified QueryParserSpec
import Test (Test, describe, run)
import qualified TimeSpec
import qualified Prelude

-- `Test.run` exits after finishing, so run other tests first.
main :: Prelude.IO ()
main = do
  postgresSettings <- Environment.decode Postgres.decoder
  Acquire.withAcquire
    (Postgres.connection postgresSettings)
    (run << tests)

tests :: Postgres.Connection -> Test
tests postgres =
  describe
    "lib/database"
    [ PostgresSettingsSpec.tests,
      QueryParserSpec.tests,
      TimeSpec.tests,
      ObservabilitySpec.tests postgres
    ]
