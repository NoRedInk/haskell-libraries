-- | Top-level specs module.
--
-- This collects and exposes this package's tests/specs, so that internal specs can
-- be written for internal modules without exposing either publicly.
--
-- The specs themselves don't live in here.
module DatabaseSpec
  ( tests,
  )
where

import qualified MySQL.InternalSpec
import qualified Postgres.SettingsSpec
import Test (Test, describe)

tests :: Test
tests =
  describe
    "lib/database"
    [ Postgres.SettingsSpec.tests,
      MySQL.InternalSpec.tests
    ]
