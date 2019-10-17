module PostgresSpecs (tests) where

{-| Top-level specs module.

This collects and exposes this package's tests/specs, so that internal specs can
be written for internal modules without exposing either publicly.

The specs themselves don't live in here.

-}
import qualified Postgres.SettingsSpec
import Test (Test, describe)

tests :: Test
tests =
  describe "Nri PostgreSQL lib"
    [Postgres.SettingsSpec.tests]
