module Postgres.SettingsSpec (tests) where

import qualified Environment (decodeDefaults)
import qualified Expect
import Nri.Prelude
import qualified Postgres.Settings
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Internal PostgreSQL settings tests"
    [ decodingTests
    ]

decodingTests :: Test
decodingTests =
  describe
    "decoding"
    [ test "default connection settings are decoded" <| \() ->
        Expect.equal
          (Right Postgres.Settings.defaultSettings :: Either Text Postgres.Settings.Settings) -- expected
          (Environment.decodeDefaults Postgres.Settings.decoder) -- observed
    ]
