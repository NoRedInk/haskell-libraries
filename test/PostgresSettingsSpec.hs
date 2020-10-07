module PostgresSettingsSpec (tests) where

import Nri.Prelude
import qualified Environment (decodeDefaults)
import qualified Expect
import qualified Postgres.Settings
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Postgres.Settings"
    [ decodingTests
    ]

decodingTests :: Test
decodingTests =
  describe
    "decoding"
    [ test "default connection settings are decoded" <| \() ->
        Expect.equal
          (Ok Postgres.Settings.defaultSettings :: Result Text Postgres.Settings.Settings) -- expected
          (Environment.decodeDefaults Postgres.Settings.decoder) -- observed
    ]
