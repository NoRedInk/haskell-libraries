module Postgres.SettingsSpec (tests) where

import Cherry.Prelude
import qualified Environment (decodeDefaults)
import qualified Expect
import qualified Postgres.Settings
import Test (Test, describe, test)
import Prelude (Either (Right))

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
          (Right Postgres.Settings.defaultSettings :: Either Text Postgres.Settings.Settings) -- expected
          (Environment.decodeDefaults Postgres.Settings.decoder) -- observed
    ]
