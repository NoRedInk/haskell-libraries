module PostgresSettingsSpec (tests) where

import qualified Dict
import qualified Environment (decodeDefaults, decodePairs)
import qualified Expect
import qualified Postgres.Settings
import qualified Postgres.Time as Time
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
          (Environment.decodeDefaults Postgres.Settings.decoder), -- observed
      test "prefixed variables are preferred" <| \() ->
        let defaults = Postgres.Settings.defaultSettings
         in Environment.decodePairs
              (Postgres.Settings.decoderWithPrefix "TEST_")
              ( Dict.fromList
                  [ ("PGHOST", "test"),
                    ("TEST_PGPORT", "123"),
                    ("TEST_PG_QUERY_TIMEOUT_SECONDS", "20")
                  ]
              )
              |> Expect.equal
                ( Ok
                    defaults
                      { Postgres.Settings.pgConnection =
                          (Postgres.Settings.pgConnection defaults)
                            { Postgres.Settings.pgHost = Postgres.Settings.PgHost "test",
                              Postgres.Settings.pgPort = Postgres.Settings.PgPort 123
                            },
                        Postgres.Settings.pgQueryTimeout = Time.fromSeconds 20
                      }
                )
    ]
