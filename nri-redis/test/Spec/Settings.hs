module Spec.Settings (tests) where

import Database.Redis hiding (String, Ok)
import qualified Dict
import qualified Environment
import qualified Expect
import qualified Redis.Settings as Settings
import qualified Test
import Prelude (Either (..), Show (..), String, pure)

tests :: Test.Test
tests =
  Test.describe "parsing settings from environment"
    [ decoderTests,
      decoderWithEnvVarPrefixTests,
      decoderWithCustomConnectionStringTests
    ]

-- | Compare equality based on `String` representation.
--
-- We lean on `show` for equality since `Database.Redis.ConnectInfo` doesn't have
-- an `Eq` instance (but does have a `Show` instance)
expectEqualShow :: Show a => a -> a -> Expect.Expectation
expectEqualShow x y = Expect.equal (show x) (show y)

decoderTests :: Test.Test
decoderTests =
  Test.describe "decoder tests"
    [ Test.test "returns expected defaults when no env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://localhost:6379"
        let expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
        Environment.decodePairs Settings.decoder (Dict.fromList [])
          |> expectEqualShow (Ok expected),
      Test.test "returns parsed values from env when env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://egg:bug@my-cool-host:36379/2"
        let env =
              Dict.fromList
                [ ("REDIS_CONNECTION_STRING", "redis://egg:bug@my-cool-host:36379/2"),
                  ("REDIS_CLUSTER", "1"),
                  ("REDIS_DEFAULT_EXPIRY_SECONDS", "10"),
                  ("REDIS_QUERY_TIMEOUT_MILLISECONDS", "0"),
                  ("REDIS_MAX_KEY_SIZE", "500")
                ]

            expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.Cluster,
                  Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 10,
                  Settings.queryTimeout = Settings.NoQueryTimeout,
                  Settings.maxKeySize = Settings.MaxKeySize 500
                }
        Environment.decodePairs Settings.decoder env
          |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with default db" <| \_ ->
        let env = Dict.fromList [("REDIS_CONNECTION_STRING", "redis+unix:///path/to/redis.sock")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/path/to/redis.sock",
                        connectDatabase = 0
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs Settings.decoder env
            |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with specified db" <| \_ ->
        let env = Dict.fromList [("REDIS_CONNECTION_STRING", "redis+unix://some:dude@/other/redis.sock?db=5")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/other/redis.sock",
                        connectAuth = Just "dude",
                        connectDatabase = 5
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs Settings.decoder env
            |> expectEqualShow (Ok expected)
    ]

decoderWithEnvVarPrefixTests :: Test.Test
decoderWithEnvVarPrefixTests =
  Test.describe "decoderWithEnvVarPrefix tests"
    [ Test.test "returns expected defaults when no env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://localhost:6379"
        let expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
        Environment.decodePairs (Settings.decoderWithEnvVarPrefix "TEST_") (Dict.fromList [])
          |> expectEqualShow (Ok expected),
      Test.test "returns parsed values from env when env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://egg:bug@my-cool-host:36379/2"
        let env =
              Dict.fromList
                [ ("TEST_REDIS_CONNECTION_STRING", "redis://egg:bug@my-cool-host:36379/2"),
                  ("TEST_REDIS_CLUSTER", "1"),
                  ("TEST_REDIS_DEFAULT_EXPIRY_SECONDS", "10"),
                  ("TEST_REDIS_QUERY_TIMEOUT_MILLISECONDS", "0"),
                  ("TEST_REDIS_MAX_KEY_SIZE", "500")
                ]

            expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.Cluster,
                  Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 10,
                  Settings.queryTimeout = Settings.NoQueryTimeout,
                  Settings.maxKeySize = Settings.MaxKeySize 500
                }
        Environment.decodePairs (Settings.decoderWithEnvVarPrefix "TEST_") env
          |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with default db" <| \_ ->
        let env = Dict.fromList [("TEST_REDIS_CONNECTION_STRING", "redis+unix:///path/to/redis.sock")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/path/to/redis.sock",
                        connectDatabase = 0
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs (Settings.decoderWithEnvVarPrefix "TEST_") env
            |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with specified db" <| \_ ->
        let env = Dict.fromList [("TEST_REDIS_CONNECTION_STRING", "redis+unix://some:dude@/other/redis.sock?db=5")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/other/redis.sock",
                        connectAuth = Just "dude",
                        connectDatabase = 5
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs (Settings.decoderWithEnvVarPrefix "TEST_") env
            |> expectEqualShow (Ok expected)
    ]

decoderWithCustomConnectionStringTests :: Test.Test
decoderWithCustomConnectionStringTests =
  Test.describe "decoderWithCustomConnectionString tests"
    [ Test.test "returns expected defaults when no env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://localhost:6379"
        let expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
        Environment.decodePairs (Settings.decoderWithCustomConnectionString "COOL_CONNECTION_STRING") (Dict.fromList [])
          |> expectEqualShow (Ok expected),
      Test.test "returns parsed values from env when env vars set" <| \_ -> do
        connectInfo <- parseConnectInfoElseFailTest "redis://egg:bug@my-cool-host:36379/2"
        let env =
              Dict.fromList
                [ ("COOL_CONNECTION_STRING", "redis://egg:bug@my-cool-host:36379/2"),
                  ("REDIS_CLUSTER", "1"),
                  ("REDIS_DEFAULT_EXPIRY_SECONDS", "10"),
                  ("REDIS_QUERY_TIMEOUT_MILLISECONDS", "0"),
                  ("REDIS_MAX_KEY_SIZE", "500")
                ]

            expected =
              Settings.Settings
                { Settings.connectionInfo = connectInfo,
                  Settings.clusterMode = Settings.Cluster,
                  Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 10,
                  Settings.queryTimeout = Settings.NoQueryTimeout,
                  Settings.maxKeySize = Settings.MaxKeySize 500
                }
        Environment.decodePairs (Settings.decoderWithCustomConnectionString "COOL_CONNECTION_STRING") env
          |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with default db" <| \_ ->
        let env = Dict.fromList [("COOL_CONNECTION_STRING", "redis+unix:///path/to/redis.sock")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/path/to/redis.sock",
                        connectDatabase = 0
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs (Settings.decoderWithCustomConnectionString "COOL_CONNECTION_STRING") env
            |> expectEqualShow (Ok expected),
      Test.test "handles unix socket scheme with specified db" <| \_ ->
        let env = Dict.fromList [("COOL_CONNECTION_STRING", "redis+unix://some:dude@/other/redis.sock?db=5")]
            expected =
              Settings.Settings
                { Settings.connectionInfo =
                    defaultConnectInfo
                      { connectPort = UnixSocket "/other/redis.sock",
                        connectAuth = Just "dude",
                        connectDatabase = 5
                      },
                  Settings.clusterMode = Settings.NotCluster,
                  Settings.defaultExpiry = Settings.NoDefaultExpiry,
                  Settings.queryTimeout = Settings.TimeoutQueryAfterMilliseconds 1000,
                  Settings.maxKeySize = Settings.NoMaxKeySize
                }
         in Environment.decodePairs (Settings.decoderWithCustomConnectionString "COOL_CONNECTION_STRING") env
            |> expectEqualShow (Ok expected)
    ]

parseConnectInfoElseFailTest :: String -> Expect.Expectation' ConnectInfo
parseConnectInfoElseFailTest uri = do
  Expect.succeeds <|
    case parseConnectInfo uri of
      Left err -> Task.fail <| "you wrote this test wrong, got err: " ++ Text.fromList err
      Right connectInfo -> pure connectInfo
