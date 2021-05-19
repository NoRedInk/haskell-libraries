module Redis.Settings (Settings (..), ClusterMode (..), DefaultExpiry (..), QueryTimeout (..), decoder, decoderWithEnvVarPrefix) where

import Database.Redis hiding (Ok)
import qualified Environment
import qualified Text
import Prelude (Either (Left, Right))

data ClusterMode = Cluster | NotCluster

-- | Settings required to initiate a redis connection.
data Settings = Settings
  { -- | Full redis connection string.
    --
    -- Default env var name is REDIS_CONNECTION_STRING
    -- default is "redis://localhost:6379"
    connectionInfo :: ConnectInfo,
    -- | Set to 1 for cluster, everything else is not.
    --
    -- Default env var name is REDIS_CLUSTER
    -- Default is 0
    clusterMode :: ClusterMode,
    -- | Set a default amount of seconds after which all keys touched by this
    -- handler will expire. The expire time of a key is reset every time it is
    -- read or written. A value of 0 means no default expiry.
    --
    -- Default env var name is REDIS_DEFAULT_EXPIRY_SECONDS
    -- default is 0
    defaultExpiry :: DefaultExpiry,
    -- | 0 means no timeout, every other value is a timeout in milliseconds.
    --
    -- Default env var name is REDIS_QUERY_TIMEOUT_MILLISECONDS
    -- default is 1000
    queryTimeout :: QueryTimeout
  }

data DefaultExpiry = NoDefaultExpiry | ExpireKeysAfterSeconds Int

data QueryTimeout = NoQueryTimeout | TimeoutQueryAfterMilliseconds Int

-- decodes Settings from environmental variables
decoder :: Environment.Decoder Settings
decoder =
  decoderWithEnvVarPrefix ""

-- decodes Settings from environmental variables prefixed with a Text
-- >>> decoderWithEnvVarPrefix "WORKER_"
decoderWithEnvVarPrefix :: Text -> Environment.Decoder Settings
decoderWithEnvVarPrefix prefix =
  map4
    Settings
    (decoderConnectInfo prefix)
    (decoderClusterMode prefix)
    (decoderDefaultExpiry prefix)
    (decoderQueryTimeout prefix)

decoderClusterMode :: Text -> Environment.Decoder ClusterMode
decoderClusterMode prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_CLUSTER",
        Environment.description = "Set to 1 for cluster, everything else is not",
        Environment.defaultValue = "0"
      }
    ( Environment.custom
        Environment.text
        ( \str ->
            if Text.trim str == "1"
              then Ok Cluster
              else Ok NotCluster
        )
    )

decoderConnectInfo :: Text -> Environment.Decoder ConnectInfo
decoderConnectInfo prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_CONNECTION_STRING",
        Environment.description = "Full redis connection string",
        Environment.defaultValue = "redis://localhost:6379"
      }
    ( Environment.custom
        Environment.text
        ( \str ->
            case str |> Text.toList |> parseConnectInfo of
              Right info' -> Ok info'
              Left parseError -> Err ("Invalid Redis connection string: " ++ Text.fromList parseError)
        )
    )

decoderDefaultExpiry :: Text -> Environment.Decoder DefaultExpiry
decoderDefaultExpiry prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_DEFAULT_EXPIRY_SECONDS",
        Environment.description = "Set a default amount of seconds after which all keys touched by this handler will expire. The expire time of a key is reset every time it is read or written. A value of 0 means no default expiry.",
        Environment.defaultValue = "0"
      }
    Environment.int
    |> map
      ( \secs ->
          if secs == 0
            then NoDefaultExpiry
            else ExpireKeysAfterSeconds secs
      )

decoderQueryTimeout :: Text -> Environment.Decoder QueryTimeout
decoderQueryTimeout prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_QUERY_TIMEOUT_MILLISECONDS",
        Environment.description = "0 means no timeout, every other value is a timeout in milliseconds.",
        Environment.defaultValue = "1000"
      }
    ( Environment.custom
        Environment.int
        ( \milliseconds ->
            if milliseconds <= 0
              then Ok NoQueryTimeout
              else Ok (TimeoutQueryAfterMilliseconds milliseconds)
        )
    )
