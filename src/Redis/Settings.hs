module Redis.Settings (Settings (..), ClusterMode (..), DefaultExpiry (..), QueryTimeout (..), decoder) where

import qualified Data.Text
import Database.Redis hiding (Ok)
import qualified Environment
import NriPrelude
import qualified Text
import Prelude (Either (Left, Right))

data ClusterMode = Cluster | NotCluster

data Settings
  = Settings
      { connectionInfo :: ConnectInfo,
        clusterMode :: ClusterMode,
        defaultExpiry :: DefaultExpiry,
        queryTimeout :: QueryTimeout
      }

data DefaultExpiry = NoDefaultExpiry | ExpireKeysAfterSeconds Int

data QueryTimeout = NoQueryTimeout | TimeoutQueryAfterMilliseconds Int

decoder :: Environment.Decoder Settings
decoder =
  map4
    Settings
    decoderConnectInfo
    decoderClusterMode
    decoderDefaultExpiry
    decoderQueryTimeout

decoderClusterMode :: Environment.Decoder ClusterMode
decoderClusterMode =
  Environment.variable
    Environment.Variable
      { Environment.name = "REDIS_CLUSTER",
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

decoderConnectInfo :: Environment.Decoder ConnectInfo
decoderConnectInfo =
  Environment.variable
    Environment.Variable
      { Environment.name = "REDIS_CONNECTION_STRING",
        Environment.description = "Full redis connection string",
        Environment.defaultValue = "redis://localhost:6379"
      }
    ( Environment.custom
        Environment.text
        ( \str ->
            case str |> Data.Text.unpack |> parseConnectInfo of
              Right info' -> Ok info'
              Left parseError -> Err ("Invalid Redis connection string: " ++ Data.Text.pack parseError)
        )
    )

decoderDefaultExpiry :: Environment.Decoder DefaultExpiry
decoderDefaultExpiry =
  Environment.variable
    Environment.Variable
      { Environment.name = "REDIS_DEFAULT_EXPIRY_SECONDS",
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

decoderQueryTimeout :: Environment.Decoder QueryTimeout
decoderQueryTimeout =
  Environment.variable
    Environment.Variable
      { Environment.name = "REDIS_QUERY_TIMEOUT_MILLISECONDS",
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
