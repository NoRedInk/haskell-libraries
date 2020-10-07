module Redis.Settings (Settings (..), ClusterMode (..), decoder) where

import Nri.Prelude
import qualified Data.Text
import Database.Redis hiding (Ok)
import qualified Environment
import qualified Text
import Prelude (Either (Left, Right))

data ClusterMode = Cluster | NotCluster

data Settings
  = Settings
      { connectionInfo :: ConnectInfo,
        clusterMode :: ClusterMode
      }

decoder :: Environment.Decoder Settings
decoder = map2 Settings decoderConnectInfo decoderClusterMode

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
