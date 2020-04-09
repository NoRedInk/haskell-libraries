module Redis.Settings (Settings (..), decoder) where

import Cherry.Prelude
import qualified Data.Text
import Database.Redis hiding (Ok)
import qualified Environment
import Prelude (Either (Left, Right))

newtype Settings
  = Settings
      { connectionInfo :: ConnectInfo
      }

decoder :: Environment.Decoder Settings
decoder = map Settings decoderConnectInfo

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
              Right info -> Ok info
              Left parseError -> Err ("Invalid Redis connection string: " ++ Data.Text.pack parseError)
        )
    )
