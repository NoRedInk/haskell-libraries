module Redis.Settings (Settings, decoder) where

import Cherry.Prelude
import qualified Data.Text
import Database.Redis
import qualified Environment

newtype Settings
  = Settings
      { connectionString :: Text
      }

decoder :: Environment.Decoder Settings
decoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "REDIS_CONNECTION_STRING",
        Environment.description = "Full redis connection string",
        Environment.defaultValue = "redis://localhost:6379"
      }
    (Environment.text |> map Settings)
