-- | A module for creating great logs in code that sends commands to Redis.
module Log.RedisCommands
  ( Details,
    emptyDetails,
    commands,
    host,
    port,
  )
where

import qualified Data.Aeson as Aeson

-- | A type describing redis commands.
--
-- > emptyDetails
-- >   { commands = [ "GET weather" ]
-- >   , host = Just "my-redis-host"
-- >   }
data Details = Details
  { -- | The commands that were sent to redis. Because Redis support for
    -- pipelining and transactions it's possible for one logical operation from
    -- the application perspective to contain multiple commands.
    --
    -- These commands are expected not to contain any sensitive information.
    -- Make sure sensitive values are mocked out, for example by replacing them
    -- with *****.
    commands :: List Text,
    -- | The host the redis commands are sent too.
    host :: Maybe Text,
    -- | The port redis is running on.
    port :: Maybe Int
  }
  deriving (Generic)

-- | An empty details value to be modified by you.
emptyDetails :: Details
emptyDetails = Details [] Nothing Nothing

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON infoEncodingOptions
  toEncoding = Aeson.genericToEncoding infoEncodingOptions

infoEncodingOptions :: Aeson.Options
infoEncodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 ' '
    }

instance Platform.TracingSpanDetails Details
