module Redis
  ( Handler,
    handler,
    Namespace (..),
    -- Settings
    Settings.Settings,
    Settings.decoder,
  )
where

import Cherry.Prelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Platform
import qualified Redis.Settings as Settings
import qualified Task
import Prelude (Either (Left, Right), pure)

data Handler
  = Handler
      { anything :: Platform.DoAnythingHandler,
        connection :: Database.Redis.Connection
      }

data Error = Error
  deriving (Show, Eq)

newtype Namespace = Namespace Text
  deriving (Show, Eq)

toT :: Data.ByteString.ByteString -> Text
toT = Data.Text.Encoding.decodeUtf8

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

handler :: Settings.Settings -> Data.Acquire.Acquire Handler
handler settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      anything <- Platform.doAnythingHandler
      connection <- Database.Redis.checkedConnect (Settings.connectionInfo settings)
      pure <| Handler anything connection
    release Handler {connection} = Database.Redis.disconnect connection

namespaceKey :: Namespace -> Text -> Data.ByteString.ByteString
namespaceKey (Namespace ns) key =
  ns ++ ":" ++ key |> toB

platformRedis :: Handler -> Database.Redis.Redis (Either Database.Redis.Reply a) -> Task Error a
platformRedis Handler {connection, anything} action =
  Platform.doAnything
    anything
    ( Database.Redis.runRedis connection action
        |> map
          ( \rep -> case rep of
              Left _ -> Err Error
              Right r -> Ok r
          )
    )

set :: Handler -> Namespace -> Text -> Text -> Task Error Database.Redis.Status
set handler ns key value =
  platformRedis handler (Database.Redis.set (namespaceKey ns key) (toB value))

get :: Handler -> Namespace -> Text -> Task Error (Maybe Text)
get handler ns key =
  platformRedis handler (Database.Redis.get (namespaceKey ns key))
    |> map (map toT)
