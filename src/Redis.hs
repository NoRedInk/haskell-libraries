module Redis
  ( Handler,
    handler,
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

data Handler
  = Handler
      { anything :: Platform.DoAnythingHandler,
        connection :: Database.Redis.Connection
      }

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
      connection <- Database.Redis.connect
