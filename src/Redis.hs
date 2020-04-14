module Redis
  ( get,
    getJSON,
    set,
    setJSON,
    getSet,
    getSetJSON,
    delete,
    -- Settings
    Settings.Settings,
    Settings.decoder,
    -- Internal
    Internal.Handler,
    Internal.NamespacedHandler,
    Internal.namespacedHandler,
    -- Real
    Real.handler,
  )
where

import Cherry.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings

toT :: Data.ByteString.ByteString -> Text
toT = Data.Text.Encoding.decodeUtf8

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe Text)
get handler key =
  Internal.get handler (toB key)
    |> map (map toT)

getJSON :: Aeson.FromJSON a => Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe a)
getJSON handler key =
  Internal.get handler (toB key)
    |> map (andThen (Lazy.fromStrict >> Aeson.decode'))

set :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) (toB value)

setJSON :: Aeson.ToJSON a => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error ()
setJSON handler key value =
  Internal.set handler (toB key) (Aeson.encode value |> Lazy.toStrict)

getSet :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error (Maybe Text)
getSet handler key value =
  Internal.getSet handler (toB key) (toB value)
    |> map (map toT)

getSetJSON :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error (Maybe a)
getSetJSON handler key value =
  Internal.getSet handler (toB key) (Aeson.encode value |> Lazy.toStrict)
    |> map (andThen (Lazy.fromStrict >> Aeson.decode'))

delete :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
delete handler keys =
  Internal.delete handler (map toB keys)
