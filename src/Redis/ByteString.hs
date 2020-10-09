module Redis.ByteString
  ( hSet,
    hGetAll,
  )
where

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import Nri.Prelude
import qualified Redis.Internal as Internal

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
hGetAll :: Internal.NamespacedHandler -> Text -> Task Internal.Error [(ByteString, ByteString)]
hGetAll handler key =
  Internal.hGetAll handler (toB key)

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
hSet :: Internal.NamespacedHandler -> Text -> ByteString -> ByteString -> Task Internal.Error ()
hSet handler key field val =
  Internal.hSet handler (toB key) field val

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
