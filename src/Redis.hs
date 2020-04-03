module Redis (Handler, handler) where

import Cherry.Prelude
import qualified Data.ByteString
import qualified Data.Text.Encoding

newtype Handler = Handler ()

toT :: Data.ByteString.ByteString -> Text
toT = Data.Text.Encoding.decodeUtf8

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

handler :: Text -> Handler
handler _ = Handler ()
