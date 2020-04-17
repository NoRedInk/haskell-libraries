module Redis.Internal where

import Cherry.Prelude
import qualified Data.ByteString
import qualified Data.Text.Encoding

data Error
  = RedisError Text
  | ConnectionLost
  | LibraryError Text
  deriving (Show)

data Handler
  = Handler
      { rawGet :: Data.ByteString.ByteString -> Task Error (Maybe Data.ByteString.ByteString),
        rawSet :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Task Error (),
        rawGetSet :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Task Error (Maybe Data.ByteString.ByteString),
        rawDelete :: [Data.ByteString.ByteString] -> Task Error Int,
        rawAtomicModify ::
          Data.ByteString.ByteString ->
          (Maybe Data.ByteString.ByteString -> Data.ByteString.ByteString) ->
          Task Error Data.ByteString.ByteString
      }

data NamespacedHandler
  = NamespacedHandler
      { get :: Data.ByteString.ByteString -> Task Error (Maybe Data.ByteString.ByteString),
        set :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Task Error (),
        getSet :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Task Error (Maybe Data.ByteString.ByteString),
        delete :: [Data.ByteString.ByteString] -> Task Error Int,
        atomicModify ::
          Data.ByteString.ByteString ->
          (Maybe Data.ByteString.ByteString -> Data.ByteString.ByteString) ->
          Task Error Data.ByteString.ByteString
      }

namespacedHandler :: Handler -> Text -> NamespacedHandler
namespacedHandler h namespace =
  let byteNamespace = namespace ++ ":" |> toB
   in NamespacedHandler
        { get = \key -> rawGet h (byteNamespace ++ key),
          set = \key value -> rawSet h (byteNamespace ++ key) value,
          getSet = \key value -> rawGetSet h (byteNamespace ++ key) value,
          delete = \keys -> rawDelete h (map (byteNamespace ++) keys),
          atomicModify = \key f -> rawAtomicModify h (byteNamespace ++ key) f
        }

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
