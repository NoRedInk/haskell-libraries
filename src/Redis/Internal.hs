{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import Cherry.Prelude
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding

data Error
  = RedisError Text
  | ConnectionLost
  | LibraryError Text
  deriving (Show)

data Handler
  = Handler
      { rawGet :: ByteString -> Task Error (Maybe ByteString),
        rawSet :: ByteString -> ByteString -> Task Error (),
        rawGetSet :: ByteString -> ByteString -> Task Error (Maybe ByteString),
        rawGetMany :: [ByteString] -> Task Error [Maybe ByteString],
        rawSetMany :: [(ByteString, ByteString)] -> Task Error (),
        rawDelete :: [ByteString] -> Task Error Int,
        rawAtomicModify ::
          forall a.
          ByteString ->
          (Maybe ByteString -> (ByteString, a)) ->
          Task Error (ByteString, a)
      }

data NamespacedHandler
  = NamespacedHandler
      { get :: ByteString -> Task Error (Maybe ByteString),
        set :: ByteString -> ByteString -> Task Error (),
        getSet :: ByteString -> ByteString -> Task Error (Maybe ByteString),
        getMany :: [ByteString] -> Task Error [Maybe ByteString],
        setMany :: [(ByteString, ByteString)] -> Task Error (),
        delete :: [ByteString] -> Task Error Int,
        atomicModify ::
          forall a.
          ByteString ->
          (Maybe ByteString -> (ByteString, a)) ->
          Task Error (ByteString, a)
      }

namespacedHandler :: Handler -> Text -> NamespacedHandler
namespacedHandler h namespace =
  let byteNamespace = namespace ++ ":" |> toB
   in NamespacedHandler
        { get = \key -> rawGet h (byteNamespace ++ key),
          set = \key value -> rawSet h (byteNamespace ++ key) value,
          getSet = \key value -> rawGetSet h (byteNamespace ++ key) value,
          getMany = \keys -> rawGetMany h (map (\k -> byteNamespace ++ k) keys),
          setMany = \assocs -> rawSetMany h (map (\(k, v) -> (byteNamespace ++ k, v)) assocs),
          delete = \keys -> rawDelete h (map (byteNamespace ++) keys),
          atomicModify = \key f -> rawAtomicModify h (byteNamespace ++ key) f
        }

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
