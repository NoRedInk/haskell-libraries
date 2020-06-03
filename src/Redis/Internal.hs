{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import Cherry.Prelude
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Database.Redis

data Error
  = RedisError Text
  | ConnectionLost
  | LibraryError Text
  deriving (Show, Generic)

instance Aeson.ToJSON Error

errorForHumans :: Error -> Text
errorForHumans topError =
  case topError of
    RedisError err -> "Redis error: " ++ err
    ConnectionLost -> "Connection Lost"
    LibraryError err -> "Library error: " ++ err

data Handler
  = Handler
      { rawPing :: Task Error Database.Redis.Status,
        rawGet :: ByteString -> Task Error (Maybe ByteString),
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
      { ping :: Task Error Database.Redis.Status,
        get :: ByteString -> Task Error (Maybe ByteString),
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
        { ping = rawPing h,
          get = \key -> rawGet h (byteNamespace ++ key),
          set = \key value -> rawSet h (byteNamespace ++ key) value,
          getSet = \key value -> rawGetSet h (byteNamespace ++ key) value,
          getMany = \keys -> rawGetMany h (map (\k -> byteNamespace ++ k) keys),
          setMany = \assocs -> rawSetMany h (map (\(k, v) -> (byteNamespace ++ k, v)) assocs),
          delete = \keys -> rawDelete h (map (byteNamespace ++) keys),
          atomicModify = \key f -> rawAtomicModify h (byteNamespace ++ key) f
        }

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
