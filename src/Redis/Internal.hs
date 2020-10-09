{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Database.Redis
import Nri.Prelude

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

data InternalHandler
  = InternalHandler
      { rawPing :: Task Error Database.Redis.Status,
        rawGet :: ByteString -> Task Error (Maybe ByteString),
        rawSet :: ByteString -> ByteString -> Task Error (),
        rawGetSet :: ByteString -> ByteString -> Task Error (Maybe ByteString),
        rawGetMany :: [ByteString] -> Task Error [Maybe ByteString],
        rawSetMany :: [(ByteString, ByteString)] -> Task Error (),
        rawDelete :: [ByteString] -> Task Error Int,
        rawHGetAll :: ByteString -> Task Error [(ByteString, ByteString)],
        rawHSet :: ByteString -> ByteString -> ByteString -> Task Error (),
        rawAtomicModify ::
          forall a.
          ByteString ->
          (Maybe ByteString -> (ByteString, a)) ->
          Task Error (ByteString, a)
      }

data Handler
  = Handler
      { ping :: Task Error Database.Redis.Status,
        get :: ByteString -> Task Error (Maybe ByteString),
        set :: ByteString -> ByteString -> Task Error (),
        getset :: ByteString -> ByteString -> Task Error (Maybe ByteString),
        mget :: [ByteString] -> Task Error [Maybe ByteString],
        mset :: [(ByteString, ByteString)] -> Task Error (),
        del :: [ByteString] -> Task Error Int,
        hgetall :: ByteString -> Task Error [(ByteString, ByteString)],
        hset :: ByteString -> ByteString -> ByteString -> Task Error (),
        atomicModify ::
          forall a.
          ByteString ->
          (Maybe ByteString -> (ByteString, a)) ->
          Task Error (ByteString, a),
        unNamespacedHandler :: InternalHandler
      }

changeNamespace :: Text -> Handler -> Handler
changeNamespace namespace Handler {unNamespacedHandler} =
  namespacedHandler namespace unNamespacedHandler

namespacedHandler :: Text -> InternalHandler -> Handler
namespacedHandler namespace h =
  let byteNamespace = namespace ++ ":" |> toB
   in Handler
        { ping = rawPing h,
          get = \key -> rawGet h (byteNamespace ++ key),
          set = \key value -> rawSet h (byteNamespace ++ key) value,
          getset = \key value -> rawGetSet h (byteNamespace ++ key) value,
          mget = \keys -> rawGetMany h (map (\k -> byteNamespace ++ k) keys),
          mset = \assocs -> rawSetMany h (map (\(k, v) -> (byteNamespace ++ k, v)) assocs),
          del = \keys -> rawDelete h (map (byteNamespace ++) keys),
          hgetall = \key -> rawHGetAll h (byteNamespace ++ key),
          hset = \key field val -> rawHSet h (byteNamespace ++ key) field val,
          atomicModify = \key f -> rawAtomicModify h (byteNamespace ++ key) f,
          unNamespacedHandler = h
        }

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
