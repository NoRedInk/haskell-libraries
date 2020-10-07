{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import Nri.Prelude
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
        rawHGetAll :: ByteString -> Task Error [(ByteString, ByteString)],
        rawHSet :: ByteString -> ByteString -> ByteString -> Task Error (),
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
        hGetAll :: ByteString -> Task Error [(ByteString, ByteString)],
        hSet :: ByteString -> ByteString -> ByteString -> Task Error (),
        atomicModify ::
          forall a.
          ByteString ->
          (Maybe ByteString -> (ByteString, a)) ->
          Task Error (ByteString, a),
        unNamespacedHandler :: Handler
      }

changeNamespace :: Text -> NamespacedHandler -> NamespacedHandler
changeNamespace namespace NamespacedHandler {unNamespacedHandler} =
  namespacedHandler namespace unNamespacedHandler

namespacedHandler :: Text -> Handler -> NamespacedHandler
namespacedHandler namespace h =
  let byteNamespace = namespace ++ ":" |> toB
   in NamespacedHandler
        { ping = rawPing h,
          get = \key -> rawGet h (byteNamespace ++ key),
          set = \key value -> rawSet h (byteNamespace ++ key) value,
          getSet = \key value -> rawGetSet h (byteNamespace ++ key) value,
          getMany = \keys -> rawGetMany h (map (\k -> byteNamespace ++ k) keys),
          setMany = \assocs -> rawSetMany h (map (\(k, v) -> (byteNamespace ++ k, v)) assocs),
          delete = \keys -> rawDelete h (map (byteNamespace ++) keys),
          hGetAll = \key -> rawHGetAll h (byteNamespace ++ key),
          hSet = \key field val -> rawHSet h (byteNamespace ++ key) field val,
          atomicModify = \key f -> rawAtomicModify h (byteNamespace ++ key) f,
          unNamespacedHandler = h
        }

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
