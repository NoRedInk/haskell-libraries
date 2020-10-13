{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Database.Redis
import Nri.Prelude
import qualified Prelude

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

data Query a where
  Ping :: Query Database.Redis.Status
  Get :: ByteString -> Query (Maybe ByteString)
  Set :: ByteString -> ByteString -> Query ()
  Getset :: ByteString -> ByteString -> Query (Maybe ByteString)
  Mget :: [ByteString] -> Query [Maybe ByteString]
  Mset :: [(ByteString, ByteString)] -> Query ()
  Del :: [ByteString] -> Query Int
  Hgetall :: ByteString -> Query [(ByteString, ByteString)]
  Hset :: ByteString -> ByteString -> ByteString -> Query ()
  -- AtomicModify is not a Redis command but a higher-level function supporting
  -- the common use case of reading a value and then writing it atomically.
  AtomicModify :: ByteString -> (Maybe ByteString -> (ByteString, a)) -> Query (ByteString, a)
  -- The constructors below are not Redis-related, but support using functions
  -- like `map` and `map2` on queries.
  Fmap :: (a -> b) -> Query a -> Query b
  Pure :: a -> Query a
  Apply :: Query (a -> b) -> Query a -> Query b

instance Prelude.Functor Query where
  fmap = Fmap

instance Prelude.Applicative Query where
  pure = Pure
  (<*>) = Apply

newtype InternalHandler
  = InternalHandler
      { doQuery :: forall a. Query a -> Task Error a
      }

data Handler
  = Handler
      { handlerWithNamespace :: InternalHandler,
        unNamespacedHandler :: InternalHandler
      }

query :: Handler -> Query a -> Task Error a
query handler query' = doQuery (handlerWithNamespace handler) query'

changeNamespace :: Text -> Handler -> Handler
changeNamespace namespace Handler {unNamespacedHandler} =
  namespacedHandler namespace unNamespacedHandler

namespacedHandler :: Text -> InternalHandler -> Handler
namespacedHandler namespace h =
  Handler
    { unNamespacedHandler = h,
      handlerWithNamespace = InternalHandler (doQuery h << namespaceQuery namespace)
    }

namespaceQuery :: Text -> Query a -> Query a
namespaceQuery namespace query' =
  let byteNamespace = namespace ++ ":" |> toB
   in case query' of
        Ping -> Ping
        Get key -> Get (byteNamespace ++ key)
        Set key value -> Set (byteNamespace ++ key) value
        Getset key value -> Getset (byteNamespace ++ key) value
        Mget keys -> Mget (map (\k -> byteNamespace ++ k) keys)
        Mset assocs -> Mset (map (\(k, v) -> (byteNamespace ++ k, v)) assocs)
        Del keys -> Del (map (byteNamespace ++) keys)
        Hgetall key -> Hgetall (byteNamespace ++ key)
        Hset key field val -> Hset (byteNamespace ++ key) field val
        AtomicModify key f -> AtomicModify (byteNamespace ++ key) f
        Fmap f q -> Fmap f (namespaceQuery namespace q)
        Pure x -> Pure x
        Apply f x -> Apply (namespaceQuery namespace f) (namespaceQuery namespace x)

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
