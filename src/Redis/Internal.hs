{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified List
import NriPrelude hiding (map)
import qualified Tuple
import qualified Prelude

data Error
  = RedisError Text
  | ConnectionLost
  | LibraryError Text
  | TransactionAborted
  deriving (Show, Generic)

instance Aeson.ToJSON Error

errorForHumans :: Error -> Text
errorForHumans topError =
  case topError of
    RedisError err -> "Redis error: " ++ err
    ConnectionLost -> "Connection Lost"
    LibraryError err -> "Library error: " ++ err
    TransactionAborted -> "Transaction aborted. Watched key has changed."

data Query a where
  Del :: [ByteString] -> Query Int
  Expire :: ByteString -> Int -> Query ()
  Get :: ByteString -> Query (Maybe ByteString)
  Getset :: ByteString -> ByteString -> Query (Maybe ByteString)
  Hdel :: ByteString -> [ByteString] -> Query Int
  Hgetall :: ByteString -> Query [(ByteString, ByteString)]
  Hget :: ByteString -> ByteString -> Query (Maybe ByteString)
  Hmget :: ByteString -> [ByteString] -> Query [Maybe ByteString]
  Hmset :: ByteString -> [(ByteString, ByteString)] -> Query ()
  Hset :: ByteString -> ByteString -> ByteString -> Query ()
  Mget :: [ByteString] -> Query [Maybe ByteString]
  Mset :: [(ByteString, ByteString)] -> Query ()
  Ping :: Query Database.Redis.Status
  Set :: ByteString -> ByteString -> Query ()
  -- The constructors below are not Redis-related, but support using functions
  -- like `map` and `map2` on queries.
  Pure :: a -> Query a
  Apply :: Query (a -> b) -> Query a -> Query b
  WithResult :: (a -> Result Error b) -> Query a -> Query b

instance Prelude.Functor Query where
  fmap = map

map :: (a -> b) -> Query a -> Query b
map a = WithResult (a >> Ok)

instance Prelude.Applicative Query where
  pure = Pure
  (<*>) = Apply

data Handler
  = Handler
      { doQuery :: forall a. Query a -> Task Error a,
        doTransaction :: forall a. Query a -> Task Error a,
        doWatch :: [ByteString] -> Task Error (),
        namespace :: ByteString
      }

-- | Run a redis Query.
query :: Handler -> Query a -> Task Error a
query handler query' =
  namespaceQuery (namespace handler ++ ":") query'
    |> doQuery handler

-- | Run a redis Query in a transaction. If the query contains several Redis
-- commands they're all executed together, and Redis will guarantee other
-- requests won't be able change values in between.
transaction :: Handler -> Query a -> Task Error a
transaction handler query' =
  namespaceQuery (namespace handler ++ ":") query'
    |> doTransaction handler

-- Runs the redis `WATCH` command. This isn't one of the `Query`
-- constructors because we're not able to run `WATCH` in a transaction,
-- only as a separate command.
watch :: Handler -> List ByteString -> Task Error ()
watch handler keys =
  List.map (\key -> namespace handler ++ ":" ++ key) keys
    |> doWatch handler

namespaceQuery :: ByteString -> Query a -> Query a
namespaceQuery prefix query' =
  case query' of
    Ping -> Ping
    Get key -> Get (prefix ++ key)
    Set key value -> Set (prefix ++ key) value
    Getset key value -> Getset (prefix ++ key) value
    Mget keys -> Mget (List.map (\k -> prefix ++ k) keys)
    Mset assocs -> Mset (List.map (\(k, v) -> (prefix ++ k, v)) assocs)
    Del keys -> Del (List.map (prefix ++) keys)
    Hgetall key -> Hgetall (prefix ++ key)
    Hmget key fields -> Hmget (prefix ++ key) fields
    Hget key field -> Hget (prefix ++ key) field
    Hset key field val -> Hset (prefix ++ key) field val
    Hmset key vals -> Hmset (prefix ++ key) vals
    Hdel key fields -> Hdel (prefix ++ key) fields
    Expire key secs -> Expire (prefix ++ key) secs
    Pure x -> Pure x
    Apply f x -> Apply (namespaceQuery prefix f) (namespaceQuery prefix x)
    WithResult f q -> WithResult f (namespaceQuery prefix q)

defaultExpiryKeysAfterSeconds :: Int -> Handler -> Handler
defaultExpiryKeysAfterSeconds secs handler =
  let doQuery :: Query a -> Task Error a
      doQuery query' =
        keysTouchedByQuery query'
          |> Prelude.traverse (\key -> Expire key secs)
          |> map2 (\res _ -> res) query'
          |> doTransaction handler
   in handler {doQuery = doQuery, doTransaction = doQuery}

keysTouchedByQuery :: Query a -> [ByteString]
keysTouchedByQuery query' =
  case query' of
    Ping -> []
    Get key -> [key]
    Set key _ -> [key]
    Getset key _ -> [key]
    Mget keys -> keys
    Mset assocs -> List.map Tuple.first assocs
    Hgetall key -> [key]
    Hmget key _ -> [key]
    Hget key _ -> [key]
    Hset key _ _ -> [key]
    Hmset key _ -> [key]
    Pure _ -> []
    Apply f x -> keysTouchedByQuery f ++ keysTouchedByQuery x
    WithResult _ q -> keysTouchedByQuery q
    -- Don't report on deleted keys. They're gone after this command so we don't
    -- want to set expiry times for them.
    Del _keys -> []
    Hdel _key _ -> []
    -- We use this function to collect keys we need to expire. If the user is
    -- explicitly setting an expiry we don't want to overwrite that.
    Expire _key _ -> []

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
