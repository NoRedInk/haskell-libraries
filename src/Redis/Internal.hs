{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified List
import NriPrelude hiding (map)
import qualified Set
import qualified Text
import qualified Tuple
import qualified Prelude

data Error
  = RedisError Text
  | ConnectionLost
  | DecodingError FailedQuery Text
  | LibraryError Text
  | TransactionAborted

instance Aeson.ToJSON Error where
  toJSON err = Aeson.toJSON (errorForHumans err)

instance Show Error where
  show = errorForHumans >> Data.Text.unpack

data FailedQuery where
  FailedQuery :: Query a -> FailedQuery

errorForHumans :: Error -> Text
errorForHumans topError =
  case topError of
    RedisError err -> "Redis error: " ++ err
    ConnectionLost -> "Connection Lost"
    LibraryError err -> "Library error when executing (probably due to a bug in the library): " ++ err
    DecodingError (FailedQuery query') err ->
      Text.join
        " "
        [ "Could not decode value in key.",
          "Cmds:",
          Text.join "," (cmds query'),
          "Keys:",
          Text.join
            ","
            ( keysTouchedByQuery query'
                |> Set.toList
                |> List.map Data.Text.Encoding.decodeUtf8
            ),
          "Decoding error:",
          err
        ]
    TransactionAborted -> "Transaction aborted. Watched key has changed."

cmds :: Query b -> [Text]
cmds query'' =
  case query'' of
    Del _ -> ["DEL"]
    Expire _ _ -> ["EXPIRE"]
    Get _ -> ["GET"]
    Getset _ _ -> ["GETSET"]
    Hdel _ _ -> ["HDEL"]
    Hgetall _ -> ["HGETALL"]
    Hget _ _ -> ["HGET"]
    Hmget _ _ -> ["HMGET"]
    Hmset _ _ -> ["HMSET"]
    Hset _ _ _ -> ["HSET"]
    Hsetnx _ _ _ -> ["HSETNX"]
    Mget _ -> ["MGET"]
    Mset _ -> ["MSET"]
    Ping -> ["PING"]
    Set _ _ -> ["SET"]
    Setnx _ _ -> ["SETNX"]
    Pure _ -> []
    Apply f x -> cmds f ++ cmds x
    WithResult _ x -> cmds x

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
  Hsetnx :: ByteString -> ByteString -> ByteString -> Query Bool
  Mget :: [ByteString] -> Query [Maybe ByteString]
  Mset :: [(ByteString, ByteString)] -> Query ()
  Ping :: Query Database.Redis.Status
  Set :: ByteString -> ByteString -> Query ()
  Setnx :: ByteString -> ByteString -> Query Bool
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
    Setnx key value -> Setnx (prefix ++ key) value
    Getset key value -> Getset (prefix ++ key) value
    Mget keys -> Mget (List.map (\k -> prefix ++ k) keys)
    Mset assocs -> Mset (List.map (\(k, v) -> (prefix ++ k, v)) assocs)
    Del keys -> Del (List.map (prefix ++) keys)
    Hgetall key -> Hgetall (prefix ++ key)
    Hmget key fields -> Hmget (prefix ++ key) fields
    Hget key field -> Hget (prefix ++ key) field
    Hset key field val -> Hset (prefix ++ key) field val
    Hsetnx key field val -> Hsetnx (prefix ++ key) field val
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
          |> Set.toList
          |> Prelude.traverse (\key -> Expire key secs)
          |> map2 (\res _ -> res) query'
          |> doTransaction handler
   in handler {doQuery = doQuery, doTransaction = doQuery}

keysTouchedByQuery :: Query a -> Set.Set ByteString
keysTouchedByQuery query' =
  case query' of
    Del keys -> Set.fromList keys
    Ping -> Set.empty
    Get key -> Set.singleton key
    Set key _ -> Set.singleton key
    Setnx key _ -> Set.singleton key
    Getset key _ -> Set.singleton key
    Mget keys -> Set.fromList keys
    Hdel key _ -> Set.singleton key
    Mset assocs -> Set.fromList (List.map Tuple.first assocs)
    Hgetall key -> Set.singleton key
    Hmget key _ -> Set.singleton key
    Hget key _ -> Set.singleton key
    Hset key _ _ -> Set.singleton key
    Hsetnx key _ _ -> Set.singleton key
    Hmset key _ -> Set.singleton key
    Pure _ -> Set.empty
    Apply f x -> Set.union (keysTouchedByQuery f) (keysTouchedByQuery x)
    WithResult _ q -> keysTouchedByQuery q
    -- We use this function to collect keys we need to expire. If the user is
    -- explicitly setting an expiry we don't want to overwrite that.
    Expire _key _ -> Set.empty

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8
