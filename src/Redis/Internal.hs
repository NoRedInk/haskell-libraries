{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Dict
import qualified List
import NriPrelude hiding (map)
import qualified Set
import qualified Task
import qualified Text
import qualified Tuple
import qualified Prelude

data Error
  = RedisError Text
  | ConnectionLost
  | DecodingError Text
  | DecodingFieldError Text
  | LibraryError Text
  | TransactionAborted
  | TimeoutError

instance Aeson.ToJSON Error where
  toJSON err = Aeson.toJSON (errorForHumans err)

instance Show Error where
  show = errorForHumans >> Data.Text.unpack

data TracedQuery where
  TracedQuery :: Query a -> TracedQuery

errorForHumans :: Error -> Text
errorForHumans topError =
  case topError of
    RedisError err -> "Redis error: " ++ err
    ConnectionLost -> "Connection Lost"
    LibraryError err -> "Library error when executing (probably due to a bug in the library): " ++ err
    DecodingError err -> "Could not decode value in key: " ++ err
    DecodingFieldError err -> "Could not decode field of hash: " ++ err
    TransactionAborted -> "Transaction aborted. Watched key has changed."
    TimeoutError -> "Redis query took too long."

-- | Render the commands a query is going to run for monitoring and debugging
-- purposes. Values we write are replaced with "*****" because they might
-- contain sensitive data.
cmds :: Query b -> [Text]
cmds query'' =
  case query'' of
    Del keys -> [unwords ("DEL" : keys)]
    Expire key val -> [unwords ["EXPIRE", key, Text.fromInt val]]
    Get key -> [unwords ["GET", key]]
    Getset key _ -> [unwords ["GETSET", key, "*****"]]
    Hdel key fields -> [unwords ("HDEL" : key : fields)]
    Hgetall key -> [unwords ["HGETALL", key]]
    Hget key field -> [unwords ["HGET", key, field]]
    Hmget key fields -> [unwords ("HMGET" : key : fields)]
    Hmset key _ -> [unwords ["HMSET", key, "*****"]]
    Hset key field _ -> [unwords ["HSET", key, field, "*****"]]
    Hsetnx key field _ -> [unwords ["HSETNX", key, field, "*****"]]
    Mget keys -> [unwords ("MGET" : keys)]
    Mset pairs -> [unwords ("MSET" : List.concatMap (\(key, _) -> [key, "*****"]) pairs)]
    Ping -> ["PING"]
    Rpush _ _ -> ["RPUSH"]
    Set key _ -> [unwords ["SET", key, "*****"]]
    Setnx key _ -> [unwords ["SETNX", key, "*****"]]
    Pure _ -> []
    Apply f x -> cmds f ++ cmds x
    WithResult _ x -> cmds x

unwords :: [Text] -> Text
unwords = Text.join " "

data Query a where
  Del :: [Text] -> Query Int
  Expire :: Text -> Int -> Query ()
  Get :: Text -> Query (Maybe ByteString)
  Getset :: Text -> ByteString -> Query (Maybe ByteString)
  Hdel :: Text -> [Text] -> Query Int
  Hgetall :: Text -> Query [(Text, ByteString)]
  Hget :: Text -> Text -> Query (Maybe ByteString)
  Hmget :: Text -> [Text] -> Query [Maybe ByteString]
  Hmset :: Text -> [(Text, ByteString)] -> Query ()
  Hset :: Text -> Text -> ByteString -> Query ()
  Hsetnx :: Text -> Text -> ByteString -> Query Bool
  Mget :: [Text] -> Query [Maybe ByteString]
  Mset :: [(Text, ByteString)] -> Query ()
  Ping :: Query Database.Redis.Status
  Rpush :: Text -> [ByteString] -> Query Int
  Set :: Text -> ByteString -> Query ()
  Setnx :: Text -> ByteString -> Query Bool
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
        doWatch :: [Text] -> Task Error (),
        namespace :: Text
      }

-- | Run a redis Query.
query :: Handler -> Query a -> Task Error a
query handler query' =
  namespaceQuery (namespace handler ++ ":") query'
    |> doQuery handler

timeoutAfterMilliseconds :: Float -> Handler -> Handler
timeoutAfterMilliseconds milliseconds handler =
  handler
    { doQuery = doQuery handler >> Task.timeout milliseconds TimeoutError,
      doTransaction = doTransaction handler >> Task.timeout milliseconds TimeoutError
    }

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
watch :: Handler -> List Text -> Task Error ()
watch handler keys =
  List.map (\key -> namespace handler ++ ":" ++ key) keys
    |> doWatch handler

namespaceQuery :: Text -> Query a -> Query a
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
    Rpush key vals -> Rpush (prefix ++ key) vals
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

keysTouchedByQuery :: Query a -> Set.Set Text
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
    Rpush key _ -> Set.singleton key
    Pure _ -> Set.empty
    Apply f x -> Set.union (keysTouchedByQuery f) (keysTouchedByQuery x)
    WithResult _ q -> keysTouchedByQuery q
    -- We use this function to collect keys we need to expire. If the user is
    -- explicitly setting an expiry we don't want to overwrite that.
    Expire _key _ -> Set.empty

toB :: Text -> ByteString
toB = Data.Text.Encoding.encodeUtf8

maybesToDict :: Ord key => List key -> List (Maybe a) -> Dict.Dict key a
maybesToDict keys values =
  List.map2 (,) keys values
    |> List.filterMap
      ( \(key, value) ->
          case value of
            Nothing -> Nothing
            Just v -> Just (key, v)
      )
    |> Dict.fromList
