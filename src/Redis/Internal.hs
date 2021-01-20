{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Redis.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Dict
import qualified List
import NriPrelude hiding (map, map2)
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
    Del keys -> [unwords ("DEL" : NonEmpty.toList keys)]
    Exists key -> [unwords ["EXISTS", key]]
    Expire key val -> [unwords ["EXPIRE", key, Text.fromInt val]]
    Get key -> [unwords ["GET", key]]
    Getset key _ -> [unwords ["GETSET", key, "*****"]]
    Hdel key fields -> [unwords ("HDEL" : key : fields)]
    Hgetall key -> [unwords ["HGETALL", key]]
    Hget key field -> [unwords ["HGET", key, field]]
    Hmget key fields -> [unwords ("HMGET" : key : NonEmpty.toList fields)]
    Hmset key pairs ->
      [ unwords
          ( "HMSET"
              : key
              : List.concatMap (\(field, _) -> [field, "*****"]) pairs
          )
      ]
    Hset key field _ -> [unwords ["HSET", key, field, "*****"]]
    Hsetnx key field _ -> [unwords ["HSETNX", key, field, "*****"]]
    Incr key -> [unwords ["INCR", key]]
    Incrby key amount -> [unwords ["INCRBY", key, Text.fromInt amount]]
    Lrange key lower upper -> [unwords ["LRANGE", key, Text.fromInt lower, Text.fromInt upper]]
    Mget keys -> [unwords ("MGET" : NonEmpty.toList keys)]
    Mset pairs -> [unwords ("MSET" : List.concatMap (\(key, _) -> [key, "*****"]) (NonEmpty.toList pairs))]
    Ping -> ["PING"]
    Rpush key vals -> [unwords ("RPUSH" : key : List.map (\_ -> "*****") vals)]
    Set key _ -> [unwords ["SET", key, "*****"]]
    Setnx key _ -> [unwords ["SETNX", key, "*****"]]
    Pure _ -> []
    Apply f x -> cmds f ++ cmds x
    WithResult _ x -> cmds x

unwords :: [Text] -> Text
unwords = Text.join " "

data Query a where
  Del :: NonEmpty Text -> Query Int
  Exists :: Text -> Query Bool
  Expire :: Text -> Int -> Query ()
  Get :: Text -> Query (Maybe ByteString)
  Getset :: Text -> ByteString -> Query (Maybe ByteString)
  Hdel :: Text -> [Text] -> Query Int
  Hgetall :: Text -> Query [(Text, ByteString)]
  Hget :: Text -> Text -> Query (Maybe ByteString)
  Hmget :: Text -> NonEmpty Text -> Query [Maybe ByteString]
  Hmset :: Text -> NonEmpty (Text, ByteString) -> Query ()
  Hset :: Text -> Text -> ByteString -> Query ()
  Hsetnx :: Text -> Text -> ByteString -> Query Bool
  Incr :: Text -> Query Int
  Incrby :: Text -> Int -> Query Int
  Lrange :: Text -> Int -> Int -> Query [ByteString]
  Mget :: NonEmpty Text -> Query [Maybe ByteString]
  Mset :: NonEmpty (Text, ByteString) -> Query ()
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
map f q = WithResult (f >> Ok) q

map2 :: (a -> b -> c) -> Query a -> Query b -> Query c
map2 f queryA queryB =
  Apply (map f queryA) queryB

map3 :: (a -> b -> c -> d) -> Query a -> Query b -> Query c -> Query d
map3 f queryA queryB queryC =
  Apply (Apply (map f queryA) queryB) queryC

sequence :: List (Query a) -> Query (List a)
sequence =
  List.foldr (map2 (:)) (Pure [])

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
    Exists key -> Exists (prefix ++ key)
    Ping -> Ping
    Get key -> Get (prefix ++ key)
    Set key value -> Set (prefix ++ key) value
    Setnx key value -> Setnx (prefix ++ key) value
    Getset key value -> Getset (prefix ++ key) value
    Mget keys -> Mget (NonEmpty.map (\k -> prefix ++ k) keys)
    Mset assocs -> Mset (NonEmpty.map (\(k, v) -> (prefix ++ k, v)) assocs)
    Del keys -> Del (NonEmpty.map (prefix ++) keys)
    Hgetall key -> Hgetall (prefix ++ key)
    Hmget key fields -> Hmget (prefix ++ key) fields
    Hget key field -> Hget (prefix ++ key) field
    Hset key field val -> Hset (prefix ++ key) field val
    Hsetnx key field val -> Hsetnx (prefix ++ key) field val
    Hmset key vals -> Hmset (prefix ++ key) vals
    Hdel key fields -> Hdel (prefix ++ key) fields
    Incr key -> Incr (prefix ++ key)
    Incrby key amount -> Incrby (prefix ++ key) amount
    Expire key secs -> Expire (prefix ++ key) secs
    Lrange key lower upper -> Lrange (prefix ++ key) lower upper
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
          |> List.map (\key -> Expire key secs)
          |> sequence
          |> map2 (\res _ -> res) query'
          |> doTransaction handler
   in handler {doQuery = doQuery, doTransaction = doQuery}

keysTouchedByQuery :: Query a -> Set.Set Text
keysTouchedByQuery query' =
  case query' of
    Del keys -> Set.fromList (NonEmpty.toList keys)
    Exists key -> Set.singleton key
    Ping -> Set.empty
    Get key -> Set.singleton key
    Set key _ -> Set.singleton key
    Setnx key _ -> Set.singleton key
    Getset key _ -> Set.singleton key
    Mget keys -> Set.fromList (NonEmpty.toList keys)
    Hdel key _ -> Set.singleton key
    Incr key -> Set.singleton key
    Incrby key _ -> Set.singleton key
    Mset assocs -> Set.fromList (NonEmpty.toList (NonEmpty.map Tuple.first assocs))
    Hgetall key -> Set.singleton key
    Hmget key _ -> Set.singleton key
    Hget key _ -> Set.singleton key
    Hset key _ _ -> Set.singleton key
    Hsetnx key _ _ -> Set.singleton key
    Hmset key _ -> Set.singleton key
    Lrange key _ _ -> Set.singleton key
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
