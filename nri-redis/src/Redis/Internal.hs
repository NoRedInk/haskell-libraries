{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- For the RedisResult Text instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Redis.Internal
  ( Error (..),
    Handler,
    Handler' (..),
    HandlerAutoExtendExpire,
    HasAutoExtendExpire (..),
    Query (..),
    Database.Redis.Cursor,
    Database.Redis.cursor0,
    cmds,
    map,
    map2,
    map3,
    sequence,
    query,
    transaction,
    eval,
    foldWithScan,
    -- internal tools
    traceQuery,
    maybesToDict,
    keysTouchedByQuery,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Dict
import qualified GHC.Stack as Stack
import qualified List
import qualified Log.RedisCommands as RedisCommands
import NriPrelude hiding (map, map2, map3)
import qualified Platform
import qualified Redis.Script as Script
import qualified Redis.Settings as Settings
import qualified Set
import qualified Text
import qualified Tuple
import qualified Prelude

-- | Redis Errors, scoped by where they originate.
data Error
  = RedisError Text
  | ConnectionLost
  | DecodingError Text
  | DecodingFieldError Text
  | LibraryError Text
  | TransactionAborted
  | TimeoutError
  | KeyExceedsMaxSize Text Int

instance Aeson.ToJSON Error where
  toJSON err = Aeson.toJSON (errorForHumans err)

instance Show Error where
  show = errorForHumans >> Text.toList

errorForHumans :: Error -> Text
errorForHumans topError =
  case topError of
    RedisError err -> "Redis error: " ++ err
    ConnectionLost -> "Connection Lost"
    LibraryError err -> "Library error when executing (probably due to a bug in the library): " ++ err
    DecodingError err -> "Could not decode value in key: " ++ err
    DecodingFieldError err -> "Could not decode field of hash: " ++ err
    TransactionAborted -> "Transaction aborted."
    TimeoutError -> "Redis query took too long."
    KeyExceedsMaxSize key maxKeySize -> "Redis key (" ++ key ++ ") exceeded max size (" ++ Text.fromInt maxKeySize ++ ")."

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
    Hdel key fields -> [unwords ("HDEL" : key : NonEmpty.toList fields)]
    Hgetall key -> [unwords ["HGETALL", key]]
    Hget key field -> [unwords ["HGET", key, field]]
    Hkeys key -> [unwords ["HKEY", key]]
    Hmget key fields -> [unwords ("HMGET" : key : NonEmpty.toList fields)]
    Hmset key pairs ->
      [unwords ("HMSET" : key : List.concatMap (\(field, _) -> [field, "*****"]) (NonEmpty.toList pairs))]
    Hset key field _ -> [unwords ["HSET", key, field, "*****"]]
    Hsetnx key field _ -> [unwords ["HSETNX", key, field, "*****"]]
    Incr key -> [unwords ["INCR", key]]
    Incrby key amount -> [unwords ["INCRBY", key, Text.fromInt amount]]
    Lrange key lower upper -> [unwords ["LRANGE", key, Text.fromInt lower, Text.fromInt upper]]
    Mget keys -> [unwords ("MGET" : NonEmpty.toList keys)]
    Mset pairs -> [unwords ("MSET" : List.concatMap (\(key, _) -> [key, "*****"]) (NonEmpty.toList pairs))]
    Ping -> ["PING"]
    Rpush key vals -> [unwords ("RPUSH" : key : List.map (\_ -> "*****") (NonEmpty.toList vals))]
    Scan cursor maybeMatch maybeCount -> [scanCmd cursor maybeMatch maybeCount]
    Set key _ -> [unwords ["SET", key, "*****"]]
    Setex key seconds _ -> [unwords ["SETEX", key, Text.fromInt seconds, "*****"]]
    Setnx key _ -> [unwords ["SETNX", key, "*****"]]
    Sadd key vals -> [unwords ("SADD" : key : List.map (\_ -> "*****") (NonEmpty.toList vals))]
    Scard key -> [unwords ["SCARD", key]]
    Srem key vals -> [unwords ("SREM" : key : List.map (\_ -> "*****") (NonEmpty.toList vals))]
    Sismember key _ -> [unwords ["SISMEMBER", key , "*****"]]
    Smembers key -> [unwords ["SMEMBERS", key]]
    Zadd key vals -> [unwords ("ZADD" : key : List.concatMap (\(_, val) -> ["*****", Text.fromFloat val]) (Dict.toList vals))]
    Zrange key start stop -> [unwords ["ZRANGE", key, Text.fromInt start, Text.fromInt stop]]
    ZrangeByScoreWithScores key start stop -> [unwords ["ZRANGE", key, "BYSCORE", Text.fromFloat start, Text.fromFloat stop, "WITHSCORES"]]
    Zrank key _ -> [unwords ["ZRANK", key, "*****"]]
    Zrevrank key _ -> [unwords ["ZREVRANK", key, "*****"]]
    Pure _ -> []
    Apply f x -> cmds f ++ cmds x
    WithResult _ x -> cmds x
  where
    scanCmd :: Database.Redis.Cursor -> Maybe Text -> Maybe Int -> Text
    scanCmd cursor maybeMatch maybeCount =
      let cursorWord =
            Text.fromList (Prelude.show cursor)
          matchWords =
            case maybeMatch of
              Nothing -> []
              Just keyPattern -> ["MATCH", keyPattern]
          countWords =
            case maybeCount of
              Nothing -> []
              Just c -> ["COUNT", Text.fromInt c]
       in unwords ("SCAN" : cursorWord : matchWords ++ countWords)

unwords :: [Text] -> Text
unwords = Text.join " "

-- | A Redis query
data Query a where
  Del :: NonEmpty Text -> Query Int
  Exists :: Text -> Query Bool
  Expire :: Text -> Int -> Query ()
  Get :: Text -> Query (Maybe ByteString)
  Getset :: Text -> ByteString -> Query (Maybe ByteString)
  Hdel :: Text -> NonEmpty Text -> Query Int
  Hgetall :: Text -> Query [(Text, ByteString)]
  Hget :: Text -> Text -> Query (Maybe ByteString)
  Hkeys :: Text -> Query [Text]
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
  Rpush :: Text -> NonEmpty ByteString -> Query Int
  Scan :: Database.Redis.Cursor -> Maybe Text -> Maybe Int -> Query (Database.Redis.Cursor, [Text])
  Set :: Text -> ByteString -> Query ()
  Setex :: Text -> Int -> ByteString -> Query ()
  Setnx :: Text -> ByteString -> Query Bool
  Sadd :: Text -> NonEmpty ByteString -> Query Int
  Scard :: Text -> Query Int
  Srem :: Text -> NonEmpty ByteString -> Query Int
  Sismember :: Text -> ByteString -> Query Bool
  Smembers :: Text -> Query (List ByteString)
  Zadd :: Text -> Dict.Dict ByteString Float -> Query Int
  Zrange :: Text -> Int -> Int -> Query [ByteString]
  ZrangeByScoreWithScores :: Text -> Float -> Float -> Query [(ByteString, Float)]
  Zrank :: Text -> ByteString -> Query (Maybe Int)
  Zrevrank :: Text -> ByteString -> Query (Maybe Int)
  -- The constructors below are not Redis-related, but support using functions
  -- like `map` and `map2` on queries.
  Pure :: a -> Query a
  Apply :: Query (a -> b) -> Query a -> Query b
  WithResult :: (a -> Result Error b) -> Query a -> Query b

instance Prelude.Functor Query where
  fmap = map

instance Prelude.Show (Query a) where
  show = Text.toList << Text.join "<|" << cmds

-- | Used to map the type of a query to another type
-- useful in combination with 'transaction'
map :: (a -> b) -> Query a -> Query b
map f q = WithResult (f >> Ok) q

-- | Used to combine two queries
-- Useful to combine two queries.
-- @
-- Redis.map2
--   (Maybe.map2 (,))
--   (Redis.get api1 key)
--   (Redis.get api2 key)
--   |> Redis.query redis
-- @
map2 :: (a -> b -> c) -> Query a -> Query b -> Query c
map2 f queryA queryB =
  Apply (map f queryA) queryB

-- | Used to combine three queries
-- Useful to combine three queries.
map3 :: (a -> b -> c -> d) -> Query a -> Query b -> Query c -> Query d
map3 f queryA queryB queryC =
  Apply (Apply (map f queryA) queryB) queryC

-- | Used to run a series of queries in sequence.
-- Useful to run a list of queries in sequence.
-- @
-- queries
--   |> Redis.sequence
--   |> Redis.query redis
-- @
sequence :: List (Query a) -> Query (List a)
sequence =
  List.foldr (map2 (:)) (Pure [])

-- | We use this to parametrize the handler. It specifies if the handler has
-- the auto extend expire feater enabled or not.
data HasAutoExtendExpire = NoAutoExtendExpire | AutoExtendExpire

-- | The redis handler allows applications to run scoped IO
-- A handler that can only be parametrized by a value of this kind.
-- Meaning that we use the values of the type parameter at a type level.
data Handler' (x :: HasAutoExtendExpire) = Handler'
  { doQuery :: Stack.HasCallStack => forall a. Query a -> Task Error a,
    doTransaction :: Stack.HasCallStack => forall a. Query a -> Task Error a,
    doEval :: Stack.HasCallStack => forall a. Database.Redis.RedisResult a => Script.Script a -> Task Error a,
    namespace :: Text,
    maxKeySize :: Settings.MaxKeySize
  }

-- | This is a type alias of a handler parametrized by a value that indicates
-- that the auto extend feature is disabled.
-- Note: The tick in front of NoAutoExtendExpire is not necessary, but good
-- practice to indicate that we are lifting a value to the type level.
type Handler = Handler' 'NoAutoExtendExpire

-- | This is a type alias of a handler parametrized by a value that indicates
-- that the auto extend feature is enabled.
-- Note: The tick in front of AutoExtendExpire is not necessary, but good
-- practice to indicate that we are lifting a value to the type level.
type HandlerAutoExtendExpire = Handler' 'AutoExtendExpire

-- | Run a 'Query'.
-- Note: A 'Query' in this library can consist of one or more queries in sequence.
-- if a 'Query' contains multiple queries, it may make more sense, if possible
-- to run them using 'transaction'
query :: Stack.HasCallStack => Handler' x -> Query a -> Task Error a
query handler query' =
  namespaceQuery (namespace handler ++ ":") query'
    |> Task.andThen (ensureMaxKeySize handler)
    |> Task.andThen (Stack.withFrozenCallStack (doQuery handler))

-- | Run a redis Query in a transaction. If the query contains several Redis
-- commands they're all executed together, and Redis will guarantee other
-- requests won't be able change values in between.
--
-- In redis terms, this is wrappping the 'Query' in `MULTI` and `EXEC
-- see redis transaction semantics here: https://redis.io/topics/transactions
transaction :: Stack.HasCallStack => Handler' x -> Query a -> Task Error a
transaction handler query' =
  namespaceQuery (namespace handler ++ ":") query'
    |> Task.andThen (ensureMaxKeySize handler)
    |> Task.andThen (Stack.withFrozenCallStack (doTransaction handler))

eval :: (Stack.HasCallStack, Database.Redis.RedisResult a) => Handler' x -> Script.Script a -> Task Error a
eval handler script =
  Script.mapKeys (\key -> Task.succeed (namespace handler ++ ":" ++ key)) script
    |> Task.andThen (Stack.withFrozenCallStack (doEval handler))

namespaceQuery :: Text -> Query a -> Task err (Query a)
namespaceQuery prefix query' =
  mapKeys (\key -> Task.succeed (prefix ++ key)) query'
    |> Task.map (mapReturnedKeys (Text.dropLeft (Text.length prefix)))

mapKeys :: (Text -> Task err Text) -> Query a -> Task err (Query a)
mapKeys fn query' =
  case query' of
    Exists key -> Task.map Exists (fn key)
    Ping -> Task.succeed Ping
    Get key -> Task.map Get (fn key)
    Set key value -> Task.map (\newKey -> Set newKey value) (fn key)
    Setex key seconds value -> Task.map (\newKey -> Setex newKey seconds value) (fn key)
    Setnx key value -> Task.map (\newKey -> Setnx newKey value) (fn key)
    Getset key value -> Task.map (\newKey -> Getset newKey value) (fn key)
    Mget keys -> Task.map Mget (Prelude.traverse (\k -> fn k) keys)
    Mset assocs -> Task.map Mset (Prelude.traverse (\(k, v) -> Task.map (\newKey -> (newKey, v)) (fn k)) assocs)
    Del keys -> Task.map Del (Prelude.traverse (fn) keys)
    Hgetall key -> Task.map Hgetall (fn key)
    Hkeys key -> Task.map Hkeys (fn key)
    Hmget key fields -> Task.map (\newKey -> Hmget newKey fields) (fn key)
    Hget key field -> Task.map (\newKey -> Hget newKey field) (fn key)
    Hset key field val -> Task.map (\newKey -> Hset newKey field val) (fn key)
    Hsetnx key field val -> Task.map (\newKey -> Hsetnx newKey field val) (fn key)
    Hmset key vals -> Task.map (\newKey -> Hmset newKey vals) (fn key)
    Hdel key fields -> Task.map (\newKey -> Hdel newKey fields) (fn key)
    Incr key -> Task.map Incr (fn key)
    Incrby key amount -> Task.map (\newKey -> Incrby newKey amount) (fn key)
    Expire key secs -> Task.map (\newKey -> Expire newKey secs) (fn key)
    Lrange key lower upper -> Task.map (\newKey -> Lrange newKey lower upper) (fn key)
    Rpush key vals -> Task.map (\newKey -> Rpush newKey vals) (fn key)
    Scan cursor maybeMatch maybeCount ->
      case maybeMatch of
        Just match -> Task.map (\newMatch -> Scan cursor (Just newMatch) maybeCount) (fn match)
        Nothing -> Task.succeed (Scan cursor Nothing maybeCount)
    Sadd key vals -> Task.map (\newKey -> Sadd newKey vals) (fn key)
    Scard key -> Task.map Scard (fn key)
    Srem key vals -> Task.map (\newKey -> Srem newKey vals) (fn key)
    Sismember key val -> Task.map (\newKey -> Sismember newKey val) (fn key)
    Smembers key -> Task.map Smembers (fn key)
    Zadd key vals -> Task.map (\newKey -> Zadd newKey vals) (fn key)
    Zrange key start stop -> Task.map (\newKey -> Zrange newKey start stop) (fn key)
    ZrangeByScoreWithScores key start stop -> Task.map (\newKey -> ZrangeByScoreWithScores newKey start stop) (fn key)
    Zrank key member -> Task.map (\newKey -> Zrank newKey member) (fn key)
    Zrevrank key member -> Task.map (\newKey -> Zrevrank newKey member) (fn key)
    Pure x -> Task.succeed (Pure x)
    Apply f x -> Task.map2 Apply (mapKeys fn f) (mapKeys fn x)
    WithResult f q -> Task.map (WithResult f) (mapKeys fn q)

mapReturnedKeys :: (Text -> Text) -> Query a -> Query a
mapReturnedKeys fn query' =
  case query' of
    Exists key -> Exists key
    Ping -> Ping
    Get key -> Get key
    Set key value -> Set key value
    Setex key seconds value -> Setex key seconds value
    Setnx key value -> Setnx key value
    Getset key value -> Getset key value
    Mget keys -> Mget keys
    Mset assocs -> Mset assocs
    Del keys -> Del keys
    Hgetall key -> Hgetall key
    Hkeys key -> Hkeys key
    Hmget key fields -> Hmget key fields
    Hget key field -> Hget key field
    Hset key field val -> Hset key field val
    Hsetnx key field val -> Hsetnx key field val
    Hmset key vals -> Hmset key vals
    Hdel key fields -> Hdel key fields
    Incr key -> Incr key
    Incrby key amount -> Incrby key amount
    Expire key secs -> Expire key secs
    Lrange key lower upper -> Lrange key lower upper
    Rpush key vals -> Rpush key vals
    Scan cursor maybeMatch maybeCount ->
      Scan cursor maybeMatch maybeCount
        |> map (\(nextCursor, keys) -> (nextCursor, List.map fn keys))
    Sadd key vals -> Sadd key vals
    Scard key -> Scard key
    Srem key vals -> Srem key vals
    Sismember key val -> Sismember key val
    Smembers key -> Smembers key
    Zadd key vals -> Zadd key vals
    Zrange key start stop -> Zrange key start stop
    ZrangeByScoreWithScores key start stop -> ZrangeByScoreWithScores key start stop
    Zrank key member -> Zrank key member
    Zrevrank key member -> Zrevrank key member
    Pure x -> Pure x
    Apply f x -> Apply (mapReturnedKeys fn f) (mapReturnedKeys fn x)
    WithResult f q -> (WithResult f) (mapReturnedKeys fn q)

ensureMaxKeySize :: Handler' x -> Query a -> Task Error (Query a)
ensureMaxKeySize handler query' =
  case maxKeySize handler of
    Settings.NoMaxKeySize -> Task.succeed query'
    Settings.MaxKeySize maxKeySize ->
      mapKeys (checkMaxKeySize maxKeySize) query'

checkMaxKeySize :: Int -> Text -> Task Error Text
checkMaxKeySize maxKeySize key =
  if Text.length key <= maxKeySize
    then Task.succeed key
    else Task.fail (KeyExceedsMaxSize key maxKeySize)

keysTouchedByQuery :: Query a -> Set.Set Text
keysTouchedByQuery query' =
  case query' of
    Apply f x -> Set.union (keysTouchedByQuery f) (keysTouchedByQuery x)
    Del keys -> Set.fromList (NonEmpty.toList keys)
    Exists key -> Set.singleton key
    -- We use this function to collect keys we need to expire. If the user is
    -- explicitly setting an expiry we don't want to overwrite that.
    Expire _key _ -> Set.empty
    Get key -> Set.singleton key
    Getset key _ -> Set.singleton key
    Hdel key _ -> Set.singleton key
    Hget key _ -> Set.singleton key
    Hgetall key -> Set.singleton key
    Hkeys key -> Set.singleton key
    Hmget key _ -> Set.singleton key
    Hmset key _ -> Set.singleton key
    Hset key _ _ -> Set.singleton key
    Hsetnx key _ _ -> Set.singleton key
    Incr key -> Set.singleton key
    Incrby key _ -> Set.singleton key
    Lrange key _ _ -> Set.singleton key
    Mget keys -> Set.fromList (NonEmpty.toList keys)
    Mset assocs -> Set.fromList (NonEmpty.toList (NonEmpty.map Tuple.first assocs))
    Ping -> Set.empty
    Pure _ -> Set.empty
    Rpush key _ -> Set.singleton key
    Scan {} -> Set.empty
    Set key _ -> Set.singleton key
    Setex key _ _ -> Set.singleton key
    Setnx key _ -> Set.singleton key
    Sadd key _ -> Set.singleton key
    Scard key -> Set.singleton key
    Srem key _ -> Set.singleton key
    Sismember key _ -> Set.singleton key
    Smembers key -> Set.singleton key
    Zadd key _ -> Set.singleton key
    Zrange key _ _ -> Set.singleton key
    ZrangeByScoreWithScores key _ _ -> Set.singleton key
    Zrank key _ -> Set.singleton key
    Zrevrank key _ -> Set.singleton key
    WithResult _ q -> keysTouchedByQuery q

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

traceQuery :: Stack.HasCallStack => [Text] -> Text -> Maybe Int -> Task e a -> Task e a
traceQuery commands host port task =
  let info =
        RedisCommands.emptyDetails
          { RedisCommands.commands = commands,
            RedisCommands.host = Just host,
            RedisCommands.port = port
          }
   in Stack.withFrozenCallStack
        Platform.tracingSpan
        "Redis Query"
        ( Platform.finally
            task
            ( do
                Platform.setTracingSpanDetails info
                Platform.setTracingSpanSummary
                  ( case commands of
                      [] -> ""
                      [cmd] -> cmd
                      cmd : _ -> cmd ++ " (+ more)"
                  )
            )
        )

-- | Use SCAN command to find keys matching a pattern, and "fold" over them in batches, producing a result value.
-- keyMatchPattern       A glob-like pattern to match keys, see https://redis.io/commands/keys/
-- approxCountPerBatch   A hint for the batch size you want to process at once. Only approximate.
-- processKeyBatch       Function to process one batch of keys (provided as plain Text without namespace prefix)
-- initAccumulator       Initial value for the fold accumulator
foldWithScan :: Handler' x -> Maybe Text -> Maybe Int -> ([Text] -> a -> Task Error a) -> a -> Task Error a
foldWithScan handler keyMatchPattern approxCountPerBatch processKeyBatch initAccumulator =
  let go accumulator cursor = do
        (nextCursor, keyBatch) <-
          Scan cursor keyMatchPattern approxCountPerBatch
            |> query handler
        nextAccumulator <-
          processKeyBatch keyBatch accumulator
        if nextCursor == Database.Redis.cursor0
          then Task.succeed nextAccumulator
          else go nextAccumulator nextCursor
   in go initAccumulator Database.Redis.cursor0

--------------------------------------
-- Orphaned instances for RedisResult
--------------------------------------
instance Database.Redis.RedisResult Text where
  decode r = do
    decodedBs <- Database.Redis.decode r
    Prelude.pure <| Data.Text.Encoding.decodeUtf8 decodedBs

instance Database.Redis.RedisResult Int where
  decode r = do
    (decodedInteger :: Prelude.Integer) <- Database.Redis.decode r
    Prelude.pure <| Prelude.fromIntegral decodedInteger

instance Database.Redis.RedisResult () where
  decode r = do
    (reply :: Database.Redis.Reply) <- Database.Redis.decode r
    case reply of
      Database.Redis.Bulk Nothing -> Prelude.pure ()
      other -> Prelude.Left other
