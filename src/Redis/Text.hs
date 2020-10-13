-- | Functions for storing `Text` values in Redis and reading them back.
--
-- When any of the functions in this module read a value from Redis that aren't
-- UTF8 encoded they will act as if the value did not exist. This should never
-- happen for values written by the functions in this module, but only when
-- reading data inserted into Redis by someone else.
module Redis.Text
  ( -- * Redis commands
    get,
    set,
    getset,
    mget,
    mset,
    del,
    hset,
    hgetall,
    hmset,

    -- * Running queries
    Internal.Query,
    Internal.query,
    Internal.Handler,
    Internal.Error,

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,
  )
where

import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Tuple
import qualified Prelude

-- | Get the value of key. If the key does not exist the special value Nothing
-- is returned. An error is returned if the value stored at key is not a
-- string, because GET only handles string values.
--
-- https://redis.io/commands/get
get :: Text -> Internal.Query (Maybe Text)
get key =
  Redis.ByteString.get key
    |> Internal.WithResult
      ( \maybeBytestring ->
          case maybeBytestring of
            Nothing -> Ok Nothing
            Just bytestring ->
              case toT bytestring of
                Nothing -> Err <| Internal.LibraryError "get failed, key exists but not parseable text"
                Just t -> Ok <| Just t
      )

-- | Set key to hold the string value. If key already holds a value, it is
-- overwritten, regardless of its type. Any previous time to live associated
-- with the key is discarded on successful SET operation.
--
-- https://redis.io/commands/set
set :: Text -> Text -> Internal.Query ()
set key value =
  Redis.ByteString.set key (toB value)

-- | Sets the given keys to their respective values. MSET replaces existing
-- values with new values, just as regular SET. See MSETNX if you don't want to
-- overwrite existing values.
--
-- MSET is atomic, so all given keys are set at once. It is not possible for
-- clients to see that some of the keys were updated while others are
-- unchanged.
--
-- https://redis.io/commands/mset
mset :: Dict.Dict Text Text -> Internal.Query ()
mset values =
  Redis.ByteString.mset
    (Dict.map (\_key val -> toB val) values)

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: Text -> Text -> Internal.Query (Maybe Text)
getset key value =
  Redis.ByteString.getset key (toB value)
    |> map (andThen toT)

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: [Text] -> Internal.Query Int
del = Redis.ByteString.del

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: List Text -> Internal.Query (Dict.Dict Text Text)
mget keys =
  Redis.ByteString.mget keys
    |> map
      ( Dict.foldl
          ( \key val dict ->
              case toT val of
                Nothing -> dict
                Just textVal -> Dict.insert key textVal dict
          )
          Dict.empty
      )

-- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
--
-- https://redis.io/commands/hset
hset :: Text -> Text -> Text -> Internal.Query ()
hset key field val =
  Redis.ByteString.hset key field (toB val)

-- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
-- Nothing in the returned value means failed utf8 decoding, not that it doesn't exist
--
-- https://redis.io/commands/hgetall
hgetall :: Text -> Internal.Query [(Maybe Text, Maybe Text)]
hgetall key =
  Redis.ByteString.hgetall key
    |> map (map (\(k, v) -> (toT k, toT v)))

-- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
--
-- equivalent to modern hset
-- https://redis.io/commands/hmset
hmset :: Text -> [(Text, Text)] -> Internal.Query ()
hmset key vals =
  vals
    |> List.map (\(k, v) -> (k, toB v))
    |> Redis.ByteString.hmset key

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Text -> (Maybe Text -> Text) -> Internal.Query Text
atomicModify key f =
  Redis.ByteString.atomicModifyWithContext
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen toT
          |> f
          |> (\res -> (toB res, res))
    )
    |> map Tuple.second

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Text -> (Maybe Text -> (Text, a)) -> Internal.Query (Text, a)
atomicModifyWithContext key f =
  Redis.ByteString.atomicModifyWithContext
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen toT
          |> f
          |> (\res -> (toB (Tuple.first res), res))
    )
    |> map Tuple.second

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

toT :: Data.ByteString.ByteString -> Maybe Text
toT bs =
  case Data.Text.Encoding.decodeUtf8' bs of
    Prelude.Right t -> Just t
    Prelude.Left _ -> Nothing
