-- | A module that automatically converts types to JSON prior to storing them in
-- Redis, and parses them back from JSON when reading.
--
-- Data stored in Redis should be, by definition, transient and disposable.
-- Because of that, we are very lenient in how we handle decoding of data:
-- if we find invalid or out of date data stored in a key, we treat that the
-- same as if the key is empty. This avoids issues with things like changes
-- of JSON representations of data - we just throw the old data away!
module Redis.Json
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

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,

    -- * Helper types
    Internal.Handler,
    Internal.Error,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple
import qualified Prelude

-- | Get the value of key. If the key does not exist the special value Nothing
-- is returned. An error is returned if the value stored at key is not a
-- string, because GET only handles string values.
--
-- https://redis.io/commands/get
get :: Aeson.FromJSON a => Internal.Handler -> Text -> Task Internal.Error (Maybe a)
get handler key =
  Redis.ByteString.get handler key
    |> Task.map (andThen Aeson.decodeStrict')

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: Aeson.FromJSON a => Internal.Handler -> List Text -> Task Internal.Error (Dict.Dict Text a)
mget handler keys =
  Redis.ByteString.mget handler keys
    |> Task.map
      ( Dict.foldr
          ( \key val dict ->
              case Aeson.decodeStrict' val of
                Nothing -> dict
                Just decodedVal -> Dict.insert key decodedVal dict
          )
          Dict.empty
      )

-- | Set key to hold the string value. If key already holds a value, it is
-- overwritten, regardless of its type. Any previous time to live associated
-- with the key is discarded on successful SET operation.
--
-- https://redis.io/commands/set
set :: Aeson.ToJSON a => Internal.Handler -> Text -> a -> Task Internal.Error ()
set handler key value =
  Redis.ByteString.set handler key (encodeStrict value)

-- | Sets the given keys to their respective values. MSET replaces existing
-- values with new values, just as regular SET. See MSETNX if you don't want to
-- overwrite existing values.
--
-- MSET is atomic, so all given keys are set at once. It is not possible for
-- clients to see that some of the keys were updated while others are
-- unchanged.
--
-- https://redis.io/commands/mset
mset :: Aeson.ToJSON a => Internal.Handler -> Dict.Dict Text a -> Task Internal.Error ()
mset handler values =
  Redis.ByteString.mset
    handler
    (Dict.map (\_key val -> encodeStrict val) values)

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: Internal.Handler -> [Text] -> Task Internal.Error Int
del = Redis.ByteString.del

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.Handler -> Text -> a -> Task Internal.Error (Maybe a)
getset handler key value =
  Redis.ByteString.getset handler key (encodeStrict value)
    |> map (andThen Aeson.decodeStrict')

-- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
--
-- https://redis.io/commands/hset
hset :: (Aeson.ToJSON a) => Internal.Handler -> Text -> Text -> a -> Task Internal.Error ()
hset handler key field val =
  Redis.ByteString.hset handler key field (encodeStrict val)

-- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
-- Nothing in the returned value means failed utf8 decoding, not that it doesn't exist
--
-- https://redis.io/commands/hgetall
hgetall :: (Aeson.FromJSON a) => Internal.Handler -> Text -> Task Internal.Error [(Text, a)]
hgetall handler key =
  Redis.ByteString.hgetall handler key
    |> map
      ( List.filterMap
          ( \(k, v) ->
              case (toT k, Aeson.decodeStrict' v) of
                (Just k', Just v') -> Just (k', v')
                _ -> Nothing
          )
      )

-- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
--
-- equivalent to modern hset
-- https://redis.io/commands/hmset
hmset :: (Aeson.ToJSON a) => Internal.Handler -> Text -> [(Text, a)] -> Task Internal.Error ()
hmset handler key vals =
  vals
    |> List.map (\(k, v) -> (k, encodeStrict v))
    |> Redis.ByteString.hmset handler key

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.Handler -> Text -> (Maybe a -> a) -> Task Internal.Error a
atomicModify handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen Aeson.decodeStrict'
          |> f
          |> (\res -> (encodeStrict res, res))
    )
    |> Task.map Tuple.second

-- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.Handler -> Text -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
atomicModifyWithContext handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen Aeson.decodeStrict'
          |> f
          |> (\res -> (encodeStrict (Tuple.first res), res))
    )
    |> Task.map Tuple.second

encodeStrict :: Aeson.ToJSON a => a -> Data.ByteString.ByteString
encodeStrict x =
  Aeson.encode x
    |> Data.ByteString.Lazy.toStrict

toT :: Data.ByteString.ByteString -> Maybe Text
toT bs =
  case Data.Text.Encoding.decodeUtf8' bs of
    Prelude.Right t -> Just t
    Prelude.Left _ -> Nothing
