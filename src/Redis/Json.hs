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

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,

    -- * Helper types
    Internal.NamespacedHandler,
    Internal.Error,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Dict
import Nri.Prelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple

-- | Get the value of key. If the key does not exist the special value Nothing
-- is returned. An error is returned if the value stored at key is not a
-- string, because GET only handles string values.
--
-- https://redis.io/commands/get
get :: Aeson.FromJSON a => Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe a)
get handler key =
  Redis.ByteString.get handler key
    |> Task.map (andThen Aeson.decodeStrict')

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: Aeson.FromJSON a => Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text a)
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
set :: Aeson.ToJSON a => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error ()
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
mset :: Aeson.ToJSON a => Internal.NamespacedHandler -> Dict.Dict Text a -> Task Internal.Error ()
mset handler values =
  Redis.ByteString.mset
    handler
    (Dict.map (\_key val -> encodeStrict val) values)

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
del = Redis.ByteString.del

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error (Maybe a)
getset handler key value =
  Redis.ByteString.getset handler key (encodeStrict value)
    |> map (andThen Aeson.decodeStrict')

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> a) -> Task Internal.Error a
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
atomicModifyWithContext :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
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
