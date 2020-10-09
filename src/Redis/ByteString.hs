-- | This module exports low-level bindings to Redis that read and write
-- `ByteString` values. We need this to integrate Redis with some libraries. In
-- most cases you'll likely want to use either `Redis.Json` or `Redis.Text`.
module Redis.ByteString
  ( -- * Redis commands
    get,
    set,
    getset,
    mget,
    mset,
    del,
    hset,
    hgetall,

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,

    -- * Helper types
    Internal.NamespacedHandler,
    Internal.Error,
  )
where

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple

-- | Get the value of key. If the key does not exist the special value Nothing
-- is returned. An error is returned if the value stored at key is not a
-- string, because GET only handles string values.
--
-- https://redis.io/commands/get
get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe ByteString)
get handler key =
  Internal.get handler (toB key)

-- | Set key to hold the string value. If key already holds a value, it is
-- overwritten, regardless of its type. Any previous time to live associated
-- with the key is discarded on successful SET operation.
--
-- https://redis.io/commands/set
set :: Internal.NamespacedHandler -> Text -> ByteString -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) value

-- | Sets the given keys to their respective values. MSET replaces existing
-- values with new values, just as regular SET. See MSETNX if you don't want to
-- overwrite existing values.
--
-- MSET is atomic, so all given keys are set at once. It is not possible for
-- clients to see that some of the keys were updated while others are
-- unchanged.
--
-- https://redis.io/commands/mset
mset :: Internal.NamespacedHandler -> Dict.Dict Text ByteString -> Task Internal.Error ()
mset handler values =
  Internal.mset
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, v))
    )

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: Internal.NamespacedHandler -> Text -> ByteString -> Task Internal.Error (Maybe ByteString)
getset handler key value =
  Internal.getset handler (toB key) value

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
del handler keys =
  Internal.del handler (map toB keys)

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text ByteString)
mget handler keys =
  keys
    |> List.map toB
    |> Internal.mget handler
    |> andThen
      ( \values ->
          if List.length keys == List.length values
            then
              List.map2 (,) keys values
                |> List.filterMap
                  ( \(key, value) ->
                      case value of
                        Nothing -> Nothing
                        Just v -> Just (key, v)
                  )
                |> Dict.fromList
                |> Task.succeed
            else
              Task.fail
                ( Internal.LibraryError
                    "We got a mismatch in the size of keys and values when post-processing the \
                    \results of an mget command. Redis guarantees this shouldn't happen, so a \
                    \mismatch here means that we did something wrong and continuing could mean \
                    \building an incorrect mapping."
                )
      )

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Internal.NamespacedHandler -> Text -> (Maybe ByteString -> ByteString) -> Task Internal.Error ByteString
atomicModify handler key f =
  atomicModifyWithContext handler key (\x -> (f x, ()))
    |> map Tuple.first

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Internal.NamespacedHandler -> Text -> (Maybe ByteString -> (ByteString, a)) -> Task Internal.Error (ByteString, a)
atomicModifyWithContext handler key f =
  Internal.atomicModify handler (toB key) f

-- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
--
-- https://redis.io/commands/hgetall
hgetall :: Internal.NamespacedHandler -> Text -> Task Internal.Error [(ByteString, ByteString)]
hgetall handler key =
  Internal.hgetall handler (toB key)

-- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
--
-- https://redis.io/commands/hset
hset :: Internal.NamespacedHandler -> Text -> ByteString -> ByteString -> Task Internal.Error ()
hset handler key field val =
  Internal.hset handler (toB key) field val

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
