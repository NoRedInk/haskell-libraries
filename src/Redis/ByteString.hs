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

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.Internal as Internal
import qualified Tuple
import qualified Prelude

-- | Get the value of key. If the key does not exist the special value Nothing
-- is returned. An error is returned if the value stored at key is not a
-- string, because GET only handles string values.
--
-- https://redis.io/commands/get
get :: Text -> Internal.Query (Maybe ByteString)
get key =
  Internal.Get (toB key)

-- | Set key to hold the string value. If key already holds a value, it is
-- overwritten, regardless of its type. Any previous time to live associated
-- with the key is discarded on successful SET operation.
--
-- https://redis.io/commands/set
set :: Text -> ByteString -> Internal.Query ()
set key value =
  Internal.Set (toB key) value

-- | Sets the given keys to their respective values. MSET replaces existing
-- values with new values, just as regular SET. See MSETNX if you don't want to
-- overwrite existing values.
--
-- MSET is atomic, so all given keys are set at once. It is not possible for
-- clients to see that some of the keys were updated while others are
-- unchanged.
--
-- https://redis.io/commands/mset
mset :: Dict.Dict Text ByteString -> Internal.Query ()
mset values =
  values
    |> Dict.toList
    |> List.map (\(k, v) -> (toB k, v))
    |> Internal.Mset

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: Text -> ByteString -> Internal.Query (Maybe ByteString)
getset key value =
  Internal.Getset (toB key) value

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: [Text] -> Internal.Query Int
del keys =
  Internal.Del (map toB keys)

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: List Text -> Internal.Query (Dict.Dict Text ByteString)
mget keys =
  keys
    |> List.map toB
    |> Internal.Mget
    |> map
      ( \values ->
          List.map2 (,) keys values
            |> List.filterMap
              ( \(key, value) ->
                  case value of
                    Nothing -> Nothing
                    Just v -> Just (key, v)
              )
            |> Dict.fromList
      )

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Text -> (Maybe ByteString -> ByteString) -> Internal.Query ByteString
atomicModify key f =
  atomicModifyWithContext key (\x -> (f x, ()))
    |> map Tuple.first

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Text -> (Maybe ByteString -> (ByteString, a)) -> Internal.Query (ByteString, a)
atomicModifyWithContext key f =
  Internal.AtomicModify (toB key) f

-- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
--
-- https://redis.io/commands/hgetall
hgetall :: Text -> Internal.Query [(Text, ByteString)]
hgetall key =
  Internal.Hgetall (toB key)
    |> Internal.WithResult
      ( \results ->
          let textResults =
                results
                  |> List.filterMap
                    ( \(byteKey, v) ->
                        case Data.Text.Encoding.decodeUtf8' byteKey of
                          Prelude.Right textKey -> Just (textKey, v)
                          Prelude.Left _ -> Nothing
                    )
           in if List.length results /= List.length textResults
                then Err <| Internal.LibraryError "key exists but not parsable text"
                else Ok textResults
      )

-- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
--
-- https://redis.io/commands/hset
hset :: Text -> Text -> ByteString -> Internal.Query ()
hset key field val =
  Internal.Hset (toB key) (toB field) val

-- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
--
-- equivalent to modern hset
-- https://redis.io/commands/hmset
hmset :: Text -> [(Text, ByteString)] -> Internal.Query ()
hmset key vals =
  vals
    |> map (\(k, v) -> (toB k, v))
    |> Internal.Hmset (toB key)

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
