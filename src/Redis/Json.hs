-- | A module that automatically converts types to JSON prior to storing them in
-- Redis, and parses them back from JSON when reading.
--
-- When data is found in Redis but cannot be decoded these functions will return
-- an error.
module Redis.Json
  ( -- * Redis commands
    del,
    expire,
    get,
    getset,
    hdel,
    hgetall,
    hget,
    hmget,
    hmset,
    hset,
    mget,
    mset,
    ping,
    set,
    watch,

    -- * Running queries
    Internal.Query,
    Internal.query,
    Internal.transaction,
    Internal.Handler,
    Internal.Error,

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Dict
import NriPrelude
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
get :: Aeson.FromJSON a => Text -> Internal.Query (Maybe a)
get key =
  Redis.ByteString.get key
    |> Internal.WithResult decodeIfFound

-- | Returns the values of all specified keys. For every key that does not hold
-- a string value or does not exist, no value is returned. Because of this, the
-- operation never fails.
--
-- https://redis.io/commands/mget
mget :: Aeson.FromJSON a => List Text -> Internal.Query (Dict.Dict Text a)
mget keys =
  Redis.ByteString.mget keys
    |> Internal.WithResult (Prelude.traverse decodeResult)

-- | Set key to hold the string value. If key already holds a value, it is
-- overwritten, regardless of its type. Any previous time to live associated
-- with the key is discarded on successful SET operation.
--
-- https://redis.io/commands/set
set :: Aeson.ToJSON a => Text -> a -> Internal.Query ()
set key value =
  Redis.ByteString.set key (encodeStrict value)

-- | Sets the given keys to their respective values. MSET replaces existing
-- values with new values, just as regular SET. See MSETNX if you don't want to
-- overwrite existing values.
--
-- MSET is atomic, so all given keys are set at once. It is not possible for
-- clients to see that some of the keys were updated while others are
-- unchanged.
--
-- https://redis.io/commands/mset
mset :: Aeson.ToJSON a => Dict.Dict Text a -> Internal.Query ()
mset values =
  Redis.ByteString.mset
    (Dict.map (\_key val -> encodeStrict val) values)

-- | Removes the specified keys. A key is ignored if it does not exist.
--
-- https://redis.io/commands/del
del :: [Text] -> Internal.Query Int
del = Redis.ByteString.del

-- | Atomically sets key to value and returns the old value stored at key.
-- Returns an error when key exists but does not hold a string value.
--
-- https://redis.io/commands/getset
getset :: (Aeson.FromJSON a, Aeson.ToJSON a) => Text -> a -> Internal.Query (Maybe a)
getset key value =
  Redis.ByteString.getset key (encodeStrict value)
    |> Internal.WithResult decodeIfFound

-- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
--
-- https://redis.io/commands/hset
hset :: (Aeson.ToJSON a) => Text -> Text -> a -> Internal.Query ()
hset key field val =
  Redis.ByteString.hset key field (encodeStrict val)

-- | Get the value of the field of a hash at key. If the key does not exist,
-- or the field in the hash does not exis the special value Nothing is returned
-- An error is returned if the value stored at key is not a
-- hash, because HGET only handles string values.
--
-- https://redis.io/commands/hget
hget :: Aeson.FromJSON a => Text -> Text -> Internal.Query (Maybe a)
hget key field =
  Redis.ByteString.hget key field
    |> Internal.WithResult decodeIfFound

-- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
-- Nothing in the returned value means failed utf8 decoding, not that it doesn't exist
--
-- https://redis.io/commands/hgetall
hgetall :: (Aeson.FromJSON a) => Text -> Internal.Query (Dict.Dict Text a)
hgetall key =
  Redis.ByteString.hgetall key
    |> Internal.WithResult (Prelude.traverse decodeResult)

-- | Returns the values associated with the specified fields in the hash stored at key.--
--
-- equivalent to modern hget
-- https://redis.io/commands/hmget
hmget :: (Aeson.FromJSON a) => Text -> [Text] -> Internal.Query (Dict.Dict Text a)
hmget key fields =
  Redis.ByteString.hmget key fields
    |> Internal.WithResult (Prelude.traverse decodeResult)

-- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
--
-- equivalent to modern hset
-- https://redis.io/commands/hmset
hmset :: (Aeson.ToJSON a) => Text -> Dict.Dict Text a -> Internal.Query ()
hmset key vals =
  vals
    |> Dict.map (\_k v -> encodeStrict v)
    |> Redis.ByteString.hmset key

-- | Set a timeout on key. After the timeout has expired, the key will
-- automatically be deleted. A key with an associated timeout is often said to
-- be volatile in Redis terminology.
--
-- https://redis.io/commands/expire
expire :: Text -> Int -> Internal.Query ()
expire = Redis.ByteString.expire

-- | Removes the specified fields from the hash stored at key. Specified fields
-- that do not exist within this hash are ignored. If key does not exist, it is
-- treated as an empty hash and this command returns 0.
--
-- https://redis.io/commands/hdel
hdel :: Text -> [Text] -> Internal.Query Int
hdel = Redis.ByteString.hdel

-- | Returns PONG if no argument is provided, otherwise return a copy of the
-- argument as a bulk. This command is often used to test if a connection is
-- still alive, or to measure latency.
--
-- https://redis.io/commands/ping
ping :: Internal.Query ()
ping = Redis.ByteString.ping

-- | Marks the given keys to be watched for conditional execution of a
-- transaction.
--
-- This returns a task because it cannot be ran as part of a transaction.
--
-- https://redis.io/commands/watch
watch :: Internal.Handler -> [Text] -> Task Internal.Error ()
watch = Redis.ByteString.watch

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify ::
  (Aeson.FromJSON a, Aeson.ToJSON a) =>
  Internal.Handler ->
  Text ->
  (Maybe a -> a) ->
  Task Internal.Error a
atomicModify handler key f =
  atomicModifyWithContext handler key (\x -> (f x, ()))
    |> map Tuple.first

-- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext ::
  (Aeson.FromJSON a, Aeson.ToJSON a) =>
  Internal.Handler ->
  Text ->
  (Maybe a -> (a, b)) ->
  Task Internal.Error (a, b)
atomicModifyWithContext handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        let context = decodeIfFound maybeByteString
         in ( case context of
                Ok maybeText -> maybeText
                Err _ -> Nothing
            )
              |> f
              |> (\r@(res, _ctx) -> (encodeStrict res, (r, context)))
    )
    |> Task.andThen
      ( \(_, (res, context)) ->
          case context of
            Err _ -> Task.fail unparsableKeyError
            Ok _ -> Task.succeed res
      )

encodeStrict :: Aeson.ToJSON a => a -> Data.ByteString.ByteString
encodeStrict x =
  Aeson.encode x
    |> Data.ByteString.Lazy.toStrict

decodeIfFound :: Aeson.FromJSON a => Maybe Data.ByteString.ByteString -> Result Internal.Error (Maybe a)
decodeIfFound maybeByteString =
  case maybeByteString of
    Nothing -> Ok Nothing
    Just byteString -> decodeResult byteString

decodeResult :: Aeson.FromJSON a => Data.ByteString.ByteString -> Result Internal.Error a
decodeResult byteString =
  case Aeson.decodeStrict' byteString of
    Just decoded -> Ok decoded
    Nothing -> Err unparsableKeyError

unparsableKeyError :: Internal.Error
unparsableKeyError = Internal.LibraryError "key exists but not parsable json"
