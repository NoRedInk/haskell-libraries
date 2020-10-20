-- | Functions for storing `Text` values in Redis and reading them back.
--
-- When any of the functions in this module read a value from Redis that aren't
-- UTF8 encoded they they will return an error. This should never happen for
-- values written by the functions in this module, but only when reading data
-- inserted into Redis by someone else.
module Redis.Text
  ( -- * Redis commands
    del,
    expire,
    get,
    getset,
    hdel,
    hgetall,
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

import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Dict
import NriPrelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Result
import qualified Task
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
    |> Internal.WithResult toTIfFound

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
    |> Internal.WithResult toTIfFound

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
    |> Internal.WithResult (Prelude.traverse toT)

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
hgetall :: Text -> Internal.Query (Dict.Dict Text Text)
hgetall key =
  Redis.ByteString.hgetall key
    |> Internal.WithResult (Prelude.traverse toT)

-- | Returns the values associated with the specified fields in the hash stored at key.--
--
-- equivalent to modern hset
-- https://redis.io/commands/hmget
hmget :: Text -> [Text] -> Internal.Query [Maybe Text]
hmget key fields =
  Redis.ByteString.hmget key fields
    |> Internal.WithResult (Prelude.traverse (Prelude.traverse toT))

-- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
--
-- equivalent to modern hset
-- https://redis.io/commands/hmset
hmset :: Text -> Dict.Dict Text Text -> Internal.Query ()
hmset key vals =
  vals
    |> Dict.map (\_k v -> toB v)
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
atomicModify :: Internal.Handler -> Text -> (Maybe Text -> Text) -> Task Internal.Error Text
atomicModify handler key f =
  atomicModifyWithContext handler key (\x -> (f x, ()))
    |> map Tuple.first

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Internal.Handler -> Text -> (Maybe Text -> (Text, a)) -> Task Internal.Error (Text, a)
atomicModifyWithContext handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        let context = toTIfFound maybeByteString
         in ( case context of
                Ok maybeText -> maybeText
                Err _ -> Nothing
            )
              |> f
              |> (\r@(res, _ctx) -> (toB res, (r, context)))
    )
    |> andThen
      ( \(_, (res, context)) ->
          case context of
            Err _ -> Task.fail unparsableKeyError
            Ok _ -> Task.succeed res
      )

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

toTIfFound :: Maybe Data.ByteString.ByteString -> Result Internal.Error (Maybe Text)
toTIfFound maybeBytestring =
  case maybeBytestring of
    Nothing -> Ok Nothing
    Just bytestring -> Result.map Just (toT bytestring)

toT :: Data.ByteString.ByteString -> Result Internal.Error Text
toT bs =
  case Data.Text.Encoding.decodeUtf8' bs of
    Prelude.Right t -> Ok t
    Prelude.Left _ -> Err unparsableKeyError

unparsableKeyError :: Internal.Error
unparsableKeyError = Internal.LibraryError "key exists but not parsable text"
