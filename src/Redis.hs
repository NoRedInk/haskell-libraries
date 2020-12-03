{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis
  ( -- Settings
    Settings.Settings (..),
    Settings.decoder,
    -- Internal
    Internal.Error (..),
    Internal.Handler,
    Internal.Query,
    Internal.transaction,
    Internal.query,
    Internal.map,
    -- Real
    Real.Info (..),
    Real.handler,
    readiness,
    watch,

    -- * Creating api access functions
    makeApi,
    Api,
    del,
    expire,
    get,
    getset,
    mget,
    mset,
    ping,
    rpush,
    set,
    setnx,
    makeHashApi,
    HashApi,
    hdel,
    hget,
    hgetall,
    hmget,
    hmset,
    hset,
    hsetnx,
    Decoder,
    Encoder,
    jsonDecoder,
    jsonEncoder,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Dict
import qualified Health
import qualified List
import NriPrelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Result
import qualified Task
import qualified Prelude

-- |
-- Check that we are ready to be take traffic.
readiness :: Internal.Handler -> Health.Check
readiness handler =
  Health.mkCheck "redis" <| do
    log <- Platform.silentHandler
    Internal.Ping
      |> Internal.query handler
      |> Task.map (\_ -> Health.Good)
      |> Task.onError (\err -> Task.succeed (Health.Bad (Internal.errorForHumans err)))
      |> Task.perform log

type Encoder a = a -> ByteString

type Decoder a = ByteString -> Result Internal.Error a

data Api key a
  = Api
      { -- | Removes the specified keys. A key is ignored if it does not exist.
        --
        -- https://redis.io/commands/del
        del :: List.List key -> Internal.Query Int,
        -- | Set a timeout on key. After the timeout has expired, the key will
        -- automatically be deleted. A key with an associated timeout is often said to
        -- be volatile in Redis terminology.
        --
        -- https://redis.io/commands/expire
        expire :: key -> Int -> Internal.Query (),
        -- | Get the value of key. If the key does not exist the special value Nothing
        -- is returned. An error is returned if the value stored at key is not a
        -- string, because GET only handles string values.
        --
        -- https://redis.io/commands/get
        get :: key -> Internal.Query (Maybe a),
        -- | Atomically sets key to value and returns the old value stored at key.
        -- Returns an error when key exists but does not hold a string value.
        --
        -- https://redis.io/commands/getset
        getset :: key -> a -> Internal.Query (Maybe a),
        -- | Returns the values of all specified keys. For every key that does not hold
        -- a string value or does not exist, no value is returned. Because of this, the
        -- operation never fails.
        --
        -- https://redis.io/commands/mget
        mget :: Ord key => List.List key -> Internal.Query (Dict.Dict key a),
        -- | Sets the given keys to their respective values. MSET replaces existing
        -- values with new values, just as regular SET. See MSETNX if you don't want to
        -- overwrite existing values.
        --
        -- MSET is atomic, so all given keys are set at once. It is not possible for
        -- clients to see that some of the keys were updated while others are
        -- unchanged.
        --
        -- https://redis.io/commands/mset
        mset :: Dict.Dict key a -> Internal.Query (),
        -- | Returns PONG if no argument is provided, otherwise return a copy of the
        -- argument as a bulk. This command is often used to test if a connection is
        -- still alive, or to measure latency.
        --
        -- https://redis.io/commands/ping
        ping :: Internal.Query (),
        -- | Insert all the specified values at the tail of the list stored at key. If key does not exist, it is created as empty list before performing the push operation. When key holds a value that is not a list, an error is returned.
        --
        -- https://redis.io/commands/rpush
        rpush :: key -> List.List a -> Internal.Query Int,
        -- | Set key to hold the string value. If key already holds a value, it is
        -- overwritten, regardless of its type. Any previous time to live associated
        -- with the key is discarded on successful SET operation.
        --
        -- https://redis.io/commands/set
        set :: key -> a -> Internal.Query (),
        -- | Set key to hold the string value. If key already holds a value, it is
        -- overwritten, regardless of its type. Any previous time to live associated
        -- with the key is discarded on successful SET operation.
        --
        -- https://redis.io/commands/set
        setnx :: key -> a -> Internal.Query Bool
      }

makeApi :: Encoder a -> Decoder a -> (key -> Text) -> Api key a
makeApi encode decode toKey =
  Api
    { del = Internal.Del << List.map toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      get = \key -> Internal.WithResult (Prelude.traverse decode) (Internal.Get (toKey key)),
      getset = \key value -> Internal.WithResult (Prelude.traverse decode) (Internal.Getset (toKey key) (encode value)),
      mget = \keys ->
        List.map toKey keys
          |> Internal.Mget
          |> map (maybesToDict keys)
          |> Internal.WithResult (Prelude.traverse decode),
      mset =
        Dict.toList
          >> List.map (\(k, v) -> (toKey k, encode v))
          >> Internal.Mset,
      ping = Internal.Ping |> map (\_ -> ()),
      rpush = \key vals ->
        Internal.Rpush (toKey key) (List.map encode vals),
      set = \key value -> Internal.Set (toKey key) (encode value),
      setnx = \key value -> Internal.Setnx (toKey key) (encode value)
    }

data HashApi key field a
  = HashApi
      { -- | Removes the specified fields from the hash stored at key. Specified fields
        -- that do not exist within this hash are ignored. If key does not exist, it is
        -- treated as an empty hash and this command returns 0.
        --
        -- https://redis.io/commands/hdel
        hdel :: key -> List.List field -> Internal.Query Int,
        -- | Get the value of the field of a hash at key. If the key does not exist,
        -- or the field in the hash does not exis the special value Nothing is returned
        -- An error is returned if the value stored at key is not a
        -- hash, because HGET only handles string values.
        --
        -- https://redis.io/commands/hget
        hget :: key -> field -> Internal.Query (Maybe a),
        -- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
        -- Nothing in the returned value means failed utf8 decoding, not that it doesn't exist
        --
        -- https://redis.io/commands/hgetall
        hgetall :: key -> Internal.Query (Dict.Dict field a),
        -- | Returns the values associated with the specified fields in the hash stored at key.--
        --
        -- equivalent to modern hget
        -- https://redis.io/commands/hmget
        hmget :: key -> List.List field -> Internal.Query (Dict.Dict field a),
        -- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
        --
        -- equivalent to modern hset
        -- https://redis.io/commands/hmset
        hmset :: key -> Dict.Dict field a -> Internal.Query (),
        -- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
        --
        -- https://redis.io/commands/hset
        hset :: key -> field -> a -> Internal.Query (),
        -- | Sets field in the hash stored at key to value, only if field does not yet
        -- exist. If key does not exist, a new key holding a hash is created. If field
        -- already exists, this operation has no effect.
        --
        -- https://redis.io/commands/hsetnx
        hsetnx :: key -> field -> a -> Internal.Query Bool
      }

makeHashApi ::
  Ord field =>
  Encoder a ->
  Decoder a ->
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  HashApi key field a
makeHashApi encode decode toKey toField fromField =
  HashApi
    { hdel = \key fields -> Internal.Hdel (toKey key) (List.map toField fields),
      hget = \key field -> Internal.WithResult (Prelude.traverse decode) (Internal.Hget (toKey key) (toField field)),
      hgetall = Internal.WithResult (toDict fromField decode) << Internal.Hgetall << toKey,
      hmget = \key fields ->
        fields
          |> List.map toField
          |> Internal.Hmget (toKey key)
          |> map (maybesToDict fields)
          |> Internal.WithResult (Prelude.traverse decode),
      hmset = \key vals ->
        vals
          |> Dict.toList
          |> List.map (\(k, v) -> (toField k, encode v))
          |> Internal.Hmset (toKey key),
      hset = \key field val ->
        Internal.Hset (toKey key) (toField field) (encode val),
      hsetnx = \key field val ->
        Internal.Hsetnx (toKey key) (toField field) (encode val)
    }

toDict :: Ord field => (Text -> Maybe field) -> Decoder a -> List (Text, ByteString) -> Result Internal.Error (Dict.Dict field a)
toDict fromField decode =
  Result.map Dict.fromList
    << Prelude.traverse
      ( \(k, v) ->
          Result.andThen
            ( \v' ->
                case fromField k of
                  Just k' -> Result.Ok (k', v')
                  Nothing -> Result.Err (Internal.DecodingFieldError k)
            )
            (decode v)
      )

jsonEncoder :: Aeson.ToJSON a => Encoder a
jsonEncoder = Aeson.encode >> Data.ByteString.Lazy.toStrict

jsonDecoder :: Aeson.FromJSON a => Decoder a
jsonDecoder byteString =
  case Aeson.eitherDecodeStrict' byteString of
    Prelude.Right decoded -> Ok decoded
    Prelude.Left err ->
      Internal.DecodingError (Data.Text.pack err)
        |> Err

-- | Marks the given keys to be watched for conditional execution of a
-- transaction.
--
-- This returns a task because it cannot be ran as part of a transaction.
--
-- https://redis.io/commands/watch
watch :: Internal.Handler -> [Text] -> Task Internal.Error ()
watch h keys =
  Internal.watch h keys

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
