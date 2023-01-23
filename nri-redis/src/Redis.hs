{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis
  ( -- * Creating a redis handler
    Real.handler,
    Internal.Handler,
    Settings.Settings (..),
    Settings.decoder,
    Settings.decoderWithEnvVarPrefix,

    -- * Creating a redis API
    jsonApi,
    textApi,
    byteStringApi,
    Api,

    -- * Creating redis queries
    del,
    exists,
    expire,
    get,
    getset,
    mget,
    mset,
    ping,
    set,
    setex,
    setnx,

    -- * Running Redis queries
    Internal.query,
    Internal.extendExpire,
    Internal.transaction,
    Internal.Query,
    Internal.Error (..),
    Internal.map,
    Internal.map2,
    Internal.map3,
    Internal.sequence,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Dict
import qualified NonEmptyDict
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Prelude

-- | a API type can be used to enforce a mapping of keys to values.
-- without an API type, it can be easy to naiively serialize the wrong type
-- into a redis key.
--
-- Out of the box, we have helpers to support
-- - 'jsonApi' for json-encodable and decodable values
-- - 'textApi' for 'Text' values
-- - 'byteStringApi' for 'ByteString' values
data Api key a = Api
  { -- | Removes the specified keys. A key is ignored if it does not exist.
    --
    -- https://redis.io/commands/del
    del :: NonEmpty key -> Internal.Query Int,
    -- | Returns if key exists.
    --
    -- https://redis.io/commands/exists
    exists :: key -> Internal.Query Bool,
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
    mget :: Ord key => NonEmpty key -> Internal.Query (Dict.Dict key a),
    -- | Sets the given keys to their respective values. MSET replaces existing
    -- values with new values, just as regular SET. See MSETNX if you don't want to
    -- overwrite existing values.
    --
    -- MSET is atomic, so all given keys are set at once. It is not possible for
    -- clients to see that some of the keys were updated while others are
    -- unchanged.
    --
    -- https://redis.io/commands/mset
    mset :: NonEmptyDict.NonEmptyDict key a -> Internal.Query (),
    -- | Returns PONG if no argument is provided, otherwise return a copy of the
    -- argument as a bulk. This command is often used to test if a connection is
    -- still alive, or to measure latency.
    --
    -- https://redis.io/commands/ping
    ping :: Internal.Query (),
    -- | Set key to hold the string value. If key already holds a value, it is
    -- overwritten, regardless of its type. Any previous time to live associated
    -- with the key is discarded on successful SET operation.
    --
    -- https://redis.io/commands/set
    set :: key -> a -> Internal.Query (),
    -- | Set key to hold the string value and set key to timeout after a given
    -- number of seconds.
    --
    -- https://redis.io/commands/setex
    setex :: key -> Int -> a -> Internal.Query (),
    -- | Set key to hold string value if key does not exist. In that case, it
    -- is equal to SET. When key already holds a value, no operation is
    -- performed. SETNX is short for "SET if Not eXists".
    --
    -- https://redis.io/commands/setnx
    setnx :: key -> a -> Internal.Query Bool
  }

-- | Creates a json API mapping a 'key' to a json-encodable-decodable type
--
-- > data Key = Key { fieldA: Text, fieldB: Text }
-- > data Val = Val { ... }
-- >
-- > myJsonApi :: Redis.Api Key Val
-- > myJsonApi = Redis.jsonApi (\Key {fieldA, fieldB}-> Text.join "-" [fieldA, fieldB, "v1"])
jsonApi ::
  forall a key.
  (Aeson.ToJSON a, Aeson.FromJSON a) =>
  (key -> Text) ->
  Api key a
jsonApi toKey = makeApi Codec.jsonCodec toKey

-- | Creates a Redis API mapping a 'key' to Text
textApi :: (key -> Text) -> Api key Text
textApi = makeApi Codec.textCodec

-- | Creates a Redis API mapping a 'key' to a ByteString
byteStringApi :: (key -> Text) -> Api key ByteString.ByteString
byteStringApi = makeApi Codec.byteStringCodec

-- | Private API used to make an API
makeApi :: Codec.Codec a -> (key -> Text) -> Api key a
makeApi Codec.Codec {Codec.codecEncoder, Codec.codecDecoder} toKey =
  Api
    { del = Internal.Del << NonEmpty.map toKey,
      exists = Internal.Exists << toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      get = \key -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Get (toKey key)),
      getset = \key value -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Getset (toKey key) (codecEncoder value)),
      mget = \keys ->
        NonEmpty.map toKey keys
          |> Internal.Mget
          |> map (Internal.maybesToDict (NonEmpty.toList keys))
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      mset = \vals ->
        NonEmptyDict.toNonEmptyList vals
          |> map (\(k, v) -> (toKey k, codecEncoder v))
          |> Internal.Mset,
      ping = Internal.Ping |> map (\_ -> ()),
      set = \key value -> Internal.Set (toKey key) (codecEncoder value),
      setex = \key seconds value -> Internal.Setex (toKey key) seconds (codecEncoder value),
      setnx = \key value -> Internal.Setnx (toKey key) (codecEncoder value)
    }
