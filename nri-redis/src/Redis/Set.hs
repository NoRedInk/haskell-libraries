{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.Set
  ( -- * Creating a redis handler
    Real.handler,
    Real.handlerAutoExtendExpire,
    Internal.HandlerNoAutoExtendExpire,
    Internal.HandlerAutoExtendExpire,
    Settings.Settings (..),
    Settings.decoder,

    -- * Creating a redis API
    jsonApi,
    textApi,
    byteStringApi,
    Api,

    -- * Creating redis queries
    del,
    exists,
    expire,
    ping,
    sadd,
    scard,
    srem,
    smembers,

    -- * Running Redis queries
    Internal.query,
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
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Set
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
    -- | Returns PONG if no argument is provided, otherwise return a copy of the
    -- argument as a bulk. This command is often used to test if a connection is
    -- still alive, or to measure latency.
    --
    -- https://redis.io/commands/ping
    ping :: Internal.Query (),
    -- | Add the specified members to the set stored at key. Specified members
    -- that are already a member of this set are ignored. If key does not
    -- exist, a new set is created before adding the specified members.
    --
    -- https://redis.io/commands/sadd
    sadd :: key -> NonEmpty a -> Internal.Query Int,
    -- | Returns the set cardinality (number of elements) of the set stored at
    -- key.
    --
    -- https://redis.io/commands/scard
    scard :: key -> Internal.Query Int,
    -- | remove the specified members from the set stored at key. specified
    -- members that are not a member of this set are ignored. if key does not
    -- exist, it is treated as an empty set and this command returns 0.
    --
    -- https://redis.io/commands/srem
    srem :: key -> NonEmpty a -> Internal.Query Int,
    -- | Returns all the members of the set value stored at key.
    --
    -- https://redis.io/commands/smembers
    smembers :: key -> Internal.Query (Set.Set a)
  }

-- | Creates a json API mapping a 'key' to a json-encodable-decodable type
--
-- > data Key = Key { fieldA: Text, fieldB: Text }
-- > data Val = Val { ... }
-- >
-- > myJsonApi :: Redis.Api Key Val
-- > myJsonApi = Redis.jsonApi (\Key {fieldA,
jsonApi ::
  forall a key.
  (Aeson.ToJSON a, Aeson.FromJSON a, Ord a) =>
  (key -> Text) ->
  Api key a
jsonApi = makeApi Codec.jsonCodec

-- | Creates a Redis API mapping a 'key' to Text
textApi :: (key -> Text) -> Api key Text
textApi = makeApi Codec.textCodec

-- | Creates a Redis API mapping a 'key' to a ByteString
byteStringApi :: (key -> Text) -> Api key ByteString.ByteString
byteStringApi = makeApi Codec.byteStringCodec

makeApi ::
  Ord a =>
  Codec.Codec a ->
  (key -> Text) ->
  Api key a
makeApi Codec.Codec {Codec.codecEncoder, Codec.codecDecoder} toKey =
  Api
    { del = Internal.Del << NonEmpty.map toKey,
      exists = Internal.Exists << toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      sadd = \key vals ->
        Internal.Sadd (toKey key) (NonEmpty.map codecEncoder vals),
      scard = \key ->
        Internal.Scard (toKey key),
      srem = \key vals ->
        Internal.Srem (toKey key) (NonEmpty.map codecEncoder vals),
      smembers = \key ->
        Internal.Smembers (toKey key)
          |> Internal.WithResult (Prelude.traverse codecDecoder)
          |> Internal.map Set.fromList
    }
