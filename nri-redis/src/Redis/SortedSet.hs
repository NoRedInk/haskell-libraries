{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.SortedSet
  ( -- * Creating a Redis handler
    Handler.handler,
    Handler.handlerAutoExtendExpire,
    Internal.Handler,
    Internal.HandlerAutoExtendExpire,
    Settings.Settings (..),
    Settings.decoder,

    -- * Creating a Redis API
    jsonApi,
    textApi,
    byteStringApi,
    Api,

    -- * Creating Redis queries
    del,
    exists,
    expire,
    ping,
    zadd,
    zrange,
    zrank,
    zrevrank,

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
import qualified Data.Map.Strict
import qualified NonEmptyDict
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Handler as Handler
import qualified Redis.Settings as Settings
import qualified Prelude

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
    -- | Adds all the specified members with the specified scores to the sorted
    -- set. If a specified member is already a member of the sorted set, the
    -- score is updated and the element reinserted at the right position to
    -- ensure the correct ordering.
    --
    -- https://redis.io/commands/zadd
    zadd :: key -> NonEmptyDict.NonEmptyDict a Float -> Internal.Query Int,
    -- | Returns the specified range of elements in the sorted set. The order of
    -- elements is from the lowest to the highest score. Elements with the same
    -- score are ordered lexicographically. The <start> and <stop> arguments
    -- represent zero-based indexes, where 0 is the first element, 1 is the next
    -- element, and so on. These arguments specify an inclusive range, so for
    -- example, ZRANGE myzset 0 1 will return both the first and the second
    -- element of the sorted set.
    --
    -- The indexes can also be negative numbers indicating offsets from the end
    -- of the sorted set, with -1 being the last element of the sorted set, -2
    -- the penultimate element, and so on.
    --
    -- Out of range indexes do not produce an error.
    --
    -- https://redis.io/commands/zrange
    zrange :: key -> Int -> Int -> Internal.Query (List a),
    -- | Returns the rank of member in the sorted set stored at key, with the
    -- scores ordered from low to high. The rank (or index) is 0-based, which
    -- means that the member with the lowest score has rank 0.
    --
    -- https://redis.io/commands/zrank
    zrank :: key -> a -> Internal.Query (Maybe Int),
    -- | Returns the rank of member in the sorted set stored at key, with the
    -- scores ordered from high to low. The rank (or index) is 0-based, which
    -- means that the member with the highest score has rank 0.
    --
    -- https://redis.io/commands/zrevrank
    zrevrank :: key -> a -> Internal.Query (Maybe Int)
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
      zadd = \key vals ->
        Internal.Zadd (toKey key) (Data.Map.Strict.mapKeys codecEncoder (NonEmptyDict.toDict vals)),
      zrange = \key start stop ->
        Internal.Zrange (toKey key) start stop
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      zrank = \key member -> Internal.Zrank (toKey key) (codecEncoder member),
      zrevrank = \key member -> Internal.Zrevrank (toKey key) (codecEncoder member)
    }
