{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.List
  ( -- * Creating a redis handler
    Real.handler,
    Internal.Handler,
    Settings.Settings (..),
    Settings.decoder,

    -- * Creating a redis API
    jsonApi,
    textApi,
    byteStringApi,
    Api,

    -- * Creating redis queries
    del,
    expire,
    ping,
    lrange,
    rpush,

    -- * Running Redis queries
    Internal.query,
    Internal.transaction,
    Internal.Query,
    Internal.Error (..),
    Internal.map,

    -- * Observability hepers
    Real.Info (..),
    Redis.readiness,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified List
import NriPrelude
import qualified Redis
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Prelude

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
        -- | Returns PONG if no argument is provided, otherwise return a copy of the
        -- argument as a bulk. This command is often used to test if a connection is
        -- still alive, or to measure latency.
        --
        -- https://redis.io/commands/ping
        ping :: Internal.Query (),
        -- | Returns the specified elements of the list stored at key. The
        -- offsets start and stop are zero-based indexes, with 0 being the
        -- first element of the list (the head of the list), 1 being the next
        -- element and so on.
        --
        -- These offsets can also be negative numbers indicating offsets
        -- starting at the end of the list. For example, -1 is the last element
        -- of the list, -2 the penultimate, and so on.
        lrange :: key -> Int -> Int -> Internal.Query [a],
        -- | Insert all the specified values at the tail of the list stored at key. If key does not exist, it is created as empty list before performing the push operation. When key holds a value that is not a list, an error is returned.
        --
        -- https://redis.io/commands/rpush
        rpush :: key -> List.List a -> Internal.Query Int
      }

jsonApi :: (Aeson.ToJSON a, Aeson.FromJSON a) => (key -> Text) -> Api key a
jsonApi = makeApi Codec.jsonCodec

textApi :: (key -> Text) -> Api key Text
textApi = makeApi Codec.textCodec

byteStringApi :: (key -> Text) -> Api key ByteString.ByteString
byteStringApi = makeApi Codec.byteStringCodec

makeApi ::
  Codec.Codec a ->
  (key -> Text) ->
  Api key a
makeApi Codec.Codec {Codec.codecEncoder, Codec.codecDecoder} toKey =
  Api
    { del = Internal.Del << List.map toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      lrange = \key lower upper ->
        Internal.Lrange (toKey key) lower upper
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      rpush = \key vals ->
        Internal.Rpush (toKey key) (List.map codecEncoder vals)
    }
