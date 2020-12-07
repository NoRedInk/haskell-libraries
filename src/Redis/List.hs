{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.List
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
    Redis.readiness,

    -- * Creating api access functions
    makeApi,
    Api,
    del,
    expire,
    ping,
    rpush,
    Redis.Codec,
    Redis.Encoder,
    Redis.Decoder,
    Redis.jsonCodec,
    Redis.byteStringCodec,
    Redis.textCodec,
  )
where

import qualified List
import NriPrelude
import qualified Redis
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings

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
        -- | Insert all the specified values at the tail of the list stored at key. If key does not exist, it is created as empty list before performing the push operation. When key holds a value that is not a list, an error is returned.
        --
        -- https://redis.io/commands/rpush
        rpush :: key -> List.List a -> Internal.Query Int
      }

makeApi ::
  Redis.Codec a ->
  (key -> Text) ->
  Api key a
makeApi Redis.Codec {Redis.codecEncoder} toKey =
  Api
    { del = Internal.Del << List.map toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      rpush = \key vals ->
        Internal.Rpush (toKey key) (List.map codecEncoder vals)
    }
