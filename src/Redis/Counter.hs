{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.Counter
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
    get,
    incr,
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
import qualified Prelude

data Api key
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
        -- | Get the value of key. If the key does not exist the special value Nothing
        -- is returned. An error is returned if the value stored at key is not a
        -- string, because GET only handles string values.
        --
        -- https://redis.io/commands/get
        get :: key -> Internal.Query (Maybe Int),
        -- | Increments the number stored at key by one. If the key does not
        -- exist, it is set to 0 before performing the operation. An error is
        -- returned if the key contains a value of the wrong type or contains a
        -- string that can not be represented as integer. This operation is
        -- limited to 64 bit signed integers.
        --
        -- https://redis.io/commands/incr
        incr :: key -> Internal.Query Int
      }

makeApi ::
  (key -> Text) ->
  Api key
makeApi toKey =
  Api
    { del = Internal.Del << List.map toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      get = \key ->
        Internal.Get (toKey key)
          |> Internal.WithResult (Prelude.traverse (Redis.codecDecoder Redis.jsonCodec)),
      incr = \key -> Internal.Incr (toKey key)
    }
