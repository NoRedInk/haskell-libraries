{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.Counter
  ( -- * Creating a redis handler
    Real.handler,
    Internal.Handler,
    Settings.Settings (..),
    Settings.decoder,

    -- * Creating a redis API
    makeApi,
    Api,

    -- * Creating redis queries
    del,
    exists,
    expire,
    ping,
    get,
    incr,
    incrby,
    set,

    -- * Running Redis queries
    Internal.query,
    Internal.transaction,
    Internal.Query,
    Internal.Error (..),
    Internal.map,
    Internal.map2,
    Internal.map3,
    Internal.traverse,

    -- * Observability helpers
    Real.Info (..),
    Redis.readiness,
  )
where

import qualified List
import NriPrelude
import qualified Redis
import qualified Redis.Codec as Codec
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
        incr :: key -> Internal.Query Int,
        -- | Increments the number stored at key by increment. If the key does
        -- not exist, it is set to 0 before performing the operation. An error
        -- is returned if the key contains a value of the wrong type or
        -- contains a string that can not be represented as integer. This
        -- operation is limited to 64 bit signed integers.
        --
        -- https://redis.io/commands/incrby
        incrby :: key -> Int -> Internal.Query Int,
        -- | Set key to hold the string value. If key already holds a value, it is
        -- overwritten, regardless of its type. Any previous time to live associated
        -- with the key is discarded on successful SET operation.
        --
        -- https://redis.io/commands/set
        set :: key -> Int -> Internal.Query ()
      }

makeApi ::
  (key -> Text) ->
  Api key
makeApi toKey =
  Api
    { del = Internal.Del << List.map toKey,
      exists = Internal.Exists << toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      get = \key ->
        Internal.Get (toKey key)
          |> Internal.WithResult (Prelude.traverse (Codec.codecDecoder Codec.jsonCodec)),
      incr = \key -> Internal.Incr (toKey key),
      incrby = \key amount -> Internal.Incrby (toKey key) amount,
      set = \key val -> Internal.Set (toKey key) (Codec.codecEncoder Codec.jsonCodec val)
    }
