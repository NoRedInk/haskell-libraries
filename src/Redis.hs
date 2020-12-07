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
    set,
    setnx,
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
      set = \key value -> Internal.Set (toKey key) (encode value),
      setnx = \key value -> Internal.Setnx (toKey key) (encode value)
    }

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
