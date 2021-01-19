{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

    -- * Creating a redis API
    flatApi,
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
    setnx,
    experimental,
    atomicModify,
    atomicModifyWithContext,

    -- * Running Redis queries
    Internal.query,
    Internal.transaction,
    Internal.Query,
    Internal.Error (..),
    Internal.map,
    Internal.map2,
    Internal.map3,
    Internal.sequence,

    -- * Observability hepers
    Real.Info (..),
    readiness,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Flat as Flat
import qualified Dict
import qualified Health
import qualified List
import NriPrelude
import qualified Platform
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task
import qualified Tuple
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

data Api key a
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
        -- | Set key to hold the string value. If key already holds a value, it is
        -- overwritten, regardless of its type. Any previous time to live associated
        -- with the key is discarded on successful SET operation.
        --
        -- https://redis.io/commands/set
        set :: key -> a -> Internal.Query (),
        -- Set key to hold string value if key does not exist. In that case, it
        -- is equal to SET. When key already holds a value, no operation is
        -- performed. SETNX is short for "SET if Not eXists".
        --
        -- https://redis.io/commands/setnx
        setnx :: key -> a -> Internal.Query Bool,
        experimental :: Experimental key a
      }

-- | These functions are experimental and may not work.
data Experimental key a
  = Experimental
      { -- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
        -- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
        -- The returned value is the value that was set.
        atomicModify :: Internal.Handler -> key -> (Maybe a -> a) -> Task Internal.Error a,
        -- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
        -- that was set.
        atomicModifyWithContext :: forall b. Internal.Handler -> key -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
      }

flatApi :: Flat.Flat a => (key -> Text) -> Api key a
flatApi = makeApi Codec.flatCodec

jsonApi :: (Aeson.ToJSON a, Aeson.FromJSON a) => (key -> Text) -> Api key a
jsonApi = makeApi Codec.jsonCodec

textApi :: (key -> Text) -> Api key Text
textApi = makeApi Codec.textCodec

byteStringApi :: (key -> Text) -> Api key ByteString.ByteString
byteStringApi = makeApi Codec.byteStringCodec

makeApi :: Codec.Codec a -> (key -> Text) -> Api key a
makeApi Codec.Codec {Codec.codecEncoder, Codec.codecDecoder} toKey =
  Api
    { del = Internal.Del << List.map toKey,
      exists = Internal.Exists << toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      get = \key -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Get (toKey key)),
      getset = \key value -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Getset (toKey key) (codecEncoder value)),
      mget = \keys ->
        List.map toKey keys
          |> Internal.Mget
          |> map (Internal.maybesToDict keys)
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      mset =
        Dict.toList
          >> List.map (\(k, v) -> (toKey k, codecEncoder v))
          >> Internal.Mset,
      ping = Internal.Ping |> map (\_ -> ()),
      set = \key value -> Internal.Set (toKey key) (codecEncoder value),
      setnx = \key value -> Internal.Setnx (toKey key) (codecEncoder value),
      experimental =
        Experimental
          { atomicModify = \handler key f ->
              atomicModifyWithContext handler key (\x -> (f x, ()))
                |> map Tuple.first,
            atomicModifyWithContext
          }
    }
  where
    atomicModifyWithContext handler key f =
      atomicModifyWithContext'
        handler
        key
        ( \maybeByteString ->
            let context = Prelude.traverse codecDecoder maybeByteString
             in ( case context of
                    Ok maybeText -> maybeText
                    Err _ -> Nothing
                )
                  |> f
                  |> (\r@(res, _ctx) -> (codecEncoder res, (r, context)))
        )
        |> Task.andThen
          ( \(_, (res, context)) ->
              case context of
                Err _ -> Task.fail unparsableKeyError
                Ok _ -> Task.succeed res
          )
    atomicModifyWithContext' handler key f =
      loop (100 :: Int)
      where
        loop count =
          action
            |> Task.onError (handleError count)
        handleError count err =
          case err of
            Internal.TransactionAborted ->
              if count > 0
                then loop (count - 1)
                else Task.fail <| Internal.RedisError "Attempted atomic update 100 times without success."
            Internal.ConnectionLost -> Task.fail err
            Internal.RedisError _ -> Task.fail err
            Internal.DecodingError _ -> Task.fail err
            Internal.DecodingFieldError _ -> Task.fail err
            Internal.LibraryError _ -> Task.fail err
            Internal.TimeoutError -> Task.fail err
        action = do
          Internal.watch handler [toKey key]
          oldValue <- Internal.query handler (Internal.Get (toKey key))
          let (setValue, returnValue) = f oldValue
          Internal.transaction handler (Internal.Set (toKey key) setValue)
          Task.succeed (setValue, returnValue)

unparsableKeyError :: Internal.Error
unparsableKeyError = Internal.LibraryError "key exists but not parsable json"
