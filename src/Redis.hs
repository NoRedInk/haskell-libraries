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
    atomicModify,
    atomicModifyWithContext,
    Codec (..),
    Encoder,
    Decoder,
    jsonCodec,
    byteStringCodec,
    textCodec,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Dict
import qualified Health
import qualified List
import NriPrelude
import qualified Platform
import qualified Redis.ByteString
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

data Codec a
  = Codec
      { codecEncoder :: Encoder a,
        codecDecoder :: Decoder a
      }

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
        setnx :: key -> a -> Internal.Query Bool,
        -- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
        -- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
        -- The returned value is the value that was set.
        atomicModify :: Internal.Handler -> key -> (Maybe a -> a) -> Task Internal.Error a,
        -- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
        -- that was set.
        atomicModifyWithContext :: forall b. Internal.Handler -> key -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
      }

makeApi :: Codec a -> (key -> Text) -> Api key a
makeApi Codec {codecEncoder, codecDecoder} toKey =
  Api
    { del = Internal.Del << List.map toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      get = \key -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Get (toKey key)),
      getset = \key value -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Getset (toKey key) (codecEncoder value)),
      mget = \keys ->
        List.map toKey keys
          |> Internal.Mget
          |> map (maybesToDict keys)
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      mset =
        Dict.toList
          >> List.map (\(k, v) -> (toKey k, codecEncoder v))
          >> Internal.Mset,
      ping = Internal.Ping |> map (\_ -> ()),
      set = \key value -> Internal.Set (toKey key) (codecEncoder value),
      setnx = \key value -> Internal.Setnx (toKey key) (codecEncoder value),
      atomicModify = \handler key f ->
        atomicModifyWithContext handler key (\x -> (f x, ()))
          |> map Tuple.first,
      atomicModifyWithContext
    }
  where
    atomicModifyWithContext handler key f =
      Redis.ByteString.atomicModifyWithContext
        handler
        (toKey key)
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

jsonCodec :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
jsonCodec = Codec jsonEncoder jsonDecoder

jsonEncoder :: Aeson.ToJSON a => Encoder a
jsonEncoder = Aeson.encode >> Data.ByteString.Lazy.toStrict

jsonDecoder :: Aeson.FromJSON a => Decoder a
jsonDecoder byteString =
  case Aeson.eitherDecodeStrict' byteString of
    Prelude.Right decoded -> Ok decoded
    Prelude.Left err ->
      Internal.DecodingError (Data.Text.pack err)
        |> Err

byteStringCodec :: Codec ByteString
byteStringCodec = Codec identity Ok

textCodec :: Codec Text
textCodec = Codec Data.Text.Encoding.encodeUtf8 (Data.Text.Encoding.decodeUtf8 >> Ok)

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

unparsableKeyError :: Internal.Error
unparsableKeyError = Internal.LibraryError "key exists but not parsable json"
