{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.Hash
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
    exists,
    expire,
    ping,
    hdel,
    hget,
    hgetall,
    hkeys,
    hmget,
    hmset,
    hset,
    hsetnx,

    -- * Running Redis queries
    Internal.query,
    Internal.transaction,
    Internal.Query,
    Internal.Error (..),
    Internal.map,
    Internal.map2,
    Internal.map3,
    Internal.sequence,

    -- * Every `Redis.Api key value` needs to implement an instance for `HasExamples value`.

    -- | This instance is used to generate golden-tests for the api's value.
    -- | You can use `Redis.Test.fromExamples yourRedisApi` in your test suite.
    Examples.HasExamples (..),
    Examples.example,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Dict
import qualified Examples
import qualified NonEmptyDict
import qualified Redis.Codec as Codec
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Result
import qualified Prelude

data Api key field a = Api
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
    -- | Removes the specified fields from the hash stored at key. Specified fields
    -- that do not exist within this hash are ignored. If key does not exist, it is
    -- treated as an empty hash and this command returns 0.
    --
    -- https://redis.io/commands/hdel
    hdel :: key -> NonEmpty field -> Internal.Query Int,
    -- | Get the value of the field of a hash at key. If the key does not exist,
    -- or the field in the hash does not exis the special value Nothing is returned
    -- An error is returned if the value stored at key is not a
    -- hash, because HGET only handles string values.
    --
    -- https://redis.io/commands/hget
    hget :: key -> field -> Internal.Query (Maybe a),
    -- | Returns all fields and values of the hash stored at key. In the returned value, every field name is followed by its value, so the length of the reply is twice the size of the hash.
    -- Nothing in the returned value means failed utf8 decoding, not that it doesn't exist
    --
    -- https://redis.io/commands/hgetall
    hgetall :: key -> Internal.Query (Dict.Dict field a),
    -- | Returns all field names in the hash stored at key.
    -- Empty list means key doesn't exist
    --
    -- https://redis.io/commands/hkeys
    hkeys :: key -> Internal.Query (List field),
    -- | Returns the values associated with the specified fields in the hash stored at key.--
    --
    -- equivalent to modern hget
    -- https://redis.io/commands/hmget
    hmget :: key -> NonEmpty field -> Internal.Query (Dict.Dict field a),
    -- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
    --
    -- equivalent to modern hset
    -- https://redis.io/commands/hmset
    hmset :: key -> NonEmptyDict.NonEmptyDict field a -> Internal.Query (),
    -- | Sets field in the hash stored at key to value. If key does not exist, a new key holding a hash is created. If field already exists in the hash, it is overwritten.
    --
    -- https://redis.io/commands/hset
    hset :: key -> field -> a -> Internal.Query (),
    -- | Sets field in the hash stored at key to value, only if field does not yet
    -- exist. If key does not exist, a new key holding a hash is created. If field
    -- already exists, this operation has no effect.
    --
    -- https://redis.io/commands/hsetnx
    hsetnx :: key -> field -> a -> Internal.Query Bool
  }

jsonApi ::
  forall a field key.
  (Examples.HasExamples a, Aeson.ToJSON a, Aeson.FromJSON a, Ord field) =>
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  Api key field a
jsonApi = makeApi Codec.jsonCodec

textApi ::
  Ord field =>
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  Api key field Text
textApi = makeApi Codec.textCodec

byteStringApi ::
  Ord field =>
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  Api key field ByteString.ByteString
byteStringApi = makeApi Codec.byteStringCodec

makeApi ::
  Ord field =>
  Codec.Codec a ->
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  Api key field a
makeApi Codec.Codec {Codec.codecEncoder, Codec.codecDecoder} toKey toField fromField =
  Api
    { del = Internal.Del << NonEmpty.map toKey,
      exists = Internal.Exists << toKey,
      expire = \key secs -> Internal.Expire (toKey key) secs,
      ping = Internal.Ping |> map (\_ -> ()),
      hdel = \key fields -> Internal.Hdel (toKey key) (NonEmpty.map toField fields),
      hget = \key field -> Internal.WithResult (Prelude.traverse codecDecoder) (Internal.Hget (toKey key) (toField field)),
      hgetall = Internal.WithResult (toDict fromField codecDecoder) << Internal.Hgetall << toKey,
      hkeys = \key ->
        Internal.Hkeys (toKey key)
          |> Internal.WithResult
            ( Prelude.traverse
                ( \field -> case fromField field of
                    Just field' -> Result.Ok field'
                    Nothing -> Result.Err (Internal.DecodingFieldError field)
                )
            ),
      hmget = \key fields ->
        fields
          |> NonEmpty.map toField
          |> Internal.Hmget (toKey key)
          |> map (Internal.maybesToDict (NonEmpty.toList fields))
          |> Internal.WithResult (Prelude.traverse codecDecoder),
      hmset = \key vals ->
        vals
          |> NonEmptyDict.toNonEmptyList
          |> NonEmpty.map (\(k, v) -> (toField k, codecEncoder v))
          |> Internal.Hmset (toKey key),
      hset = \key field val ->
        Internal.Hset (toKey key) (toField field) (codecEncoder val),
      hsetnx = \key field val ->
        Internal.Hsetnx (toKey key) (toField field) (codecEncoder val)
    }

toDict :: Ord field => (Text -> Maybe field) -> Codec.Decoder a -> List (Text, ByteString) -> Result Internal.Error (Dict.Dict field a)
toDict fromField decode =
  Result.map Dict.fromList
    << Prelude.traverse
      ( \(k, v) ->
          Result.andThen
            ( \v' ->
                case fromField k of
                  Just k' -> Result.Ok (k', v')
                  Nothing -> Result.Err (Internal.DecodingFieldError k)
            )
            (decode v)
      )
