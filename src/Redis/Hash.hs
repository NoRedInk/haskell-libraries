{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis.Hash
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
    Redis.watch,

    -- * Creating api access functions
    makeHashApi,
    HashApi,
    hdel,
    hget,
    hgetall,
    hmget,
    hmset,
    hset,
    hsetnx,
    Redis.Decoder,
    Redis.Encoder,
    Redis.jsonDecoder,
    Redis.jsonEncoder,
  )
where

import Data.ByteString (ByteString)
import qualified Dict
import qualified List
import NriPrelude
import qualified Redis
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Result
import qualified Prelude

data HashApi key field a
  = HashApi
      { -- | Removes the specified fields from the hash stored at key. Specified fields
        -- that do not exist within this hash are ignored. If key does not exist, it is
        -- treated as an empty hash and this command returns 0.
        --
        -- https://redis.io/commands/hdel
        hdel :: key -> List.List field -> Internal.Query Int,
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
        -- | Returns the values associated with the specified fields in the hash stored at key.--
        --
        -- equivalent to modern hget
        -- https://redis.io/commands/hmget
        hmget :: key -> List.List field -> Internal.Query (Dict.Dict field a),
        -- | Sets fields in the hash stored at key to values. If key does not exist, a new key holding a hash is created. If any fields exists, they are overwritten.
        --
        -- equivalent to modern hset
        -- https://redis.io/commands/hmset
        hmset :: key -> Dict.Dict field a -> Internal.Query (),
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

makeHashApi ::
  Ord field =>
  Redis.Encoder a ->
  Redis.Decoder a ->
  (key -> Text) ->
  (field -> Text) ->
  (Text -> Maybe field) ->
  HashApi key field a
makeHashApi encode decode toKey toField fromField =
  HashApi
    { hdel = \key fields -> Internal.Hdel (toKey key) (List.map toField fields),
      hget = \key field -> Internal.WithResult (Prelude.traverse decode) (Internal.Hget (toKey key) (toField field)),
      hgetall = Internal.WithResult (toDict fromField decode) << Internal.Hgetall << toKey,
      hmget = \key fields ->
        fields
          |> List.map toField
          |> Internal.Hmget (toKey key)
          |> map (maybesToDict fields)
          |> Internal.WithResult (Prelude.traverse decode),
      hmset = \key vals ->
        vals
          |> Dict.toList
          |> List.map (\(k, v) -> (toField k, encode v))
          |> Internal.Hmset (toKey key),
      hset = \key field val ->
        Internal.Hset (toKey key) (toField field) (encode val),
      hsetnx = \key field val ->
        Internal.Hsetnx (toKey key) (toField field) (encode val)
    }

toDict :: Ord field => (Text -> Maybe field) -> Redis.Decoder a -> List (Text, ByteString) -> Result Internal.Error (Dict.Dict field a)
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
