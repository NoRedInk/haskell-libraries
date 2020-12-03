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
    Api (..),
    makeHashApi,
    HashApi (..),
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
import qualified Result
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
      { del :: List.List key -> Internal.Query Int,
        expire :: key -> Int -> Internal.Query (),
        get :: key -> Internal.Query (Maybe a),
        getset :: key -> a -> Internal.Query (Maybe a),
        mget :: Ord key => List.List key -> Internal.Query (Dict.Dict key a),
        mset :: Dict.Dict key a -> Internal.Query (),
        ping :: Internal.Query (),
        rpush :: key -> List.List a -> Internal.Query Int,
        set :: key -> a -> Internal.Query (),
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
      rpush = \key vals ->
        Internal.Rpush (toKey key) (List.map encode vals),
      set = \key value -> Internal.Set (toKey key) (encode value),
      setnx = \key value -> Internal.Setnx (toKey key) (encode value)
    }

data HashApi key field a
  = HashApi
      { hdel :: key -> List.List field -> Internal.Query Int,
        hget :: key -> field -> Internal.Query (Maybe a),
        hgetall :: key -> Internal.Query (Dict.Dict field a),
        hmget :: key -> List.List field -> Internal.Query (Dict.Dict field a),
        hmset :: key -> Dict.Dict field a -> Internal.Query (),
        hset :: key -> field -> a -> Internal.Query (),
        hsetnx :: key -> field -> a -> Internal.Query Bool
      }

makeHashApi ::
  Ord field =>
  Encoder a ->
  Decoder a ->
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

toDict :: Ord field => (Text -> Maybe field) -> Decoder a -> List (Text, ByteString) -> Result Internal.Error (Dict.Dict field a)
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
