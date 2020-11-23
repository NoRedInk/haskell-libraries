{-# LANGUAGE RankNTypes #-}

-- | A module that automatically converts types to JSON prior to storing them in
-- Redis, and parses them back from JSON when reading.
--
-- When data is found in Redis but cannot be decoded these functions will return
-- an error.
--
-- TODO expose only one module `module Redis (fromCordec, Codec, Api, jsonCodec, textCodec) where`
module Redis.Generic
  ( -- * Creating api access functions
    makeApi,
    Api (..),
    Codec (..),
    jsonCodec,

    -- * Running queries
    Internal.Query,
    Internal.query,
    Internal.transaction,
    Internal.Handler,
    Internal.Error,
    Internal.map,
    watch,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Dict
import qualified List
import NriPrelude
import qualified Redis.Internal as Internal
import qualified Prelude

data Codec a
  = Codec
      { encode :: a -> Data.ByteString.ByteString,
        decode :: Data.ByteString.ByteString -> Result Internal.Error a
      }

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

makeApi :: (key -> Text) -> Codec a -> Api key a
makeApi toKey Codec {encode, decode} =
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

jsonCodec :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
jsonCodec =
  Codec
    { encode = Aeson.encode >> Data.ByteString.Lazy.toStrict,
      decode = \byteString ->
        case Aeson.eitherDecodeStrict' byteString of
          Prelude.Right decoded -> Ok decoded
          Prelude.Left err ->
            Internal.DecodingError (Data.Text.pack err)
              |> Err
    }

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
