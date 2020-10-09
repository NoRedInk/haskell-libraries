-- | A module that automatically converts types to JSON prior to storing them in
-- Redis, and parses them back from JSON when reading.
--
-- Data stored in Redis should be, by definition, transient and disposable.
-- Because of that, we are very lenient in how we handle decoding of data:
-- if we find invalid or out of date data stored in a key, we treat that the
-- same as if the key is empty. This avoids issues with things like changes
-- of JSON representations of data - we just throw the old data away!
module Redis.Json
  ( get,
    set,
    getSet,
    getMany,
    setMany,
    atomicModify,
    atomicModifyWithContext,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple

-- | Get a value from a namespaced Redis key, assuming it is valid JSON data of
-- the expected type.
-- Returns `Nothing` if no value is set, or if the JSON decoding fails.
get :: Aeson.FromJSON a => Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe a)
get handler key =
  key
    |> toB
    |> Internal.get handler
    |> map (andThen Aeson.decodeStrict')

-- | Get multiple values from a namespaced Redis key, assuming it is valid JSON
-- data of the expected type.
getMany :: Aeson.FromJSON a => Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text a)
getMany handler keys =
  getManyInternal handler keys
    |> Task.map
      ( List.filterMap
          ( \(key, value) ->
              value
                |> Aeson.decodeStrict'
                |> map (\decoded -> (key, decoded))
          )
          >> Dict.fromList
      )

-- | Set the value at a namespaced Redis key with a JSON representation of the value provided.
set :: Aeson.ToJSON a => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) (Aeson.encode value |> Data.ByteString.Lazy.toStrict)

-- | Set the multiple JSON values with namespaced keys.
setMany :: Aeson.ToJSON a => Internal.NamespacedHandler -> Dict.Dict Text a -> Task Internal.Error ()
setMany handler values =
  Internal.setMany
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, Aeson.encode v |> Data.ByteString.Lazy.toStrict))
    )

-- | Set the namespaced Redis key with JSON representing the provided value,
-- returning the previous value (if any and if it can be decoded to the same type).
getSet :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error (Maybe a)
getSet handler key value =
  Internal.getSet handler (toB key) (Aeson.encode value |> Data.ByteString.Lazy.toStrict)
    |> map (andThen Aeson.decodeStrict')

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> a) -> Task Internal.Error a
atomicModify handler key f =
  Internal.atomicModify
    handler
    (toB key)
    ( andThen Aeson.decodeStrict'
        >> f
        >> (Aeson.encode >> Data.ByteString.Lazy.toStrict >> \x -> (x, ()))
    )
    |> andThen
      ( \(bs, _) -> case bs |> Aeson.decodeStrict' of
          Just v -> Task.succeed v
          Nothing ->
            Task.fail
              <| Internal.LibraryError "We failed to decode the ByteStream we successfully wrote; this should never happen and indicates a bug in our Redis library or our JSON encoding/decoding."
      )

-- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
atomicModifyWithContext handler key f =
  Internal.atomicModify
    handler
    (toB key)
    ( andThen Aeson.decodeStrict'
        >> f
        >> Tuple.mapFirst (Aeson.encode >> Data.ByteString.Lazy.toStrict)
    )
    |> andThen
      ( \(bs, context) -> case bs |> Aeson.decodeStrict' of
          Just v -> Task.succeed (v, context)
          Nothing ->
            Task.fail
              <| Internal.LibraryError "We failed to decode the ByteStream we successfully wrote; this should never happen and indicates a bug in our Redis library or our JSON encoding/decoding."
      )

getManyInternal :: Internal.NamespacedHandler -> List Text -> Task Internal.Error [(Text, Data.ByteString.ByteString)]
getManyInternal handler keys =
  keys
    |> List.map toB
    |> Internal.getMany handler
    |> andThen
      ( \values ->
          if List.length keys == List.length values
            then
              List.map2 (,) keys values
                |> List.filterMap
                  ( \(key, value) ->
                      case value of
                        Nothing -> Nothing
                        Just v -> Just (key, v)
                  )
                |> Task.succeed
            else
              Task.fail
                ( Internal.LibraryError
                    "We got a mismatch in the size of keys and values when post-processing the \
                    \results of an mget command. Redis guarantees this shouldn't happen, so a \
                    \mismatch here means that we did something wrong and continuing could mean \
                    \building an incorrect mapping."
                )
      )

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
