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
    delete,
    atomicModify,
    atomicModifyWithContext,
    Internal.NamespacedHandler,
    Internal.Error,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Dict
import Nri.Prelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple

-- | Get a value from a namespaced Redis key, assuming it is valid JSON data of
-- the expected type.
-- Returns `Nothing` if no value is set, or if the JSON decoding fails.
get :: Aeson.FromJSON a => Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe a)
get handler key =
  Redis.ByteString.get handler key
    |> Task.map (andThen Aeson.decodeStrict')

-- | Get multiple values from a namespaced Redis key, assuming it is valid JSON
-- data of the expected type.
getMany :: Aeson.FromJSON a => Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text a)
getMany handler keys =
  Redis.ByteString.getMany handler keys
    |> Task.map
      ( Dict.foldr
          ( \key val dict ->
              case Aeson.decodeStrict' val of
                Nothing -> dict
                Just decodedVal -> Dict.insert key decodedVal dict
          )
          Dict.empty
      )

-- | Set the value at a namespaced Redis key with a JSON representation of the value provided.
set :: Aeson.ToJSON a => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error ()
set handler key value =
  Redis.ByteString.set handler key (encodeStrict value)

-- | Set the multiple JSON values with namespaced keys.
setMany :: Aeson.ToJSON a => Internal.NamespacedHandler -> Dict.Dict Text a -> Task Internal.Error ()
setMany handler values =
  Redis.ByteString.setMany
    handler
    (Dict.map (\_key val -> encodeStrict val) values)

-- | Delete the values at all of the provided keys. Return how many of those keys existed
-- (and hence were deleted)
delete :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
delete = Redis.ByteString.delete

-- | Set the namespaced Redis key with JSON representing the provided value,
-- returning the previous value (if any and if it can be decoded to the same type).
getSet :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error (Maybe a)
getSet handler key value =
  Redis.ByteString.getSet handler key (encodeStrict value)
    |> map (andThen Aeson.decodeStrict')

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> a) -> Task Internal.Error a
atomicModify handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen Aeson.decodeStrict'
          |> f
          |> (\res -> (encodeStrict res, res))
    )
    |> Task.map Tuple.second

-- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
atomicModifyWithContext handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen Aeson.decodeStrict'
          |> f
          |> (\res -> (encodeStrict (Tuple.first res), res))
    )
    |> Task.map Tuple.second

encodeStrict :: Aeson.ToJSON a => a -> Data.ByteString.ByteString
encodeStrict x =
  Aeson.encode x
    |> Data.ByteString.Lazy.toStrict
