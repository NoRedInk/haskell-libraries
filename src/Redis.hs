{-# LANGUAGE TupleSections #-}

-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- We make some assumptions in this module that are worth bearing in mind:
-- - As with our Ruby Redis access, we enforce working within a "namespace",
--   so most functions take a "NamespacedHandler".
-- - Data stored in Redis should be, by definition, transient and disposable.
--   Because of that, we are very lenient in how we handle decoding of data:
--   if we find invalid or out of date data stored in a key, we treat that the
--   same as if the key is empty. This avoids issues with things like changes
--   of JSON representations of data - we just throw the old data away!
module Redis
  ( get,
    getJSON,
    set,
    setJSON,
    getSet,
    getSetJSON,
    getMany,
    getManyJSON,
    setMany,
    setManyJSON,
    delete,
    atomicModify,
    atomicModifyJSON,
    atomicModifyWithContext,
    atomicModifyWithContextJSON,
    -- Settings
    Settings.Settings,
    Settings.decoder,
    -- Internal
    Internal.Error (..),
    Internal.Handler,
    Internal.NamespacedHandler,
    Internal.namespacedHandler,
    -- Real
    Real.handler,
    readiness,
  )
where

import Cherry.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.List (zip)
import qualified Data.Text.Encoding
import qualified Dict
import Dict (Dict)
import qualified Health
import qualified List
import List (List)
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task
import qualified Tuple
import Prelude (Either (Left, Right))

toT :: Data.ByteString.ByteString -> Maybe Text
toT bs =
  Data.Text.Encoding.decodeUtf8' bs
    |> \r -> case r of
      Right t -> Just t
      Left _ -> Nothing

-- We should only use this function when we created
-- the ByteString ourselves and know it to be UTF8
knowT :: Data.ByteString.ByteString -> Text
knowT = Data.Text.Encoding.decodeUtf8

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe Text)
get handler key =
  Internal.get handler (toB key)
    |> map (andThen toT)

-- | Get a value from a namespaced Redis key, assuming it is valid JSON data of
-- the expected type.
-- Returns `Nothing` if no value is set, or if the JSON decoding fails.
getJSON :: Aeson.FromJSON a => Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe a)
getJSON handler key =
  Internal.get handler (toB key)
    |> map (andThen Aeson.decodeStrict')

-- | Get multiple values from  a namespaced Redis key, assuming it is valid UTF8 data.
getMany :: Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict Text Text)
getMany handler keys =
  getManyInternal handler keys
    |> Task.map
      ( List.filterMap
          ( \(key, value) ->
              value
                |> toT
                |> map (key,)
          )
          >> Dict.fromList
      )

-- | Get multiple values from a namespaced Redis key, assuming it is valid JSON
-- data of the expected type.
getManyJSON :: Aeson.FromJSON a => Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict Text a)
getManyJSON handler keys =
  getManyInternal handler keys
    |> Task.map
      ( List.filterMap
          ( \(key, value) ->
              value
                |> Aeson.decodeStrict'
                |> map (key,)
          )
          >> Dict.fromList
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
              zip keys values
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

-- | Set the value at a namespaced Redis key.
set :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) (toB value)

-- | Set the value at a namespaced Redis key with a JSON representation of the value provided.
setJSON :: Aeson.ToJSON a => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error ()
setJSON handler key value =
  Internal.set handler (toB key) (Aeson.encode value |> Lazy.toStrict)

-- | Set the multiple values with namespaced keys.
setMany :: Internal.NamespacedHandler -> Dict Text Text -> Task Internal.Error ()
setMany handler values =
  Internal.setMany
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, toB v))
    )

-- | Set the multiple JSON values with namespaced keys.
setManyJSON :: Aeson.ToJSON a => Internal.NamespacedHandler -> Dict Text a -> Task Internal.Error ()
setManyJSON handler values =
  Internal.setMany
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, Aeson.encode v |> Lazy.toStrict))
    )

-- | Set the value at a namespaced Redis key, returning the previous value (if any)
getSet :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error (Maybe Text)
getSet handler key value =
  Internal.getSet handler (toB key) (toB value)
    |> map (andThen toT)

-- | Set the namespaced Redis key with JSON representing the provided value,
-- returning the previous value (if any and if it can be decoded to the same type).
getSetJSON :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> a -> Task Internal.Error (Maybe a)
getSetJSON handler key value =
  Internal.getSet handler (toB key) (Aeson.encode value |> Lazy.toStrict)
    |> map (andThen Aeson.decodeStrict')

-- | Delete the values at all of the provided keys. Return how many of those keys existed
-- (and hence were deleted)
delete :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
delete handler keys =
  Internal.delete handler (map toB keys)

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Internal.NamespacedHandler -> Text -> (Maybe Text -> Text) -> Task Internal.Error Text
atomicModify handler key f =
  Internal.atomicModify handler (toB key) wrapAndUnwrap
    |> map (Tuple.first >> knowT)
  where
    wrapAndUnwrap bs =
      bs
        |> andThen toT
        |> f
        |> toB
        |> (,())

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModifyJSON :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> a) -> Task Internal.Error a
atomicModifyJSON handler key f =
  Internal.atomicModify
    handler
    (toB key)
    ( andThen Aeson.decodeStrict'
        >> f
        >> (Aeson.encode >> Lazy.toStrict >> (,()))
    )
    |> andThen
      ( \(bs, _) -> case bs |> Aeson.decodeStrict' of
          Just v -> Task.succeed v
          Nothing ->
            Task.fail
              <| Internal.LibraryError "We failed to decode the ByteStream we successfully wrote; this should never happen and indicates a bug in our Redis library or our JSON encoding/decoding."
      )

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Internal.NamespacedHandler -> Text -> (Maybe Text -> (Text, a)) -> Task Internal.Error (Text, a)
atomicModifyWithContext handler key f =
  Internal.atomicModify handler (toB key) wrapAndUnwrap
    |> map (Tuple.mapFirst knowT)
  where
    wrapAndUnwrap bs =
      bs
        |> andThen toT
        |> f
        |> Tuple.mapFirst toB

-- | As `atomicModifyJSON`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContextJSON :: (Aeson.FromJSON a, Aeson.ToJSON a) => Internal.NamespacedHandler -> Text -> (Maybe a -> (a, b)) -> Task Internal.Error (a, b)
atomicModifyWithContextJSON handler key f =
  Internal.atomicModify
    handler
    (toB key)
    ( andThen Aeson.decodeStrict'
        >> f
        >> Tuple.mapFirst (Aeson.encode >> Lazy.toStrict)
    )
    |> andThen
      ( \(bs, context) -> case bs |> Aeson.decodeStrict' of
          Just v -> Task.succeed (v, context)
          Nothing ->
            Task.fail
              <| Internal.LibraryError "We failed to decode the ByteStream we successfully wrote; this should never happen and indicates a bug in our Redis library or our JSON encoding/decoding."
      )

-- |
-- Check that we are ready to be take traffic.
readiness :: Internal.Handler -> Health.Check
readiness handler =
  Health.mkCheck "redis" <| do
    log <- Platform.silentHandler
    Internal.rawPing handler
      |> Task.map (\_ -> Health.Good)
      |> Task.onError (\err -> Task.succeed (Health.Bad (Internal.errorForHumans err)))
      |> Task.perform log
