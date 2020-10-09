module Redis.ByteString
  ( get,
    set,
    getSet,
    getMany,
    setMany,
    delete,
    hSet,
    hGetAll,
    atomicModify,
    atomicModifyWithContext,
  )
where

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe ByteString)
get handler key =
  Internal.get handler (toB key)

-- | Set the value at a namespaced Redis key.
set :: Internal.NamespacedHandler -> Text -> ByteString -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) value

-- | Set the multiple values with namespaced keys.
setMany :: Internal.NamespacedHandler -> Dict.Dict Text ByteString -> Task Internal.Error ()
setMany handler values =
  Internal.setMany
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, v))
    )

-- | Set the value at a namespaced Redis key, returning the previous value (if any)
getSet :: Internal.NamespacedHandler -> Text -> ByteString -> Task Internal.Error (Maybe ByteString)
getSet handler key value =
  Internal.getSet handler (toB key) value

-- | Delete the values at all of the provided keys. Return how many of those keys existed
-- (and hence were deleted)
delete :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
delete handler keys =
  Internal.delete handler (map toB keys)

-- | Get multiple values from  a namespaced Redis key, assuming it is valid UTF8 data.
getMany :: Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text ByteString)
getMany handler keys =
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
                |> Dict.fromList
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

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Internal.NamespacedHandler -> Text -> (Maybe ByteString -> ByteString) -> Task Internal.Error ByteString
atomicModify handler key f =
  atomicModifyWithContext handler key (\x -> (f x, ()))
    |> map Tuple.first

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Internal.NamespacedHandler -> Text -> (Maybe ByteString -> (ByteString, a)) -> Task Internal.Error (ByteString, a)
atomicModifyWithContext handler key f =
  Internal.atomicModify handler (toB key) f

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
hGetAll :: Internal.NamespacedHandler -> Text -> Task Internal.Error [(ByteString, ByteString)]
hGetAll handler key =
  Internal.hGetAll handler (toB key)

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
hSet :: Internal.NamespacedHandler -> Text -> ByteString -> ByteString -> Task Internal.Error ()
hSet handler key field val =
  Internal.hSet handler (toB key) field val

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
