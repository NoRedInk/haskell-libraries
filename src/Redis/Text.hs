module Redis.Text
  ( get,
    set,
    getSet,
    getMany,
    setMany,
    delete,
    atomicModify,
    atomicModifyWithContext,
  )
where

import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import Nri.Prelude
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple
import qualified Prelude

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe Text)
get handler key =
  Internal.get handler (toB key)
    |> map (andThen toT)

-- | Set the value at a namespaced Redis key.
set :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error ()
set handler key value =
  Internal.set handler (toB key) (toB value)

-- | Set the multiple values with namespaced keys.
setMany :: Internal.NamespacedHandler -> Dict.Dict Text Text -> Task Internal.Error ()
setMany handler values =
  Internal.setMany
    handler
    ( values
        |> Dict.toList
        |> List.map (\(k, v) -> (toB k, toB v))
    )

-- | Set the value at a namespaced Redis key, returning the previous value (if any)
getSet :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error (Maybe Text)
getSet handler key value =
  Internal.getSet handler (toB key) (toB value)
    |> map (andThen toT)

-- | Delete the values at all of the provided keys. Return how many of those keys existed
-- (and hence were deleted)
delete :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
delete handler keys =
  Internal.delete handler (map toB keys)

-- | Get multiple values from  a namespaced Redis key, assuming it is valid UTF8 data.
getMany :: Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text Text)
getMany handler keys =
  getManyInternal handler keys
    |> Task.map
      ( List.filterMap
          ( \(key, value) ->
              value
                |> toT
                |> map (\x -> (key, x))
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
        |> (\x -> (x, ()))

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

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

toT :: Data.ByteString.ByteString -> Maybe Text
toT bs =
  Data.Text.Encoding.decodeUtf8' bs
    |> \r -> case r of
      Prelude.Right t -> Just t
      Prelude.Left _ -> Nothing

-- We should only use this function when we created
-- the ByteString ourselves and know it to be UTF8
knowT :: Data.ByteString.ByteString -> Text
knowT = Data.Text.Encoding.decodeUtf8
