module Redis.Text
  ( -- * Redis commands
    get,
    set,
    getset,
    mget,
    mset,
    del,

    -- * helper functions
    atomicModify,
    atomicModifyWithContext,

    -- * Helper types
    Internal.NamespacedHandler,
    Internal.Error,
  )
where

import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Dict
import Nri.Prelude
import qualified Redis.ByteString
import qualified Redis.Internal as Internal
import qualified Task
import qualified Tuple
import qualified Prelude

-- | Get a value from a namespaced Redis key, assuming it is valid UTF8 data.
-- Returns `Nothing` if no value is set.
get :: Internal.NamespacedHandler -> Text -> Task Internal.Error (Maybe Text)
get handler key =
  Redis.ByteString.get handler key
    |> map (andThen toT)

-- | Set the value at a namespaced Redis key.
set :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error ()
set handler key value =
  Redis.ByteString.set handler key (toB value)

-- | Set the multiple values with namespaced keys.
mset :: Internal.NamespacedHandler -> Dict.Dict Text Text -> Task Internal.Error ()
mset handler values =
  Redis.ByteString.mset
    handler
    (Dict.map (\_key val -> toB val) values)

-- | Set the value at a namespaced Redis key, returning the previous value (if any)
getset :: Internal.NamespacedHandler -> Text -> Text -> Task Internal.Error (Maybe Text)
getset handler key value =
  Redis.ByteString.getset handler key (toB value)
    |> map (andThen toT)

-- | Delete the values at all of the provided keys. Return how many of those keys existed
-- (and hence were deld)
del :: Internal.NamespacedHandler -> [Text] -> Task Internal.Error Int
del = Redis.ByteString.del

-- | Get multiple values from  a namespaced Redis key, assuming it is valid UTF8 data.
mget :: Internal.NamespacedHandler -> List Text -> Task Internal.Error (Dict.Dict Text Text)
mget handler keys =
  Redis.ByteString.mget handler keys
    |> Task.map
      ( Dict.foldl
          ( \key val dict ->
              case toT val of
                Nothing -> dict
                Just textVal -> Dict.insert key textVal dict
          )
          Dict.empty
      )

-- | Retrieve a value from Redis, apply it to the function provided and set the value to the result.
-- This update is guaranteed to be atomic (i.e. no one changed the value between it being read and being set).
-- The returned value is the value that was set.
atomicModify :: Internal.NamespacedHandler -> Text -> (Maybe Text -> Text) -> Task Internal.Error Text
atomicModify handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen toT
          |> f
          |> (\res -> (toB res, res))
    )
    |> Task.map Tuple.second

-- | As `atomicModify`, but allows you to pass contextual information back as well as the new value
-- that was set.
atomicModifyWithContext :: Internal.NamespacedHandler -> Text -> (Maybe Text -> (Text, a)) -> Task Internal.Error (Text, a)
atomicModifyWithContext handler key f =
  Redis.ByteString.atomicModifyWithContext
    handler
    key
    ( \maybeByteString ->
        maybeByteString
          |> andThen toT
          |> f
          |> (\res -> (toB (Tuple.first res), res))
    )
    |> Task.map Tuple.second

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

toT :: Data.ByteString.ByteString -> Maybe Text
toT bs =
  Data.Text.Encoding.decodeUtf8' bs
    |> \r -> case r of
      Prelude.Right t -> Just t
      Prelude.Left _ -> Nothing
