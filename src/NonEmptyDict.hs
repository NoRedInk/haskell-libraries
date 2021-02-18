module NonEmptyDict
  ( NonEmptyDict,
    fromDict,
    toDict,
    toNonEmptyList,
    init,
    keys,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Dict

data NonEmptyDict k v
  = NonEmptyDict (k, v) (Dict.Dict k v)
  deriving (Show)

fromDict :: Ord k => Dict.Dict k v -> Maybe (NonEmptyDict k v)
fromDict dict =
  case Dict.toList dict of
    [] -> Nothing
    (k, v) : _ -> Just <| init k v dict

toDict :: Ord k => NonEmptyDict k v -> Dict k v
toDict (NonEmptyDict (k, v) dict) =
  Dict.insert k v dict

toNonEmptyList :: NonEmptyDict k v -> NonEmpty (k, v)
toNonEmptyList (NonEmptyDict kv dict) =
  kv :| Dict.toList dict

init :: Ord k => k -> v -> Dict.Dict k v -> NonEmptyDict k v
init key val dict =
  NonEmptyDict (key, val) (Dict.remove key dict)
