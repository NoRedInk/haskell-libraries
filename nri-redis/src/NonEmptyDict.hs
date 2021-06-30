-- | A simple NonEmpty dict wrapper to protect us from writing invalid empty Dicts
-- to Redis Hashes.
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

-- | A Dict with at least one entry. For use in writing to redis, where it's an
-- error when writing nothing
data NonEmptyDict k v
  = NonEmptyDict (k, v) (Dict.Dict k v)
  deriving (Show)

-- | tries to create a 'NonEmptyDict' from a 'Dict'
fromDict :: Ord k => Dict.Dict k v -> Maybe (NonEmptyDict k v)
fromDict dict =
  case Dict.toList dict of
    [] -> Nothing
    (k, v) : _ -> Just <| init k v dict

-- | creates a 'Dict' from a 'NonEmptyDict'
toDict :: Ord k => NonEmptyDict k v -> Dict k v
toDict (NonEmptyDict (k, v) dict) =
  Dict.insert k v dict

-- | creates a 'Dict' from a 'NonEmptyDict'
toNonEmptyList :: NonEmptyDict k v -> NonEmpty (k, v)
toNonEmptyList (NonEmptyDict kv dict) =
  kv :| Dict.toList dict

-- | creates a `Dict` from a key, value, and dict
init :: Ord k => k -> v -> Dict.Dict k v -> NonEmptyDict k v
init key val dict =
  NonEmptyDict (key, val) (Dict.remove key dict)
