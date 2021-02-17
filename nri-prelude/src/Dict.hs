-- | A dictionary mapping unique keys to values. The keys can be any comparable
-- type. This includes @Int@, @Float@, @Time@, @Char@, @String@, and tuples or
-- lists of comparable types.
--
-- Insert, remove, and query operations all take _O(log n)_ time.
module Dict
  ( -- * Dictionaries
    Dict,

    -- * Build
    empty,
    singleton,
    insert,
    update,
    remove,

    -- * Query
    isEmpty,
    member,
    get,
    size,

    -- * Lists
    keys,
    values,
    toList,
    fromList,

    -- * Transform
    map,
    foldl,
    foldr,
    filter,
    partition,

    -- * Combine
    union,
    intersect,
    diff,
    merge,
  )
where

import Basics
import qualified Data.Map.Strict
import List (List)
import qualified List
import Maybe (Maybe (..))
import Prelude (Ord, fromIntegral, otherwise)

-- DICTIONARIES

-- | A dictionary of keys and values. So a @Dict String User@ is a dictionary
-- that lets you look up a @String@ (such as user names) and find the associated
-- @User@.
--
-- > import Dict exposing (Dict)
-- >
-- > users : Dict String User
-- > users =
-- >   Dict.fromList
-- >     [ ("Alice", User "Alice" 28 1.65)
-- >     , ("Bob"  , User "Bob"   19 1.82)
-- >     , ("Chuck", User "Chuck" 33 1.75)
-- >     ]
-- >
-- > type alias User =
-- >   { name : String
-- >   , age : Int
-- >   , height : Float
-- >   }
type Dict k v =
  Data.Map.Strict.Map k v

-- | Create an empty dictionary.
empty :: Dict k v
empty =
  Data.Map.Strict.empty

-- | Get the value associated with a key. If the key is not found, return
-- @Nothing@. This is useful when you are not sure if a key will be in the
-- dictionary.
--
-- > animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
-- >
-- > get "Tom"   animals == Just Cat
-- > get "Jerry" animals == Just Mouse
-- > get "Spike" animals == Nothing
get :: Ord comparable => comparable -> Dict comparable v -> Maybe v
get =
  Data.Map.Strict.lookup

-- | Determine if a key is in a dictionary.
member :: Ord comparable => comparable -> Dict comparable v -> Bool
member =
  Data.Map.Strict.member

-- | Determine the number of key-value pairs in the dictionary.
size :: Dict k v -> Int
size =
  Data.Map.Strict.size >> fromIntegral

-- | Determine if a dictionary is empty.
--
-- > isEmpty empty == True
isEmpty :: Dict k v -> Bool
isEmpty =
  Data.Map.Strict.null

-- | Insert a key-value pair into a dictionary. Replaces value when there is
-- a collision.
insert :: Ord comparable => comparable -> v -> Dict comparable v -> Dict comparable v
insert =
  Data.Map.Strict.insert

-- | Remove a key-value pair from a dictionary. If the key is not found,
-- no changes are made.
remove :: Ord comparable => comparable -> Dict comparable v -> Dict comparable v
remove =
  Data.Map.Strict.delete

-- | Update the value of a dictionary for a specific key with a given function.
update :: Ord comparable => comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update targetKey alter dictionary =
  let maybeItemToSet =
        Data.Map.Strict.lookup targetKey dictionary |> alter
   in case maybeItemToSet of
        Just itemToSet ->
          Data.Map.Strict.insert targetKey itemToSet dictionary
        Nothing ->
          Data.Map.Strict.delete targetKey dictionary

-- | Create a dictionary with one key-value pair.
singleton :: comparable -> v -> Dict comparable v
singleton =
  Data.Map.Strict.singleton

-- COMBINE

-- | Combine two dictionaries. If there is a collision, preference is given
-- to the first dictionary.
union :: Ord comparable => Dict comparable v -> Dict comparable v -> Dict comparable v
union =
  Data.Map.Strict.union

-- | Keep a key-value pair when its key appears in the second dictionary.
-- Preference is given to values in the first dictionary.
intersect :: Ord comparable => Dict comparable v -> Dict comparable v -> Dict comparable v
intersect =
  Data.Map.Strict.intersection

-- | Keep a key-value pair when its key does not appear in the second dictionary.
diff :: Ord comparable => Dict comparable a -> Dict comparable b -> Dict comparable a
diff =
  Data.Map.Strict.difference

-- | The most general way of combining two dictionaries. You provide three
-- accumulators for when a given key appears:
--
--  1. Only in the left dictionary.
--  2. In both dictionaries.
--  3. Only in the right dictionary.
--
-- You then traverse all the keys from lowest to highest, building up whatever
-- you want.
merge ::
  Ord comparable =>
  (comparable -> a -> result -> result) ->
  (comparable -> a -> b -> result -> result) ->
  (comparable -> b -> result -> result) ->
  Dict comparable a ->
  Dict comparable b ->
  result ->
  result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
  let stepState rKey rValue (list, result) =
        case list of
          [] ->
            (list, rightStep rKey rValue result)
          (lKey, lValue) : rest
            | lKey < rKey -> stepState rKey rValue (rest, leftStep lKey lValue result)
            | lKey > rKey -> (list, rightStep rKey rValue result)
            | otherwise -> (rest, bothStep lKey lValue rValue result)
      (leftovers, intermediateResult) =
        foldl stepState (toList leftDict, initialResult) rightDict
   in List.foldl (\(k, v) result -> leftStep k v result) intermediateResult leftovers

-- TRANSFORM

-- | Apply a function to all values in a dictionary.
map :: (k -> a -> b) -> Dict k a -> Dict k b
map = Data.Map.Strict.mapWithKey

-- | Fold over the key-value pairs in a dictionary from lowest key to highest key.
--
-- > import Dict exposing (Dict)
-- >
-- > getAges : Dict String User -> List String
-- > getAges users =
-- >   Dict.foldl addAge [] users
-- >
-- > addAge : String -> User -> List String -> List String
-- > addAge _ user ages =
-- >   user.age :: ages
-- >
-- > -- getAges users == [33,19,28]
foldl :: (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fun =
  Data.Map.Strict.foldlWithKey' flippedFun
  where
    flippedFun acc key value = fun key value acc

-- | Fold over the key-value pairs in a dictionary from highest key to lowest key.
--
-- > import Dict exposing (Dict)
-- >
-- > getAges : Dict String User -> List String
-- > getAges users =
-- >   Dict.foldr addAge [] users
-- >
-- > addAge : String -> User -> List String -> List String
-- > addAge _ user ages =
-- >   user.age :: ages
-- >
-- > -- getAges users == [28,19,33]
foldr :: (k -> v -> b -> b) -> b -> Dict k v -> b
foldr = Data.Map.Strict.foldrWithKey

-- | Keep only the key-value pairs that pass the given test.
filter :: (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter = Data.Map.Strict.filterWithKey

-- | Partition a dictionary according to some test. The first dictionary
-- contains all key-value pairs which passed the test, and the second contains
-- the pairs that did not.
partition :: (comparable -> v -> Bool) -> Dict comparable v -> (Dict comparable v, Dict comparable v)
partition = Data.Map.Strict.partitionWithKey

-- LISTS

-- | Get all of the keys in a dictionary, sorted from lowest to highest.
--
-- > keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
keys :: Dict k v -> List k
keys = Data.Map.Strict.keys

-- | Get all of the values in a dictionary, in the order of their keys.
--
-- > values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
values :: Dict k v -> List v
values = Data.Map.Strict.elems

-- | Convert a dictionary into an association list of key-value pairs, sorted by keys.
toList :: Dict k v -> List (k, v)
toList = Data.Map.Strict.toList

-- | Convert an association list into a dictionary.
fromList :: Ord comparable => List (comparable, v) -> Dict comparable v
fromList = Data.Map.Strict.fromList
