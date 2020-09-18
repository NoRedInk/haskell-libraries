-- | A set of unique values. The values can be any comparable type. This
-- includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
-- of comparable types.
--
-- Insert, remove, and query operations all take *O(log n)* time.
--
-- Sets
-- @docs Set
--
-- Build
-- @docs empty, singleton, insert, remove
--
-- Query
-- @docs isEmpty, member, size
--
-- Combine
-- @docs union, intersect, diff
--
-- Lists
-- @docs toList, fromList
--
-- Transform
-- @docs map, foldl, foldr, filter, partition
module Set
  ( Set,
    empty,
    singleton,
    insert,
    remove,
    isEmpty,
    member,
    size,
    union,
    intersect,
    diff,
    toList,
    fromList,
    map,
    foldl,
    foldr,
    filter,
    partition,
  )
where

import Basics ((>>), Bool, Int, Ord)
import qualified Data.Set
import List (List)
import qualified Prelude

-- | Represents a set of unique values. So `(Set Int)` is a set of integers and
-- `(Set String)` is a set of strings.
type Set t =
  Data.Set.Set t

-- | Create an empty set.
empty :: Set a
empty =
  Data.Set.empty

-- | Create a set with one value.
singleton :: comparable -> Set comparable
singleton =
  Data.Set.singleton

-- | Insert a value into a set.
insert :: Ord comparable => comparable -> Set comparable -> Set comparable
insert =
  Data.Set.insert

-- | Remove a value from a set. If the value is not found, no changes are made.
remove :: Ord comparable => comparable -> Set comparable -> Set comparable
remove =
  Data.Set.delete

-- | Determine if a set is empty.
isEmpty :: Set a -> Bool
isEmpty =
  Data.Set.null

-- | Determine if a value is in a set.
member :: Ord comparable => comparable -> Set comparable -> Bool
member =
  Data.Set.member

-- | Determine the number of elements in a set.
size :: Set a -> Int
size =
  Data.Set.size >> Prelude.fromIntegral

-- | Get the union of two sets, preferring the first set when equal elements are
--  encountered.
--
-- In Elm it's not possible to have two comparable elements that are not equal, but
-- it is possible in Haskell.
union :: Ord comparable => Set comparable -> Set comparable -> Set comparable
union =
  Data.Set.union

-- | Get the intersection of two sets, preferring the first set when equal elements
--  are encountered.
--
-- In Elm it's not possible to have two comparable elements that are not equal, but
-- it is possible in Haskell.
intersect :: Ord comparable => Set comparable -> Set comparable -> Set comparable
intersect =
  Data.Set.intersection

-- | Get the difference between the first set and the second. Keeps values
-- that do not appear in the second set.
diff :: Ord comparable => Set comparable -> Set comparable -> Set comparable
diff =
  Data.Set.difference

-- | Convert a set into a list, sorted from lowest to highest.
toList :: Set a -> List a
toList =
  Data.Set.toAscList

-- | Convert a list into a set, removing any duplicates.
fromList :: Ord comparable => List comparable -> Set comparable
fromList =
  Data.Set.fromList

-- | Fold over the values in a set, in order from lowest to highest.
foldl :: (a -> b -> b) -> b -> Set a -> b
foldl func =
  Data.Set.foldl' (\a b -> func b a)

-- | Fold over the values in a set, in order from highest to lowest.
foldr :: (a -> b -> b) -> b -> Set a -> b
foldr =
  Data.Set.foldr'

-- | Map a function onto a set, creating a new set with no duplicates.
map :: Ord comparable2 => (comparable -> comparable2) -> Set comparable -> Set comparable2
map =
  Data.Set.map

-- | Only keep elements that pass the given test.
--
--    import Set exposing (Set)
--
--    numbers : Set Int
--    numbers =
--      Set.fromList [-2,-1,0,1,2]
--
--    positives : Set Int
--    positives =
--      Set.filter (\x -> x > 0) numbers
--
--    -- positives == Set.fromList [1,2]
filter :: (comparable -> Bool) -> Set comparable -> Set comparable
filter =
  Data.Set.filter

-- | Create two new sets. The first contains all the elements that passed the
-- given test, and the second contains all the elements that did not.
partition :: (comparable -> Bool) -> Set comparable -> (Set comparable, Set comparable)
partition =
  Data.Set.partition
