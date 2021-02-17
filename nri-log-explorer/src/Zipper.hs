{-# LANGUAGE DeriveFunctor #-}

module Zipper
  ( Zipper,

    -- * Construction and converstion
    fromList,
    singleton,

    -- * Moving focus
    next,
    prev,
    first,
    last,

    -- * Querying
    current,
    currentIndex,
    length,
    toList,

    -- * Manipulation
    Zipper.map,
    indexedMap,
    prepend,
    append,
  )
where

import qualified List
import NriPrelude
import qualified Prelude

data Zipper a = Zipper [a] a [a]
  deriving (Prelude.Functor)

fromList :: [a] -> Maybe (Zipper a)
fromList list =
  case list of
    [] -> Nothing
    first' : rest -> Just (Zipper [] first' rest)

toList :: Zipper a -> [a]
toList (Zipper before current' after) =
  List.reverse before ++ (current' : after)

map :: (a -> b) -> Zipper a -> Zipper b
map = Prelude.fmap

-- The index passed in the mapping function is zero for the current element,
-- positive for the elements after the current element, and negative for the
-- elements before it!
indexedMap :: (Int -> a -> b) -> Zipper a -> Zipper b
indexedMap fn (Zipper before current' after) =
  Zipper
    (List.indexedMap (\i el -> fn (- (1 + i)) el) before)
    (fn 0 current')
    (List.indexedMap (\i el -> fn (1 + i) el) after)

singleton :: a -> Zipper a
singleton item = Zipper [] item []

next :: Zipper a -> Zipper a
next (Zipper before current' []) = Zipper before current' []
next (Zipper before current' (next' : after)) = Zipper (current' : before) next' after

prev :: Zipper a -> Zipper a
prev (Zipper [] current' after) = Zipper [] current' after
prev (Zipper (prev' : before) current' after) = Zipper before prev' (current' : after)

first :: Zipper a -> Zipper a
first zipper =
  case toList zipper of
    [] -> zipper -- this can't happen
    first' : rest -> Zipper [] first' rest

last :: Zipper a -> Zipper a
last zipper =
  case List.reverse (toList zipper) of
    [] -> zipper -- this can't happen
    last' : rest -> Zipper rest last' []

current :: Zipper a -> a
current (Zipper _ current' _) = current'

currentIndex :: Zipper a -> Int
currentIndex (Zipper before _ _) = List.length before

length :: Zipper a -> Int
length (Zipper before _ after) = 1 + List.length before + List.length after

prepend :: List a -> Zipper a -> Zipper a
prepend list (Zipper before current' after) =
  Zipper (before ++ List.reverse list) current' after

append :: Zipper a -> List a -> Zipper a
append (Zipper before current' after) list =
  Zipper before current' (after ++ list)
