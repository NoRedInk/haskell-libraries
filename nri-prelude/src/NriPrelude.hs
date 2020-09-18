{-# OPTIONS_HADDOCK not-home #-}

-- | These are functions and types that in Elm are in scope by default, without
-- needing to import anything. In Haskell we refer to such imports as a
-- "Prelude".
module NriPrelude
  ( -- * Elm Prelude
    Platform.Internal.Task,
    module Basics,
    module Internal.Shortcut,
    List.List,
    Maybe.Maybe (..),
    Result.Result (..),
    Text.Text,
    Char.Char,

    -- * The following exports are Non-Elm, but we can't really do without them.
    Prelude.Show,
    GHC.Generics.Generic,

    -- * We're exposing these so users can define custom Functor, Applicative, and
    -- Monad instances. If you use them outside of type class instance definitions
    -- hlint should ask you to replace them with Elm-ish functions instead.
    fmap,
    (<*>),
    (>>=),
  )
where

-- Elm implicitly imports a variety of names into each module. There isn't a
-- formal "prelude" like in Haskell; it's defined in the language. See
-- https://package.elm-lang.org/packages/elm/core/latest/ for the full list.
--
--   import Basics exposing (..)
--   import List exposing (List, (::))
--   import Maybe exposing (Maybe(..))
--   import Result exposing (Result(..))
--   import String exposing (String)
--   import Char exposing (Char)
--   import Tuple
--   import Debug
import Basics
import qualified Char
import qualified GHC.Generics
import Internal.Shortcut
import qualified List
import qualified Maybe
import qualified Platform.Internal
import qualified Result
import qualified Text
import Prelude
  ( (<*>),
    (>>=),
    fmap,
  )
import qualified Prelude
