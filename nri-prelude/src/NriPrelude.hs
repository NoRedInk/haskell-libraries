{-# OPTIONS_HADDOCK not-home #-}

-- | These are functions and types that in Elm are in scope by default, without
-- needing to import anything. In Haskell we refer to such imports as a
-- "Prelude".
module NriPrelude
  ( -- * Elm-like Prelude
    Platform.Internal.Task,
    module Basics,
    module Internal.Shortcut,
    List.List,
    Maybe.Maybe (..),
    Result.Result (..),
    Text.Text,
    Char.Char,

    -- * Other essentials
    Prelude.Show,
    GHC.Generics.Generic,
    fmap,
    (<*>),
    (>>=),
  )
where

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
  ( fmap,
    (<*>),
    (>>=),
  )
import qualified Prelude
