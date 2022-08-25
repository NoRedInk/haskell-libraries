module NriPrelude.Plugin.GhcVersionDependent
  ( module GHC.Hs,
    isQualified,
    mkQualified,
  )
where

import GHC.Hs
import Prelude

-- There's more than one way to do a qualified import. See:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/import_qualified_post.html

isQualified :: ImportDecl pass -> Bool
isQualified imp =
  case ideclQualified imp of
    QualifiedPre -> True
    QualifiedPost -> True
    NotQualified -> False

mkQualified :: ImportDeclQualifiedStyle
mkQualified = QualifiedPre
