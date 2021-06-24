-- |
-- Description : QuasiQuoter for caseless regex
module Internal.CaselessRegex
  ( caselessRegex,
    module Control.Lens.Regex.Text,
  )
where

import Control.Lens.Regex.Text
import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Text.Regex.PCRE.Light as PCRE

caselessRegex :: QuasiQuoter
caselessRegex = mkRegexTraversalQQ [PCRE.caseless]
