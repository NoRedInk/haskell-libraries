{-# LANGUAGE QuasiQuotes #-}

module MySQL.Internal
  ( anyToIn,
  )
where

import qualified Control.Lens as Lens
import qualified Internal.CaselessRegex as R
import Nri.Prelude
import qualified Text

-- | MySQL doesn't support `= ANY`, we can use `IN` instead.
-- We need to write the query with `ANY` though, because postgres-typed is
-- expecting that. The code won't compile otherwise.
anyToIn :: Text -> Text
anyToIn =
  Lens.over
    ( -- Matches `= ANY ('{1,2,3,4}')`
      [R.caselessRegex|=\s*any\s*\(\s*'{.*}'\s*\)|]
        << R.match -- Get the matched text
    )
    -- Replace with `IN (1,2,3,4)
    ( \matched ->
        matched
          |> Text.replace "= any" "IN"
          -- It can also be upper-cased
          |> Text.replace "= ANY" "IN"
          |> Text.replace "'{" ""
          |> Text.replace "}'" ""
    )
