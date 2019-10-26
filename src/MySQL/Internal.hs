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
    ( [R.caselessRegex|(=\s*any)\s*\(\s*('{.*}')\s*\)|]
        --             ^^^^^^^^^    ^^^^^^^^^^^^
        -- "...where id  = ANY      ('{1,2,3,4}')  ..."
        -- Matches       ["= ANY",    "'{1,2,3,4}'"]
        << R.groups
    )
    replaceAny
  where
    -- Replace matched groups with SQL that MySQL understands.
    -- Example groupes from regex:
    --   Groups:    ["= ANY", "'{1,2,3,4}'"]
    --   Converted: ["IN", "1,2,3,4"]
    replaceAny :: [Text] -> [Text]
    replaceAny groups =
      case groups of
        [_, elems] ->
          [ "IN",
            elems
              |> Text.dropLeft 2 -- '{
              |> Text.dropRight 2 -- }'
          ]
        _ -> groups -- Didn't match the right groups.
