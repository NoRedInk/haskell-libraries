{-# LANGUAGE QuasiQuotes #-}

module MySQL.Internal
  ( anyToIn,
    inToAny,
  )
where

import qualified Control.Lens as Lens
import qualified Internal.CaselessRegex as R
import Nri.Prelude
import qualified Text

-- | MySQL doesn't support `= ANY`, we can use `IN` during compilation.
inToAny :: Text -> Text
inToAny =
  Lens.set
    ( [R.caselessRegex|\b(in)\s+\(\${.*}\)|] << R.group 0
      --                ^^      ^^^^^^^^
      -- "...where id   IN      (${ids})  ..."
      -- Matches       "IN"
    )
    -- Replace `IN` with `= ANY`
    "= ANY"

-- | MySQL doesn't support `= ANY`, we can use `IN` when running the query.
anyToIn :: Text -> Text
anyToIn =
  Lens.over
    ( [R.caselessRegex|(\s+[^\s-]+\s+)(=\s*any)\s*\(\s*('{.*}')\s*\)|] << R.groups
      --             ^^^^^^^^^    ^^^^^^^^^^^^
      -- "...where id  = ANY      ('{1,2,3,4}')  ..."
      -- Matches ["id", "= ANY",  "'{1,2,3,4}'"]
    )
    replaceAny
  where
    -- Replace matched groups with SQL that MySQL understands.
    -- Example groupes from regex:
    --   Groups:    ["id", "= ANY", "'{1,2,3,4}'"]
    --   Converted: ["id", "IN", "1,2,3,4"]
    replaceAny :: [Text] -> [Text]
    replaceAny groups =
      case groups of
        [field, _, "'{}'"] ->
          [field, "!=", field]
        [field, _, elems] ->
          [ field,
            "IN",
            elems
              |> Text.dropLeft 2 -- '{
              |> Text.dropRight 2 -- }'
          ]
        _ -> groups -- Didn't match the right groups.
