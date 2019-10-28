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
  Lens.over
    ( [R.caselessRegex|\b(in)\s+\((.*)\)|]
        --                ^^      ^^^^
        -- "...where id   IN    (1,2,3,4)  ..."
        -- Matches      ["IN",  "1,2,3,4"]
        << R.groups
    )
    replaceIn
  where
    -- Replace matched groups with SQL that MySQL understands.
    -- Example groupes from regex:
    --   Groups:    ["IN", "1,2,3,4"]
    --   Converted: ["= ANY", "1,2,3,4"]
    replaceIn :: [Text] -> [Text]
    replaceIn groups =
      case groups of
        [_, elems] -> ["= ANY", elems]
        _ -> groups -- Didn't match the right groups.

-- | MySQL doesn't support `= ANY`, we can use `IN` when running the query.
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
