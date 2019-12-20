{-# LANGUAGE QuasiQuotes #-}

module MySQL.Internal
  ( anyToIn,
    inToAny,
  )
where

import qualified Control.Lens as Lens
import qualified Internal.CaselessRegex as R
import Cherry.Prelude
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
    ( [R.caselessRegex|(=\s*any)\s*\(\s*('{.*}')\s*\)|] << R.groups
      --             ^^^^^^^^^    ^^^^^^^^^^^^
      -- "...where id  = ANY      ('{1,2,3,4}')  ..."
      -- Matches       ["= ANY",    "'{1,2,3,4}'"]
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
