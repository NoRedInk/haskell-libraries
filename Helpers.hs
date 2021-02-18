module Observability.Helpers (toHashMap) where

import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text
import qualified List
import qualified Text
import qualified Prelude

-- | Our span details are arbitrary JSON structures, but some of our reporters
-- require metadata as a flast list of key,value pairs.
--
-- Given a type that has the following JSON representation:
--
--     {
--       "treasure": {
--         "coords: { "x": 12, "y" 14 },
--         "worth": "Tons!"
--       }
--     }
--
-- This function will create a flat list of key,value pairs like this:
--
--     HashMap.fromList
--       [ ("treasure.coords.x", "12"   )
--       , ("treasure.coords.y", "14"   )
--       , ("treasure.worth"   , "Tons!")
--       ]
toHashMap :: Aeson.ToJSON a => a -> HashMap.HashMap Text Text
toHashMap x =
  case Aeson.toJSON x of
    Aeson.Object dict ->
      HashMap.foldlWithKey'
        (\acc key value -> acc ++ jsonAsText key value)
        HashMap.empty
        dict
    val -> jsonAsText "value" val

jsonAsText :: Text -> Aeson.Value -> HashMap.HashMap Text Text
jsonAsText key val =
  case val of
    Aeson.Object dict ->
      HashMap.foldlWithKey'
        (\acc key2 value -> acc ++ jsonAsText (key ++ "." ++ key2) value)
        HashMap.empty
        dict
    Aeson.Array vals ->
      Foldable.toList vals
        |> List.indexedMap (\i elem -> jsonAsText (key ++ "." ++ Text.fromInt i) elem)
        |> HashMap.unions
    Aeson.String str -> HashMap.singleton key str
    Aeson.Number n -> HashMap.singleton key (Data.Text.pack (Prelude.show n))
    Aeson.Bool bool -> HashMap.singleton key (Data.Text.pack (Prelude.show bool))
    Aeson.Null -> HashMap.empty
