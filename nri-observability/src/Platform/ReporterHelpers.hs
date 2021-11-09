module Platform.ReporterHelpers (toHashMap, srcString) where

import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified GHC.Stack as Stack
import qualified List
import qualified Platform.AesonHelpers as AesonHelpers
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
    Aeson.Object object ->
      AesonHelpers.foldObject
        (\key value acc -> jsonAsText key value ++ acc)
        HashMap.empty
        object
    val -> jsonAsText "value" val

jsonAsText :: Text -> Aeson.Value -> HashMap.HashMap Text Text
jsonAsText key val =
  case val of
    Aeson.Object dict ->
      AesonHelpers.foldObject
        (\key2 value acc -> jsonAsText (key ++ "." ++ key2) value ++ acc)
        HashMap.empty
        dict
    Aeson.Array vals ->
      Foldable.toList vals
        |> List.indexedMap (\i elem -> jsonAsText (key ++ "." ++ Text.fromInt i) elem)
        |> HashMap.unions
    Aeson.String str -> HashMap.singleton key str
    Aeson.Number n -> HashMap.singleton key (Text.fromList (Prelude.show n))
    Aeson.Bool bool -> HashMap.singleton key (Text.fromList (Prelude.show bool))
    Aeson.Null -> HashMap.empty

srcString :: Stack.SrcLoc -> Text
srcString frame =
  Text.fromList (Stack.srcLocFile frame)
    ++ ":"
    ++ Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine frame))
