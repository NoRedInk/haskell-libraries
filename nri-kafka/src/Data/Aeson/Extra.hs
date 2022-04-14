module Data.Aeson.Extra (decodeIntoFlatDict) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector
import Dict (Dict)
import qualified Dict
import Prelude (Either (Left, Right))
import qualified Prelude

-- | Decodes JSON into a flat dict.
--
-- > decodeIntoFlatDict "{\"a\": {\"b|": 1}}"
-- > ==> Ok (Dict.fromList [("a.b", 1)])
--
-- > decodeIntoFlatDict "{\"a\": [1,2,3]}"
-- > ==> Ok (Dict.fromList [("a[0]", 1), ("a[1]", 2), ("a[2]", 3)])
decodeIntoFlatDict :: ByteString -> Result Text (Dict Text Aeson.Value)
decodeIntoFlatDict content =
  case Aeson.eitherDecodeStrict content of
    Left err -> Err (Text.fromList err)
    Right value ->
      case value of
        Aeson.Object obj ->
          obj
            |> HM.foldlWithKey' (objectToDict identity) Dict.empty
            |> Ok
        Aeson.Array arr -> Ok (arrayToDict identity Dict.empty arr)
        _ -> Err "We can only parse top-level objects or arrays"

objectToDict :: (Text -> Text) -> Dict Text Aeson.Value -> Text -> Aeson.Value -> Dict Text Aeson.Value
objectToDict toKey acc key val =
  case val of
    Aeson.Object obj ->
      HM.foldlWithKey' (objectToDict (\subKey -> toKey (key ++ "." ++ subKey))) acc obj
    Aeson.Array arr -> arrayToDict (\subKey -> toKey (key ++ subKey)) acc arr
    _ -> Dict.insert (toKey key) val acc

arrayToDict :: (Text -> Text) -> Dict Text Aeson.Value -> Aeson.Array -> Dict Text Aeson.Value
arrayToDict toKey =
  Vector.ifoldl
    ( \acc index item ->
        objectToDict
          (\index' -> toKey ("[" ++ index' ++ "]"))
          acc
          (Text.fromInt (Prelude.fromIntegral index))
          item
    )
