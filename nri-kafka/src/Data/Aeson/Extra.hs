module Data.Aeson.Extra (decodeIntoFlatDict, Path, Segment (..)) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector
import Dict (Dict)
import qualified Dict
import Prelude (Either (Left, Right))
import qualified Prelude

data Segment = Key Text | Index Int
  deriving (Ord, Eq, Show)

type Path = List Segment

-- | Decodes JSON into a flat dict.
--
-- > decodeIntoFlatDict "{\"a\": {\"b|": 1}}"
-- > ==> Ok (Dict.fromList [("a.b", 1)])
--
-- > decodeIntoFlatDict "{\"a\": [1,2,3]}"
-- > ==> Ok (Dict.fromList [("a[0]", 1), ("a[1]", 2), ("a[2]", 3)])
decodeIntoFlatDict :: ByteString -> Result Text (Dict Path Aeson.Value)
decodeIntoFlatDict content =
  case Aeson.eitherDecodeStrict content of
    Left err -> Err (Text.fromList err)
    Right value ->
      case value of
        Aeson.Object obj ->
          obj
            |> HM.foldlWithKey' (\acc' k -> objectToDict [] acc' (Key k)) Dict.empty
            |> Ok
        Aeson.Array arr -> Ok (arrayToDict [] Dict.empty arr)
        _ -> Err "We can only parse top-level objects or arrays"

objectToDict :: Path -> Dict Path Aeson.Value -> Segment -> Aeson.Value -> Dict Path Aeson.Value
objectToDict path acc segment val =
  case val of
    Aeson.Object obj ->
      HM.foldlWithKey' (\acc' k -> objectToDict (segment : path) acc' (Key k)) acc obj
    Aeson.Array arr -> arrayToDict (segment : path) acc arr
    _ -> Dict.insert (List.reverse (segment : path)) val acc

arrayToDict :: Path -> Dict Path Aeson.Value -> Aeson.Array -> Dict Path Aeson.Value
arrayToDict path =
  Vector.ifoldl
    ( \acc index item ->
        objectToDict
          path
          acc
          (Index <| Prelude.fromIntegral index)
          item
    )
