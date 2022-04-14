module Data.Aeson.Extra (decodeIntoFlatDict, Path, Segment (..), pathToText) where

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

-- | Turns a path into a json-path like text
--
-- > pathToText [Key "foo", Index 0, Key "bar"]
-- > ==> "foo[0].bar"
pathToText :: Path -> Text
pathToText path =
  case path of
    [] -> ""
    segment : [] -> segmentToText segment
    segment : next : rest ->
      segmentToText segment ++ separator next ++ pathToText (next : rest)

segmentToText :: Segment -> Text
segmentToText (Key k) =
  k
    |> Text.replace "." "\\."
    |> Text.replace "[" "\\["
    |> Text.replace "]" "\\]"
segmentToText (Index idx) = "[" ++ Text.fromInt idx ++ "]"

separator :: Segment -> Text
separator (Key _) = "."
separator (Index _) = ""

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
    Right value -> Ok (valueToDict [] Dict.empty Nothing value)

valueToDict :: Path -> Dict Path Aeson.Value -> Maybe Segment -> Aeson.Value -> Dict Path Aeson.Value
valueToDict path acc maybeSegment val =
  let newPath =
        case maybeSegment of
          Nothing -> path
          Just segment -> segment : path
   in case val of
        Aeson.Object obj ->
          HM.foldlWithKey' (\acc' k -> valueToDict newPath acc' (Just (Key k))) acc obj
        Aeson.Array arr -> arrayToDict newPath acc arr
        _ -> Dict.insert (List.reverse newPath) val acc

arrayToDict :: Path -> Dict Path Aeson.Value -> Aeson.Array -> Dict Path Aeson.Value
arrayToDict path =
  Vector.ifoldl
    ( \acc index item ->
        valueToDict
          path
          acc
          (Just (Index (Prelude.fromIntegral index)))
          item
    )
