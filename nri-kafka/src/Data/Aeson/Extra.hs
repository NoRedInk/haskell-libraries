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
    Right value -> Ok (valueToDict [] value Nothing)

valueToDict :: Path -> Aeson.Value -> Maybe Segment -> Dict Path Aeson.Value
valueToDict path val maybeSegment =
  let newPath =
        case maybeSegment of
          Nothing -> path
          Just segment -> segment : path
   in case val of
        Aeson.Array arr -> arrayToDict newPath arr
        Aeson.Object obj ->
          HM.foldlWithKey'
            ( \acc k v ->
                Just (Key k)
                  |> valueToDict newPath v
                  |> Dict.union acc
            )
            Dict.empty
            obj
        _ -> Dict.singleton (List.reverse newPath) val

arrayToDict :: Path -> Aeson.Array -> Dict Path Aeson.Value
arrayToDict path =
  Vector.ifoldl
    ( \acc index item ->
        Index (Prelude.fromIntegral index)
          |> Just
          |> valueToDict path item
          |> Dict.union acc
    )
    Dict.empty
