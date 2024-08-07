module Platform.AesonHelpers (foldObject, singleton) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

foldObject :: (Text -> Aeson.Value -> acc -> acc) -> acc -> Aeson.Object -> acc
foldObject fn = KeyMap.foldrWithKey (\key val acc -> fn (Key.toText key) val acc)

singleton :: Text -> Aeson.Value -> Aeson.Object
singleton key = KeyMap.singleton (Key.fromText key)
