{-# LANGUAGE CPP #-}

-- | This module supports working with aeson objects in a way compatible with
-- both the 1.x and 2.x versions of the aseon library.
--
-- Aeson has a Value type for representing JSON values. The Value type has
-- constructors for JSON strings, numbers, arrays, and objects. Aeson represents
-- JSON objects using an Object type, which in versions 1.x of aeson is a type
-- alias for a HashMap. Version 2.x of library make Object an opaque type.
--
-- nri-observability needs to perform some operations on JSON objects. Depending
-- on which version of aeson we got, we need to use different functions to
-- perform these operations. This module contains implementations for both 1.x
-- and 2.x versions of Aeson, and automatically picks the right version
-- depending on which version of the library is detected.
--
-- Once we're done supporting versions 1.x of aeson we can drop this module and
-- inline the 2.x versions of functions wherever they are called.
module Platform.AesonHelpers (foldObject, mergeObjects) where

#if MIN_VERSION_aeson(2,0,0)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

foldObject :: (Text -> Aeson.Value -> acc -> acc) -> acc -> Aeson.Object -> acc
foldObject fn = KeyMap.foldrWithKey (\key val acc -> fn (Key.toText key) val acc)

mergeObjects ::
  (Aeson.Value -> Aeson.Value -> Aeson.Value) ->
  Aeson.Object ->
  Aeson.Object ->
  Aeson.Object
mergeObjects = KeyMap.unionWith

#else

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

foldObject :: (Text -> Aeson.Value -> acc -> acc) -> acc -> Aeson.Object -> acc
foldObject = HashMap.foldrWithKey

mergeObjects ::
  (Aeson.Value -> Aeson.Value -> Aeson.Value) ->
  Aeson.Object ->
  Aeson.Object ->
  Aeson.Object
mergeObjects merge object1 object2 = HashMap.unionWith merge object1 object2

#endif
