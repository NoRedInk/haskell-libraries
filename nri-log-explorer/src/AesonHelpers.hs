{-# LANGUAGE CPP #-}

-- | This module supports working with aeson objects in a way compatible with
-- both the 1.x and 2.x versions of the aeson library.
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
module AesonHelpers (hashMapFromObject, textFromKey) where

import qualified Data.Aeson.Types as Types
import qualified Data.HashMap.Strict as HashMap

#if MIN_VERSION_aeson(2,0,0)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

hashMapFromObject :: Types.Object -> HashMap.HashMap Key.Key Types.Value
hashMapFromObject object = KeyMap.toHashMap object

textFromKey :: Key.Key -> Text
textFromKey key = Key.toText key

#else

hashMapFromObject :: Types.Object -> HashMap.HashMap Key.Key Types.Value
hashMapFromObject object = object

textFromKey :: Text -> Text
textFromKey key = key

#endif
