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
module Internal.AesonHelpers (keyFromText) where

#if MIN_VERSION_aeson(2,0,0)

import qualified Data.Aeson.Key as Key
import Text

keyFromText :: Text -> Key.Key
keyFromText key = Key.fromText key


#else

import Text

keyFromText :: Text -> Text
keyFromText key = key

#endif
