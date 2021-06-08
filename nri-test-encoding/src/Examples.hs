{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

-- | Helpers for associating example values with all the types we use in our
-- APIs. This allows us to write tests that will warn us when the encoding of
-- our types change, potentially in backwards-incompatible ways.
module Examples
  ( HasExamples (..),
    Examples,
    example,
    render,
  )
where

import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Dict
import qualified List
import NriPrelude
import Prelude ((<>))
import qualified Prelude

-- | Example values of a type.
newtype Examples = Examples (NonEmpty.NonEmpty Example)

data Example = Example
  { description :: Text,
    encodedValue :: Text
  }
  deriving (Eq, Ord)

-- | Create an example for a type. Examples consists of a description and an
-- encoded value.
example :: Data.Aeson.ToJSON a => Text -> a -> Examples
example description x =
  Example
    { description,
      encodedValue =
        Data.Aeson.Encode.Pretty.encodePretty x
          |> Data.ByteString.Lazy.toStrict
          |> Data.Text.Encoding.decodeUtf8
    }
    |> Prelude.pure
    |> Examples

-- | A helper type class that provides us example values of particular types.
-- The `IsApi` typeclass below will demand we define an instance of this type
-- class for each type used in a request or response body.
class HasExamples t where
  examples :: Proxy t -> Examples

instance Prelude.Semigroup Examples where
  (Examples xs) <> (Examples ys) = Examples (xs <> ys)

-- | Render example values to a Text.
render :: Examples -> Text
render (Examples examples') =
  NonEmpty.toList examples'
    |> List.map renderExample
    |> Data.Text.intercalate "\n\n"

renderExample :: Example -> Text
renderExample example' =
  description example'
    ++ "\n"
    ++ encodedValue example'

instance (HasExamples a, HasExamples b) => HasExamples (a, b) where
  examples _ = examples (Proxy :: Proxy a) ++ examples (Proxy :: Proxy b)

instance (HasExamples a, HasExamples b, HasExamples c) => HasExamples (a, b, c) where
  examples _ =
    examples (Proxy :: Proxy a)
      ++ examples (Proxy :: Proxy b)
      ++ examples (Proxy :: Proxy c)

instance (HasExamples a, HasExamples b) => HasExamples (Dict.Dict a b) where
  examples _ = examples (Proxy :: Proxy a) ++ examples (Proxy :: Proxy b)

instance (HasExamples a) => HasExamples (Maybe a) where
  examples _ = examples (Proxy :: Proxy a)

instance (HasExamples a) => HasExamples (a, List a) where
  examples _ = examples (Proxy :: Proxy a)

instance (HasExamples a) => HasExamples (List a) where
  examples _ = examples (Proxy :: Proxy a)

instance HasExamples Int where
  examples _ = example "int" (1 :: Int)

instance HasExamples () where
  examples _ = example "unit" ()

instance HasExamples Text where
  examples _ = example "text" ("" :: Text)
