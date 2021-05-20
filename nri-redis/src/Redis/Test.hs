-- | Useful to test encoding of data stored in redis.
module Redis.Test (encoding) where

import Data.Proxy (Proxy (Proxy))
import qualified Examples
import Test (Test)
import qualified Test.Encoding

encoding :: forall m key a. Examples.HasExamples a => Text -> Text -> m key a -> Test
encoding name fileName _ =
  Examples.examples (Proxy :: Proxy a)
    |> Test.Encoding.examplesToTest name fileName
