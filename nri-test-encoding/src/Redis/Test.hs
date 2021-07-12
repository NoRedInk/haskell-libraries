-- | Useful to test encoding of data stored in redis.
module Redis.Test (encoding) where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Typeable as Typeable
import qualified Examples
import NriPrelude
import Test (Test)
import qualified Test.Encoding
import qualified Text

-- | Turns a Redis.Api into a test.
-- The test does the following:
--
-- 1. get examples from the `HasExamples` constraint
-- 2. encoded the examples into JSON
-- 3. check the encoded JSON against the generated file
--   NOTE: it will generate the file if it doesn't exist yet
encoding :: forall m key a. (Typeable.Typeable a, Examples.HasExamples a) => m key a -> Test
encoding _ =
  let proxy = Proxy :: Proxy a
      tyCon =
        Typeable.typeRep proxy
          |> Typeable.typeRepTyCon
      typeName =
        Typeable.tyConModule tyCon ++ "." ++ Typeable.tyConName tyCon
          |> Text.fromList
   in Examples.examples proxy
        |> Test.Encoding.examplesToTest typeName ("redis-encoding-" ++ typeName)
