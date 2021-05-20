-- | Useful to test encoding of data stored in redis.
module Redis.Test (encoding) where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Typeable as Typeable
import qualified Examples
import Test (Test)
import qualified Test.Encoding
import qualified Text

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
