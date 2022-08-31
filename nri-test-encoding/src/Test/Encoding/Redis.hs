-- | Useful to test encoding of data stored in redis.
module Test.Encoding.Redis (test) where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Typeable as Typeable
import qualified Examples
import qualified GHC.Stack as Stack
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
test :: forall m key a. (Typeable.Typeable a, Examples.HasExamples a, Stack.HasCallStack) => m key a -> Test
test proxy =
  -- We need to freeze the call stack before proceeding.
  -- If we don't, the callstack for these generated tests would point to haskell-libraries code
  -- and not client code.
  -- This didn't matter that much until we had the ability to specify `--files tests/BlaSpec.hs`
  -- If we don't fix this, we can't ever `--files tests/SomeApiSpec.hs`
  Stack.withFrozenCallStack frozenTest proxy

frozenTest :: forall m key a. (Typeable.Typeable a, Examples.HasExamples a, Stack.HasCallStack) => m key a -> Test
frozenTest _ =
  let proxy = Proxy :: Proxy a
      tyCon =
        Typeable.typeRep proxy
          |> Typeable.typeRepTyCon
      typeName =
        Typeable.tyConModule tyCon ++ "." ++ Typeable.tyConName tyCon
          |> Text.fromList
   in Examples.examples proxy
        |> Test.Encoding.examplesToTest typeName ("redis-encoding-" ++ typeName)
