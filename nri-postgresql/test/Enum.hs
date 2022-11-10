{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}

module Enum where

import Test (Test, describe)
import qualified Test
import qualified Database.PostgreSQL.Typed as Core
import Postgres.Enum (generatePGEnum)
import qualified Postgres.TH
import qualified Language.Haskell.TH as TH
import qualified Expect
import Language.Haskell.TH.TestUtils as THTest
import qualified Prelude

{- Set up an enum type on the database at compile time to test with -}
Postgres.TH.useNRIDatabase

[Core.pgSQL| CREATE TYPE test_enum as ENUM ('value_1', 'value_2') |]

data TestEnum
  = Value1
  | Value2

data TestNotEnum
  = NotValue Int

data UndersizedEnum 
  = UndersizedValue1

data OversizedEnum
  = OversizedValue1
  | OversizedValue2
  | OversizedValue3

$(generatePGEnum ''TestEnum "test_enum"
    [ ('Value1, "value_1")
    , ('Value2, "value_2")
    ]
 )

-- | Tests that test cases we expect to get compile time failures for
-- 
-- Language.Haskell.TH has a `runQ` method which looks like it could be used
-- to run `generatePGEnum` in a test.  But unfortunately for technical reasons
-- `runQ` does not allow reifying names (which we use to look up the constructors of
-- `TestEnum`).  So instead we call in the `th-test-utils` library which lets us 
-- mock in the names that will be reified.
generateFailureTests :: Test
generateFailureTests =  
  let qState = THTest.QState 
        { THTest.mode = THTest.MockQAllowIO
        , THTest.knownNames = []
        , THTest.reifyInfo = $(loadNames [''TestEnum, ''UndersizedEnum, ''OversizedEnum, ''TestNotEnum])
        }

      expectCompileFailure :: TH.Q [TH.Dec] -> Expect.Expectation
      expectCompileFailure q = do
        either <- Expect.fromIO (THTest.tryTestQ qState q)
        case either of 
          Prelude.Left _ ->
            Expect.pass
          Prelude.Right _ -> 
            Expect.fail "Expected a compile time error but none were generated"
  in
  describe "generatePGEnum [compile-time failure]" 
  [ Test.test "data type is not an enum" <| \_ -> 
      expectCompileFailure (generatePGEnum ''TestNotEnum "test_enum" [('NotValue, "value_1")] )
  , Test.test "mapping contains duplicate database values" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "test_enum" [('Value1, "value_1"), ('Value2, "value_1")] )
  , Test.test "mapping contains duplicate constructors" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "test_enum" [('Value1, "value_1"), ('Value1, "value_2")] )
  , Test.test "constructor not from data type" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "test_enum" [('Value1, "value_1"), ('NotValue, "value_2")] )
  , Test.test "constructor missing from mapping" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "test_enum" [('Value1, "value_1")] )
  , Test.test "database enum name does not exist" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "not_an_enum_name" [('Value1, "value_1"), ('Value2, "value_2")] )
  , Test.test "database name not an enum" <| \_ ->
      expectCompileFailure (generatePGEnum ''TestEnum "boolean" [('Value1, "value_1"), ('Value2, "value_2")] )
  , Test.test "datatype smaller than PG enum" <| \_ ->
      expectCompileFailure (generatePGEnum ''UndersizedEnum "test_enum" [('UndersizedValue1, "value_1")] )
  , Test.test "datatype larger than PG enum" <| \_ ->
      expectCompileFailure (generatePGEnum ''OversizedEnum "test_enum" [('OversizedValue1, "value_1"), ('OversizedValue2, "value_2")] )
  ] 


tests :: Test
tests = 
  describe
    "Postgres.Enum"
    [ generateFailureTests
    ]