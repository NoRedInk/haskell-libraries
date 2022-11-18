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
import qualified Postgres

{- Set up an enum type on the database at compile time to test with -}
Postgres.TH.useNRIDatabase

[Core.pgSQL| CREATE TYPE test_enum as ENUM ('value_1', 'value_2') |]
[Core.pgSQL| CREATE TABLE test_table (enum_col test_enum NOT NULL) |]

data TestEnum
  = Value1
  | Value2
  deriving (Eq, Show)

data TestNotEnum
  = NotValue Int

data UndersizedEnum 
  = UndersizedValue1

data OversizedEnum
  = OversizedValue1
  | OversizedValue2
  | OversizedValue3

$(generatePGEnum ''TestEnum "public.test_enum"
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

queryTests :: Postgres.Connection -> Test
queryTests rawConnection = 
  let expectSuccess :: Result Postgres.Error [row] -> Task Postgres.Error [row]
      expectSuccess result = 
        case result of 
          Ok rows -> Task.succeed rows
          Err err -> Task.fail err

      -- | I'm seeing flaky results with regards to `test_table` and `test_enum` existing on the db 
      -- at the point the test is run.  Let's just drop them and re-create in each test. Since the 
      -- compilers see's them being created at the top level above this will all typecheck.
      withContext :: (Postgres.Connection -> Task Postgres.Error a) -> Task Postgres.Error a
      withContext todo =
         Postgres.inTestTransaction rawConnection (\connection -> do
          _ <- Postgres.doQuery connection [Postgres.sql| DROP TABLE test_table |] expectSuccess
          _ <- Postgres.doQuery connection [Postgres.sql| DROP TYPE test_enum |] expectSuccess
          _ <- Postgres.doQuery connection [Postgres.sql| CREATE TYPE test_enum as ENUM ('value_1', 'value_2') |] expectSuccess
          _ <- Postgres.doQuery connection [Postgres.sql| CREATE TABLE test_table (enum_col test_enum NOT NULL) |] expectSuccess
          todo connection
         )

  in
  describe "Query Tests"
  [ Test.test "Insert into column" <| \_ -> do
      _ <- withContext (\connection ->
            Postgres.doQuery
              connection
              [Postgres.sql|
                INSERT INTO test_table (enum_col)
                VALUES (${Value1})
              |]
              expectSuccess
          ) |> Expect.succeeds

      Expect.pass
  , Test.test "Select from column" <| \_ -> do 
      rows <- withContext (\connection -> do
            _ <- Postgres.doQuery
              connection
              [Postgres.sql|
                INSERT INTO test_table (enum_col)
                VALUES (${Value1}), (${Value2})
              |]
              expectSuccess
            
            Postgres.doQuery
              connection
              [Postgres.sql|
                SELECT enum_col
                FROM test_table
              |]
              expectSuccess
          ) |> Expect.succeeds
      
      Expect.equal rows [Value1, Value2]
  ]

tests :: Postgres.Connection -> Test
tests connection = 
  describe
    "Postgres.Enum"
    [ generateFailureTests
    , queryTests connection
    ]