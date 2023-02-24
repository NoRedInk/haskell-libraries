{-# LANGUAGE QuasiQuotes #-}

module ConflictTest where

import Test (Test)
import qualified Test
import qualified Postgres
import qualified Expect

expectSuccess :: Result Postgres.Error [row] -> Task Text [row]
expectSuccess result = 
  case result of 
    Ok rows -> Task.succeed rows
    Err err -> Task.fail <| "Unexpected postgres error: " ++ (Debug.toString err)

expectError :: Result Postgres.Error [row] -> Task Text Postgres.Error
expectError result =
  case result of 
    Ok _ -> Task.fail "Expected to encounter an error"
    Err err -> Task.succeed err

tests :: Postgres.Connection -> Test
tests rawConnection =
    Test.describe
      "Postgres.Test"
      [ Test.test "test transaction can recover from a conflict" <| \_ ->
          Postgres.inTestTransaction 
            rawConnection
            (\connection -> do
              _ <- Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] expectSuccess
              _ <- Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] expectError
              Task.succeed ()
            )
            |> Expect.succeeds
      ]