{-# LANGUAGE QuasiQuotes #-}

module ConflictTest where

import Test (Test)
import qualified Test
import qualified Postgres
import qualified Expect

expectSuccess :: Result Postgres.Error [row] -> Task Postgres.Error [row]
expectSuccess result = 
  case result of 
    Ok rows -> Task.succeed rows
    Err err -> Task.fail err

tests :: Postgres.Connection -> Test
tests rawConnection =
    Test.describe
      "Postgres.Test"
      [ Test.test "test transaction can recover from a conflict" <| \_ ->
          Postgres.inTestTransaction 
            rawConnection
            (\connection -> do
              _ <- Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] expectSuccess
              _ <- Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] expectSuccess
              Task.succeed ()
            )
            |> Expect.succeeds
      ]