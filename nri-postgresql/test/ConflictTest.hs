{-# LANGUAGE QuasiQuotes #-}

module ConflictTest where

import qualified Expect
import qualified Postgres
import qualified Postgres.Test
import Test (Test)
import qualified Test

tests :: a -> Test
tests _ =
  Test.describe
    "Postgres.Test"
    [ Postgres.Test.test "test transaction can recover from a conflict" <| \connection -> do
        _ <-
          Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] Task.succeed
            |> Expect.andCheck Expect.ok
        Postgres.doQuery connection [Postgres.sql| INSERT INTO constraints_table (user_id) VALUES (1) |] Task.succeed
          |> Expect.andCheck Expect.err
    ]
