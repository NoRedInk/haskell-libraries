{-# LANGUAGE QuasiQuotes #-}

module MySQLQuerySpec
  ( tests,
  )
where

import Cherry.Prelude
import qualified Database.MySQL.Base as Base
import qualified Expect
import qualified Log
import MySQL.Query
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "MySQL.Query"
    [ test "removed monolith. prefixes from table names" <| \_ ->
        [sql|SELECT id FROM monolith.users|]
          |> preparedStatement
          |> Expect.equal "SELECT id FROM users",
      test "stand-alone question mark is replaced with a placeholder" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"?" :: Text}|]
          |> preparedStatement
          |> Expect.equal "SELECT id FROM users WHERE username = ?",
      test "stand-alone question mark ends up in params list" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"?" :: Text}|]
          |> params
          |> Log.unSecret
          |> Expect.equal [Base.MySQLText "?"]
    ]
