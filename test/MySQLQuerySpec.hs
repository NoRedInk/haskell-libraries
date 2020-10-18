{-# LANGUAGE QuasiQuotes #-}

module MySQLQuerySpec
  ( tests,
  )
where

import qualified Database.MySQL.Base as Base
import qualified Expect
import qualified Log
import MySQL.Query
import NriPrelude
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "MySQL.sql"
    [ test "removes monolith. prefixes from table names" <| \_ ->
        [sql|SELECT id FROM monolith.users|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users",
                params = Log.mkSecret [],
                quasiQuotedString = "SELECT id FROM monolith.users",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              },
      test "removes postgresql-typed flags" <| \_ ->
        [sql|!$SELECT id FROM monolith.users|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users",
                params = Log.mkSecret [],
                quasiQuotedString = "!$SELECT id FROM monolith.users",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              },
      test "replaces interpolation groups with question marks" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"jasper" :: Text} AND id > ${5 :: Int}|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users WHERE username = ? AND id > ?",
                params = Log.mkSecret [Base.MySQLText "jasper", Base.MySQLInt64 5],
                quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"jasper\" :: Text} AND id > ${5 :: Int}",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              },
      test "deals with empty lists correctly" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE id IN (${[] :: [Int]})|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users WHERE id IN (\"THIS_IS_NEVER_TRUE_ELSE_COMPLAIN_TO_PUFFERFISH\")",
                params = Log.mkSecret [],
                quasiQuotedString = "SELECT id FROM monolith.users WHERE id IN (${[] :: [Int]})",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              },
      test "expands interpolation groups that are lists" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users WHERE id IN (?,?,?)",
                params = Log.mkSecret [Base.MySQLInt64 1, Base.MySQLInt64 2, Base.MySQLInt64 3],
                quasiQuotedString = "SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              },
      test "stand-alone question mark is replaced with a placeholder" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"?" :: Text}|]
          |> Expect.equal
            Query
              { preparedStatement = "SELECT id FROM users WHERE username = ?",
                params = Log.mkSecret [Base.MySQLText "?"],
                quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"?\" :: Text}",
                sqlOperation = "SELECT",
                queriedRelation = "users"
              }
    ]
