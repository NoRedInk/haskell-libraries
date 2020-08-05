{-# LANGUAGE QuasiQuotes #-}

module MySQLQuerySpec
  ( tests,
  )
where

import Cherry.Prelude
import qualified Database.MySQL.Base as Base
import qualified Expect
import qualified Fuzz
import qualified List
import qualified Log
import MySQL.Query
import Test (Test, describe, fuzz, test)

tests :: Test
tests =
  describe
    "MySQL.sql"
    [ test "removes monolith. prefixes from table names" <| \_ ->
        [sql|SELECT id FROM monolith.users|]
          |> Expect.equal Query
            { preparedStatement = "SELECT id FROM users",
              params = Log.mkSecret [],
              prepareQuery = Prepare,
              quasiQuotedString = "SELECT id FROM monolith.users",
              sqlOperation = "SELECT",
              queriedRelation = "users"
            },
      test "removes postgresql-typed flags" <| \_ ->
        [sql|!$SELECT id FROM monolith.users|]
          |> Expect.equal Query
            { preparedStatement = "SELECT id FROM users",
              params = Log.mkSecret [],
              prepareQuery = Prepare,
              quasiQuotedString = "!$SELECT id FROM monolith.users",
              sqlOperation = "SELECT",
              queriedRelation = "users"
            },
      test "replaces interpolation groups with question marks" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"jasper" :: Text} AND id > ${5 :: Int}|]
          |> Expect.equal Query
            { preparedStatement = "SELECT id FROM users WHERE username = ? AND id > ?",
              params = Log.mkSecret [Base.MySQLText "jasper", Base.MySQLInt64 5],
              prepareQuery = Prepare,
              quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"jasper\" :: Text} AND id > ${5 :: Int}",
              sqlOperation = "SELECT",
              queriedRelation = "users"
            },
      test "expands interpolation groups that are lists" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})|]
          |> Expect.equal Query
            { preparedStatement = "SELECT id FROM users WHERE id IN (?,?,?)",
              params = Log.mkSecret [Base.MySQLInt64 1, Base.MySQLInt64 2, Base.MySQLInt64 3],
              prepareQuery = Prepare,
              quasiQuotedString = "SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})",
              sqlOperation = "SELECT",
              queriedRelation = "users"
            },
      test "stand-alone question mark is replaced with a placeholder" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"?" :: Text}|]
          |> Expect.equal Query
            { preparedStatement = "SELECT id FROM users WHERE username = ?",
              params = Log.mkSecret [Base.MySQLText "?"],
              prepareQuery = Prepare,
              quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"?\" :: Text}",
              sqlOperation = "SELECT",
              queriedRelation = "users"
            },
      describe
        "when to prepare and when not"
        [ fuzz (Fuzz.list Fuzz.int) "we only prepare queries with 3 or less items" <| \xs ->
            [sql|SELECT id FROM monolith.users WHERE id IN (${xs})|]
              |> prepareQuery
              |> Expect.equal
                ( if List.length xs <= 3
                    then Prepare
                    else DontPrepare
                )
        ]
    ]
