{-# LANGUAGE QuasiQuotes #-}

module MySQLQuerySpec
  ( tests,
  )
where

import qualified Database.MySQL.Base as Base
import qualified Expect
import qualified Log
import MySQL.Query
import Test (Test, describe, test)
import qualified Text

tests :: Test
tests =
  describe
    "MySQL.Query"
    [ anyToInTests,
      inToAnyTests,
      emptyAnyTests,
      sqlTests
    ]

sqlTests :: Test
sqlTests =
  describe
    "MySQL.sql"
    [ test "removes monolith. prefixes from table names" <| \_ ->
        [sql|SELECT id FROM monolith.users|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users",
                  params = Log.mkSecret [],
                  quasiQuotedString = "SELECT id FROM monolith.users",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            ),
      test "removes postgresql-typed flags" <| \_ ->
        [sql|!$SELECT id FROM monolith.users|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users",
                  params = Log.mkSecret [],
                  quasiQuotedString = "!$SELECT id FROM monolith.users",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            ),
      test "replaces interpolation groups with question marks" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"jasper" :: Text} AND id > ${5 :: Int}|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users WHERE username = ? AND id > ?",
                  params = Log.mkSecret [Base.MySQLText "jasper", Base.MySQLInt64 5],
                  quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"jasper\" :: Text} AND id > ${5 :: Int}",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            ),
      test "deals with empty lists correctly" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE id IN (${[] :: [Int]})|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users WHERE id IN (FALSE)",
                  params = Log.mkSecret [],
                  quasiQuotedString = "SELECT id FROM monolith.users WHERE id IN (${[] :: [Int]})",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            ),
      test "expands interpolation groups that are lists" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users WHERE id IN (?,?,?)",
                  params = Log.mkSecret [Base.MySQLInt64 1, Base.MySQLInt64 2, Base.MySQLInt64 3],
                  quasiQuotedString = "SELECT id FROM monolith.users WHERE id IN (${[1, 2, 3] :: [Int]})",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            ),
      test "stand-alone question mark is replaced with a placeholder" <| \_ ->
        [sql|SELECT id FROM monolith.users WHERE username = ${"?" :: Text}|]
          |> Expect.equal
            ( Query
                { preparedStatement = "SELECT id FROM users WHERE username = ?",
                  params = Log.mkSecret [Base.MySQLText "?"],
                  quasiQuotedString = "SELECT id FROM monolith.users WHERE username = ${\"?\" :: Text}",
                  sqlOperation = "SELECT",
                  queriedRelation = "users"
                } ::
                Query Int
            )
    ]

queryInFixValues :: Text
queryInFixValues =
  Text.join
    "\n"
    [ "SELECT hat FROM royalty",
      "WHERE hat IN (\"crown\", \"fedora\", \"cap\");"
    ]

anyToInTests :: Test
anyToInTests =
  describe
    "anyToIn"
    [ test "Replaces ANY query with IN query" <| \_ ->
        [ "SELECT hat FROM royalty",
          "WHERE hat = ANY ('{\"crown\", \"fedora\", \"cap\"}');"
        ]
          |> Text.join "\n"
          |> anyToIn
          |> Expect.equal queryInFixValues
    ]

inToAnyTests :: Test
inToAnyTests =
  describe
    "inToAny"
    [ test "DON'T replaces IN with fix values." <| \_ ->
        queryInFixValues
          |> inToAny
          |> Expect.equal queryInFixValues,
      test "Replaces IN query with ANY query" <| \_ ->
        Text.join
          "\n"
          [ "SELECT hat FROM royalty",
            "WHERE hat IN (${ids});"
          ]
          |> inToAny
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat = ANY (${ids});"
                ]
            ),
      test "Multiple INs" <| \_ ->
        Text.join
          "\n"
          [ "SELECT hat FROM royalty",
            "WHERE hat IN (${ids})",
            "AND hat IN (${hats});"
          ]
          |> inToAny
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat = ANY (${ids})",
                  "AND hat = ANY (${hats});"
                ]
            ),
      test "Select in brackets" <| \_ ->
        let subQuery =
              [ "DELETE FROM content_creation.seeds as seeds",
                "WHERE seeds.submitted = 'draft'",
                "AND seeds.id IN (",
                "SELECT DISTINCT ON(seeds.public_id) seeds.id",
                "FROM content_creation.seeds as seeds",
                "WHERE seeds.public_id = ${publicId}",
                "ORDER BY seeds.public_id, seeds.created_at DESC",
                ")"
              ]
                |> Text.join "\n"
         in subQuery
              |> inToAny
              |> Expect.equal subQuery
    ]

emptyAnyTests :: Test
emptyAnyTests =
  describe
    "emptyAnyTests"
    [ test "Replaces ANY with no elements with a predicate that is always false" <| \_ ->
        [ "SELECT hat FROM royalty",
          "WHERE hat = ANY ('{}');"
        ]
          |> Text.join "\n"
          |> anyToIn
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat != ( hat );"
                ]
            )
    ]
