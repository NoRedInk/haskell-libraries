module MySQLInternalSpec (tests) where

import qualified Expect
import qualified MySQL.Internal
import Nri.Prelude
import Test (Test, describe, test)
import qualified Text

tests :: Test
tests =
  describe
    "MySQL.Internal"
    [ anyToInTests,
      inToAnyTests,
      emptyAnyTests
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
          |> MySQL.Internal.anyToIn
          |> Expect.equal queryInFixValues
    ]

inToAnyTests :: Test
inToAnyTests =
  describe
    "inToAny"
    [ test "DON'T replaces IN with fix values." <| \_ ->
        queryInFixValues
          |> MySQL.Internal.inToAny
          |> Expect.equal queryInFixValues,
      test "Replaces IN query with ANY query" <| \_ ->
        Text.join
          "\n"
          [ "SELECT hat FROM royalty",
            "WHERE hat IN (${ids});"
          ]
          |> MySQL.Internal.inToAny
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
          |> MySQL.Internal.inToAny
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
              |> MySQL.Internal.inToAny
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
          |> MySQL.Internal.anyToIn
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat != ( hat );"
                ]
            )
    ]
