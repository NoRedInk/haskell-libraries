module MySQL.InternalSpec (tests) where

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
      inToAnyTests
    ]

queryIn :: Text
queryIn =
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
          |> Expect.equal queryIn
    ]

inToAnyTests :: Test
inToAnyTests =
  describe
    "inToAny"
    [ test "Replaces IN query with ANY query" <| \_ ->
        queryIn
          |> MySQL.Internal.inToAny
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat = ANY (\"crown\", \"fedora\", \"cap\");"
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
