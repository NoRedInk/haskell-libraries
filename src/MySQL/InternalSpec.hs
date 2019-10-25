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
    [ anyToInTests
    ]

anyToInTests :: Test
anyToInTests =
  describe
    "anyToIn"
    [ test "Replaces ANY query with IN query" <| \_ ->
        Text.join
          "\n"
          [ "SELECT hat FROM royalty",
            "WHERE hat = ANY ('{\"crown\", \"fedora\", \"cap\"}');"
          ]
          |> MySQL.Internal.anyToIn
          |> Expect.equal
            ( Text.join
                "\n"
                [ "SELECT hat FROM royalty",
                  "WHERE hat IN (\"crown\", \"fedora\", \"cap\");"
                ]
            )
    ]
