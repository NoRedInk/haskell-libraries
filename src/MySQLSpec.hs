module MySQLSpec (tests) where

import Cherry.Prelude
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Expect
import Internal.Query (Query (..))
import qualified MySQL
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "MySQL"
    [ unsafeBulkifyInsertsTests
    ]

unsafeBulkifyInsertsTests :: Test
unsafeBulkifyInsertsTests =
  describe
    "unsafeBulkifyInserts"
    [ test "works when passed a single insert" <| \_ ->
        mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)" :| []
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (Ok "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"),
      test "works when passed multiple inserts" <| \_ ->
        mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"
          :| [ mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (4,5,6)"
             ]
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (Ok "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3), (4,5,6)"),
      test "works with inconsistent casing of the word VALUES" <| \_ ->
        mockQuery "INSERT INTO foos (id, bars, bazs) valUES (1,2,3)"
          :| [ mockQuery "INSERT INTO foos (id, bars, bazs) vALues (4,5,6)"
             ]
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (Ok "INSERT INTO foos (id, bars, bazs) valUES (1,2,3), (4,5,6)"),
      test "fails if no values in the SQL string" <| \_ ->
        mockQuery "SELECT foos"
          :| [ mockQuery "SELECT bars"
             ]
          |> MySQL.unsafeBulkifyInserts
          -- Even though we always expect Err values here, the type system does
          -- not see this and things we could see `Ok (Query a)` values as well.
          -- Without the line below compilation will fail because `Query a` does
          -- not have Show and Eq instances.
          |> map sqlString
          |> Expect.equal (Err "Not all queries are inserts with a VALUES keyword.")
    ]

mockQuery :: Text -> Query ()
mockQuery sqlString =
  Query
    { sqlString,
      runQuery = \_ -> Prelude.pure [],
      quasiQuotedString = "",
      sqlOperation = "",
      queriedRelation = ""
    }
