{-# LANGUAGE QuasiQuotes #-}

module MySQLSpec
  ( tests,
  )
where

import Cherry.Prelude
import qualified Data.Acquire as Acquire
import qualified Debug
import qualified Environment
import qualified Expect
import Internal.Query (Query (..))
import qualified MySQL
import qualified Platform
import qualified Task
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "MySQL"
    [ unsafeBulkifyInsertsTests,
      queriesWithQuestionMarks
    ]

unsafeBulkifyInsertsTests :: Test
unsafeBulkifyInsertsTests =
  describe
    "unsafeBulkifyInserts"
    [ test "works when passed a single insert" <| \_ ->
        [mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"]
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"),
      test "works when passed multiple inserts" <| \_ ->
        [ mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)",
          mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (4,5,6)"
        ]
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3), (4,5,6)"),
      test "works with inconsistent casing of the word VALUES" <| \_ ->
        [ mockQuery "INSERT INTO foos (id, bars, bazs) valUES (1,2,3)",
          mockQuery "INSERT INTO foos (id, bars, bazs) vALues (4,5,6)"
        ]
          |> MySQL.unsafeBulkifyInserts
          |> map sqlString
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) valUES (1,2,3), (4,5,6)"),
      test "fails if no values in the SQL string" <| \_ ->
        [ mockQuery "SELECT foos",
          mockQuery "SELECT bars"
        ]
          |> MySQL.unsafeBulkifyInserts
          -- Even though we always expect Err values here, the type system does
          -- not see this and things we could see `Ok (Query a)` values as well.
          -- Without the line below compilation will fail because `Query a` does
          -- not have Show and Eq instances.
          |> map sqlString
          |> Expect.equal (MySQL.UnableToBulkify "Not all queries are inserts with a VALUES keyword.")
    ]

queriesWithQuestionMarks :: Test
queriesWithQuestionMarks =
  describe
    "queries with question marks don't fail"
    [ test "inserts" <| \_ ->
        expectTask <| \conn ->
          MySQL.doQuery
            conn
            [MySQL.sql|!
              INSERT INTO monolith.topics
                ( id
                , name
                )
              VALUES
                ( 12
                , '?'
                )
            |]
            ( \res ->
                Task.succeed
                  <| case res of
                    Ok () -> Expect.pass
                    Err err -> Expect.fail (Debug.toString err)
            ),
      test "selects" <| \_ ->
        expectTask <| \conn ->
          MySQL.doQuery
            conn
            [MySQL.sql|!
              SELECT 1
              FROM monolith.topics
              WHERE name = '?'
            |]
            ( \res ->
                Task.succeed
                  <| case res of
                    Ok (_ :: [Int]) -> Expect.pass
                    Err err -> Expect.fail (Debug.toString err)
            )
    ]

expectTask :: (MySQL.Connection -> Task Never Expect.Expectation) -> Expect.Expectation
expectTask run =
  Expect.withIO identity <| do
    settings <- Environment.decode MySQL.decoder
    noLogger <- Platform.silentContext
    Acquire.withAcquire
      (MySQL.connection settings)
      (Task.perform noLogger << run)

mockQuery :: Text -> Query ()
mockQuery sqlString =
  Query
    { sqlString,
      runQuery = \_ -> Prelude.pure [],
      quasiQuotedString = "",
      sqlOperation = "",
      queriedRelation = ""
    }
