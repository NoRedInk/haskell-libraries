{-# LANGUAGE QuasiQuotes #-}

module MySQLSpec
  ( tests,
  )
where

import Cherry.Prelude
import qualified Debug
import qualified Expect
import qualified Expect.Task
import qualified Log
import qualified MySQL
import MySQL.Query (Query (..))
import qualified MySQL.Query as Query
import qualified Task
import Test (Test, describe, test)
import qualified Test
import qualified Text

tests :: MySQL.Connection -> Test
tests mysqlConn =
  describe
    "MySQL"
    [ unsafeBulkifyInsertsTests,
      onDuplicateDoNothingTests,
      queriesWithQuestionMarks mysqlConn,
      exceptionTests mysqlConn
    ]

unsafeBulkifyInsertsTests :: Test
unsafeBulkifyInsertsTests =
  describe
    "unsafeBulkifyInserts"
    [ test "works when passed a single insert" <| \_ ->
        [mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"]
          |> MySQL.unsafeBulkifyInserts
          |> map preparedStatement
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)"),
      test "works when passed multiple inserts" <| \_ ->
        [ mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)",
          mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (4,5,6)"
        ]
          |> MySQL.unsafeBulkifyInserts
          |> map preparedStatement
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3), (4,5,6)"),
      test "works with inconsistent casing of the word VALUES" <| \_ ->
        [ mockQuery "INSERT INTO foos (id, bars, bazs) valUES (1,2,3)",
          mockQuery "INSERT INTO foos (id, bars, bazs) vALues (4,5,6)"
        ]
          |> MySQL.unsafeBulkifyInserts
          |> map preparedStatement
          |> Expect.equal
            (MySQL.BulkifiedInsert "INSERT INTO foos (id, bars, bazs) valUES (1,2,3), (4,5,6)")
    ]

queriesWithQuestionMarks :: MySQL.Connection -> Test
queriesWithQuestionMarks mysqlConn =
  describe
    "queries with question marks don't fail"
    [ Test.task "inserts and selects"
        <| MySQL.inTestTransaction mysqlConn
        <| \conn -> do
          (_ :: Int) <-
            MySQL.doQuery
              conn
              [MySQL.sql|!INSERT INTO monolith.topics (name, percent_correct) VALUES ('?', 5)|]
              resultToTask
              |> Expect.Task.succeeds
          MySQL.doQuery
            conn
            [MySQL.sql|!SELECT name, percent_correct FROM monolith.topics WHERE name = '?'|]
            resultToTask
            |> Expect.Task.andCheck (Expect.equal [("?", 5) :: (Text, Int)])
    ]

exceptionTests :: MySQL.Connection -> Test
exceptionTests mysqlConn =
  describe
    "exceptions"
    [ Test.task "dupplicate key errors have groupable error messages"
        <| MySQL.inTestTransaction mysqlConn
        <| \conn -> do
          (_ :: Int) <-
            MySQL.doQuery
              conn
              [MySQL.sql|!INSERT INTO monolith.topics (id, name) VALUES (1234, 'hi')|]
              resultToTask
              |> Expect.Task.succeeds
          MySQL.doQuery
            conn
            [MySQL.sql|!INSERT INTO monolith.topics (id, name) VALUES (1234, 'hi')|]
            ( \res ->
                case res of
                  Err err -> Task.succeed err
                  Ok (_ :: Int) -> Task.fail ("Expected an error, but none was returned." :: Text)
            )
            |> Expect.Task.andCheck
              ( Expect.equal "Query failed with unexpected error: MySQL query failed with error code 1062" << Debug.toString
              )
    ]

resultToTask :: Result e a -> Task e a
resultToTask res =
  case res of
    Ok x -> Task.succeed x
    Err x -> Task.fail x

mockQuery :: Text -> Query a
mockQuery sqlString =
  Query
    { preparedStatement = sqlString,
      params = Log.mkSecret [],
      prepareQuery = Query.Prepare,
      quasiQuotedString = "",
      sqlOperation = "",
      queriedRelation = ""
    }

onDuplicateDoNothingTests :: Test
onDuplicateDoNothingTests =
  describe
    "onDuplicateDoNothingTests"
    [ test "Replaces `insert into` with `INSERT IGNORE INTO` and doesn't care about whitespaces" <| \_ ->
        [ "insert   ",
          " into",
          "foo"
        ]
          |> Text.join "\n"
          |> mockQuery
          |> MySQL.onDuplicateDoNothing
          |> preparedStatement
          |> Expect.equal "INSERT IGNORE INTO\nfoo"
    ]
