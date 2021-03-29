{-# LANGUAGE QuasiQuotes #-}

module MySQLSpec
  ( tests,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Expect
import qualified Log
import qualified MySQL
import MySQL.Query (Query (..))
import qualified MySQL.Test
import Test (Test, describe, test)
import qualified Text

tests :: Test
tests =
  describe
    "MySQL"
    [ unsafeBulkifyInsertsTests,
      onDuplicateDoNothingTests,
      queriesWithQuestionMarks,
      queriesWithBackquotesTests,
      exceptionTests
    ]

unsafeBulkifyInsertsTests :: Test
unsafeBulkifyInsertsTests =
  describe
    "unsafeBulkifyInserts"
    [ test "works when passed a single insert" <| \_ ->
        MySQL.unsafeBulkifyInserts preparedStatement (mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)") []
          |> Expect.equal "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)",
      test "works when passed multiple inserts" <| \_ ->
        MySQL.unsafeBulkifyInserts preparedStatement (mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3)") [mockQuery "INSERT INTO foos (id, bars, bazs) VALUES (4,5,6)"]
          |> Expect.equal "INSERT INTO foos (id, bars, bazs) VALUES (1,2,3), (4,5,6)",
      test "works with inconsistent casing of the word VALUES" <| \_ ->
        MySQL.unsafeBulkifyInserts preparedStatement (mockQuery "INSERT INTO foos (id, bars, bazs) valUES (1,2,3)") [mockQuery "INSERT INTO foos (id, bars, bazs) vALues (4,5,6)"]
          |> Expect.equal "INSERT INTO foos (id, bars, bazs) valUES (1,2,3), (4,5,6)"
    ]

queriesWithQuestionMarks :: Test
queriesWithQuestionMarks =
  describe
    "queries with question marks don't fail"
    [ MySQL.Test.test "inserts and selects" <| \conn -> do
        let x = "?" :: Text
        (_ :: Int) <-
          MySQL.doQuery
            conn
            [MySQL.sql|!INSERT INTO monolith.topics (name, percent_correct) VALUES (${x}, 5)|]
            fromResult
            |> Expect.succeeds
        res <-
          MySQL.doQuery
            conn
            [MySQL.sql|!SELECT name, percent_correct FROM monolith.topics WHERE name = ${x}|]
            fromResult
            |> Expect.succeeds
        Expect.equal [("?", 5) :: (Text, Int)] res
    ]

queriesWithBackquotesTests :: Test
queriesWithBackquotesTests =
  describe
    "queries with backquotes work (they get replaces by double quotes)"
    [ MySQL.Test.test "inserts and selects" <| \conn -> do
        let x = "`hello`" :: Text
        (_ :: Int) <-
          MySQL.doQuery
            conn
            [MySQL.sql|!INSERT INTO monolith.topics (`name`, `percent_correct`) VALUES (${x}, 5)|]
            fromResult
            |> Expect.succeeds
        res <-
          MySQL.doQuery
            conn
            [MySQL.sql|!SELECT `name`, `percent_correct` FROM monolith.topics WHERE `name` = ${x}|]
            fromResult
            |> Expect.succeeds
        Expect.equal [("`hello`", 5) :: (Text, Int)] res
    ]

fromResult :: Result err a -> Task err a
fromResult res =
  case res of
    Err err -> Task.fail err
    Ok ok -> Task.succeed ok

exceptionTests :: Test
exceptionTests =
  describe
    "exceptions"
    [ MySQL.Test.test "dupplicate key errors have groupable error messages" <| \conn -> do
        (_ :: Int) <-
          MySQL.doQuery
            conn
            ( [MySQL.sql|!INSERT INTO monolith.topics (id, name) VALUES (1234, 'hi')|]
                -- If this topic already exists that's fine for the purpose of
                -- this test. Don't fail on that.
                |> MySQL.onDuplicateDoNothing
            )
            fromResult
            |> Expect.succeeds
        res <-
          MySQL.doQuery
            conn
            [MySQL.sql|!INSERT INTO monolith.topics (id, name) VALUES (1234, 'hi')|]
            fromResult
            |> Expect.fails
        Expect.equal
          "Query failed with unexpected error: MySQL query failed with error code 1062"
          (Exception.displayException res)
    ]

mockQuery :: Text -> Query a
mockQuery sqlString =
  Query
    { preparedStatement = sqlString,
      params = Log.mkSecret [],
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
