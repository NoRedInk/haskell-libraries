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
import Fuzz (Fuzzer)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Internal.Query (Query (..))
import qualified List
import qualified MySQL
import qualified Platform
import qualified Task
import Test (Test, describe, fuzz, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "MySQL"
    [ unsafeBulkifyInsertsTests,
      transactionTests
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

mockQuery :: Text -> Query ()
mockQuery sqlString =
  Query
    { sqlString,
      runQuery = \_ -> Prelude.pure [],
      quasiQuotedString = "",
      sqlOperation = "",
      queriedRelation = ""
    }

transactionTests :: Test
transactionTests =
  describe
    "transaction"
    [ fuzz mysqlCommandsFuzzer "transactions don't crash" <| \cmds ->
        Expect.withIO identity <| do
          settings <- Environment.decode MySQL.decoder
          noLogger <- Platform.silentContext
          res <- Acquire.withAcquire (MySQL.connection settings) <| \conn ->
            runCmds conn cmds
              |> Task.attempt noLogger
          Prelude.pure
            <| case res of
              Ok _ -> Expect.pass
              Err "oops" -> Expect.pass
              Err err -> Expect.fail err
    ]

-- | Representation of a database command. We use this type to fuzz all sorts
-- of complicated database interacts, to ensure transactions always work.
data MySQLCommand
  = DoQuery
  | ThrowError
  | InTransaction [MySQLCommand]
  deriving (Show)

mysqlCommandsFuzzer :: Fuzzer [MySQLCommand]
mysqlCommandsFuzzer =
  Gen.list (Range.linear 0 5) mysqlCommandFuzzer

mysqlCommandFuzzer :: Fuzzer MySQLCommand
mysqlCommandFuzzer =
  Gen.recursive
    Gen.choice
    [Gen.constant DoQuery, Gen.constant ThrowError]
    [map InTransaction mysqlCommandsFuzzer]

runCmd :: MySQL.Connection -> MySQLCommand -> Task Text ()
runCmd conn cmd =
  case cmd of
    DoQuery ->
      MySQL.doQuery
        conn
        [MySQL.sql|!SELECT 1|]
        ( \res ->
            case res of
              Ok (_ :: [Int]) -> Task.succeed ()
              Err err -> Task.fail (Debug.toString err)
        )
    ThrowError ->
      Task.fail "oops"
    InTransaction subCmds ->
      MySQL.transaction conn (\conn' -> runCmds conn' subCmds)

runCmds :: MySQL.Connection -> [MySQLCommand] -> Task Text ()
runCmds conn cmds =
  List.map (runCmd conn) cmds
    |> Task.sequence
    |> Task.map (\_ -> ())
