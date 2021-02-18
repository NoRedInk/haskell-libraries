{-# LANGUAGE QuasiQuotes #-}

module MySQLTransactionSpec
  ( main,
  )
where

import qualified Data.Acquire as Acquire
import qualified Debug
import qualified Environment
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified List
import qualified MySQL
import qualified Platform
import qualified Task
import qualified Prelude

main :: Prelude.IO ()
main = do
  res <- Hedgehog.check spec
  if res
    then Prelude.pure ()
    else Prelude.fail "test failed"

spec :: Hedgehog.Property
spec =
  Hedgehog.property <| do
    cmd <- Hedgehog.forAll mysqlCommandGen
    settings <- Hedgehog.evalIO <| Environment.decode MySQL.decoder
    noLogger <- Hedgehog.evalIO <| Platform.silentHandler
    res <-
      Hedgehog.evalIO <| Acquire.withAcquire (MySQL.connection settings) <| \conn ->
        runCmd conn cmd
          |> Task.attempt noLogger
    case res of
      Ok _ -> Prelude.pure ()
      -- These are exceptions we intentionally throw in the test to
      -- the effect of exceptions on transactions. They shouldn't fail
      -- the test.
      Err "oops" -> Prelude.pure ()
      Err err -> Prelude.fail (Text.toList err)

-- | Representation of a database command. We use this type to fuzz all sorts
-- of complicated database interacts, to ensure transactions always work.
data MySQLCommand
  = DoQuery
  | ThrowError
  | InTransaction MySQLCommand
  | Sequence [MySQLCommand]
  | Parallel [MySQLCommand]
  deriving (Show)

mysqlCommandGen :: Hedgehog.Gen MySQLCommand
mysqlCommandGen =
  Gen.recursive
    Gen.choice
    [ Gen.constant DoQuery,
      Gen.constant ThrowError
    ]
    [ map InTransaction mysqlCommandGen,
      map Sequence mysqlCommandsGen,
      map Parallel mysqlCommandsGen
    ]

mysqlCommandsGen :: Hedgehog.Gen [MySQLCommand]
mysqlCommandsGen =
  Gen.list (Range.linear 0 10) mysqlCommandGen

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
    InTransaction subCmd ->
      MySQL.transaction conn (\conn' -> runCmd conn' subCmd)
    Sequence subCmds ->
      List.map (runCmd conn) subCmds
        |> Task.sequence
        |> Task.map (\_ -> ())
    Parallel subCmds ->
      List.map (runCmd conn) subCmds
        |> Task.parallel
        |> Task.map (\_ -> ())
