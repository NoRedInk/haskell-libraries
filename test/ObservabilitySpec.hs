{-# LANGUAGE QuasiQuotes #-}

module ObservabilitySpec
  ( tests,
  )
where

import Cherry.Prelude
import qualified Control.Concurrent.MVar as MVar
import qualified Debug
import qualified Expect
import qualified MySQL
import qualified Platform
import qualified Postgres
import qualified Task
import Test (Test, describe, test)
import qualified Prelude

tests :: MySQL.Connection -> Postgres.Connection -> Test
tests mysql postgres =
  describe
    "ObservabilitySpec"
    [ test "Postgres queries report the span data we expect" <| \_ ->
        Postgres.doQuery
          postgres
          [Postgres.sql|!SELECT 1|]
          ( \res ->
              case res of
                Err err -> Task.fail err
                Ok (_ :: [Int]) -> Task.succeed ()
          )
          |> spanForTask
          |> Expect.withIO (Debug.toString >> Expect.equalToFile "test/golden-results/observability-spec-postgres-reporting"),
      test "MySQL queries report the span data we expect" <| \_ ->
        MySQL.doQuery
          mysql
          [MySQL.sql|!SELECT 1|]
          ( \res ->
              case res of
                Err err -> Task.fail err
                Ok (_ :: [Int]) -> Task.succeed ()
          )
          |> spanForTask
          |> Expect.withIO (Debug.toString >> Expect.equalToFile "test/golden-results/observability-spec-mysql-reporting")
    ]

spanForTask :: Show e => Task e () -> Prelude.IO Platform.Span
spanForTask task = do
  spanVar <- MVar.newEmptyMVar
  res <-
    Platform.rootSpanIO
      "test-request"
      (MVar.putMVar spanVar)
      "test-root"
      (\log -> Task.attempt log task)
  case res of
    Err err -> Prelude.fail (Prelude.show err)
    Ok _ ->
      MVar.takeMVar spanVar
        |> map setAllTimestampsToZero

-- | Timestamps recorded in spans would make each test result different from the
-- last. This helper sets all timestamps to zero to prevent this.
setAllTimestampsToZero :: Platform.Span -> Platform.Span
setAllTimestampsToZero span =
  span
    { Platform.started = 0,
      Platform.finished = 0,
      Platform.children = map setAllTimestampsToZero (Platform.children span)
    }
