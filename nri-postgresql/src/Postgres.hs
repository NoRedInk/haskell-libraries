{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Functions for running Postgres queries.
module Postgres
  ( -- Connection
    Connection.Connection,
    Connection.connection,
    -- Settings
    Settings.Settings,
    Settings.decoder,
    Settings.decoderWithPrefix,
    -- Querying
    Query.Query,
    Query.Error (..),
    Query.sql,
    doQuery,
    -- Handling transactions
    transaction,
    inTestTransaction,
    -- Reexposing useful postgresql-typed types
    PGTypes.PGColumn (pgDecode),
    PGTypes.PGParameter (pgEncode),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Pool
import Database.PostgreSQL.Typed (PGConnection)
import qualified Database.PostgreSQL.Typed.Array as PGArray
import Database.PostgreSQL.Typed.Protocol
  ( PGError,
    pgBegin,
    pgCommit,
    pgErrorCode,
    pgRollback,
    pgRollbackAll,
  )
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified List
import qualified Log
import qualified Log.SqlQuery as SqlQuery
import qualified Platform
import Postgres.Connection (Connection)
import qualified Postgres.Connection as Connection
import qualified Postgres.Query as Query
import qualified Postgres.Settings as Settings
import qualified Postgres.Time as Time
import qualified Task
import qualified Tuple
import qualified Prelude

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction conn func =
  let start :: PGConnection -> Task x PGConnection
      start c =
        doIO conn <| do
          pgBegin c
          Prelude.pure c
      --
      end :: Platform.Succeeded -> PGConnection -> Task x ()
      end succeeded c =
        doIO conn
          <| case succeeded of
            Platform.Succeeded -> pgCommit c
            Platform.Failed -> pgRollback c
            Platform.FailedWith _ -> pgRollback c
      --
      setSingle :: PGConnection -> Connection
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {Connection.singleOrPool = Connection.Single c}
   in withConnection conn <| \c ->
        Platform.bracketWithError (start c) end (setSingle >> func)

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction conn func =
  let start :: PGConnection -> Task x PGConnection
      start c = do
        rollbackAllSafe conn c
        doIO conn <| pgBegin c
        Prelude.pure c
      --
      end :: Platform.Succeeded -> PGConnection -> Task x ()
      end _ c =
        doIO conn <| pgRollbackAll c
      --
      setSingle :: PGConnection -> Connection
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {Connection.singleOrPool = Connection.Single c}
   in --
      withConnection conn <| \c ->
        Platform.bracketWithError (start c) end (setSingle >> func)

rollbackAllSafe :: Connection -> PGConnection -> Task x ()
rollbackAllSafe conn c =
  doIO conn <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    pgBegin c
    pgRollbackAll c

-- | Run a query against MySql. This will return a list of rows, where the @row@
-- type is a tuple containing the queried columns.
--
-- > doQuery
-- >   connection
-- >   [sql| SELECT name, breed FROM doggos |]
-- >   (\result ->
-- >     case result of
-- >       Ok rows -> Task.succeed rows
-- >       Err err -> Task.fail err
-- >   )
doQuery ::
  HasCallStack =>
  Connection ->
  Query.Query row ->
  (Result Query.Error [row] -> Task e a) ->
  Task e a
doQuery conn query handleResponse =
  runQuery conn query
    -- Handle the response before wrapping the operation in a context. This way,
    -- if the response handling logic creates errors, those errors can inherit
    -- context values like the query string.
    |> ( \task ->
           withFrozenCallStack Platform.tracingSpan "Postgresql Query" <| do
             res <-
               Platform.finally
                 task
                 ( do
                     Platform.setTracingSpanDetails queryInfo
                     Platform.setTracingSpanSummary
                       ( (SqlQuery.sqlOperation queryInfo |> Maybe.withDefault "?")
                           ++ " "
                           ++ (SqlQuery.queriedRelation queryInfo |> Maybe.withDefault "?")
                       )
                 )
             -- If we end up here it means the query succeeded. Overwrite the tracing
             -- details to contain the amount of selected rows. This information can be
             -- useful when debugging slow queries.
             Platform.setTracingSpanDetails
               queryInfo {SqlQuery.rowsReturned = Just (List.length res)}
             Prelude.pure res
       )
    |> map Ok
    |> Task.onError (Task.succeed << Err)
    |> andThen handleResponse
  where
    queryInfo = Query.details query (Connection.connDetails conn)

fromPGError :: Connection -> PGError -> Query.Error
fromPGError c pgError =
  -- There's a lot of errors Postgres might throw. For a couple we have custom
  -- `Error` constructors defined, because we've seen a couple of them and would
  -- like to handle them in special ways or define custom error messages for
  -- them. If a Postgres error starts showing up in our log, please feel free
  -- to add a special case for it to this list!
  case pgErrorCode pgError of
    "23505" ->
      Exception.displayException pgError
        |> Text.fromList
        |> Query.UniqueViolation
    "57014" ->
      Query.Timeout (Time.milliseconds (Connection.timeout c))
    "23503" ->
      Exception.displayException pgError
        |> Text.fromList
        |> Query.ForeignKeyConstraintViolation
    _ ->
      Exception.displayException pgError
        |> Text.fromList
        -- We add the full error in the context array rather than the
        -- message string, to help errors being grouped correctly in a
        -- bug tracker. Errors might contain unique bits of data like
        -- generated id's or timestamps which when included in the main
        -- error message would result in each error being grouped by
        -- itself.
        |> (\err -> Query.Other "Postgres query failed with unexpected error" [Log.context "error" err])

--
-- CONNECTION HELPERS
--

runQuery :: Connection -> Query.Query row -> Task Query.Error [row]
runQuery conn query =
  withConnection conn <| \c ->
    Query.runQuery query c
      |> Exception.try
      |> map
        ( \res -> case res of
            Prelude.Right x -> Ok x
            Prelude.Left err -> Err (fromPGError conn err)
        )
      |> Platform.doAnything (Connection.doAnything conn)
      |> withTimeout conn

withTimeout :: Connection -> Task Query.Error a -> Task Query.Error a
withTimeout conn task =
  if Time.microseconds (Connection.timeout conn) > 0
    then
      Task.timeout
        (Time.milliseconds (Connection.timeout conn))
        (Query.Timeout (Time.milliseconds (Connection.timeout conn)))
        task
    else task

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection -> (PGConnection -> Task e a) -> Task e a
withConnection conn func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        Log.withContext "acquiring Postgres connection from pool" []
          <| doIO conn
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool conn -> Platform.Succeeded -> (conn, Data.Pool.LocalPool conn) -> Task y ()
      release pool succeeded (c, localPool) =
        doIO conn
          <| case succeeded of
            Platform.Succeeded ->
              Data.Pool.putResource localPool c
            Platform.Failed ->
              Data.Pool.destroyResource pool localPool c
            Platform.FailedWith _ ->
              Data.Pool.destroyResource pool localPool c
   in --
      case Connection.singleOrPool conn of
        (Connection.Single c) ->
          func c
        --
        (Connection.Pool pool) ->
          Platform.bracketWithError (acquire pool) (release pool) (Tuple.first >> func)

doIO :: Connection -> Prelude.IO a -> Task x a
doIO conn io =
  Platform.doAnything (Connection.doAnything conn) (io |> map Ok)

-- useful typeclass instances
instance PGTypes.PGType "jsonb" => PGTypes.PGType "jsonb[]" where
  type PGVal "jsonb[]" = PGArray.PGArray (PGTypes.PGVal "jsonb")

instance PGTypes.PGType "jsonb" => PGArray.PGArrayType "jsonb[]" where
  type PGElemType "jsonb[]" = "jsonb"
