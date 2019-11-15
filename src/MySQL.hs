{-# LANGUAGE GADTs #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries, but for
-- MySQL. They return the correct amount of results in a Servant handler, or throw
-- a Rollbarred error.
module MySQL
  ( -- Connection
    Connection,
    connection,
    readiness, -- Creating a connection handler.
    -- Settings
    Settings.Settings,
    Settings.decoder,
    -- Querying
    Query.Query,
    Query.sql,
    Query.Error (..),
    doQuery,
    getMany,
    getOne,
    modifyExactlyOne,
    -- Reexposing useful Database.Persist.MySQL types
    MySQL.RawSql (..),
    MySQL.Single (..),
  )
where

import qualified Control.Monad.Logger
import qualified Data.Acquire
import qualified Data.Pool
import Data.String (fromString)
import qualified Database.MySQL.Connection
import qualified Database.Persist.MySQL as MySQL
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import qualified Log
import qualified MySQL.Internal as Internal
import qualified MySQL.Settings as Settings
import Nri.Prelude
import qualified Nri.Task as Task
import qualified Text

type Connection = GenericDb.Connection MySQL.SqlBackend

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Task.handler
      pool <-
        MySQL.createMySQLPool database size
          -- TODO: Log at the Debug level here.
          |> Control.Monad.Logger.runNoLoggingT
          |> map GenericDb.Pool
      pure (GenericDb.Connection doAnything pool (toConnectionLogContext settings))
    release GenericDb.Connection {GenericDb.singleOrPool} =
      case singleOrPool of
        GenericDb.Pool pool -> Data.Pool.destroyAllResources pool
        GenericDb.Single _ -> pure ()
    size = Settings.unMysqlPoolSize (Settings.mysqlPoolSize (Settings.mysqlPool settings)) |> fromIntegral
    database = toConnectInfo settings

-- |
-- Check that we are ready to be take traffic.
readiness :: Log.Handler -> Connection -> Health.Check
readiness log conn =
  Health.Check "mysql" Health.Fatal (GenericDb.readiness go log conn)
  where
    go :: MySQL.SqlBackend -> Text -> IO ()
    go c q =
      MySQL.rawExecute q []
        |> (\reader -> runReaderT reader c)

-- | Find multiple rows.
--
--   @
--     getMany c
--       [MySQL.sql|
--         SELECT id, name FROM my_table
--       |]
--   @
getMany ::
  (HasCallStack, MySQL.RawSql row) =>
  Connection ->
  Query.Query row ->
  Task e [row]
getMany = withFrozenCallStack doQuery

-- | returns one object!
--
--   @
--     getOne c
--       [MySQL.sql|
--         select title from my_table where id = 1
--       |]
--   @
getOne ::
  (HasCallStack, MySQL.RawSql row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
getOne = withFrozenCallStack modifyExactlyOne

doQuery ::
  (HasCallStack, MySQL.RawSql row) =>
  Connection ->
  Query.Query row ->
  Task e [row]
doQuery conn query = do
  withFrozenCallStack Log.debug (Query.quasiQuotedString query) []
  GenericDb.runTaskWithConnection conn (runQuery query)
    |> Log.withContext "mysql-query" [Log.Query queryInfo]
  where
    queryInfo = Log.QueryInfo
      { Log.queryText = Log.mkSecret (Query.sqlString query),
        Log.queryTemplate = Query.quasiQuotedString query,
        Log.queryConn = GenericDb.logContext conn,
        Log.queryOperation = Query.sqlOperation query,
        Log.queryCollection = Query.queriedRelation query
      }

-- | Modify exactly one row or fail with a 500.
--
--   @
--     modifyExactlyOne c
--       [MySQL.sql|
--         INSERT INTO my_table (name)
--           VALUES ($1)
--         RETURNING id, name
--       |]
--   @
modifyExactlyOne ::
  (HasCallStack, MySQL.RawSql row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
modifyExactlyOne conn query =
  doQuery conn query
    |> andThen (Query.expectOne (Query.quasiQuotedString query))

runQuery ::
  (MySQL.RawSql row) =>
  Query.Query row ->
  MySQL.SqlBackend ->
  IO [row]
runQuery query conn =
  query
    |> Query.sqlString
    |> toS
    -- We need this prefix on tables to allow compile-time checks of the query.
    |> Text.replace "monolith." ""
    |> Internal.anyToIn
    |> toS
    |> fromString
    |> (\query' -> MySQL.rawSql query' [])
    |> (\reader -> runReaderT reader conn)

toConnectionLogContext :: Settings.Settings -> Log.QueryConnectionInfo
toConnectionLogContext settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Settings.unDatabase (Settings.database connectionSettings)
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Log.UnixSocket
            Log.MySQL
            (toS (Settings.unSocket socket))
            database
        Settings.ConnectTcp host port ->
          Log.TcpSocket
            Log.MySQL
            (Settings.unHost host)
            (show (Settings.unPort port))
            database

toConnectInfo :: Settings.Settings -> MySQL.MySQLConnectInfo
toConnectInfo settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = toS (Settings.unDatabase (Settings.database connectionSettings))
      user = toS (Settings.unUser (Settings.user connectionSettings))
      password = toS (Log.unSecret (Settings.unPassword (Settings.password connectionSettings)))
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          MySQL.mkMySQLConnectInfo
            (Settings.unSocket socket)
            user
            password
            database
            |> MySQL.setMySQLConnectInfoCharset Database.MySQL.Connection.utf8mb4_unicode_ci
        Settings.ConnectTcp host port ->
          MySQL.mkMySQLConnectInfo
            (toS (Settings.unHost host))
            user
            password
            database
            |> MySQL.setMySQLConnectInfoPort (fromIntegral (Settings.unPort port))
            |> MySQL.setMySQLConnectInfoCharset Database.MySQL.Connection.utf8mb4_unicode_ci
