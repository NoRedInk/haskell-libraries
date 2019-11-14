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
    -- Handling transactions
    transaction,
    inTestTransaction,
    -- Reexposing useful MySQL.Simple types
    Simple.Result,
    Simple.ResultError (..),
    Simple.convert,
    Simple.Only (..),
    Simple.QueryResults,
  )
where

import Control.Exception.Safe (MonadCatch)
import qualified Data.Acquire
import Data.String (fromString)
import qualified Database.MySQL.Simple as Simple
import qualified Database.MySQL.Simple.QueryResults as Simple
import qualified Database.MySQL.Simple.Result as Simple
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import List (List)
import qualified Log
import qualified MySQL.Internal as Internal
import qualified MySQL.Settings as Settings
import Nri.Prelude
import qualified Text

type Connection = GenericDb.Connection Simple.Connection

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  GenericDb.connection
    (Settings.toConnectInfo settings)
    ( GenericDb.PoolConfig
        { GenericDb.connect = Simple.connect,
          GenericDb.disconnect = Simple.close,
          GenericDb.stripes = Settings.unMysqlPoolStripes (Settings.mysqlPoolStripes (Settings.mysqlPool settings)) |> fromIntegral,
          GenericDb.maxIdleTime = Settings.unMysqlPoolMaxIdleTime (Settings.mysqlPoolMaxIdleTime (Settings.mysqlPool settings)),
          GenericDb.size = Settings.unMysqlPoolSize (Settings.mysqlPoolSize (Settings.mysqlPool settings)) |> fromIntegral,
          GenericDb.toConnectionString = toConnectionString,
          GenericDb.toConnectionLogContext = toConnectionLogContext
        }
    )

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction =
  GenericDb.transaction GenericDb.Transaction
    { GenericDb.begin = \c -> void (Simple.execute_ c "start transaction"),
      GenericDb.commit = Simple.commit,
      GenericDb.rollback = Simple.rollback,
      GenericDb.rollbackAll = Simple.rollback -- there is no rollbackAll for mysql
    }

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction ::
  forall m a.
  (MonadIO m, MonadCatch m) =>
  Connection ->
  (Connection -> m a) ->
  m a
inTestTransaction =
  GenericDb.inTestTransaction GenericDb.Transaction
    { GenericDb.begin = \c -> void (Simple.execute_ c "start transaction"),
      GenericDb.commit = Simple.commit,
      GenericDb.rollback = Simple.rollback,
      GenericDb.rollbackAll = Simple.rollback -- there is no rollbackAll for mysql
    }

-- |
-- Check that we are ready to be take traffic.
readiness :: Log.Handler -> Connection -> Health.Check
readiness log conn =
  Health.Check "mysql" Health.Fatal (GenericDb.readiness go log conn)
  where
    go :: Simple.Connection -> Simple.Query -> IO ()
    go c q = do
      _ :: List (Simple.Only Int) <- Simple.query_ c q
      pure ()

-- | Find multiple rows.
--
--   @
--     getMany c
--       [MySQL.sql|
--         SELECT id, name FROM my_table
--       |]
--   @
getMany ::
  (HasCallStack, Simple.QueryResults row) =>
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
  (HasCallStack, Simple.QueryResults row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
getOne = withFrozenCallStack modifyExactlyOne

doQuery ::
  (HasCallStack, Simple.QueryResults row) =>
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
  (HasCallStack, Simple.QueryResults row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
modifyExactlyOne conn query =
  doQuery conn query
    |> andThen (Query.expectOne (Query.quasiQuotedString query))

runQuery ::
  (Simple.QueryResults row) =>
  Query.Query row ->
  Simple.Connection ->
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
    |> Simple.query_ conn

toConnectionString :: Simple.ConnectInfo -> Text
toConnectionString
  Simple.ConnectInfo
    { Simple.connectHost,
      Simple.connectPort,
      Simple.connectUser,
      Simple.connectDatabase,
      Simple.connectPath
    } =
    [ connectUser,
      ":*****@",
      if connectHost == ""
        then connectPath
        else
          mconcat
            [ connectHost,
              ":",
              show connectPort,
              "/"
            ],
      connectDatabase
    ]
      |> mconcat
      |> toS

toConnectionLogContext :: Simple.ConnectInfo -> Log.QueryConnectionInfo
toConnectionLogContext
  Simple.ConnectInfo
    { Simple.connectHost,
      Simple.connectPort,
      Simple.connectDatabase,
      Simple.connectPath
    } =
    if connectHost == ""
      then Log.UnixSocket Log.MySQL (toS connectPath) databaseName
      else Log.TcpSocket Log.MySQL (toS connectHost) (show connectPort) databaseName
    where
      databaseName = toS connectDatabase
