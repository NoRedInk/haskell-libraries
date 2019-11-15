-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module Postgres
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
    -- Reexposing useful postgresql-typed types
    PGArray.PGArray,
    PGArray.PGArrayType,
    PGTypes.PGColumn (pgDecode),
    PGTypes.PGParameter (pgEncode),
  )
where

import Control.Exception.Safe (MonadCatch)
import qualified Data.Acquire
import qualified Data.Pool
import Database.PostgreSQL.Typed
  ( PGConnection,
    PGDatabase (PGDatabase),
    pgConnect,
    pgDBAddr,
    pgDBName,
    pgDBUser,
    pgDisconnect,
    pgQuery,
  )
import qualified Database.PostgreSQL.Typed.Array as PGArray
import Database.PostgreSQL.Typed.Protocol
  ( pgBegin,
    pgCommit,
    pgRollback,
    pgRollbackAll,
  )
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import qualified Log
import Network.Socket (SockAddr (..))
import Nri.Prelude
import qualified Nri.Task as Task
import qualified Postgres.Settings as Settings
import Prelude (error)

type Connection = GenericDb.Connection PGConnection

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Task.handler
      pool <-
        map GenericDb.Pool
          <| Data.Pool.createPool
            (pgConnect database `catch` GenericDb.handleError (toConnectionString database))
            pgDisconnect
            stripes
            maxIdleTime
            size
      pure (GenericDb.Connection doAnything pool (toConnectionLogContext database))
    release GenericDb.Connection {GenericDb.singleOrPool} =
      case singleOrPool of
        GenericDb.Pool pool -> Data.Pool.destroyAllResources pool
        GenericDb.Single single -> pgDisconnect single
    stripes = Settings.unPgPoolStripes (Settings.pgPoolStripes (Settings.pgPool settings)) |> fromIntegral
    maxIdleTime = Settings.unPgPoolMaxIdleTime (Settings.pgPoolMaxIdleTime (Settings.pgPool settings))
    size = Settings.unPgPoolSize (Settings.pgPoolSize (Settings.pgPool settings)) |> fromIntegral
    database = Settings.toPGDatabase settings

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction =
  GenericDb.transaction GenericDb.Transaction
    { GenericDb.begin = pgBegin,
      GenericDb.commit = pgCommit,
      GenericDb.rollback = pgRollback,
      GenericDb.rollbackAll = pgRollbackAll
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
    { GenericDb.begin = pgBegin,
      GenericDb.commit = pgCommit,
      GenericDb.rollback = pgRollback,
      GenericDb.rollbackAll = pgRollbackAll
    }

-- |
-- Check that we are ready to be take traffic.
readiness :: Log.Handler -> Connection -> Health.Check
readiness log conn = Health.Check "postgres" Health.Fatal (GenericDb.readiness go log conn)
  where
    go :: PGConnection -> ByteString -> IO ()
    go c = pgQuery c >> void

-- | Find multiple rows.
--
--   @
--     getMany c
--       [Postgres.sql|
--         SELECT id, name FROM my_table
--       |]
--   @
getMany ::
  (HasCallStack) =>
  Connection ->
  Query.Query row ->
  Task e [row]
getMany = withFrozenCallStack doQuery

-- | returns one object!
--
--   @
--     getOne c
--       [Postgres.sql|
--         select title from my_table where id = 1
--       |]
--   @
getOne ::
  (HasCallStack) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
getOne = withFrozenCallStack modifyExactlyOne

doQuery ::
  (HasCallStack) =>
  Connection ->
  Query.Query row ->
  Task e [row]
doQuery conn query = do
  withFrozenCallStack Log.debug (Query.quasiQuotedString query) []
  GenericDb.runTaskWithConnection conn (Query.runQuery query)
    |> Log.withContext "postgresql-query" [Log.Query queryInfo]
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
--       [Postgres.sql|
--         INSERT INTO my_table (name)
--           VALUES ($1)
--         RETURNING id, name
--       |]
--   @
modifyExactlyOne ::
  (HasCallStack) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
modifyExactlyOne conn query =
  doQuery conn query
    |> andThen (Query.expectOne (Query.quasiQuotedString query))

toConnectionString :: PGDatabase -> Text
toConnectionString PGDatabase {pgDBUser, pgDBAddr, pgDBName} =
  mconcat
    [ toS pgDBUser,
      ":*****@",
      case pgDBAddr of
        Right sockAddr ->
          show sockAddr
        Left (hostName, serviceName) ->
          toS hostName
            <> ":"
            <> toS serviceName,
      "/",
      toS pgDBName
    ] ::
    Text

toConnectionLogContext :: PGDatabase -> Log.QueryConnectionInfo
toConnectionLogContext db =
  case pgDBAddr db of
    Left (hostName, serviceName) ->
      Log.TcpSocket Log.Postgres (toS hostName) (toS serviceName) databaseName
    Right (SockAddrInet portNum hostAddr) ->
      Log.TcpSocket Log.Postgres (show hostAddr) (show portNum) databaseName
    Right (SockAddrInet6 portNum _flowInfo hostAddr _scopeId) ->
      Log.TcpSocket Log.Postgres (show hostAddr) (show portNum) databaseName
    Right (SockAddrUnix sockPath) ->
      Log.UnixSocket Log.Postgres (toS sockPath) databaseName
    Right somethingElse ->
      -- There's a deprecated `SockAddr` constructor called `SockAddrCan`.
      error
        ( "Failed to convert PostgreSQL database address; no idea what a "
            ++ show somethingElse
            ++ " is."
        )
  where
    databaseName = pgDBName db |> toS
