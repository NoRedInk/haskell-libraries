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

import Cherry.Prelude
import Control.Exception.Safe (MonadCatch, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Acquire
import Data.ByteString (ByteString)
import qualified Data.Pool
import qualified Data.Text
import qualified Data.Text.Encoding
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
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import qualified Log
import Network.Socket (SockAddr (..))
import qualified Platform
import qualified Postgres.Settings as Settings
import Prelude ((<>), Either (Left, Right), IO, error, fromIntegral, mconcat, pure, show)

type Connection = GenericDb.Connection PGConnection

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Platform.doAnythingHandler
      pool <-
        map GenericDb.Pool
          <| Data.Pool.createPool
            (pgConnect database `catch` GenericDb.handleError (toConnectionString database))
            pgDisconnect
            stripes
            maxIdleTime
            size
      pure
        ( GenericDb.Connection
            doAnything
            pool
            (toConnectionLogContext database)
            (floor (micro * Settings.pgQueryTimeoutSeconds settings))
        )
    release GenericDb.Connection {GenericDb.singleOrPool} =
      case singleOrPool of
        GenericDb.Pool pool -> Data.Pool.destroyAllResources pool
        GenericDb.Single single -> pgDisconnect single
    stripes = Settings.unPgPoolStripes (Settings.pgPoolStripes (Settings.pgPool settings)) |> fromIntegral
    maxIdleTime = Settings.unPgPoolMaxIdleTime (Settings.pgPoolMaxIdleTime (Settings.pgPool settings))
    size = Settings.unPgPoolSize (Settings.pgPoolSize (Settings.pgPool settings)) |> fromIntegral
    database = Settings.toPGDatabase settings
    micro = 1000 * 1000

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
readiness :: Platform.LogHandler -> Connection -> Health.Check
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
    |> Log.withContext "postgresql-query" [Platform.Query queryInfo]
  where
    queryInfo = Platform.QueryInfo
      { Platform.queryText = Log.mkSecret (Query.sqlString query),
        Platform.queryTemplate = Query.quasiQuotedString query,
        Platform.queryConn = GenericDb.logContext conn,
        Platform.queryOperation = Query.sqlOperation query,
        Platform.queryCollection = Query.queriedRelation query
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
    [ Data.Text.Encoding.decodeUtf8 pgDBUser,
      ":*****@",
      case pgDBAddr of
        Right sockAddr ->
          Data.Text.pack (show sockAddr)
        Left (hostName, serviceName) ->
          Data.Text.pack hostName
            <> ":"
            <> Data.Text.pack serviceName,
      "/",
      Data.Text.Encoding.decodeUtf8 pgDBName
    ] ::
    Text

toConnectionLogContext :: PGDatabase -> Platform.QueryConnectionInfo
toConnectionLogContext db =
  case pgDBAddr db of
    Left (hostName, serviceName) ->
      Platform.TcpSocket Platform.Postgres (Data.Text.pack hostName) (Data.Text.pack serviceName) databaseName
    Right (SockAddrInet portNum hostAddr) ->
      Platform.TcpSocket Platform.Postgres (Data.Text.pack (show hostAddr)) (Data.Text.pack (show portNum)) databaseName
    Right (SockAddrInet6 portNum _flowInfo hostAddr _scopeId) ->
      Platform.TcpSocket Platform.Postgres (Data.Text.pack (show hostAddr)) (Data.Text.pack (show portNum)) databaseName
    Right (SockAddrUnix sockPath) ->
      Platform.UnixSocket Platform.Postgres (Data.Text.pack sockPath) databaseName
    Right somethingElse ->
      -- There's a deprecated `SockAddr` constructor called `SockAddrCan`.
      error
        ( "Failed to convert PostgreSQL database address; no idea what a "
            ++ show somethingElse
            ++ " is."
        )
  where
    databaseName = pgDBName db |> Data.Text.Encoding.decodeUtf8
