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
    PGQuery,
    PGTypes.PGColumn (pgDecode),
    PGTypes.PGParameter (pgEncode),
  )
where

import Control.Exception.Safe (MonadCatch)
import qualified Data.Acquire
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
-- Import orphan `postgresql-typed` array instances.
-- By performing this import here, we're sure these instances will be in scope
-- in every module that contains SQL, because all those modules import this one.
import Database.PostgreSQL.Typed.Array ()
import qualified Database.PostgreSQL.Typed.Array as PGArray
import Database.PostgreSQL.Typed.Protocol
  ( pgBegin,
    pgCommit,
    pgRollback,
    pgRollbackAll,
  )
import Database.PostgreSQL.Typed.Query (PGQuery, getQueryString)
import Database.PostgreSQL.Typed.Types (unknownPGTypeEnv)
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import qualified Log
import Nri.Prelude
import qualified Postgres.Settings as Settings
import qualified Tracer.NewRelic

type Connection = GenericDb.Connection PGConnection

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  GenericDb.connection (Settings.toPGDatabase settings) GenericDb.PoolConfig
    { GenericDb.connect = pgConnect,
      GenericDb.disconnect = pgDisconnect,
      GenericDb.stripes = Settings.unPgPoolStripes (Settings.pgPoolStripes (Settings.pgPool settings)) |> fromIntegral,
      GenericDb.maxIdleTime = Settings.unPgPoolMaxIdleTime (Settings.pgPoolMaxIdleTime (Settings.pgPool settings)),
      GenericDb.size = Settings.unPgPoolSize (Settings.pgPoolSize (Settings.pgPool settings)) |> fromIntegral,
      GenericDb.toConnectionString
    }

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
readiness :: Log.Handler -> Connection -> IO Health.Status
readiness = GenericDb.readiness go
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
  (HasCallStack, PGQuery q a, Show q) =>
  Connection ->
  Query.Query q ->
  Task e [a]
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
  (HasCallStack, PGQuery q a, Show q) =>
  Connection ->
  Query.Query q ->
  Task Query.Error a
getOne = withFrozenCallStack modifyExactlyOne

doQuery ::
  (HasCallStack, PGQuery q a, Show q) =>
  Connection ->
  Query.Query q ->
  Task e [a]
doQuery conn query =
  Query.execute (flip pgQuery) conn query
    |> wrapWithQueryContext query

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
  (HasCallStack, PGQuery q a, Show q) =>
  Connection ->
  Query.Query q ->
  Task Query.Error a
modifyExactlyOne conn query =
  Query.modifyExactlyOne (flip pgQuery) conn query
    |> wrapWithQueryContext query

wrapWithQueryContext :: PGQuery q a => Query.Query q -> Task e b -> Task e b
wrapWithQueryContext (Query.Query query) task =
  Log.withContext "database-query" [Log.context "query" queryInfo] task
  where
    queryInfo = Tracer.NewRelic.QueryInfo
      { Tracer.NewRelic.query = toS <| getQueryString unknownPGTypeEnv query,
        Tracer.NewRelic.engine = Tracer.NewRelic.Postgres
      }

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
