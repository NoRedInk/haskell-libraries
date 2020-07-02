{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    Query.Error (..),
    Query.sql,
    doQuery,
    -- Handling transactions
    transaction,
    inTestTransaction,
    inTestTransactionIo,
    -- Reexposing useful postgresql-typed types
    PGArray.PGArray,
    PGArray.PGArrayType,
    PGTypes.PGColumn (pgDecode),
    PGTypes.PGParameter (pgEncode),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad (void)
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
  ( PGError,
    pgBegin,
    pgCommit,
    pgErrorCode,
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
import qualified Result
import qualified Task
import Prelude ((<>), Either (Left, Right), IO, error, fromIntegral, mconcat, pure, show)

type Connection = GenericDb.Connection PGConnection PGConnection

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Platform.doAnythingHandler
      pool <-
        map GenericDb.Pool
          <| Data.Pool.createPool
            (pgConnect database `Exception.catch` GenericDb.handleError (toConnectionString database))
            pgDisconnect
            stripes
            maxIdleTime
            size
      pure
        ( GenericDb.Connection
            doAnything
            pool
            identity
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
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction =
  GenericDb.inTestTransaction GenericDb.Transaction
    { GenericDb.begin = pgBegin,
      GenericDb.commit = pgCommit,
      GenericDb.rollback = pgRollback,
      GenericDb.rollbackAll = pgRollbackAll
    }

-- | DON'T USE. Prefer to arrange your tests around Task, not IO.
--   Same as `inTestTransaction` but for IO. Should be removed when no
--   tests depend on it anymore.
inTestTransactionIo :: Postgres.Connection -> (Postgres.Connection -> IO a) -> IO a
inTestTransactionIo postgres io = do
  doAnything <- Platform.doAnythingHandler
  logHandler <- Platform.silentContext
  result <- Task.attempt logHandler <| Postgres.inTestTransaction postgres <| \c -> Platform.doAnything doAnything (io c |> map Ok)
  case result of
    Ok a -> pure a
    Err _ -> error "This should never happen."

-- |
-- Check that we are ready to be take traffic.
readiness :: Platform.LogHandler -> Connection -> Health.Check
readiness log conn = Health.mkCheck "postgres" (GenericDb.readiness go log conn)
  where
    go :: PGConnection -> ByteString -> IO ()
    go c = pgQuery c >> void

doQuery ::
  HasCallStack =>
  Connection ->
  Query.Query row ->
  (Result Query.Error [row] -> Task e a) ->
  Task e a
doQuery conn query handleResponse = do
  withFrozenCallStack Log.info (Query.asMessage query) []
  let runQuery c =
        Query.runQuery query c
          |> Exception.try
          |> map (Result.mapError (fromPGError conn) << GenericDb.eitherToResult)
  GenericDb.runTaskWithConnection conn runQuery
    -- Handle the response before wrapping the operation in a context. This way,
    -- if the response handling logic creates errors, those errors can inherit
    -- context values like the query string.
    |> intoResult
    |> andThen handleResponse
    |> Log.withContext "postgresql-query" [Platform.queryContext queryInfo]
  where
    queryInfo = Platform.QueryInfo
      { Platform.queryText = Log.mkSecret (Query.sqlString query),
        Platform.queryTemplate = Query.quasiQuotedString query,
        Platform.queryConn = GenericDb.logContext conn,
        Platform.queryOperation = Query.sqlOperation query,
        Platform.queryCollection = Query.queriedRelation query
      }

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
        |> Data.Text.pack
        |> Query.UniqueViolation
    "57014" ->
      Query.TimeoutAfterSeconds Query.ServerTimeout (fromIntegral (GenericDb.timeoutMicroSeconds c) / 10e6)
    _ ->
      Exception.displayException pgError
        |> Data.Text.pack
        |> Query.Other

intoResult :: Task e a -> Task e2 (Result e a)
intoResult task =
  map Ok task
    |> Task.onError (Task.succeed << Err)

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

-- useful typeclass instances
instance PGTypes.PGType "jsonb" => PGTypes.PGType "jsonb[]" where
  type PGVal "jsonb[]" = PGArray.PGArray (PGTypes.PGVal "jsonb")

instance PGTypes.PGType "jsonb" => PGArray.PGArrayType "jsonb[]" where
  type PGElemType "jsonb[]" = "jsonb"
