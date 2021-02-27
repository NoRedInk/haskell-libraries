{-# LANGUAGE QuasiQuotes #-}
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
    Query.Info (..),
    Query.ConnectionInfo (..),
    -- Handling transactions
    transaction,
    inTestTransaction,
    inTestTransactionIo,
    -- Reexposing useful postgresql-typed types
    PGArray.PGArray,
    PGArray.PGArrayType,
    PGTypes.PGColumn (pgDecode),
    PGTypes.PGParameter (pgEncode),
    -- tests only!
    connectionIO,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Acquire
import qualified Data.Pool
import qualified Data.Text.Encoding
import Database.PostgreSQL.Typed
  ( PGConnection,
    PGDatabase (PGDatabase),
    pgConnect,
    pgDBAddr,
    pgDBName,
    pgDBUser,
    pgDisconnect,
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
import qualified Internal.Time as Time
import qualified List
import qualified Log
import Network.Socket (SockAddr (..))
import qualified Platform
import qualified Postgres.Query as Query
import qualified Postgres.Settings as Settings
import qualified System.Exit
import qualified Task
import qualified Tuple
import Prelude (Either (Left, Right), IO, error, fromIntegral, mconcat, pure, putStrLn, show, (<>))

data Connection = Connection
  { doAnything :: Platform.DoAnythingHandler,
    singleOrPool :: SingleOrPool PGConnection,
    logContext :: Query.ConnectionInfo,
    timeout :: Time.Interval
  }

-- | A database connection type.
--   Defining our own type makes it easier to change it in the future, without
--   having to fix compilation errors all over the codebase.
data SingleOrPool c
  = -- | By default a connection pool is passed around. It will:
    --   - Create new connections in the pool up to a certain limit.
    --   - Remove connections from the pool after a query in a connection errored.
    Pool (Data.Pool.Pool c)
  | -- | A single connection is only used in the context of a transaction, where
    --   we need to insure several SQL statements happen on the same connection.
    Single c

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire (connectionIO settings) release
  where
    release Connection {singleOrPool} =
      case singleOrPool of
        Pool pool -> Data.Pool.destroyAllResources pool
        Single single -> pgDisconnect single

connectionIO :: Settings.Settings -> IO Connection
connectionIO settings = do
  let database = Settings.toPGDatabase settings
  let stripes = Settings.unPgPoolStripes (Settings.pgPoolStripes (Settings.pgPool settings)) |> fromIntegral
  let maxIdleTime = Settings.unPgPoolMaxIdleTime (Settings.pgPoolMaxIdleTime (Settings.pgPool settings))
  let size = Settings.unPgPoolSize (Settings.pgPoolSize (Settings.pgPool settings)) |> fromIntegral
  doAnything <- Platform.doAnythingHandler
  pool <-
    map Pool
      <| Data.Pool.createPool
        (pgConnect database `Exception.catch` handleError (toConnectionString database))
        pgDisconnect
        stripes
        maxIdleTime
        size
  pure
    ( Connection
        doAnything
        pool
        (toConnectionLogContext database)
        (Settings.pgQueryTimeout settings)
    )

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction conn func =
  let start :: PGConnection -> Task x PGConnection
      start c =
        doIO conn <| do
          pgBegin c
          pure c
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
        conn {singleOrPool = Single c}
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
        pure c
      --
      end :: Platform.Succeeded -> PGConnection -> Task x ()
      end _ c =
        rollbackAllSafe conn c
      --
      setSingle :: PGConnection -> Connection
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single c}
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

-- | DON'T USE. Prefer to arrange your tests around Task, not IO.
--   Same as `inTestTransaction` but for IO. Should be removed when no
--   tests depend on it anymore.
inTestTransactionIo :: Postgres.Connection -> (Postgres.Connection -> IO a) -> IO a
inTestTransactionIo postgres io = do
  doAnything <- Platform.doAnythingHandler
  logHandler <- Platform.silentHandler
  result <- Task.attempt logHandler <| Postgres.inTestTransaction postgres <| \c -> Platform.doAnything doAnything (io c |> map Ok)
  case result of
    Ok a -> pure a
    Err _ -> error "This should never happen."

-- |
-- Check that we are ready to be take traffic.
readiness :: Connection -> Health.Check
readiness conn =
  Health.mkCheck "postgres" <| do
    log <- Platform.silentHandler
    runQuery conn [Query.sql|!SELECT 1|]
      |> map (\(_ :: [Int]) -> ())
      |> Task.mapError (Exception.displayException >> Text.fromList)
      |> Task.attempt log
      |> map Health.fromResult

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
                       ( Query.infoSqlOperation queryInfo
                           ++ " "
                           ++ Query.infoQueriedRelation queryInfo
                       )
                 )
             -- If we end up here it means the query succeeded. Overwrite the tracing
             -- details to contain the amount of selected rows. This information can be
             -- useful when debugging slow queries.
             Platform.setTracingSpanDetails queryInfo {Query.infoRowsReturned = List.length res}
             Prelude.pure res
       )
    |> map Ok
    |> Task.onError (Task.succeed << Err)
    |> andThen handleResponse
  where
    queryInfo = Query.mkInfo query (logContext conn)

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
      Query.Timeout Query.ServerTimeout (timeout c)
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

toConnectionString :: PGDatabase -> Text
toConnectionString PGDatabase {pgDBUser, pgDBAddr, pgDBName} =
  mconcat
    [ Data.Text.Encoding.decodeUtf8 pgDBUser,
      ":*****@",
      case pgDBAddr of
        Right sockAddr ->
          Text.fromList (show sockAddr)
        Left (hostName, serviceName) ->
          Text.fromList hostName
            <> ":"
            <> Text.fromList serviceName,
      "/",
      Data.Text.Encoding.decodeUtf8 pgDBName
    ] ::
    Text

toConnectionLogContext :: PGDatabase -> Query.ConnectionInfo
toConnectionLogContext db =
  case pgDBAddr db of
    Left (hostName, serviceName) ->
      Query.TcpSocket (Text.fromList hostName) (Text.fromList serviceName) databaseName
    Right (SockAddrInet portNum hostAddr) ->
      Query.TcpSocket (Text.fromList (show hostAddr)) (Text.fromList (show portNum)) databaseName
    Right (SockAddrInet6 portNum _flowInfo hostAddr _scopeId) ->
      Query.TcpSocket (Text.fromList (show hostAddr)) (Text.fromList (show portNum)) databaseName
    Right (SockAddrUnix sockPath) ->
      Query.UnixSocket (Text.fromList sockPath) databaseName
  where
    databaseName = pgDBName db |> Data.Text.Encoding.decodeUtf8

handleError :: Text -> Exception.IOException -> IO a
handleError connectionString err = do
  putStrLn "I couldn't connect to Postgres"
  putStrLn ""
  putStrLn "Is the database running? You can start it by running `aide setup-postgres`."
  putStrLn ("I tried to connect to: " ++ Text.toList connectionString)
  System.Exit.die (Exception.displayException err)

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
            Right x -> Ok x
            Left err -> Err (fromPGError conn err)
        )
      |> Platform.doAnything (doAnything conn)
      |> withTimeout conn

withTimeout :: Connection -> Task Query.Error a -> Task Query.Error a
withTimeout conn task =
  if Time.microseconds (timeout conn) > 0
    then
      Task.timeout
        (Time.milliseconds (timeout conn))
        (Query.Timeout Query.ClientTimeout (timeout conn))
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
      case singleOrPool conn of
        (Single c) ->
          func c
        --
        (Pool pool) ->
          Platform.bracketWithError (acquire pool) (release pool) (Tuple.first >> func)

doIO :: Connection -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)

-- useful typeclass instances
instance PGTypes.PGType "jsonb" => PGTypes.PGType "jsonb[]" where
  type PGVal "jsonb[]" = PGArray.PGArray (PGTypes.PGVal "jsonb")

instance PGTypes.PGType "jsonb" => PGArray.PGArrayType "jsonb[]" where
  type PGElemType "jsonb[]" = "jsonb"
