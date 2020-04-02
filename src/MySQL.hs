{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    Query.Error (..),
    Query.sql,
    doQuery,
    -- Handling transactions
    transaction,
    inTestTransaction,
    -- Reexposing useful Database.Persist.MySQL types
    QueryResults,
    MySQL.PersistField (..),
    MySQL.Single (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import qualified Data.Acquire
import qualified Data.Coerce
import Data.IORef
import qualified Data.Pool
import qualified Data.Text
import qualified Data.Text.Encoding
import Data.Word (Word)
import qualified Database.MySQL.Connection
import qualified Database.Persist.MySQL as MySQL
import qualified Debug
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Health
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import qualified Log
import qualified MySQL.Internal as Internal
import qualified MySQL.Settings as Settings
import qualified Platform
import qualified Result
import qualified Task
import qualified Text
import Prelude (IO, error, fromIntegral, pure, show)

type Connection = GenericDb.Connection (TransactionCount, MySQL.SqlBackend) MySQL.SqlBackend

data TransactionCount
  = TransactionCount (IORef Word)

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Platform.doAnythingHandler
      transactionCount <- newIORef 0
      pool <-
        MySQL.createMySQLPool database size
          |> Control.Monad.Logger.runNoLoggingT
          |> map GenericDb.Pool
      pure
        ( GenericDb.Connection
            doAnything
            pool
            (\c -> (TransactionCount transactionCount, c))
            (toConnectionLogContext settings)
            (floor (micro * Settings.mysqlQueryTimeoutSeconds settings))
        )
    release GenericDb.Connection {GenericDb.singleOrPool} =
      case singleOrPool of
        GenericDb.Pool pool -> Data.Pool.destroyAllResources pool
        GenericDb.Single _ -> pure ()
    size = Settings.unMysqlPoolSize (Settings.mysqlPoolSize (Settings.mysqlPool settings)) |> fromIntegral
    database = toConnectInfo settings
    micro = 1000 * 1000

--
-- CONNECTION HELPERS
--

toConnectionLogContext :: Settings.Settings -> Platform.QueryConnectionInfo
toConnectionLogContext settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Settings.unDatabase (Settings.database connectionSettings)
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Platform.UnixSocket
            Platform.MySQL
            (Data.Text.pack (Settings.unSocket socket))
            database
        Settings.ConnectTcp host port ->
          Platform.TcpSocket
            Platform.MySQL
            (Settings.unHost host)
            (Data.Text.pack (show (Settings.unPort port)))
            database

toConnectInfo :: Settings.Settings -> MySQL.MySQLConnectInfo
toConnectInfo settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Data.Text.Encoding.encodeUtf8 (Settings.unDatabase (Settings.database connectionSettings))
      user = Data.Text.Encoding.encodeUtf8 (Settings.unUser (Settings.user connectionSettings))
      password = Data.Text.Encoding.encodeUtf8 (Log.unSecret (Settings.unPassword (Settings.password connectionSettings)))
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
            (Data.Text.unpack (Settings.unHost host))
            user
            password
            database
            |> MySQL.setMySQLConnectInfoPort (fromIntegral (Settings.unPort port))
            |> MySQL.setMySQLConnectInfoCharset Database.MySQL.Connection.utf8mb4_unicode_ci

--
-- READINESS
--

-- |
-- Check that we are ready to be take traffic.
readiness :: Platform.LogHandler -> Connection -> Health.Check
readiness log conn =
  let executeSql :: ( TransactionCount, MySQL.SqlBackend) -> Text -> IO ()
      executeSql (_, backend) query =
        void (executeQuery backend query :: IO [MySQL.Single Int])
   in Health.mkCheck "mysql" (GenericDb.readiness executeSql log conn)

--
-- EXECUTE QUERIES
--
class MySqlQueryable query result | result -> query where
  doQuery :: Connection -> Query.Query query -> (Result Query.Error result -> Task e a) -> Task e a

instance QueryResults row => MySqlQueryable row [row] where
  doQuery = execute executeQuery

instance MySqlQueryable () () where
  doQuery = execute executeCommand

execute :: HasCallStack => (MySQL.SqlBackend -> Text -> IO result) -> Connection -> Query.Query row -> (Result Query.Error result -> Task e a) -> Task e a
execute executeSql conn query handleResponse =
  let --
      queryAsText :: Text
      queryAsText =
        Query.sqlString query
          -- We need this prefix on tables to allow compile-time checks of the query.
          |> Text.replace "monolith." ""
          |> Internal.anyToIn
      --
      runQuery (TransactionCount transactionCount, backend) = do
        result <- attempt executeSql backend queryAsText
        currentCount <- readIORef transactionCount
        case (currentCount, result) of
          (0, Ok value) -> do
            -- If not currently inside a transaction and original query succeeded, then commit
            result2 <- attempt executeCommand backend "COMMIT"
            pure <| Result.map (always value) result2
          _ ->
            pure result
      --
      infoForContext :: Platform.QueryInfo
      infoForContext = Platform.QueryInfo
        { Platform.queryText = Log.mkSecret (Query.sqlString query),
          Platform.queryTemplate = Query.quasiQuotedString query,
          Platform.queryConn = GenericDb.logContext conn,
          Platform.queryOperation = Query.sqlOperation query,
          Platform.queryCollection = Query.queriedRelation query
        }
   in do
        withFrozenCallStack Log.info (Query.asMessage query) []
        GenericDb.runTaskWithConnection conn runQuery
          -- Handle the response before wrapping the operation in a context. This way,
          -- if the response handling logic creates errors, those errors can inherit
          -- context values like the query string.
          |> Task.map Ok
          |> Task.onError (Task.succeed << Err)
          |> andThen handleResponse
          |> Log.withContext "mysql-query" [Platform.queryContext infoForContext]

attempt :: (MySQL.SqlBackend -> Text -> IO result) -> MySQL.SqlBackend -> Text -> IO (Result Query.Error result)
attempt executeSql backend query = do
  either <- Exception.tryAny (executeSql backend query)
  pure <| Result.mapError GenericDb.toQueryError (GenericDb.eitherToResult either)

executeQuery :: QueryResults row => MySQL.SqlBackend -> Text -> IO [row]
executeQuery backend query =
  MySQL.rawSql query []
    |> (\reader -> runReaderT reader backend)
    |> map (map toQueryResult)

executeCommand :: MySQL.SqlBackend -> Text -> IO ()
executeCommand backend query =
  MySQL.rawExecute query []
    |> (\reader -> runReaderT reader backend)

--
-- TRANSACTIONS
--

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction =
  GenericDb.transaction GenericDb.Transaction
    { GenericDb.begin = begin,
      GenericDb.commit = commit,
      GenericDb.rollback = rollback,
      GenericDb.rollbackAll = rollbackAll
    }

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: forall m a. (MonadIO m, Exception.MonadCatch m) => Connection -> (Connection -> m a) -> m a
inTestTransaction =
  GenericDb.inTestTransaction GenericDb.Transaction
    { GenericDb.begin = begin,
      GenericDb.commit = commit,
      GenericDb.rollback = rollback,
      GenericDb.rollbackAll = rollbackAll
    }

addTransaction :: Word -> (Word, Word)
addTransaction count =
  (count + 1, count)

subTransaction :: Word -> (Word, Word)
subTransaction count =
  case count of
    0 -> (0, error "MySQL transaction: Trying to close transaction, but there is no transaction running.")
    nonZero -> (nonZero - 1, nonZero - 1)

-- | Begin a new transaction. If there is already a transaction in progress (created with 'begin' or 'pgTransaction') instead creates a savepoint.
begin :: (TransactionCount, MySQL.SqlBackend) -> IO ()
begin (TransactionCount transactionCount, conn) = do
  current <- atomicModifyIORef' transactionCount addTransaction
  void <| executeCommand conn <| if current == 0 then "BEGIN" else "SAVEPOINT pgt" ++ Debug.toString current

-- | Rollback to the most recent 'begin'.
rollback :: (TransactionCount, MySQL.SqlBackend) -> IO ()
rollback (TransactionCount transactionCount, conn) = do
  current <- atomicModifyIORef' transactionCount subTransaction
  void <| executeCommand conn <| if current == 0 then "ROLLBACK" else "ROLLBACK TO SAVEPOINT pgt" ++ Debug.toString current

-- | Commit the most recent 'begin'.
commit :: (TransactionCount, MySQL.SqlBackend) -> IO ()
commit (TransactionCount transactionCount, conn) = do
  current <- atomicModifyIORef' transactionCount subTransaction
  void <| executeCommand conn <| if current == 0 then "COMMIT" else "RELEASE SAVEPOINT pgt" ++ Debug.toString current

-- | Rollback all active 'begin's.
rollbackAll :: (TransactionCount, MySQL.SqlBackend) -> IO ()
rollbackAll (TransactionCount transactionCount, conn) = do
  writeIORef transactionCount 0
  void <| executeCommand conn "ROLLBACK"

--
-- TYPE CLASSES
--

-- |
-- The persistent library gives us back types matching the constaint `RawSql`.
-- These instances are tuples correspoding to rows, as we like, but
-- unfortunately every field is wrapped in a `Single` constructor. The purpose
-- of this typeclass is to unwrap these constructors so we can return records
-- of plain unwrapped values from this module.
class MySQL.RawSql (FromRawSql a) => QueryResults a where

  type FromRawSql a

  toQueryResult :: FromRawSql a -> a

-- |
-- It would be really sweet if we could get rid of the `Single` wrapper here,
-- and allow single-column values to be QueryResults by themselves without the
-- need for the wrapper. This is how it works on the Postgres side too.
--
-- The straight-forward attempt to remove the `MySQL.Single` wrapper here will
-- result in overlapping instances. Pretty certain there's type-level trickery
-- to work around that, which might be worth exploring at some point to get a
-- cleaner API.
instance (MySQL.PersistField a) => QueryResults (MySQL.Single a) where

  type
    FromRawSql (MySQL.Single a) =
      MySQL.Single a

  toQueryResult = identity

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b
  ) =>
  QueryResults (a, b)
  where

  type
    FromRawSql (a, b) =
      ( MySQL.Single a,
        MySQL.Single b
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c
  ) =>
  QueryResults (a, b, c)
  where

  type
    FromRawSql (a, b, c) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d
  ) =>
  QueryResults (a, b, c, d)
  where

  type
    FromRawSql (a, b, c, d) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e
  ) =>
  QueryResults (a, b, c, d, e)
  where

  type
    FromRawSql (a, b, c, d, e) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f
  ) =>
  QueryResults (a, b, c, d, e, f)
  where

  type
    FromRawSql (a, b, c, d, e, f) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g
  ) =>
  QueryResults (a, b, c, d, e, f, g)
  where

  type
    FromRawSql (a, b, c, d, e, f, g) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g
      )

  toQueryResult = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h
  ) =>
  QueryResults (a, b, c, d, e, f, g, h)
  where

  type
    FromRawSql (a, b, c, d, e, f, g, h) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h
      )

  toQueryResult = Data.Coerce.coerce
