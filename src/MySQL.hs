{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
    Query,
    Query.Error (..),
    sql,
    doQuery,
    -- Handling transactions
    transaction,
    inTestTransaction,
    -- Reexposing useful Database.Persist.MySQL types
    MySQL.PersistField (..),
    -- Helpers for uncommon queries
    unsafeBulkifyInserts,
    BulkifiedInsert (..),
    onConflictUpdate,
    onDuplicateDoNothing,
    sqlYearly,
    lastInsertedPrimaryKey,
    escape,
    replace,
  )
where

import Cherry.Prelude hiding (e)
import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Lens.Regex.Text as R
import Control.Monad (void)
import qualified Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import qualified Data.Acquire
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Coerce
import Data.IORef
import qualified Data.Int
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import qualified Data.Text.Encoding
import Data.Word (Word)
import qualified Database.MySQL.Connection
import qualified Database.MySQL.Protocol.Escape as Escape
import qualified Database.Persist.MySQL as MySQL
import Database.Persist.MySQL (RawSql (..))
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Debug
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (Symbol)
import qualified Health
import Internal.CaselessRegex (caselessRegex)
import qualified Internal.GenericDb as GenericDb
import qualified Internal.Query as Query
import Internal.Query (Query (..))
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified List
import qualified Log
import qualified MySQL.Internal as Internal
import qualified MySQL.Settings as Settings
import qualified Platform
import qualified Result
import qualified Task
import qualified Text
import qualified Tuple
import Prelude (IO, error, fromIntegral, pure, show)
import qualified Prelude

type Connection = GenericDb.Connection (TransactionCount, MySQL.SqlBackend) MySQL.SqlBackend

newtype TransactionCount
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
  let executeSql :: (TransactionCount, MySQL.SqlBackend) -> Text -> IO ()
      executeSql (_, backend) query =
        void (executeQuery backend query :: IO [Int])
   in Health.mkCheck "mysql" (GenericDb.readiness executeSql log conn)

--
-- EXECUTE QUERIES
--
class MySqlQueryable query result | result -> query where
  doQuery :: Connection -> Query.Query query -> (Result Query.Error result -> Task e a) -> Task e a

instance QueryResults (CountColumns row) row => MySqlQueryable row [row] where
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

executeQuery :: forall row. QueryResults (CountColumns row) row => MySQL.SqlBackend -> Text -> IO [row]
executeQuery backend query =
  MySQL.rawSql query []
    |> (\reader -> runReaderT reader backend)
    |> map (map (toQueryResult (Proxy :: Proxy (CountColumns row))))

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
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
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
  current <- readIORef transactionCount
  if current == 0
    then pure ()
    else do
      rollback (TransactionCount transactionCount, conn)
      rollbackAll (TransactionCount transactionCount, conn)

--
-- TYPE CLASSES
--

-- |
-- The persistent library gives us back types matching the constaint `RawSql`.
-- These instances are tuples correspoding to rows, as we like, but
-- unfortunately every field is wrapped in a `Single` constructor. The purpose
-- of this typeclass is to unwrap these constructors so we can return records
-- of plain unwrapped values from this module.
class MySQL.RawSql (FromRawSql c a) => QueryResults (c :: ColumnCount) a where

  type FromRawSql c a

  toQueryResult :: Proxy c -> FromRawSql c a -> a

data ColumnCount = SingleColumn | MultipleColumns

type family CountColumns (c :: Type) :: ColumnCount where
  CountColumns (a, b) = 'MultipleColumns
  CountColumns (a, b, c) = 'MultipleColumns
  CountColumns (a, b, c, d) = 'MultipleColumns
  CountColumns (a, b, c, d, e) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = 'MultipleColumns
  CountColumns x = 'SingleColumn

instance (MySQL.PersistField a) => QueryResults 'SingleColumn a where

  type
    FromRawSql 'SingleColumn a =
      (MySQL.Single a)

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b
  ) =>
  QueryResults 'MultipleColumns (a, b)
  where

  type
    FromRawSql 'MultipleColumns (a, b) =
      ( MySQL.Single a,
        MySQL.Single b
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c
  ) =>
  QueryResults 'MultipleColumns (a, b, c)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g
      )

  toQueryResult _ = Data.Coerce.coerce

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
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l,
    MySQL.PersistField m
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l,
        MySQL.Single m
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l,
    MySQL.PersistField m,
    MySQL.PersistField n
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l,
        MySQL.Single m,
        MySQL.Single n
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l,
    MySQL.PersistField m,
    MySQL.PersistField n,
    MySQL.PersistField o
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l,
        MySQL.Single m,
        MySQL.Single n,
        MySQL.Single o
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l,
    MySQL.PersistField m,
    MySQL.PersistField n,
    MySQL.PersistField o,
    MySQL.PersistField p
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l,
        MySQL.Single m,
        MySQL.Single n,
        MySQL.Single o,
        MySQL.Single p
      )

  toQueryResult _ = Data.Coerce.coerce

instance
  ( MySQL.PersistField a,
    MySQL.PersistField b,
    MySQL.PersistField c,
    MySQL.PersistField d,
    MySQL.PersistField e,
    MySQL.PersistField f,
    MySQL.PersistField g,
    MySQL.PersistField h,
    MySQL.PersistField i,
    MySQL.PersistField j,
    MySQL.PersistField k,
    MySQL.PersistField l,
    MySQL.PersistField m,
    MySQL.PersistField n,
    MySQL.PersistField o,
    MySQL.PersistField p,
    MySQL.PersistField q
  ) =>
  QueryResults 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  where

  type
    FromRawSql 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
      ( MySQL.Single a,
        MySQL.Single b,
        MySQL.Single c,
        MySQL.Single d,
        MySQL.Single e,
        MySQL.Single f,
        MySQL.Single g,
        MySQL.Single h,
        MySQL.Single i,
        MySQL.Single j,
        MySQL.Single k,
        MySQL.Single l,
        MySQL.Single m,
        MySQL.Single n,
        MySQL.Single o,
        MySQL.Single p,
        MySQL.Single q
      )

  toQueryResult _ = Data.Coerce.coerce

-- | Combine a number of insert queries that all insert a single row into the
-- same table into a single query.
--
-- This helper is not type-safe. It projects you from accidentally combining
-- queries that are selects, because it only work for queries that return `()`.
-- But if you try to combine queries that insert into different tables, or
-- insert different columns, then you're going to see some weird runtime errors.
--
-- For queries against Postgres there's a better ways to do this, see this
-- how-to:
-- https://github.com/NoRedInk/NoRedInk/blob/master/docs/how-tos/insert-multiple-rows-postgresql-typed.md
--
-- For MySQL there might be other approaches we could take, which might offer
-- more compile-time guarantees.
data BulkifiedInsert a
  = BulkifiedInsert a
  | UnableToBulkify Text
  | EmptyInsert
  deriving (Prelude.Functor, Show, Eq)

unsafeBulkifyInserts :: [Query ()] -> BulkifiedInsert (Query ())
unsafeBulkifyInserts [] = EmptyInsert
unsafeBulkifyInserts (first : rest) =
  case maybeBrokenQueries of
    Nothing -> UnableToBulkify "Not all queries are inserts with a VALUES keyword."
    Just (_ :| otherQueries) ->
      first {sqlString = Data.Text.intercalate "," (sqlString first : otherQueries)}
        |> BulkifiedInsert
  where
    maybeBrokenQueries = Prelude.traverse (dropUntilCaseInsensitive "VALUES" << sqlString) (first :| rest)

-- | Appends a query with `ON DUPLICATE KEY UPDATE` to allow updating in case
-- the key isn't unique.
onConflictUpdate :: [Text] -> Query () -> Query ()
onConflictUpdate columns q@Query {sqlString, sqlOperation} =
  let onDuplicateKeyUPDATE = "ON DUPLICATE KEY UPDATE"
   in q
        { sqlString =
            Text.join
              " "
              [ sqlString,
                onDuplicateKeyUPDATE,
                columns
                  |> List.map (\column -> column ++ " = VALUES(" ++ column ++ ")")
                  |> Text.join ","
              ],
          sqlOperation = sqlOperation ++ " " ++ onDuplicateKeyUPDATE
        }

dropUntilCaseInsensitive :: Text -> Text -> Maybe Text
dropUntilCaseInsensitive breaker original =
  let (start, end) = Data.Text.breakOn (Data.Text.toLower breaker) (Data.Text.toLower original)
   in if Data.Text.null end
        then Nothing
        else
          Data.Text.splitAt (Data.Text.length (start ++ "values")) original
            |> Tuple.second
            |> Just

-- | Use for insert queries that are allowed to fail. In Postgres we would use
-- an `ON CONFLICT DO NOTHING` clause for this, but MySQL doesn't support it.
-- `MySQL` recommends using `INSERT IGNORE` syntax, but that Postgres does not
-- support. This helper hacks the `IGNORE` clause into a query after we run
-- Postgres compile-time checks.
onDuplicateDoNothing :: Query () -> Query ()
onDuplicateDoNothing query =
  query
    { sqlString =
        Lens.over
          ([caselessRegex|^\s*INSERT INTO|] << R.match)
          (\_ -> "INSERT IGNORE INTO")
          (sqlString query)
    }

-- | Special quasi quoter for accessing yearly tables like `mastery_2019`. Use
-- it like this:
--
--     MySQL.doQuery
--       handler
--       (
--         [sqlYearly|!
--           SELECT * FROM mastery_[[YEAR]]
--           LIMIT 1
--         |]
--         2019
--       )
--
-- How this works: whereas the `sql` quasiquoter generates code of a type
-- `Query row`, `sqlYearly` generates code of a type `Int -> Query row`: That's
-- a function that takes a year and substitutes it in the place of the
-- `[[YEAR]]` placeholder.
sqlYearly :: QuasiQuoter
sqlYearly =
  QuasiQuoter
    { quoteExp = qqSQLYearly,
      quoteType = Prelude.fail "sql not supported in types",
      quotePat = Prelude.fail "sql not supported in patterns",
      quoteDec = Prelude.fail "sql not supported in declarations"
    }

qqSQLYearly :: Prelude.String -> ExpQ
qqSQLYearly query =
  let queryFor :: Int -> Prelude.String
      queryFor year =
        Data.Text.pack query
          |> Data.Text.replace "[[YEAR]]" (Text.fromInt year)
          |> Data.Text.unpack
   in [e|
        ( \(year :: Int) ->
            case year of
              2015 -> $(quoteExp sql (queryFor 2015))
              2016 -> $(quoteExp sql (queryFor 2016))
              2017 -> $(quoteExp sql (queryFor 2017))
              2018 -> $(quoteExp sql (queryFor 2018))
              2019 -> $(quoteExp sql (queryFor 2019))
              2020 -> $(quoteExp sql (queryFor 2020))
              2021 -> $(quoteExp sql (queryFor 2021))
              2022 -> $(quoteExp sql (queryFor 2022))
              2023 -> $(quoteExp sql (queryFor 2023))
              2024 -> $(quoteExp sql (queryFor 2024))
              2025 -> $(quoteExp sql (queryFor 2025))
              2026 -> $(quoteExp sql (queryFor 2026))
              2027 -> $(quoteExp sql (queryFor 2027))
              2028 -> $(quoteExp sql (queryFor 2028))
              2029 -> $(quoteExp sql (queryFor 2029))
              _ -> Prelude.error ("Unsupported school year: " ++ Prelude.show year)
        )
        |]

-- |
-- Get the primary key of the last row inserted by the MySQL connection we're
-- currently in. This uses MySQL's `LAST_INSERT_ID()` function and has all the
-- same caveats and limitations. In particular:
--
-- - This gets the last inserted by the current connection. This means we can
--   only use it within a MySQL transaction created using this library. Such a
--   transaction holds on to a reserved connection, preventing other threads
--   from using the connection to make requests in between our `INSERT` query
--   and our use of this function.
-- - If the last insert inserted multiple rows this function will return the
--   primary key of the first of this batch of rows that was inserted.
--
-- In Postgres use a `RETURNING` statement to get inserted id's for inserted
-- rows:
--
--    insertedIds <-
--      Postgres.doQuery
--        [Postgres.sql|!
--          INSERT INTO peanut_butters (brand, chunkiness)
--          VALUES ('Original', 'granite')
--          RETURNING id
--        |]
--        expectQuerySuccess
--
-- For more information: https://dev.mysql.com/doc/refman/8.0/en/getting-unique-id.html
lastInsertedPrimaryKey :: Connection -> Task Query.Error (Maybe Int)
lastInsertedPrimaryKey c =
  let query =
        Query.Query
          { runQuery = \_ -> pure [],
            sqlString = "SELECT LAST_INSERT_ID()",
            quasiQuotedString = "SELECT LAST_INSERT_ID()",
            sqlOperation = "SELECT",
            queriedRelation = "LAST_INSERT_ID()"
          }
   in doQuery
        c
        query
        ( \res -> case res of
            Ok [] -> Task.succeed Nothing
            Ok (x : _) -> Task.succeed x
            Err err -> Task.fail err
        )

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = qqSQL,
      quoteType = Prelude.fail "sql not supported in types",
      quotePat = Prelude.fail "sql not supported in patterns",
      quoteDec = Prelude.fail "sql not supported in declarations"
    }

qqSQL :: Prelude.String -> ExpQ
qqSQL query =
  [e|
    $(quoteExp Query.sql (Data.Text.unpack (escapeInterpolations (Data.Text.pack query))))
    |]

escapeInterpolations :: Text -> Text
escapeInterpolations =
  Lens.over
    ([caselessRegex|\$\{([^\}]+)\}|] << R.group 0)
    (\match -> "MySQL.escape (" ++ match ++ ")")

-- | Types wrapped in `Escaped` get escaped in a MySQL rather than a
-- Postgresql fashion when used as column values.
newtype Escaped a = Escaped a deriving (Show, Eq, PGTypes.PGColumn t)

instance
  ( PGTypes.PGParameter t a,
    KnownEscapingStrategy t a (HowToEscape t a)
  ) =>
  PGTypes.PGParameter t (MySQL.Escaped a)
  where

  pgEncode p (Escaped t) = PGTypes.pgEncode p t

  pgLiteral p (Escaped t) =
    escapeType (Proxy :: Proxy (HowToEscape t a)) p t

-- A type family is a function for types. The type family (function) below
-- takes two arguments: The postgres type to encode into and the Haskell type
-- to encode. It returns a type representing the escaping strategy to use.
-- Example usage:
--
--     HowToEscape "text" Text       --> EscapeMySQLText
--     HowToEscape "text" Maybe Text --> Nullable EscapeMySQLText
type family HowToEscape (t :: Symbol) (a :: Type) :: EscapingStrategy where
  HowToEscape t (Maybe a) = 'Nullable (HowToEscape t a)
  HowToEscape "text" a = 'EscapeMySqlText
  HowToEscape "character varying" a = 'EscapeMySqlText
  HowToEscape "json" a = 'EscapeMySqlText
  HowToEscape t a = 'EscapeSameAsPostgres

-- The different escaping strategies we perform for different types.
data EscapingStrategy
  = EscapeSameAsPostgres -- We let `postgresql-typed` escape for us.
  | EscapeMySqlText -- MySQL-specific escaping for text-columns.
  | Nullable EscapingStrategy -- We don't want to escape `NULL` values.

-- The type above enumerates the escaping strategies. The class below and it's
-- instances represent the implementations for the enumerated strategies.
class KnownEscapingStrategy t a (e :: EscapingStrategy) where
  escapeType :: PGTypes.PGParameter t a => Proxy e -> PGTypes.PGTypeID t -> a -> BS.ByteString

instance KnownEscapingStrategy t a 'EscapeSameAsPostgres where
  escapeType _ p t = PGTypes.pgLiteral p t

instance KnownEscapingStrategy t a 'EscapeMySqlText where
  escapeType _ p t = mysqlEscape (PGTypes.pgEncode p t)

instance
  (KnownEscapingStrategy t a e, PGTypes.PGParameter t a) =>
  KnownEscapingStrategy t (Maybe a) ('Nullable e)
  where
  escapeType _ p t =
    case t of
      Nothing -> BSC.pack "NULL"
      Just justT -> escapeType (Proxy :: Proxy e) p justT

-- | Wrap a value in a newtype that will ensure correct MySQL escaping logic is
-- applied. You don't need to do this manually, the `sql` quasiquoter will wrap
-- for you automatically.
escape :: a -> Escaped a
escape = Escaped

mysqlEscape :: BS.ByteString -> BS.ByteString
mysqlEscape = wrapInSingleQuotes << Escape.escapeBytes

wrapInSingleQuotes :: BS.ByteString -> BS.ByteString
wrapInSingleQuotes s = BSC.snoc (BSC.cons '\'' s) '\''

instance PGTypes.PGColumn "boolean" Data.Int.Int16 where
  pgDecode tid tv =
    PGTypes.pgDecode tid tv |> boolToSmallInt

instance PGTypes.PGParameter "real" Float where
  pgEncode tid tv =
    let (i :: Prelude.Float) = Prelude.realToFrac tv
     in PGTypes.pgEncode tid i

boolToSmallInt :: Bool -> Data.Int.Int16
boolToSmallInt b =
  if b
    then 1
    else 0

-- |
-- Hack to allow us to do things like replace "-quoted idenitifier with `-quoted
-- identifiers.
--
-- If we turn on ANSI quotes on the MySQL client MySQL would accept "-quotes and
-- this hack would no longer be necessary. At the moment the Haskell quiz engine
-- code is using the Rails MySQL client, so we cannot enable it for queries from
-- Haskell alone, and are scared to enable it for all queries. This might change
-- in the future.
replace :: Text -> Text -> Query row -> Query row
replace original replacement query =
  query {sqlString = Text.replace original replacement (sqlString query)}

-- Support results with additional columns then what `persistent` supports out
-- of the box. From version `2.10.2` of persistent onwards up to 12 columns will
-- be supported by default. We'll need to remove our custom instances for up to
-- twelve columns once we reach that point.
instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i)
  where

  rawSqlCols e = rawSqlCols e << from9

  rawSqlColCountReason = rawSqlColCountReason << from9

  rawSqlProcessRow = fmap to9 << rawSqlProcessRow

from9 :: (a, b, c, d, e, f, g, h, i) -> ((a, b), (c, d), (e, f), (g, h), i)
from9 (a, b, c, d, e, f, g, h, i) = ((a, b), (c, d), (e, f), (g, h), i)

to9 :: ((a, b), (c, d), (e, f), (g, h), i) -> (a, b, c, d, e, f, g, h, i)
to9 ((a, b), (c, d), (e, f), (g, h), i) = (a, b, c, d, e, f, g, h, i)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j)
  where

  rawSqlCols e = rawSqlCols e << from10

  rawSqlColCountReason = rawSqlColCountReason << from10

  rawSqlProcessRow = fmap to10 << rawSqlProcessRow

from10 :: (a, b, c, d, e, f, g, h, i, j) -> ((a, b), (c, d), (e, f), (g, h), (i, j))
from10 (a, b, c, d, e, f, g, h, i, j) = ((a, b), (c, d), (e, f), (g, h), (i, j))

to10 :: ((a, b), (c, d), (e, f), (g, h), (i, j)) -> (a, b, c, d, e, f, g, h, i, j)
to10 ((a, b), (c, d), (e, f), (g, h), (i, j)) = (a, b, c, d, e, f, g, h, i, j)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k)
  where

  rawSqlCols e = rawSqlCols e << from11

  rawSqlColCountReason = rawSqlColCountReason << from11

  rawSqlProcessRow = fmap to11 << rawSqlProcessRow

from11 :: (a, b, c, d, e, f, g, h, i, j, k) -> ((a, b), (c, d), (e, f), (g, h), (i, j), k)
from11 (a, b, c, d, e, f, g, h, i, j, k) = ((a, b), (c, d), (e, f), (g, h), (i, j), k)

to11 :: ((a, b), (c, d), (e, f), (g, h), (i, j), k) -> (a, b, c, d, e, f, g, h, i, j, k)
to11 ((a, b), (c, d), (e, f), (g, h), (i, j), k) = (a, b, c, d, e, f, g, h, i, j, k)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l)
  where

  rawSqlCols e = rawSqlCols e << from12

  rawSqlColCountReason = rawSqlColCountReason << from12

  rawSqlProcessRow = fmap to12 << rawSqlProcessRow

from12 :: (a, b, c, d, e, f, g, h, i, j, k, l) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l))
from12 (a, b, c, d, e, f, g, h, i, j, k, l) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l))

to12 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l)) -> (a, b, c, d, e, f, g, h, i, j, k, l)
to12 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l)) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l,
    RawSql m
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where

  rawSqlCols e = rawSqlCols e << from13

  rawSqlColCountReason = rawSqlColCountReason << from13

  rawSqlProcessRow = fmap to13 << rawSqlProcessRow

from13 :: (a, b, c, d, e, f, g, h, i, j, k, l, m) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), m)
from13 (a, b, c, d, e, f, g, h, i, j, k, l, m) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), m)

to13 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), m) -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
to13 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l,
    RawSql m,
    RawSql n
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where

  rawSqlCols e = rawSqlCols e << from14

  rawSqlColCountReason = rawSqlColCountReason << from14

  rawSqlProcessRow = fmap to14 << rawSqlProcessRow

from14 :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n))
from14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n))

to14 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
to14 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n)) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l,
    RawSql m,
    RawSql n,
    RawSql o
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where

  rawSqlCols e = rawSqlCols e << from15

  rawSqlColCountReason = rawSqlColCountReason << from15

  rawSqlProcessRow = fmap to15 << rawSqlProcessRow

from15 :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), o)
from15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), o)

to15 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), o) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
to15 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l,
    RawSql m,
    RawSql n,
    RawSql o,
    RawSql p
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  where

  rawSqlCols e = rawSqlCols e << from16

  rawSqlColCountReason = rawSqlColCountReason << from16

  rawSqlProcessRow = fmap to16 << rawSqlProcessRow

from16 :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p))
from16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p))

to16 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p)) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
to16 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p)) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance
  ( RawSql a,
    RawSql b,
    RawSql c,
    RawSql d,
    RawSql e,
    RawSql f,
    RawSql g,
    RawSql h,
    RawSql i,
    RawSql j,
    RawSql k,
    RawSql l,
    RawSql m,
    RawSql n,
    RawSql o,
    RawSql p,
    RawSql q
  ) =>
  RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  where

  rawSqlCols e = rawSqlCols e << from17

  rawSqlColCountReason = rawSqlColCountReason << from17

  rawSqlProcessRow = fmap to17 << rawSqlProcessRow

from17 :: (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p), q)
from17 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p), q)

to17 :: ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p), q) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
to17 ((a, b), (c, d), (e, f), (g, h), (i, j), (k, l), (m, n), (o, p), q) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
