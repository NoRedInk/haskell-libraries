{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.Either (Either (Right))
import qualified Data.Pool
import Data.String (fromString)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.MySQL.Connection
import qualified Database.Persist.MySQL as MySQL
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
import Prelude (IO, fromIntegral, pure, show)

type Connection = GenericDb.Connection MySQL.SqlBackend

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Platform.doAnythingHandler
      pool <-
        MySQL.createMySQLPool database size
          |> Control.Monad.Logger.runNoLoggingT
          |> map GenericDb.Pool
      pure
        ( GenericDb.Connection
            doAnything
            pool
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

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction =
  GenericDb.transaction GenericDb.Transaction
    { GenericDb.begin = execute "BEGIN" >> void,
      GenericDb.commit = execute "COMMIT" >> void,
      GenericDb.rollback = execute "ROLLBACK" >> void,
      GenericDb.rollbackAll = execute "ROLLBACK" >> void
    }

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction ::
  forall m a.
  (MonadIO m, Exception.MonadCatch m) =>
  Connection ->
  (Connection -> m a) ->
  m a
inTestTransaction =
  GenericDb.inTestTransaction GenericDb.Transaction
    { GenericDb.begin = execute "BEGIN" >> void,
      GenericDb.commit = execute "COMMIT" >> void,
      GenericDb.rollback = execute "ROLLBACK" >> void,
      GenericDb.rollbackAll = execute "ROLLBACK" >> void
    }

execute :: Text -> MySQL.SqlBackend -> IO [MySQL.Single Int]
execute query conn =
  MySQL.rawSql query []
    |> (\reader -> runReaderT reader conn :: IO [MySQL.Single Int])

-- |
-- Check that we are ready to be take traffic.
readiness :: Platform.LogHandler -> Connection -> Health.Check
readiness log conn =
  Health.mkCheck "mysql" (GenericDb.readiness go log conn)
  where
    go :: MySQL.SqlBackend -> Text -> IO ()
    go backend q = void <| go' backend q
    go' :: MySQL.SqlBackend -> Text -> IO [MySQL.Single Int]
    go' c q =
      MySQL.rawSql q []
        |> (\reader -> runReaderT reader c)

doQuery ::
  (HasCallStack, QueryResults row) =>
  Connection ->
  Query.Query row ->
  (Result Query.Error [row] -> Task e a) ->
  Task e a
doQuery conn query handleResponse = do
  withFrozenCallStack Log.info (Query.asMessage query) []
  GenericDb.runTaskWithConnection conn (runQuery query)
    -- Handle the response before wrapping the operation in a context. This way,
    -- if the response handling logic creates errors, those errors can inherit
    -- context values like the query string.
    |> intoResult
    |> andThen handleResponse
    |> Log.withContext "mysql-query" [Platform.queryContext queryInfo]
  where
    queryInfo = Platform.QueryInfo
      { Platform.queryText = Log.mkSecret (Query.sqlString query),
        Platform.queryTemplate = Query.quasiQuotedString query,
        Platform.queryConn = GenericDb.logContext conn,
        Platform.queryOperation = Query.sqlOperation query,
        Platform.queryCollection = Query.queriedRelation query
      }

intoResult :: Task e a -> Task e2 (Result e a)
intoResult task =
  map Ok task
    |> Task.onError (Task.succeed << Err)

runQuery ::
  (QueryResults row) =>
  Query.Query row ->
  MySQL.SqlBackend ->
  IO (Result Query.Error [row])
runQuery query conn =
  query
    |> Query.sqlString
    -- We need this prefix on tables to allow compile-time checks of the query.
    |> Text.replace "monolith." ""
    |> Internal.anyToIn
    |> Data.Text.unpack
    |> fromString
    |> (\query' -> MySQL.rawSql query' [])
    |> (\reader -> runReaderT reader conn)
    |> map (map toQueryResult)
    |> Exception.tryAny
    |> map (Result.mapError GenericDb.toQueryError << GenericDb.eitherToResult)

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

-- |
-- The MySQL library expects inserts and updates to be run via the execute
-- function and so doesn't provide a QueryResults instance for `()`. Unfortunately
-- that doesn't play nice with the Postgres wrapper and its type checking, where
-- we run inserts via `doQuery` returning `()`. Hence the instance here
instance MySQL.PersistField () where

  toPersistValue () = MySQL.PersistNull

  fromPersistValue _ = Right ()

instance QueryResults () where

  type FromRawSql () = MySQL.Single ()

  toQueryResult _ = ()

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
