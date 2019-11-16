{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
    QueryResults,
    MySQL.PersistField (..),
    MySQL.Single (..),
  )
where

import qualified Control.Monad.Logger
import qualified Data.Acquire
import qualified Data.Coerce
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
import qualified Platform
import qualified Text

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
      pure (GenericDb.Connection doAnything pool (toConnectionLogContext settings))
    release GenericDb.Connection {GenericDb.singleOrPool} =
      case singleOrPool of
        GenericDb.Pool pool -> Data.Pool.destroyAllResources pool
        GenericDb.Single _ -> pure ()
    size = Settings.unMysqlPoolSize (Settings.mysqlPoolSize (Settings.mysqlPool settings)) |> fromIntegral
    database = toConnectInfo settings

-- |
-- Check that we are ready to be take traffic.
readiness :: Platform.LogHandler -> Connection -> Health.Check
readiness log conn =
  Health.Check "mysql" Health.Fatal (GenericDb.readiness go log conn)
  where
    go :: MySQL.SqlBackend -> Text -> IO ()
    go backend q = void <| go' backend q
    go' :: MySQL.SqlBackend -> Text -> IO [MySQL.Single Int]
    go' c q =
      MySQL.rawSql q []
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
  (HasCallStack, QueryResults row) =>
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
  (HasCallStack, QueryResults row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
getOne = withFrozenCallStack modifyExactlyOne

doQuery ::
  (HasCallStack, QueryResults row) =>
  Connection ->
  Query.Query row ->
  Task e [row]
doQuery conn query = do
  withFrozenCallStack Log.debug (Query.quasiQuotedString query) []
  GenericDb.runTaskWithConnection conn (runQuery query)
    |> Log.withContext "mysql-query" [Platform.Query queryInfo]
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
--       [MySQL.sql|
--         INSERT INTO my_table (name)
--           VALUES ($1)
--         RETURNING id, name
--       |]
--   @
modifyExactlyOne ::
  (HasCallStack, QueryResults row) =>
  Connection ->
  Query.Query row ->
  Task Query.Error row
modifyExactlyOne conn query =
  doQuery conn query
    |> andThen (Query.expectOne (Query.quasiQuotedString query))

runQuery ::
  (QueryResults row) =>
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
    |> map (map toQueryResult)

toConnectionLogContext :: Settings.Settings -> Platform.QueryConnectionInfo
toConnectionLogContext settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Settings.unDatabase (Settings.database connectionSettings)
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Platform.UnixSocket
            Platform.MySQL
            (toS (Settings.unSocket socket))
            database
        Settings.ConnectTcp host port ->
          Platform.TcpSocket
            Platform.MySQL
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
