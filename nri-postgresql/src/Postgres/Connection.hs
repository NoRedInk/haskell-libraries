module Postgres.Connection (connection, connectionIO, SingleOrPool (..), Connection (..)) where

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
import qualified Internal.Time as Time
import qualified Log.SqlQuery as SqlQuery
import qualified Network.Socket as Socket
import qualified Postgres.Settings as Settings
import qualified System.Exit
import qualified Prelude

data Connection = Connection
  { doAnything :: Platform.DoAnythingHandler,
    singleOrPool :: SingleOrPool PGConnection,
    connDetails :: SqlQuery.Details,
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

connectionIO :: Settings.Settings -> Prelude.IO Connection
connectionIO settings = do
  let database = Settings.toPGDatabase settings
  let stripes =
        Settings.unPgPoolStripes (Settings.pgPoolStripes (Settings.pgPool settings))
          |> Prelude.fromIntegral
  let maxIdleTime = Settings.unPgPoolMaxIdleTime (Settings.pgPoolMaxIdleTime (Settings.pgPool settings))
  let size =
        Settings.unPgPoolSize (Settings.pgPoolSize (Settings.pgPool settings))
          |> Prelude.fromIntegral
  doAnything <- Platform.doAnythingHandler
  pool <-
    map Pool
      <| Data.Pool.createPool
        (pgConnect database `Exception.catch` handleError (toConnectionString database))
        pgDisconnect
        stripes
        maxIdleTime
        size
  Prelude.pure
    ( Connection
        doAnything
        pool
        (connectionDetails database)
        (Settings.pgQueryTimeout settings)
    )

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire (connectionIO settings) release
  where
    release Connection {singleOrPool} =
      case singleOrPool of
        Pool pool -> Data.Pool.destroyAllResources pool
        Single single -> pgDisconnect single

handleError :: Text -> Exception.IOException -> Prelude.IO a
handleError connectionString err = do
  Prelude.putStrLn "I couldn't connect to Postgres"
  Prelude.putStrLn ""
  Prelude.putStrLn "Is the database running?"
  Prelude.putStrLn ("I tried to connect to: " ++ Text.toList connectionString)
  System.Exit.die (Exception.displayException err)

toConnectionString :: PGDatabase -> Text
toConnectionString PGDatabase {pgDBUser, pgDBAddr, pgDBName} =
  Text.join
    ""
    [ Data.Text.Encoding.decodeUtf8 pgDBUser,
      ":*****@",
      case pgDBAddr of
        Prelude.Right sockAddr ->
          Text.fromList (Prelude.show sockAddr)
        Prelude.Left (hostName, serviceName) ->
          Text.fromList hostName
            ++ ":"
            ++ Text.fromList serviceName,
      "/",
      Data.Text.Encoding.decodeUtf8 pgDBName
    ]

connectionDetails :: PGDatabase -> SqlQuery.Details
connectionDetails db =
  case pgDBAddr db of
    Prelude.Left (hostName, serviceName) ->
      SqlQuery.emptyDetails
        { SqlQuery.databaseType = Just SqlQuery.postgresql,
          SqlQuery.host = Just (Text.fromList hostName),
          SqlQuery.port = Text.toInt (Text.fromList serviceName),
          SqlQuery.database = Just databaseName
        }
    Prelude.Right (Socket.SockAddrInet portNum hostAddr) ->
      SqlQuery.emptyDetails
        { SqlQuery.databaseType = Just SqlQuery.postgresql,
          SqlQuery.host = Just (Text.fromList (Prelude.show hostAddr)),
          SqlQuery.port = Just (Prelude.fromIntegral portNum),
          SqlQuery.database = Just databaseName
        }
    Prelude.Right (Socket.SockAddrInet6 portNum _flowInfo hostAddr _scopeId) ->
      SqlQuery.emptyDetails
        { SqlQuery.databaseType = Just SqlQuery.postgresql,
          SqlQuery.host = Just (Text.fromList (Prelude.show hostAddr)),
          SqlQuery.port = Just (Prelude.fromIntegral portNum),
          SqlQuery.database = Just databaseName
        }
    Prelude.Right (Socket.SockAddrUnix sockPath) ->
      SqlQuery.emptyDetails
        { SqlQuery.databaseType = Just SqlQuery.postgresql,
          SqlQuery.host = Just (Text.fromList sockPath),
          SqlQuery.port = Nothing,
          SqlQuery.database = Just databaseName
        }
  where
    databaseName = pgDBName db |> Data.Text.Encoding.decodeUtf8
