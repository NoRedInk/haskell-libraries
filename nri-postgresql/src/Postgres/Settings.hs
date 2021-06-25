module Postgres.Settings
  ( Settings
      ( Settings,
        pgConnection,
        pgPool,
        pgQueryTimeout
      ),
    ConnectionSettings
      ( ConnectionSettings,
        pgDatabase,
        pgUser,
        pgHost,
        pgPassword,
        pgPort
      ),
    PoolSettings
      ( PoolSettings,
        pgPoolStripes,
        pgPoolMaxIdleTime,
        pgPoolSize
      ),
    decoder,
    decoderWithPrefix,
    PgDatabase (PgDatabase, unPgDatabase),
    PgUser (PgUser, unPgUser),
    PgHost (PgHost, unPgHost),
    PgPassword (PgPassword, unPgPassword),
    PgPort (PgPort, unPgPort),
    PgPoolStripes (PgPoolStripes, unPgPoolStripes),
    PgPoolMaxIdleTime (PgPoolMaxIdleTime, unPgPoolMaxIdleTime),
    PgPoolSize (PgPoolSize, unPgPoolSize),
    defaultSettings,
    toPGDatabase,
  )
where

import qualified Data.ByteString.Char8
import qualified Data.Text.Encoding
import qualified Data.Time
import Database.PostgreSQL.Typed
  ( PGDatabase,
    defaultPGDatabase,
    pgDBAddr,
    pgDBName,
    pgDBParams,
    pgDBPass,
    pgDBUser,
  )
import qualified Environment
import qualified Log
import Network.Socket (SockAddr (SockAddrUnix))
import qualified Postgres.Time as Time
import System.FilePath ((</>))
import Prelude (Either (Left, Right), pure, realToFrac, round, show)

-- | Postgres connection details. You can use 'decoder' to create one of these.
data Settings = Settings
  { pgConnection :: ConnectionSettings,
    pgPool :: PoolSettings,
    pgQueryTimeout :: Time.Interval
  }
  deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { pgConnection =
        ConnectionSettings
          { pgDatabase = PgDatabase "postgres",
            pgUser = PgUser "postgres",
            pgHost = PgHost "localhost",
            pgPassword = PgPassword (Log.mkSecret ""),
            pgPort = PgPort 5432
          },
      pgPool =
        PoolSettings
          { pgPoolSize =
              -- Connections in the pool are allocated on demand, so we won't
              -- create all these connections unless the application can make use
              -- of them.
              PgPoolSize 500,
            pgPoolMaxIdleTime = PgPoolMaxIdleTime (toNominalDiffTime 3600),
            pgPoolStripes = PgPoolStripes 1
          },
      pgQueryTimeout = Time.fromSeconds 5
    }

data ConnectionSettings = ConnectionSettings
  { pgDatabase :: PgDatabase,
    pgUser :: PgUser,
    pgHost :: PgHost,
    pgPassword :: PgPassword,
    pgPort :: PgPort
  }
  deriving (Eq, Show)

data PoolSettings = PoolSettings
  { pgPoolSize :: PgPoolSize,
    pgPoolMaxIdleTime :: PgPoolMaxIdleTime,
    pgPoolStripes :: PgPoolStripes
  }
  deriving (Eq, Show)

-- | Create a 'Settings' value by reading settings from environment values.
--
-- [@environment variable@] PGHOST
-- [@default value@] localhost
--
-- [@environment variable@] PGPORT
-- [@default value@] 5432
--
-- [@environment variable@] PGDATABASE
-- [@default value@] postgresql
--
-- [@environment variable@] PGUSER
-- [@default value@] postgresql
--
-- [@environment variable@] PGPASSWORD
-- [@default value@]
--
-- [@environment variable@] PG_POOL_SIZE
-- [@default value@] 500
--
-- [@environment variable@] PG_POOL_STRIPES
-- [@default value@] 1
--
-- [@environment variable@] PG_POOL_MAX_IDLE_TIME
-- [@default value@] 3600
--
-- [@environment variable@] PG_QUERY_TIMEOUT_SECONDS
-- [@default value@] 5
decoder :: Environment.Decoder Settings
decoder = decoderWithPrefix ""

decoderWithPrefix :: Text -> Environment.Decoder Settings
decoderWithPrefix prefix =
  pure Settings
    |> andMap (connectionDecoder prefix)
    |> andMap poolDecoder
    |> andMap queryTimeoutDecoder

connectionDecoder :: Text -> Environment.Decoder ConnectionSettings
connectionDecoder prefix =
  pure ConnectionSettings
    |> andMap (pgDatabaseDecoder prefix)
    |> andMap (pgUserDecoder prefix)
    |> andMap (pgHostDecoder prefix)
    |> andMap (pgPasswordDecoder prefix)
    |> andMap (pgPortDecoder prefix)

poolDecoder :: Environment.Decoder PoolSettings
poolDecoder =
  pure PoolSettings
    |> andMap pgPoolSizeDecoder
    |> andMap pgPoolMaxIdleTimeDecoder
    |> andMap pgPoolStripesDecoder

newtype PgPort = PgPort {unPgPort :: Int}
  deriving (Eq, Show)

pgPortDecoder :: Text -> Environment.Decoder PgPort
pgPortDecoder prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "PGPORT",
        Environment.description = "The port postgres is running on.",
        Environment.defaultValue =
          defaultSettings |> pgConnection |> pgPort |> unPgPort |> show |> Text.fromList
      }
    (Environment.int |> map PgPort)

newtype PgPassword = PgPassword {unPgPassword :: Log.Secret Text}
  deriving (Eq, Show)

pgPasswordDecoder :: Text -> Environment.Decoder PgPassword
pgPasswordDecoder prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "PGPASSWORD",
        Environment.description = "The postgres user password.",
        Environment.defaultValue =
          defaultSettings |> pgConnection |> pgPassword |> unPgPassword |> Log.unSecret
      }
    (Environment.secret Environment.text |> map PgPassword)

newtype PgHost = PgHost {unPgHost :: Text}
  deriving (Eq, Show)

pgHostDecoder :: Text -> Environment.Decoder PgHost
pgHostDecoder prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "PGHOST",
        Environment.description = "The hostname of the postgres server to connect to.",
        Environment.defaultValue =
          defaultSettings |> pgConnection |> pgHost |> unPgHost
      }
    (Environment.text |> map PgHost)

newtype PgUser = PgUser {unPgUser :: Text}
  deriving (Eq, Show)

pgUserDecoder :: Text -> Environment.Decoder PgUser
pgUserDecoder prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "PGUSER",
        Environment.description = "The postgres user to connect with.",
        Environment.defaultValue =
          defaultSettings |> pgConnection |> pgUser |> unPgUser
      }
    (Environment.text |> map PgUser)

newtype PgDatabase = PgDatabase {unPgDatabase :: Text}
  deriving (Eq, Show)

pgDatabaseDecoder :: Text -> Environment.Decoder PgDatabase
pgDatabaseDecoder prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "PGDATABASE",
        Environment.description = "The postgres database to connect to.",
        Environment.defaultValue =
          defaultSettings |> pgConnection |> pgDatabase |> unPgDatabase
      }
    (map PgDatabase Environment.text)

newtype PgPoolStripes = PgPoolStripes {unPgPoolStripes :: Int}
  deriving (Eq, Show)

pgPoolStripesDecoder :: Environment.Decoder PgPoolStripes
pgPoolStripesDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "PG_POOL_STRIPES",
        Environment.description = "The amount of sub-connection pools to create. Best refer to the resource-pool package for more info on this one. 1 is a good value for most applications.",
        Environment.defaultValue =
          defaultSettings |> pgPool |> pgPoolStripes |> unPgPoolStripes |> show |> Text.fromList
      }
    (Environment.int |> map PgPoolStripes)

newtype PgPoolMaxIdleTime = PgPoolMaxIdleTime {unPgPoolMaxIdleTime :: Data.Time.NominalDiffTime}
  deriving (Eq, Show)

pgPoolMaxIdleTimeDecoder :: Environment.Decoder PgPoolMaxIdleTime
pgPoolMaxIdleTimeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "PG_POOL_MAX_IDLE_TIME",
        Environment.description = "The maximum time a database connection will be able remain idle until it is closed.",
        Environment.defaultValue =
          defaultSettings |> pgPool |> pgPoolMaxIdleTime |> unPgPoolMaxIdleTime |> fromNominalDiffTime |> show |> Text.fromList
      }
    (Environment.int |> map (PgPoolMaxIdleTime << toNominalDiffTime))

toNominalDiffTime :: Int -> Data.Time.NominalDiffTime
toNominalDiffTime = realToFrac

fromNominalDiffTime :: Data.Time.NominalDiffTime -> Int
fromNominalDiffTime = Prelude.round

newtype PgPoolSize = PgPoolSize {unPgPoolSize :: Int}
  deriving (Eq, Show)

pgPoolSizeDecoder :: Environment.Decoder PgPoolSize
pgPoolSizeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "PG_POOL_SIZE",
        Environment.description = "The size of the postgres connection pool. This is the maximum amount of parallel database connections the app will be able to use.",
        Environment.defaultValue =
          defaultSettings |> pgPool |> pgPoolSize |> unPgPoolSize |> show |> Text.fromList
      }
    (Environment.int |> map PgPoolSize)

toPGDatabase :: Settings -> PGDatabase
toPGDatabase
  Settings
    { pgConnection =
        ConnectionSettings
          { pgDatabase,
            pgUser,
            pgHost,
            pgPassword,
            pgPort
          },
      pgQueryTimeout
    } =
    defaultPGDatabase
      { pgDBName = Data.Text.Encoding.encodeUtf8 (unPgDatabase pgDatabase),
        pgDBUser = Data.Text.Encoding.encodeUtf8 (unPgUser pgUser),
        pgDBPass = Data.Text.Encoding.encodeUtf8 <| Log.unSecret (unPgPassword pgPassword),
        pgDBParams =
          if Time.milliseconds pgQueryTimeout > 0
            then
              [ -- We configure Postgres to automatically kill queries when they run
                -- too long. That should offer some protection against queries
                -- locking up the database.
                -- https://www.postgresql.org/docs/9.4/runtime-config-client.html
                ("statement_timeout", pgQueryTimeout |> Time.milliseconds |> floor |> show |> Data.ByteString.Char8.pack)
              ]
            else [],
        pgDBAddr =
          -- The rule that PostgreSQL/libpq applies to `host`:
          --
          --   If this begins with a slash, it specifies Unix-domain
          --   communication rather than TCP/IP communication; the value is the
          --   name of the directory in which the socket file is stored
          --
          -- https://www.postgresql.org/docs/9.6/libpq-connect.html#LIBPQ-CONNECT-HOST
          if Text.startsWith "/" host
            then
              Text.toList host </> ".s.PGSQL." ++ show port
                |> SockAddrUnix
                |> Right
            else Left (Text.toList host, show port)
      }
    where
      host = unPgHost pgHost
      port = unPgPort pgPort

queryTimeoutDecoder :: Environment.Decoder Time.Interval
queryTimeoutDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "PG_QUERY_TIMEOUT_SECONDS",
        Environment.description = "The maximum time a query can run before it is cancelled.",
        Environment.defaultValue = defaultSettings |> pgQueryTimeout |> Time.seconds |> show |> Text.fromList
      }
    (Environment.float |> map Time.fromSeconds)
