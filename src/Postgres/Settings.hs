module Postgres.Settings
  ( Settings
      ( Settings,
        pgConnection,
        pgPool
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
    PgDatabase (PgDatabase, unPgDatabase),
    PgUser (PgUser, unPgUser),
    PgHost (PgHost, unPgHost),
    PgPassword (PgPassword, unPgPassword),
    PgPort (PgPort, unPgPort),
    PgPoolStripes (PgPoolStripes, unPgPoolStripes),
    PgPoolMaxIdleTime (PgPoolMaxIdleTime, unPgPoolMaxIdleTime),
    PgPoolSize (PgPoolSize, unPgPoolSize),
    defaultSettings,
    toPGDatabase
    )
where

import qualified Data.Text as Text (isPrefixOf)
import qualified Data.Time
import Database.PostgreSQL.Typed
  ( PGDatabase,
    defaultPGDatabase,
    pgDBAddr,
    pgDBName,
    pgDBPass,
    pgDBUser
    )
import qualified Environment
import qualified Log
import Network.Socket (SockAddr (SockAddrUnix))
import Nri.Prelude
import System.FilePath ((</>))
import qualified Prelude (round)

data Settings
  = Settings
      { pgConnection :: ConnectionSettings,
        pgPool :: PoolSettings
        }
  deriving (Eq, Show, Generic)

defaultSettings :: Settings
defaultSettings = Settings
  { pgConnection = ConnectionSettings
      { pgDatabase = PgDatabase "noredink_dev",
        pgUser = PgUser "noredink_dev",
        pgHost = PgHost "localhost",
        pgPassword = PgPassword (Log.mkSecret ""),
        pgPort = PgPort 8088
        },
    pgPool = PoolSettings
      { pgPoolSize = PgPoolSize 2,
        pgPoolMaxIdleTime = PgPoolMaxIdleTime (toNominalDiffTime 3600),
        pgPoolStripes = PgPoolStripes 1
        }
    }

data ConnectionSettings
  = ConnectionSettings
      { pgDatabase :: PgDatabase,
        pgUser :: PgUser,
        pgHost :: PgHost,
        pgPassword :: PgPassword,
        pgPort :: PgPort
        }
  deriving (Eq, Show, Generic)

data PoolSettings
  = PoolSettings
      { pgPoolSize :: PgPoolSize,
        pgPoolMaxIdleTime :: PgPoolMaxIdleTime,
        pgPoolStripes :: PgPoolStripes
        }
  deriving (Eq, Show, Generic)

decoder :: Environment.Decoder Settings
decoder =
  pure Settings
    |> andMap connectionDecoder
    |> andMap poolDecoder

connectionDecoder :: Environment.Decoder ConnectionSettings
connectionDecoder =
  pure ConnectionSettings
    |> andMap pgDatabaseDecoder
    |> andMap pgUserDecoder
    |> andMap pgHostDecoder
    |> andMap pgPasswordDecoder
    |> andMap pgPortDecoder

poolDecoder :: Environment.Decoder PoolSettings
poolDecoder =
  pure PoolSettings
    |> andMap pgPoolSizeDecoder
    |> andMap pgPoolMaxIdleTimeDecoder
    |> andMap pgPoolStripesDecoder

newtype PgPort
  = PgPort {unPgPort :: Int}
  deriving (Eq, Show, Generic)

pgPortDecoder :: Environment.Decoder PgPort
pgPortDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PGPORT",
      Environment.description = "The port postgres is running on.",
      Environment.defaultValue =
        defaultSettings |> pgConnection |> pgPort |> unPgPort |> show
      }
    (Environment.int |> map PgPort)

newtype PgPassword
  = PgPassword {unPgPassword :: Log.Secret Text}
  deriving (Eq, Show, Generic)

pgPasswordDecoder :: Environment.Decoder PgPassword
pgPasswordDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PGPASSWORD",
      Environment.description = "The postgres user password.",
      Environment.defaultValue =
        defaultSettings |> pgConnection |> pgPassword |> unPgPassword |> Log.unSecret
      }
    (Environment.secret Environment.text |> map PgPassword)

newtype PgHost
  = PgHost {unPgHost :: Text}
  deriving (Eq, Show, Generic)

pgHostDecoder :: Environment.Decoder PgHost
pgHostDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PGHOST",
      Environment.description = "The hostname of the postgres server to connect to.",
      Environment.defaultValue =
        defaultSettings |> pgConnection |> pgHost |> unPgHost
      }
    (Environment.text |> map PgHost)

newtype PgUser
  = PgUser {unPgUser :: Text}
  deriving (Eq, Show, Generic)

pgUserDecoder :: Environment.Decoder PgUser
pgUserDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PGUSER",
      Environment.description = "The postgres user to connect with.",
      Environment.defaultValue =
        defaultSettings |> pgConnection |> pgUser |> unPgUser
      }
    (Environment.text |> map PgUser)

newtype PgDatabase
  = PgDatabase {unPgDatabase :: Text}
  deriving (Eq, Show, Generic)

pgDatabaseDecoder :: Environment.Decoder PgDatabase
pgDatabaseDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PGDATABASE",
      Environment.description = "The postgres database to connect to.",
      Environment.defaultValue =
        defaultSettings |> pgConnection |> pgDatabase |> unPgDatabase
      }
    (map PgDatabase Environment.text)

newtype PgPoolStripes
  = PgPoolStripes {unPgPoolStripes :: Int}
  deriving (Eq, Show, Generic)

pgPoolStripesDecoder :: Environment.Decoder PgPoolStripes
pgPoolStripesDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PG_POOL_STRIPES",
      Environment.description = "The amount of sub-connection pools to create. Best refer to the resource-pool package for more info on this one. 1 is a good value for most applications.",
      Environment.defaultValue =
        defaultSettings |> pgPool |> pgPoolStripes |> unPgPoolStripes |> show
      }
    (Environment.int |> map PgPoolStripes)

newtype PgPoolMaxIdleTime
  = PgPoolMaxIdleTime {unPgPoolMaxIdleTime :: Data.Time.NominalDiffTime}
  deriving (Eq, Show, Generic)

pgPoolMaxIdleTimeDecoder :: Environment.Decoder PgPoolMaxIdleTime
pgPoolMaxIdleTimeDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PG_POOL_MAX_IDLE_TIME",
      Environment.description = "The maximum time a database connection will be able remain idle until it is closed.",
      Environment.defaultValue =
        defaultSettings |> pgPool |> pgPoolMaxIdleTime |> unPgPoolMaxIdleTime |> fromNominalDiffTime |> show
      }
    (Environment.int |> map (PgPoolMaxIdleTime << toNominalDiffTime))

toNominalDiffTime :: Int -> Data.Time.NominalDiffTime
toNominalDiffTime = realToFrac

fromNominalDiffTime :: Data.Time.NominalDiffTime -> Int
fromNominalDiffTime = Prelude.round

newtype PgPoolSize
  = PgPoolSize {unPgPoolSize :: Int}
  deriving (Eq, Show, Generic)

pgPoolSizeDecoder :: Environment.Decoder PgPoolSize
pgPoolSizeDecoder =
  Environment.variable Environment.Variable
    { Environment.name = "PG_POOL_SIZE",
      Environment.description = "The size of the postgres connection pool. This is the maximum amount of parallel database connections the app will be able to use.",
      Environment.defaultValue =
        defaultSettings |> pgPool |> pgPoolSize |> unPgPoolSize |> show
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
            }
      } =
    defaultPGDatabase
      { pgDBName = toS (unPgDatabase pgDatabase),
        pgDBUser = toS (unPgUser pgUser),
        pgDBPass = toS <| Log.unSecret (unPgPassword pgPassword),
        pgDBAddr =
          -- The rule that PostgreSQL/libpq applies to `host`:
          --
          --   If this begins with a slash, it specifies Unix-domain
          --   communication rather than TCP/IP communication; the value is the
          --   name of the directory in which the socket file is stored
          --
          -- https://www.postgresql.org/docs/9.6/libpq-connect.html#LIBPQ-CONNECT-HOST
          if "/" `Text.isPrefixOf` host
            then
              toS host </> ".s.PGSQL." ++ show port
                |> SockAddrUnix
                |> Right
            else Left (toS host, show port)
        }
    where
      host = unPgHost pgHost
      port = unPgPort pgPort
