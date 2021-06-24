module MySQL.Settings
  ( Settings (..),
    ConnectionSettings (..),
    ConnectionType (..),
    Database (..),
    User (..),
    Host (..),
    Password (..),
    Port (..),
    Socket (..),
    PoolSettings (..),
    MysqlPoolSize (..),
    MysqlPoolStripes (..),
    MysqlPoolMaxIdleTime (..),
    decoder,
    defaultSettings,
    defaultPoolSettings,
    defaultConnectionSettings,
  )
where

import qualified Data.Time
import qualified Environment
import qualified Internal.Time as Time
import qualified Log
import Prelude (FilePath, pure)
import qualified Prelude

data Settings = Settings
  { mysqlConnection :: ConnectionSettings,
    mysqlPool :: PoolSettings,
    mysqlQueryTimeoutSeconds :: Time.Interval
  }

data ConnectionSettings = ConnectionSettings
  { database :: Database,
    user :: User,
    password :: Password,
    connection :: ConnectionType
  }

data ConnectionType
  = ConnectTcp Host Port
  | ConnectSocket Socket

data PoolSettings = PoolSettings
  { mysqlPoolSize :: MysqlPoolSize,
    mysqlPoolMaxIdleTime :: MysqlPoolMaxIdleTime,
    mysqlPoolStripes :: MysqlPoolStripes
  }
  deriving (Eq, Show, Generic)

defaultSettings :: Settings
defaultSettings =
  Settings
    { mysqlConnection = defaultConnectionSettings,
      mysqlPool = defaultPoolSettings,
      mysqlQueryTimeoutSeconds = Time.fromSeconds 5
    }

defaultPoolSettings :: PoolSettings
defaultPoolSettings =
  PoolSettings
    { mysqlPoolSize =
        -- Connections in the pool are allocated on demand, so we won't
        -- create all these connections unless the application can make use
        -- of them.
        MysqlPoolSize 1000,
      mysqlPoolMaxIdleTime = MysqlPoolMaxIdleTime (toNominalDiffTime 3600),
      mysqlPoolStripes = MysqlPoolStripes 1
    }

defaultConnectionSettings :: ConnectionSettings
defaultConnectionSettings =
  ConnectionSettings
    { database = Database "noredink_development",
      user = User "noredink_dev",
      password = Password <| Log.mkSecret "",
      connection = ConnectTcp defaultHost defaultPort
    }

defaultHost :: Host
defaultHost = Host "localhost"

defaultPort :: Port
defaultPort = Port 3306

decoder :: Environment.Decoder Settings
decoder =
  pure Settings
    |> andMap connectionSettingsDecoder
    |> andMap poolDecoder
    |> andMap queryTimeoutSecondsDecoder

connectionSettingsDecoder :: Environment.Decoder ConnectionSettings
connectionSettingsDecoder =
  pure ConnectionSettings
    |> andMap databaseDecoder
    |> andMap userDecoder
    |> andMap passwordDecoder
    |> andMap (Environment.either decoderTcp decoderSocket)

poolDecoder :: Environment.Decoder PoolSettings
poolDecoder =
  pure PoolSettings
    |> andMap mysqlPoolSizeDecoder
    |> andMap mysqlPoolMaxIdleTimeDecoder
    |> andMap mysqlPoolStripesDecoder

decoderTcp :: Environment.Decoder ConnectionType
decoderTcp =
  pure ConnectTcp
    |> andMap hostDecoder
    |> andMap portDecoder

decoderSocket :: Environment.Decoder ConnectionType
decoderSocket =
  pure ConnectSocket
    |> andMap socketDecoder

newtype Database = Database
  { unDatabase :: Text
  }

databaseDecoder :: Environment.Decoder Database
databaseDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_DATABASE",
        Environment.description = "The monolith database to connect to.",
        Environment.defaultValue = defaultConnectionSettings |> database |> unDatabase
      }
    (map Database Environment.text)

newtype User = User
  { unUser :: Text
  }

userDecoder :: Environment.Decoder User
userDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_USER",
        Environment.description = "The monolith user to connect as.",
        Environment.defaultValue = defaultConnectionSettings |> user |> unUser
      }
    (map User Environment.text)

newtype Host = Host
  { unHost :: Text
  }

hostDecoder :: Environment.Decoder Host
hostDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_HOST",
        Environment.description = "The monolith host to connect to.",
        Environment.defaultValue = unHost defaultHost
      }
    (map Host Environment.text)

newtype Password = Password
  { unPassword :: Log.Secret Text
  }

passwordDecoder :: Environment.Decoder Password
passwordDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_PASSWORD",
        Environment.description = "The monolith password to connect with.",
        Environment.defaultValue = defaultConnectionSettings |> password |> unPassword |> Log.unSecret
      }
    (map (Password << Log.mkSecret) Environment.text)

newtype Port = Port
  { unPort :: Int
  }

portDecoder :: Environment.Decoder Port
portDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_PORT",
        Environment.description = "The monolith port to connect to.",
        Environment.defaultValue = unPort defaultPort |> Prelude.show |> Text.fromList
      }
    (map Port Environment.int)

newtype Socket = Socket
  { unSocket :: FilePath
  }

socketDecoder :: Environment.Decoder Socket
socketDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_SOCKET",
        Environment.description = "The monolith socket to connect to.",
        Environment.defaultValue = ""
      }
    (map (Text.toList >> Socket) Environment.text)

newtype MysqlPoolStripes = MysqlPoolStripes {unMysqlPoolStripes :: Int}
  deriving (Eq, Show, Generic)

mysqlPoolStripesDecoder :: Environment.Decoder MysqlPoolStripes
mysqlPoolStripesDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_POOL_STRIPES",
        Environment.description = "The amount of sub-connection pools to create. Best refer to the resource-pool package for more info on this one. 1 is a good value for most applications.",
        Environment.defaultValue =
          defaultSettings |> mysqlPool |> mysqlPoolStripes |> unMysqlPoolStripes |> Prelude.show |> Text.fromList
      }
    (Environment.int |> map MysqlPoolStripes)

newtype MysqlPoolMaxIdleTime = MysqlPoolMaxIdleTime {unMysqlPoolMaxIdleTime :: Data.Time.NominalDiffTime}
  deriving (Eq, Show, Generic)

mysqlPoolMaxIdleTimeDecoder :: Environment.Decoder MysqlPoolMaxIdleTime
mysqlPoolMaxIdleTimeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_POOL_MAX_IDLE_TIME",
        Environment.description = "The maximum time a database connection will be able remain idle until it is closed.",
        Environment.defaultValue =
          defaultSettings |> mysqlPool |> mysqlPoolMaxIdleTime |> unMysqlPoolMaxIdleTime |> fromNominalDiffTime |> Prelude.show |> Text.fromList
      }
    (Environment.int |> map (MysqlPoolMaxIdleTime << toNominalDiffTime))

toNominalDiffTime :: Int -> Data.Time.NominalDiffTime
toNominalDiffTime = Prelude.realToFrac

fromNominalDiffTime :: Data.Time.NominalDiffTime -> Int
fromNominalDiffTime = Prelude.round

newtype MysqlPoolSize = MysqlPoolSize {unMysqlPoolSize :: Int}
  deriving (Eq, Show, Generic)

mysqlPoolSizeDecoder :: Environment.Decoder MysqlPoolSize
mysqlPoolSizeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_POOL_SIZE",
        Environment.description = "The size of the postgres connection pool. This is the maximum amount of parallel database connections the app will be able to use.",
        Environment.defaultValue =
          defaultSettings |> mysqlPool |> mysqlPoolSize |> unMysqlPoolSize |> Prelude.show |> Text.fromList
      }
    (Environment.int |> map MysqlPoolSize)

queryTimeoutSecondsDecoder :: Environment.Decoder Time.Interval
queryTimeoutSecondsDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_QUERY_TIMEOUT_SECONDS",
        Environment.description = "The maximum time a query can run before it is cancelled.",
        Environment.defaultValue = defaultSettings |> mysqlQueryTimeoutSeconds |> Time.seconds |> Prelude.show |> Text.fromList
      }
    (Environment.float |> map Time.fromSeconds)
