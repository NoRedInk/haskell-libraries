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
    PoolSettings
      ( PoolSettings,
        mysqlPoolSize
      ),
    MysqlPoolSize (MysqlPoolSize, unMysqlPoolSize),
    decoder,
    defaultSettings,
    defaultConnectionSettings,
  )
where

import Cherry.Prelude
import qualified Data.Text
import qualified Environment
import qualified Log
import Prelude (FilePath, pure, show)

data Settings
  = Settings
      { mysqlConnection :: ConnectionSettings,
        mysqlPool :: PoolSettings,
        mysqlQueryTimeoutSeconds :: Float
      }

data ConnectionSettings
  = ConnectionSettings
      { database :: Database,
        user :: User,
        password :: Password,
        connection :: ConnectionType
      }

data ConnectionType
  = ConnectTcp Host Port
  | ConnectSocket Socket

newtype PoolSettings
  = PoolSettings
      { mysqlPoolSize :: MysqlPoolSize
      }
  deriving (Eq, Show, Generic)

defaultSettings :: Settings
defaultSettings =
  Settings
    { mysqlConnection = defaultConnectionSettings,
      mysqlPool = PoolSettings
        { mysqlPoolSize = MysqlPoolSize 2
        },
      mysqlQueryTimeoutSeconds = 5
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

decoderTcp :: Environment.Decoder ConnectionType
decoderTcp =
  pure ConnectTcp
    |> andMap hostDecoder
    |> andMap portDecoder

decoderSocket :: Environment.Decoder ConnectionType
decoderSocket =
  pure ConnectSocket
    |> andMap socketDecoder

newtype Database
  = Database
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

newtype User
  = User
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

newtype Host
  = Host
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

newtype Password
  = Password
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

newtype Port
  = Port
      { unPort :: Int
      }

portDecoder :: Environment.Decoder Port
portDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MONOLITH_MYSQL_PORT",
        Environment.description = "The monolith port to connect to.",
        Environment.defaultValue = unPort defaultPort |> show |> Data.Text.pack
      }
    (map Port Environment.int)

newtype Socket
  = Socket
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
    (map (Data.Text.unpack >> Socket) Environment.text)

newtype MysqlPoolSize
  = MysqlPoolSize {unMysqlPoolSize :: Int}
  deriving (Eq, Show, Generic)

mysqlPoolSizeDecoder :: Environment.Decoder MysqlPoolSize
mysqlPoolSizeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_POOL_SIZE",
        Environment.description = "The size of the postgres connection pool. This is the maximum amount of parallel database connections the app will be able to use.",
        Environment.defaultValue =
          defaultSettings |> mysqlPool |> mysqlPoolSize |> unMysqlPoolSize |> show |> Data.Text.pack
      }
    (Environment.int |> map MysqlPoolSize)

queryTimeoutSecondsDecoder :: Environment.Decoder Float
queryTimeoutSecondsDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "MYSQL_QUERY_TIMEOUT_SECONDS",
        Environment.description = "The maximum time a query can run before it is cancelled.",
        Environment.defaultValue = defaultSettings |> mysqlQueryTimeoutSeconds |> show |> Data.Text.pack
      }
    Environment.float
