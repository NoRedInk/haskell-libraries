module Redis.Settings
  ( Settings (..),
    ClusterMode (..),
    DefaultExpiry (..),
    QueryTimeout (..),
    MaxKeySize (..),
    decoder,
    decoderWithEnvVarPrefix,
    decoderWithCustomConnectionString,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text.Encoding (encodeUtf8)
import Database.Redis hiding (Ok)
import qualified Environment
import NriPrelude
import qualified Text
import qualified Text.URI as URI
import Prelude (Either (Left, Right), foldr, fromIntegral, id, pure)

data ClusterMode = Cluster | NotCluster
  deriving (Show)

-- | Settings required to initiate a redis connection.
data Settings = Settings
  { -- | Full redis connection string.
    --
    -- Default env var name is REDIS_CONNECTION_STRING
    -- default is "redis://localhost:6379"
    connectionInfo :: ConnectInfo,
    -- | Set to 1 for cluster, everything else is not.
    --
    -- Default env var name is REDIS_CLUSTER
    -- Default is 0
    clusterMode :: ClusterMode,
    -- | Set a default amount of seconds after which all keys touched by this
    -- handler will expire. The expire time of a key is reset every time it is
    -- read or written. A value of 0 means no default expiry.
    --
    -- Default env var name is REDIS_DEFAULT_EXPIRY_SECONDS
    -- default is 0
    defaultExpiry :: DefaultExpiry,
    -- | 0 means no timeout, every other value is a timeout in milliseconds.
    --
    -- Default env var name is REDIS_QUERY_TIMEOUT_MILLISECONDS
    -- default is 1000
    queryTimeout :: QueryTimeout,
    maxKeySize :: MaxKeySize
  }
  deriving (Show)

data MaxKeySize = NoMaxKeySize | MaxKeySize Int
  deriving (Show)

data DefaultExpiry = NoDefaultExpiry | ExpireKeysAfterSeconds Int
  deriving (Show)

data QueryTimeout = NoQueryTimeout | TimeoutQueryAfterMilliseconds Int
  deriving (Show)

-- | decodes Settings from environmental variables
decoder :: Environment.Decoder Settings
decoder =
  decoderWithEnvVarPrefix ""

-- | decodes Settings from environmental variables with custom connection string
decoderWithCustomConnectionString :: Text -> Environment.Decoder Settings
decoderWithCustomConnectionString connectionStringEnvVar =
  map5
    Settings
    (decoderConnectInfo connectionStringEnvVar)
    (decoderClusterMode "")
    (decoderDefaultExpiry "")
    (decoderQueryTimeout "")
    (decoderMaxKeySize "")

-- | decodes Settings from environmental variables prefixed with a Text
-- >>> decoderWithEnvVarPrefix "WORKER_"
decoderWithEnvVarPrefix :: Text -> Environment.Decoder Settings
decoderWithEnvVarPrefix prefix =
  map5
    Settings
    (decoderConnectInfo (prefix ++ "REDIS_CONNECTION_STRING"))
    (decoderClusterMode prefix)
    (decoderDefaultExpiry prefix)
    (decoderQueryTimeout prefix)
    (decoderMaxKeySize prefix)

decoderClusterMode :: Text -> Environment.Decoder ClusterMode
decoderClusterMode prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_CLUSTER",
        Environment.description = "Set to 1 for cluster, everything else is not",
        Environment.defaultValue = "0"
      }
    ( Environment.custom
        Environment.text
        ( \str ->
            if Text.trim str == "1"
              then Ok Cluster
              else Ok NotCluster
        )
    )

decoderConnectInfo :: Text -> Environment.Decoder ConnectInfo
decoderConnectInfo envVarName =
  Environment.variable
    Environment.Variable
      { Environment.name = envVarName,
        Environment.description = "Full redis connection string",
        Environment.defaultValue = "redis://localhost:6379"
      }
    ( Environment.custom
        Environment.uri
        ( \uri ->
            case map URI.unRText (URI.uriScheme uri) of
              Just "redis" -> parseRedisSchemeURI uri
              Just "redis+unix" -> parseRedisSocketSchemeURI uri
              Just unrecognizedScheme -> Err ("Invalid URI scheme for connection string: " ++ unrecognizedScheme)
              Nothing -> Err "URI scheme missing from connection string"
        )
    )

parseRedisSchemeURI :: URI.URI -> NriPrelude.Result Text ConnectInfo
parseRedisSchemeURI uri =
  case parseConnectInfo (URI.renderStr uri) of
    Left parseError -> Err ("Invalid Redis connection string: " ++ Text.fromList parseError)
    Right info' -> Ok info'

parseRedisSocketSchemeURI :: URI.URI -> NriPrelude.Result Text ConnectInfo
parseRedisSocketSchemeURI uri =
  let uriPathTextFromURI =
        case URI.uriPath uri of
          Nothing -> Err "URI path missing from connection string"
          Just (_, uriPathSegments) ->
            uriPathSegments
              |> map URI.unRText
              |> NonEmpty.intersperse "/"
              |> foldr (++) ""
              |> (if URI.isPathAbsolute uri then ("/" ++) else id)
              |> Ok

      dbNumFromParams queryParams =
        case queryParams of
          [] -> Ok 0
          URI.QueryParam key value : rest ->
            if URI.unRText key == "db"
              then case Text.toInt (URI.unRText value) of
                Nothing -> Err "Expected an integer for db in connection string"
                Just dbNum -> Ok (fromIntegral dbNum)
              else dbNumFromParams rest
          _ : rest -> dbNumFromParams rest

      maybePasswordFromURI =
        case URI.uriAuthority uri of
          Right (URI.Authority (Just (URI.UserInfo _ (Just passwordRText))) _ _) ->
            Just (encodeUtf8 <| URI.unRText passwordRText)
          _ -> Nothing
   in do
        uriPathText <- uriPathTextFromURI
        dbNum <- dbNumFromParams (URI.uriQuery uri)
        pure
          <| defaultConnectInfo
            { connectPort = UnixSocket (Text.toList uriPathText),
              connectDatabase = dbNum,
              connectAuth = maybePasswordFromURI
            }

decoderDefaultExpiry :: Text -> Environment.Decoder DefaultExpiry
decoderDefaultExpiry prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_DEFAULT_EXPIRY_SECONDS",
        Environment.description = "Set a default amount of seconds after which all keys touched by this handler will expire. The expire time of a key is reset every time it is read or written. A value of 0 means no default expiry.",
        Environment.defaultValue = "0"
      }
    Environment.int
    |> map
      ( \secs ->
          if secs == 0
            then NoDefaultExpiry
            else ExpireKeysAfterSeconds secs
      )

decoderQueryTimeout :: Text -> Environment.Decoder QueryTimeout
decoderQueryTimeout prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_QUERY_TIMEOUT_MILLISECONDS",
        Environment.description = "0 means no timeout, every other value is a timeout in milliseconds.",
        Environment.defaultValue = "1000"
      }
    ( Environment.custom
        Environment.int
        ( \milliseconds ->
            if milliseconds <= 0
              then Ok NoQueryTimeout
              else Ok (TimeoutQueryAfterMilliseconds milliseconds)
        )
    )

decoderMaxKeySize :: Text -> Environment.Decoder MaxKeySize
decoderMaxKeySize prefix =
  Environment.variable
    Environment.Variable
      { Environment.name = prefix ++ "REDIS_MAX_KEY_SIZE",
        Environment.description = "0 means no max key size, every other value is a max key size.",
        Environment.defaultValue = "0"
      }
    ( Environment.custom
        Environment.int
        ( \maxKeySize ->
            if maxKeySize <= 0
              then Ok NoMaxKeySize
              else Ok (MaxKeySize maxKeySize)
        )
    )
