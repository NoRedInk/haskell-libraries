{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Redis.Real
  ( handler,
    Info (..),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Acquire
import qualified Data.Aeson as Aeson
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified List
import qualified Maybe
import NriPrelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Text
import Prelude (Either (Left, Right), IO, fromIntegral, pure, show)
import qualified Prelude

handler :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.Handler
handler namespace settings = do
  (namespacedHandler, _) <- Data.Acquire.mkAcquire (acquireHandler namespace settings) releaseHandler
  Prelude.pure
    <| case Settings.defaultExpiry settings of
      Settings.NoDefaultExpiry -> namespacedHandler
      Settings.ExpireKeysAfterSeconds secs ->
        Internal.defaultExpiryKeysAfterSeconds secs namespacedHandler

acquireHandler :: Text -> Settings.Settings -> IO (Internal.Handler, Connection)
acquireHandler namespace settings = do
  connection <- do
    let connectionInfo = Settings.connectionInfo settings
    connectionHedis <-
      case Settings.clusterMode settings of
        Settings.Cluster ->
          Database.Redis.connectCluster connectionInfo
        Settings.NotCluster ->
          Database.Redis.checkedConnect connectionInfo
    let connectionHost = Data.Text.pack (Database.Redis.connectHost connectionInfo)
    let connectionPort =
          case Database.Redis.connectPort connectionInfo of
            Database.Redis.PortNumber port -> Data.Text.pack (show port)
            Database.Redis.UnixSocket socket -> Data.Text.pack socket
    pure Connection {connectionHedis, connectionHost, connectionPort}
  anything <- Platform.doAnythingHandler
  pure
    ( Internal.Handler
        { Internal.doQuery = \query ->
            let PreparedQuery {cmds, redisCtx} = doRawQuery query
             in platformRedis (Text.join " " cmds) connection anything redisCtx,
          Internal.doTransaction = \query ->
            let PreparedQuery {cmds, redisCtx} = doRawQuery query
                redisCmd =
                  case Settings.clusterMode settings of
                    Settings.Cluster ->
                      -- The current implementation of hedis we're using
                      -- requires us to tell which hashslot a transaction is
                      -- going to work on by passing one of the keys used in the
                      -- transaction. This way it knows which node to send the
                      -- initial MULTI command to.
                      let firstKey =
                            Internal.keysTouchedByQuery query
                              |> List.head
                              -- If a command is not touching any keys then we
                              -- can send it to any node in the cluster. We're
                              -- going to send it to the node that contains the
                              -- `any-key` key. We can't come up with an example
                              -- of a real-world transaction that would fall
                              -- into this case, so we're not going to worry
                              -- about load-balancing these transactions across
                              -- nodes just yet.
                              |> Maybe.withDefault "any-key"
                       in Database.Redis.multiExecWithHash firstKey redisCtx
                    Settings.NotCluster ->
                      Database.Redis.multiExec redisCtx
             in redisCmd
                  |> map
                    ( \txResult ->
                        case txResult of
                          Database.Redis.TxSuccess y -> Right y
                          Database.Redis.TxAborted -> Right (Err Internal.TransactionAborted)
                          Database.Redis.TxError err -> Right (Err (Internal.RedisError (Data.Text.pack err)))
                    )
                  |> platformRedis (Text.join " " cmds) connection anything,
          Internal.doWatch = \keys ->
            Database.Redis.watch keys
              |> map (map (\_ -> Ok ()))
              |> platformRedis "watch" connection anything,
          Internal.namespace = Data.Text.Encoding.encodeUtf8 namespace
        },
      connection
    )

data PreparedQuery m f result
  = PreparedQuery
      { cmds :: [Text],
        redisCtx :: m (f result)
      }
  deriving (Prelude.Functor)

instance (Prelude.Applicative m, Prelude.Applicative f) => Prelude.Applicative (PreparedQuery m f) where
  pure x =
    PreparedQuery
      { cmds = [],
        redisCtx = pure (pure x)
      }
  f <*> x =
    PreparedQuery
      { cmds = cmds f ++ cmds x,
        redisCtx = map2 (map2 (<|)) (redisCtx f) (redisCtx x)
      }

-- Construct a query in the underlying `hedis` library we depend on. It has a
-- polymorphic type signature that allows the returning query to be passed to
-- `Database.Redis.run` for direct execution, or `Database.Redis.multiExec` for
-- executation as part of a transaction.
doRawQuery :: (Prelude.Applicative f, Database.Redis.RedisCtx m f) => Internal.Query result -> PreparedQuery m f (Result Internal.Error result)
doRawQuery query =
  case query of
    Internal.Ping -> PreparedQuery ["ping"] Database.Redis.ping |> map Ok
    Internal.Get key -> PreparedQuery ["get"] (Database.Redis.get key) |> map Ok
    Internal.Set key val -> PreparedQuery ["set"] (Database.Redis.set key val) |> map (\_ -> Ok ())
    Internal.Getset key val -> PreparedQuery ["getset"] (Database.Redis.getset key val) |> map Ok
    Internal.Mget keys -> PreparedQuery ["mget"] (Database.Redis.mget keys) |> map Ok
    Internal.Mset vals -> PreparedQuery ["mset"] (Database.Redis.mset vals) |> map (\_ -> Ok ())
    Internal.Del keys -> PreparedQuery ["del"] (Database.Redis.del keys) |> map (Ok << Prelude.fromIntegral)
    Internal.Hgetall key -> PreparedQuery ["hgetall"] (Database.Redis.hgetall key) |> map Ok
    Internal.Hset key field val -> PreparedQuery ["hset"] (Database.Redis.hset key field val) |> map (\_ -> Ok ())
    Internal.Hmget key fields -> PreparedQuery ["hmget"] (Database.Redis.hmget key fields) |> map Ok
    Internal.Hmset key vals -> PreparedQuery ["hmset"] (Database.Redis.hmset key vals) |> map (\_ -> Ok ())
    Internal.Hdel key fields -> PreparedQuery ["hdel"] (Database.Redis.hdel key fields) |> map (Ok << Prelude.fromIntegral)
    Internal.Expire key secs -> PreparedQuery ["expire"] (Database.Redis.expire key (fromIntegral secs)) |> map (\_ -> Ok ())
    Internal.Pure x -> pure (Ok x)
    Internal.Apply f x -> map2 (map2 (<|)) (doRawQuery f) (doRawQuery x)
    Internal.WithResult f q ->
      let PreparedQuery cmds redisCtx = doRawQuery q
       in PreparedQuery
            cmds
            ( (map << map)
                ( \result -> case result of
                    Err a -> Err a
                    Ok res -> f res
                )
                redisCtx
            )

releaseHandler :: (Internal.Handler, Connection) -> IO ()
releaseHandler (_, Connection {connectionHedis}) = Database.Redis.disconnect connectionHedis

data Connection
  = Connection
      { connectionHedis :: Database.Redis.Connection,
        connectionHost :: Text,
        connectionPort :: Text
      }

platformRedis ::
  Text ->
  Connection ->
  Platform.DoAnythingHandler ->
  Database.Redis.Redis (Either Database.Redis.Reply (Result Internal.Error a)) ->
  Task Internal.Error a
platformRedis command connection anything action =
  Database.Redis.runRedis (connectionHedis connection) action
    |> map toResult
    |> map
      ( \result -> -- Result Internal.Error (Result Internal.Error a) -> Result Internal.Error a
          case result of
            Ok a -> a
            Err err -> Err err
      )
    |> Exception.handle (\(_ :: Database.Redis.ConnectionLostException) -> pure <| Err Internal.ConnectionLost)
    |> Platform.doAnything anything
    |> traceQuery command connection

traceQuery :: Text -> Connection -> Task e a -> Task e a
traceQuery command connection task =
  let info =
        Info
          { infoCommand = command,
            infoHost = connectionHost connection,
            infoPort = connectionPort connection
          }
   in Platform.tracingSpan
        "Redis Query"
        (Platform.finally task (Platform.setTracingSpanDetails info))

data Info
  = Info
      { infoCommand :: Text,
        infoHost :: Text,
        infoPort :: Text
      }
  deriving (Generic)

instance Aeson.ToJSON Info

instance Platform.TracingSpanDetails Info

toResult :: Either Database.Redis.Reply a -> Result Internal.Error a
toResult reply =
  case reply of
    Left (Database.Redis.Error err) -> Err (Internal.RedisError <| Data.Text.Encoding.decodeUtf8 err)
    Left _ -> Err (Internal.RedisError "The Redis library said this was an error but returned no error message.")
    Right r -> Ok r
