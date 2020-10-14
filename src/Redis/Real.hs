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
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Task
import qualified Text
import Prelude (Either (Left, Right), IO, fromIntegral, fst, pure, show)
import qualified Prelude

handler :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.Handler
handler namespace settings =
  Data.Acquire.mkAcquire (acquireHandler settings) releaseHandler
    |> map fst
    |> map (Internal.namespacedHandler namespace)

acquireHandler :: Settings.Settings -> IO (Internal.InternalHandler, Connection)
acquireHandler settings = do
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
    ( Internal.InternalHandler (doQuery connection anything),
      connection
    )

doQuery :: Connection -> Platform.DoAnythingHandler -> Internal.Query a -> Task Internal.Error a
doQuery connection anything query =
  case query of
    -- Special treatment for constructors that don't represent
    Internal.Pure x -> Task.succeed x
    Internal.AtomicModify key f -> rawAtomicModify connection anything key f
    -- If we see an `Apply` it means we're stringing multiple commands together.
    -- We run these inside a Redis transaction to ensure atomicity.
    Internal.Apply f x ->
      let PreparedQuery {cmds, redisCtx} = map2 (map2 (<|)) (doRawQuery f) (doRawQuery x)
       in redisCtx
            |> Database.Redis.multiExec
            |> map
              ( \txResult ->
                  case txResult of
                    Database.Redis.TxSuccess y -> Right y
                    Database.Redis.TxAborted ->
                      -- We haven't exposed `watch` from this package's API yet, and
                      -- as long as we don't this error shouldn't happen.
                      Left (Database.Redis.Error "Aborted due to WATCH failing")
                    Database.Redis.TxError err -> Left (Database.Redis.Error (Data.ByteString.Char8.pack err))
              )
            |> platformRedis (Text.join " " cmds) connection anything
    Internal.WithResult f q -> do
      result <- doQuery connection anything q
      case f result of
        Ok a -> Task.succeed a
        Err err -> Task.fail err
    _ ->
      let PreparedQuery {cmds, redisCtx} = doRawQuery query
       in platformRedis (Text.join " " cmds) connection anything redisCtx

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
    Internal.Del keys -> PreparedQuery ["keys"] (Database.Redis.del keys) |> map (Ok << Prelude.fromIntegral)
    Internal.Hgetall key -> PreparedQuery ["hgetall"] (Database.Redis.hgetall key) |> map Ok
    Internal.Hset key field val -> PreparedQuery ["hset"] (Database.Redis.hset key field val) |> map (\_ -> Ok ())
    Internal.Hmset key vals -> PreparedQuery ["hset"] (Database.Redis.hmset key vals) |> map (\_ -> Ok ())
    Internal.Pure x -> pure (Ok x)
    Internal.Apply f x -> map2 (map2 (<|)) (doRawQuery f) (doRawQuery x)
    Internal.AtomicModify _ _ -> Prelude.error "Use of `AtomicModify` within a Redis transaction is not supported."
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

releaseHandler :: (Internal.InternalHandler, Connection) -> IO ()
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

rawAtomicModify ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  (Maybe Data.ByteString.ByteString -> (Data.ByteString.ByteString, a)) ->
  Task Internal.Error (Data.ByteString.ByteString, a)
rawAtomicModify connection anything key f =
  inner (100 :: Int)
  where
    inner count =
      platformRedis
        "watch get multi set exec"
        connection
        anything
        (map (map Ok) action)
        |> andThen (processTxResult count)
    processTxResult count (txResult, newValue) =
      case txResult of
        Database.Redis.TxSuccess _ -> pure newValue
        Database.Redis.TxAborted ->
          if count > 0
            then inner (count - 1)
            else Task.fail <| Internal.RedisError "Attempted atomic update 100 times without success."
        Database.Redis.TxError err -> Task.fail <| Internal.RedisError (Data.Text.pack err)
    action = do
      _ <- Database.Redis.watch [key]
      resp <- Database.Redis.get key
      case resp of
        Right r -> do
          let (newValue, context) = f r
          txResult <- Database.Redis.multiExec (Database.Redis.set key newValue)
          pure <| Right (txResult, (newValue, context))
        Left e' -> pure <| Left e'
