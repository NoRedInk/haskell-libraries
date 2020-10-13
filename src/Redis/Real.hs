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
    -- Single-query commands we run directly
    Internal.Ping -> platformRedis "ping" connection anything (doRawQuery query)
    Internal.Get _ -> platformRedis "get" connection anything (doRawQuery query)
    Internal.Set _ _ -> platformRedis "set" connection anything (doRawQuery query)
    Internal.Getset _ _ -> platformRedis "getset" connection anything (doRawQuery query)
    Internal.Mget _ -> platformRedis "mget" connection anything (doRawQuery query)
    Internal.Mset _ -> platformRedis "mset" connection anything (doRawQuery query)
    Internal.Del _ -> platformRedis "del" connection anything (doRawQuery query)
    Internal.Hgetall _ -> platformRedis "hgetall" connection anything (doRawQuery query)
    Internal.Hset _ _ _ -> platformRedis "hset" connection anything (doRawQuery query)
    -- Special treatment for constructors that don't represent
    Internal.Fmap f q -> doQuery connection anything q |> map f
    Internal.Pure x -> Task.succeed x
    Internal.AtomicModify key f -> rawAtomicModify connection anything key f
    -- If we see an `Apply` it means we're stringing multiple commands together.
    -- We run these inside a Redis transaction to ensure atomicity.
    Internal.Apply f x ->
      map2 (map2 (\f' x' -> f' x')) (doRawQuery f) (doRawQuery x)
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
        |> platformRedis "multi" connection anything

-- Construct a query in the underlying `hedis` library we depend on. It has a
-- polymorphic type signature that allows the returning query to be passed to
-- `Database.Redis.run` for direct execution, or `Database.Redis.multiExec` for
-- executation as part of a transaction.
doRawQuery :: (Prelude.Applicative f, Database.Redis.RedisCtx m f) => Internal.Query a -> m (f a)
doRawQuery query =
  case query of
    Internal.Ping -> Database.Redis.ping
    Internal.Get key -> Database.Redis.get key
    Internal.Set key val -> Database.Redis.set key val |> map (map (\_ -> ()))
    Internal.Getset key val -> Database.Redis.getset key val
    Internal.Mget keys -> Database.Redis.mget keys
    Internal.Mset vals -> Database.Redis.mset vals |> map (map (\_ -> ()))
    Internal.Del keys -> Database.Redis.del keys |> map (map Prelude.fromIntegral)
    Internal.Hgetall key -> Database.Redis.hgetall key
    Internal.Hset key field val -> Database.Redis.hset key field val |> map (map (\_ -> ()))
    Internal.Fmap f q -> doRawQuery q |> map (map f)
    Internal.Pure x -> pure (pure x)
    Internal.Apply f x -> map2 (map2 (\f' x' -> f' x')) (doRawQuery f) (doRawQuery x)
    Internal.AtomicModify _ _ -> Prelude.error "Use of `AtomicModify` within a Redis transaction is not supported."

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
  Database.Redis.Redis (Either Database.Redis.Reply a) ->
  Task Internal.Error a
platformRedis command connection anything action =
  Database.Redis.runRedis (connectionHedis connection) action
    |> map toResult
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
        action
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
