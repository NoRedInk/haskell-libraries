module Redis.Real where

import Nri.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Acquire
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Task
import Prelude (Either (Left, Right), IO, fromIntegral, fst, pure, show)

handler :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.NamespacedHandler
handler namespace settings =
  Data.Acquire.mkAcquire (acquireHandler settings) releaseHandler
    |> map fst
    |> map (Internal.namespacedHandler namespace)

acquireHandler :: Settings.Settings -> IO (Internal.Handler, Connection)
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
    <| ( Internal.Handler
           { Internal.rawPing = rawPing connection anything,
             Internal.rawGet = rawGet connection anything,
             Internal.rawSet = rawSet connection anything,
             Internal.rawGetSet = rawGetSet connection anything,
             Internal.rawGetMany = rawGetMany connection anything,
             Internal.rawSetMany = rawSetMany connection anything,
             Internal.rawDelete = rawDelete connection anything,
             Internal.rawHGetAll = rawHGetAll connection anything,
             Internal.rawHSet = rawHSet connection anything,
             Internal.rawAtomicModify = rawAtomicModify connection anything
           },
         connection
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

rawPing ::
  Connection ->
  Platform.DoAnythingHandler ->
  Task Internal.Error Database.Redis.Status
rawPing connection anything =
  platformRedis "ping" connection anything Database.Redis.ping

rawGet ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Task Internal.Error (Maybe Data.ByteString.ByteString)
rawGet connection anything key =
  platformRedis "get" connection anything (Database.Redis.get key)

rawSet ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Task Internal.Error ()
rawSet connection anything key value =
  platformRedis "set" connection anything (Database.Redis.set key value)
    |> map (\_ -> ())

rawGetSet ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Task Internal.Error (Maybe Data.ByteString.ByteString)
rawGetSet connection anything key value =
  platformRedis "getset" connection anything (Database.Redis.getset key value)

rawGetMany ::
  Connection ->
  Platform.DoAnythingHandler ->
  [Data.ByteString.ByteString] ->
  Task Internal.Error [Maybe Data.ByteString.ByteString]
rawGetMany connection anything keys =
  platformRedis "mget" connection anything (Database.Redis.mget keys)

rawSetMany ::
  Connection ->
  Platform.DoAnythingHandler ->
  [(Data.ByteString.ByteString, Data.ByteString.ByteString)] ->
  Task Internal.Error ()
rawSetMany connection anything assocs =
  platformRedis "mset" connection anything (Database.Redis.mset assocs)
    |> map (\_ -> ())

rawDelete ::
  Connection ->
  Platform.DoAnythingHandler ->
  [Data.ByteString.ByteString] ->
  Task Internal.Error Int
rawDelete connection anything keys =
  platformRedis "del" connection anything (Database.Redis.del keys)
    |> map fromIntegral

rawHGetAll ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Task Internal.Error [(Data.ByteString.ByteString, Data.ByteString.ByteString)]
rawHGetAll connection anything key =
  platformRedis "hgetall" connection anything (Database.Redis.hgetall key)

rawHSet ::
  Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Task Internal.Error ()
rawHSet connection anything key value field =
  platformRedis "hset" connection anything (Database.Redis.hset key value field)
    |> map (\_ -> ())

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
