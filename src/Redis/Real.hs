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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import NriPrelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import Prelude (Either (Left, Right), IO, fromIntegral, pure, show)
import qualified Prelude

handler :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.Handler
handler namespace settings = do
  (namespacedHandler, _) <- Data.Acquire.mkAcquire (acquireHandler namespace settings) releaseHandler
  namespacedHandler
    |> ( \handler' ->
           case Settings.defaultExpiry settings of
             Settings.NoDefaultExpiry -> handler'
             Settings.ExpireKeysAfterSeconds secs ->
               Internal.defaultExpiryKeysAfterSeconds secs handler'
       )
    |> ( \handler' ->
           case Settings.queryTimeout settings of
             Settings.NoQueryTimeout -> handler'
             Settings.TimeoutQueryAfterMilliseconds milliseconds ->
               Internal.timeoutAfterMilliseconds (toFloat milliseconds) handler'
       )
    |> Prelude.pure

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
            let PreparedQuery {redisCtx} = doRawQuery query
             in platformRedis (Internal.TracedQuery query) connection anything redisCtx,
          Internal.doTransaction = \query ->
            let PreparedQuery {redisCtx} = doRawQuery query
                redisCmd = Database.Redis.multiExec redisCtx
             in redisCmd
                  |> map
                    ( \txResult ->
                        case txResult of
                          Database.Redis.TxSuccess y -> Right y
                          Database.Redis.TxAborted -> Right (Err Internal.TransactionAborted)
                          Database.Redis.TxError err -> Right (Err (Internal.RedisError (Data.Text.pack err)))
                    )
                  |> platformRedis (Internal.TracedQuery query) connection anything,
          Internal.doWatch = \keys ->
            Database.Redis.watch (map toB keys)
              |> map (map (\_ -> Ok ()))
              |> platformRedis (Internal.TracedQuery (Internal.Pure ())) connection anything,
          Internal.namespace = namespace
        },
      connection
    )

newtype PreparedQuery m f result
  = PreparedQuery
      { redisCtx :: m (f result)
      }
  deriving (Prelude.Functor)

instance (Prelude.Applicative m, Prelude.Applicative f) => Prelude.Applicative (PreparedQuery m f) where
  pure x =
    PreparedQuery
      { redisCtx = pure (pure x)
      }
  f <*> x =
    PreparedQuery
      { redisCtx = map2 (map2 (<|)) (redisCtx f) (redisCtx x)
      }

-- Construct a query in the underlying `hedis` library we depend on. It has a
-- polymorphic type signature that allows the returning query to be passed to
-- `Database.Redis.run` for direct execution, or `Database.Redis.multiExec` for
-- executation as part of a transaction.
doRawQuery :: (Prelude.Applicative f, Database.Redis.RedisCtx m f) => Internal.Query result -> PreparedQuery m f (Result Internal.Error result)
doRawQuery query =
  case query of
    Internal.Exists key ->
      Database.Redis.exists (toB key)
        |> PreparedQuery
        |> map Ok
    Internal.Ping ->
      Database.Redis.ping
        |> PreparedQuery
        |> map Ok
    Internal.Get key ->
      Database.Redis.get (toB key)
        |> PreparedQuery
        |> map Ok
    Internal.Set key val ->
      Database.Redis.set (toB key) val
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Setnx key val ->
      Database.Redis.setnx (toB key) val
        |> PreparedQuery
        |> map Ok
    Internal.Getset key val ->
      Database.Redis.getset (toB key) val
        |> PreparedQuery
        |> map Ok
    Internal.Mget keys ->
      Database.Redis.mget (NonEmpty.toList (map toB keys))
        |> PreparedQuery
        |> map Ok
    Internal.Mset vals ->
      Database.Redis.mset (NonEmpty.toList (NonEmpty.map (\(key, val) -> (toB key, val)) vals))
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Del keys ->
      Database.Redis.del (NonEmpty.toList (map toB keys))
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Hgetall key ->
      Database.Redis.hgetall (toB key)
        |> PreparedQuery
        |> map
          ( \results ->
              results
                |> Prelude.traverse
                  ( \(byteKey, v) ->
                      case Data.Text.Encoding.decodeUtf8' byteKey of
                        Prelude.Right textKey -> Ok (textKey, v)
                        Prelude.Left _ -> Err (Internal.LibraryError "key exists but not parsable text")
                  )
          )
    Internal.Hset key field val ->
      Database.Redis.hset (toB key) (toB field) val
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Hsetnx key field val ->
      Database.Redis.hsetnx (toB key) (toB field) val
        |> PreparedQuery
        |> map Ok
    Internal.Hget key field ->
      Database.Redis.hget (toB key) (toB field)
        |> PreparedQuery
        |> map Ok
    Internal.Hmget key fields ->
      Database.Redis.hmget (toB key) (map toB fields)
        |> PreparedQuery
        |> map Ok
    Internal.Hmset key vals ->
      Database.Redis.hmset (toB key) (map (\(field, val) -> (toB field, val)) vals)
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Hdel key fields ->
      Database.Redis.hdel (toB key) (map toB fields)
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Incr key ->
      Database.Redis.incr (toB key)
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Incrby key amount ->
      Database.Redis.incrby (toB key) (fromIntegral amount)
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Expire key secs ->
      Database.Redis.expire (toB key) (fromIntegral secs)
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Lrange key lower upper ->
      Database.Redis.lrange (toB key) (fromIntegral lower) (fromIntegral upper)
        |> PreparedQuery
        |> map Ok
    Internal.Rpush key vals ->
      Database.Redis.rpush (toB key) vals
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Pure x ->
      pure (Ok x)
    Internal.Apply f x ->
      map2 (map2 (<|)) (doRawQuery f) (doRawQuery x)
    Internal.WithResult f q ->
      let PreparedQuery redisCtx = doRawQuery q
       in PreparedQuery
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
  Internal.TracedQuery ->
  Connection ->
  Platform.DoAnythingHandler ->
  Database.Redis.Redis (Either Database.Redis.Reply (Result Internal.Error a)) ->
  Task Internal.Error a
platformRedis query connection anything action =
  Database.Redis.runRedis (connectionHedis connection) action
    |> map toResult
    |> map
      ( \result -> -- Result Internal.Error (Result Internal.Error a) -> Result Internal.Error a
          case result of
            Ok a -> a
            Err err -> Err err
      )
    |> Exception.handle (\(_ :: Database.Redis.ConnectionLostException) -> pure <| Err Internal.ConnectionLost)
    |> Exception.handleAny
      ( \err ->
          Exception.displayException err
            |> Data.Text.pack
            |> Internal.LibraryError
            |> Err
            |> pure
      )
    |> Platform.doAnything anything
    |> traceQuery query connection

traceQuery :: Internal.TracedQuery -> Connection -> Task e a -> Task e a
traceQuery (Internal.TracedQuery query) connection task =
  let info =
        Info
          { infoCommands = Internal.cmds query,
            infoHost = connectionHost connection,
            infoPort = connectionPort connection
          }
   in Platform.tracingSpan
        "Redis Query"
        (Platform.finally task (Platform.setTracingSpanDetails info))

data Info
  = Info
      { infoCommands :: List Text,
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

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
