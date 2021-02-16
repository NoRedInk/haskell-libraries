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
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding as Encoding
import qualified Data.UUID as UUID
import qualified Data.UUID.V4
import qualified Database.Redis
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform
import qualified Process
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Task
import qualified Text
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
             in Stack.withFrozenCallStack platformRedis (Internal.cmds query) connection anything redisCtx,
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
                  |> Stack.withFrozenCallStack platformRedis (Internal.cmds query) connection anything,
          Internal.doWatch = \keys ->
            Database.Redis.watch (map toB keys)
              |> map (map (\_ -> Ok ()))
              |> Stack.withFrozenCallStack platformRedis (Internal.cmds (Internal.Pure ())) connection anything,
          Internal.doLock = lock connection anything,
          Internal.namespace = namespace
        },
      connection
    )

newtype PreparedQuery m f result = PreparedQuery
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
    Internal.Apply f x ->
      map2 (map2 (<|)) (doRawQuery f) (doRawQuery x)
    Internal.Del keys ->
      Database.Redis.del (NonEmpty.toList (map toB keys))
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Exists key ->
      Database.Redis.exists (toB key)
        |> PreparedQuery
        |> map Ok
    Internal.Expire key secs ->
      Database.Redis.expire (toB key) (fromIntegral secs)
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Get key ->
      Database.Redis.get (toB key)
        |> PreparedQuery
        |> map Ok
    Internal.Getset key val ->
      Database.Redis.getset (toB key) val
        |> PreparedQuery
        |> map Ok
    Internal.Hdel key fields ->
      Database.Redis.hdel (toB key) (NonEmpty.toList (map toB fields))
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Hget key field ->
      Database.Redis.hget (toB key) (toB field)
        |> PreparedQuery
        |> map Ok
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
    Internal.Hkeys key ->
      Database.Redis.hkeys (toB key)
        |> PreparedQuery
        |> map
          ( Prelude.traverse
              ( \byteKey -> case Data.Text.Encoding.decodeUtf8' byteKey of
                  Prelude.Right textKey -> Ok textKey
                  Prelude.Left _ -> Err (Internal.LibraryError "key exists but not parsable text")
              )
          )
    Internal.Hsetnx key field val ->
      Database.Redis.hsetnx (toB key) (toB field) val
        |> PreparedQuery
        |> map Ok
    Internal.Hmget key fields ->
      Database.Redis.hmget (toB key) (NonEmpty.toList (map toB fields))
        |> PreparedQuery
        |> map Ok
    Internal.Hmset key vals ->
      Database.Redis.hmset (toB key) (map (\(field, val) -> (toB field, val)) (NonEmpty.toList vals))
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Hset key field val ->
      Database.Redis.hset (toB key) (toB field) val
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Incr key ->
      Database.Redis.incr (toB key)
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Incrby key amount ->
      Database.Redis.incrby (toB key) (fromIntegral amount)
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Lrange key lower upper ->
      Database.Redis.lrange (toB key) (fromIntegral lower) (fromIntegral upper)
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
    Internal.Ping ->
      Database.Redis.ping
        |> PreparedQuery
        |> map Ok
    Internal.Pure x ->
      pure (Ok x)
    Internal.Rpush key vals ->
      Database.Redis.rpush (toB key) (NonEmpty.toList vals)
        |> PreparedQuery
        |> map (Ok << fromIntegral)
    Internal.Set key val ->
      Database.Redis.set (toB key) val
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Setex key seconds val ->
      Database.Redis.setex (toB key) (Prelude.fromIntegral seconds) val
        |> PreparedQuery
        |> map (\_ -> Ok ())
    Internal.Setnx key val ->
      Database.Redis.setnx (toB key) val
        |> PreparedQuery
        |> map Ok
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

data Connection = Connection
  { connectionHedis :: Database.Redis.Connection,
    connectionHost :: Text,
    connectionPort :: Text
  }

platformRedis ::
  Stack.HasCallStack =>
  [Text] ->
  Connection ->
  Platform.DoAnythingHandler ->
  Database.Redis.Redis (Either Database.Redis.Reply (Result Internal.Error a)) ->
  Task Internal.Error a
platformRedis cmds connection anything action =
  Database.Redis.runRedis (connectionHedis connection) action
    |> map toResult
    |> map
      ( \result ->
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
    |> Stack.withFrozenCallStack traceQuery cmds connection

traceQuery :: Stack.HasCallStack => [Text] -> Connection -> Task e a -> Task e a
traceQuery cmds connection task =
  let info =
        Info
          { infoCommands = cmds,
            infoHost = connectionHost connection,
            infoPort = connectionPort connection
          }
   in Stack.withFrozenCallStack
        Platform.tracingSpan
        "Redis Query"
        (Platform.finally task (Platform.setTracingSpanDetails info))

data Info = Info
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
    Left err -> Err (Internal.RedisError ("Redis library got back a value with a type it didn't expect: " ++ Data.Text.pack (Prelude.show err)))
    Right r -> Ok r

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8

-- This implements the locking algorithm described at the bottom of the redis
-- documentation for the SET command: https://redis.io/commands/set
--
-- The documentation makes a point that this lock isn't entirely safe. When
-- Redis nodes fail and are replaced it's possible for two processes to obtain
-- the same lock. The documentation recommends using the redlock algorithm to
-- protect against that.
--
-- For now we are satisfied with the trade-off of this simpler lock. It's the
-- same algorithm as the one used by the redis-mutex library in the monolith, so
-- if it's good enough there it's good enough here too.
lock ::
  Connection ->
  Platform.DoAnythingHandler ->
  Internal.Lock e a ->
  Task e a ->
  Task e a
lock conn doAnything config task = do
  -- Create a random value to store in the locking key. We use this to ensure
  -- during cleanup that we only delete the locking key if it was set by us. If
  -- by some race condition another process wrote to the lock before we get to
  -- clean it up then we don't touch it.
  uuid <-
    Data.UUID.V4.nextRandom
      |> map Ok
      |> Platform.doAnything doAnything
  Platform.bracketWithError
    (acquireLock conn doAnything config uuid |> Task.mapError RedisError)
    (\_ _ -> releaseLock conn doAnything config uuid |> Task.mapError RedisError)
    ( \_ ->
        -- Our code has to finish before the lock expires.
        Task.mapError OtherError task
          |> Task.timeout
            (Internal.lockTimeoutInMs config)
            (RedisError Internal.TimeoutError)
    )
    |> Task.onError
      ( \err ->
          case err of
            RedisError redisErr -> Internal.lockHandleError config redisErr
            OtherError otherErr -> Task.fail otherErr
      )

data LockError e
  = RedisError Internal.Error
  | OtherError e

acquireLock ::
  Connection ->
  Platform.DoAnythingHandler ->
  Internal.Lock e a ->
  UUID.UUID ->
  Task Internal.Error ()
acquireLock conn doAnything config uuid =
  if Internal.lockMaxTries config <= 0
    then Task.fail Internal.ExhaustedRetriesWhileAcquiringLock
    else do
      result <- do
        let cmdChunks =
              [ "SET",
                Encoding.encodeUtf8 (Internal.lockKey config),
                UUID.toASCIIBytes uuid,
                "NX",
                "PX",
                Text.fromInt (round (Internal.lockTimeoutInMs config))
                  |> Encoding.encodeUtf8
              ]
        let tracingCmd =
              Data.ByteString.intercalate " " cmdChunks
                |> Encoding.decodeUtf8
        -- We only want to write to the lock key if it's not set yet, and if we
        -- write we want to set an expiry time. The only way to do this is using
        -- the REDIS set command in combination with some options, but hedis
        -- doesn't support these yet, so we use a lower level function to build
        -- the correct redis command ourselves.
        Database.Redis.sendRequest cmdChunks
          |> map (map Ok)
          |> platformRedis [tracingCmd] conn doAnything
      case result of
        Database.Redis.Ok -> Task.succeed ()
        _ -> do
          Process.sleep (Internal.lockTimeoutInMs config)
          acquireLock
            conn
            doAnything
            config {Internal.lockMaxTries = Internal.lockMaxTries config - 1}
            uuid

releaseLock ::
  Connection ->
  Platform.DoAnythingHandler ->
  Internal.Lock e a ->
  UUID.UUID ->
  Task Internal.Error ()
releaseLock conn doAnything config uuid = do
  -- We could use evalsha here, but the script we send is not that much larger
  -- than a sha would be so we save ourselves the trouble. Redis documentation
  -- on the EVALSHA command makes clear that other than bandwidt there's no
  -- performance penalty to using EVAL.
  let tracingCmd =
        Text.join
          " "
          [ "EVAL <delete key if value matches script> 1",
            Internal.lockKey config,
            UUID.toText uuid
          ]
  Database.Redis.eval
    deleteKeyIfValueMatchesScript
    [Encoding.encodeUtf8 (Internal.lockKey config)]
    [UUID.toASCIIBytes uuid]
    |> map (map Ok)
    |> platformRedis [tracingCmd] conn doAnything
    |> map (\(_ :: Prelude.Integer) -> ())

-- This script is copied verbatim from https://redis.io/commands/set.
deleteKeyIfValueMatchesScript :: Data.ByteString.ByteString
deleteKeyIfValueMatchesScript =
  "if redis.call(\"get\",KEYS[1]) == ARGV[1]\n"
    ++ "then\n"
    ++ "    return redis.call(\"del\",KEYS[1])\n"
    ++ "else\n"
    ++ "    return 0\n"
    ++ "end"
    |> Data.ByteString.Builder.toLazyByteString
    |> Data.ByteString.Lazy.toStrict
