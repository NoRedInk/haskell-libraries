{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Redis.Handler
  ( handler,
    handlerAutoExtendExpire,
  )
where

import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Data.ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Dict
import qualified GHC.Stack as Stack
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Script as Script
import qualified Redis.Settings as Settings
import qualified Set
import qualified Text
import Prelude (Either (Left, Right), IO, fromIntegral, pure)
import qualified Prelude

-- | Produce a namespaced handler for Redis access.
handler :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.Handler
handler namespace settings = do
  (namespacedHandler, _) <- Data.Acquire.mkAcquire (acquireHandler namespace settings) releaseHandler
  namespacedHandler
    |> ( \handler' ->
           case Settings.queryTimeout settings of
             Settings.NoQueryTimeout -> handler'
             Settings.TimeoutQueryAfterMilliseconds milliseconds ->
               timeoutAfterMilliseconds (toFloat milliseconds) handler'
       )
    |> Prelude.pure

-- | Produce a namespaced handler for Redis access.
-- This will ensure that we extend all keys accessed by a query by a configured default time (see Settings.defaultExpiry)
handlerAutoExtendExpire :: Text -> Settings.Settings -> Data.Acquire.Acquire Internal.HandlerAutoExtendExpire
handlerAutoExtendExpire namespace settings = do
  (namespacedHandler, _) <- Data.Acquire.mkAcquire (acquireHandler namespace settings) releaseHandler
  namespacedHandler
    |> ( \handler' ->
           case Settings.queryTimeout settings of
             Settings.NoQueryTimeout -> handler'
             Settings.TimeoutQueryAfterMilliseconds milliseconds ->
               timeoutAfterMilliseconds (toFloat milliseconds) handler'
       )
    |> ( \handler' -> case Settings.defaultExpiry settings of
           Settings.NoDefaultExpiry ->
             -- We create the handler as part of starting the application. Throwing
             -- means that if there's a problem with the settings the application will
             -- fail immediately upon start. It won't result in runtime errors during
             -- operation.
             [ "Setting up an auto extend expire handler for",
               "redis failed. Auto extending the expire of keys only works if",
               "there is a setting for `REDIS_DEFAULT_EXPIRY_SECONDS`."
             ]
               |> Text.join " "
               |> Text.toList
               |> Exception.throwString
           Settings.ExpireKeysAfterSeconds secs ->
             defaultExpiryKeysAfterSeconds secs handler'
               |> Prelude.pure
       )
    |> liftIO

timeoutAfterMilliseconds :: Float -> Internal.Handler' x -> Internal.Handler' x
timeoutAfterMilliseconds milliseconds handler' =
  handler'
    { Internal.doQuery =
        Stack.withFrozenCallStack (Internal.doQuery handler')
          >> Task.timeout milliseconds Internal.TimeoutError,
      Internal.doTransaction =
        Stack.withFrozenCallStack (Internal.doTransaction handler')
          >> Task.timeout milliseconds Internal.TimeoutError
    }

defaultExpiryKeysAfterSeconds :: Int -> Internal.HandlerAutoExtendExpire -> Internal.HandlerAutoExtendExpire
defaultExpiryKeysAfterSeconds secs handler' =
  let wrapWithExpire :: Internal.Query a -> Internal.Query a
      wrapWithExpire query' =
        Internal.keysTouchedByQuery query'
          |> Set.toList
          |> List.map (\key -> Internal.Expire key secs)
          |> Internal.sequence
          |> Internal.map2 (\res _ -> res) query'
   in handler'
        { Internal.doQuery = \query' ->
            wrapWithExpire query'
              |> Stack.withFrozenCallStack (Internal.doQuery handler'),
          Internal.doTransaction = \query' ->
            wrapWithExpire query'
              |> Stack.withFrozenCallStack (Internal.doTransaction handler')
        }

acquireHandler :: Text -> Settings.Settings -> IO (Internal.Handler' x, Connection)
acquireHandler namespace settings = do
  connection <- do
    let connectionInfo = Settings.connectionInfo settings
    connectionHedis <-
      case Settings.clusterMode settings of
        Settings.Cluster ->
          Database.Redis.connectCluster connectionInfo
        Settings.NotCluster ->
          Database.Redis.checkedConnect connectionInfo
    let connectionHost = Text.fromList (Database.Redis.connectHost connectionInfo)
    let connectionPort =
          case Database.Redis.connectPort connectionInfo of
            Database.Redis.PortNumber port -> Just (Prelude.fromIntegral port)
            Database.Redis.UnixSocket _ -> Nothing
    pure Connection {connectionHedis, connectionHost, connectionPort}
  anything <- Platform.doAnythingHandler
  pure
    ( Internal.Handler'
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
                          Database.Redis.TxError err -> Right (Err (Internal.RedisError (Text.fromList err)))
                    )
                  |> Stack.withFrozenCallStack (platformRedis (Internal.cmds query) connection anything),
          Internal.namespace = namespace,
          Internal.maxKeySize = Settings.maxKeySize settings
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
    Internal.Eval script ->
      Database.Redis.eval (toB (Script.luaScript script)) (map toB (Script.paramNames script)) (map toB (Script.paramValues script))
        |> PreparedQuery
        |> map Ok
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
    Internal.Scan cursor maybeMatch maybeCount ->
      Database.Redis.ScanOpts (map toB maybeMatch) (map fromIntegral maybeCount)
        |> Database.Redis.scanOpts cursor
        |> PreparedQuery
        |> map
          ( \(nextCursor, byteKeys) ->
              byteKeys
                |> Prelude.traverse
                  ( \byteKey -> case Data.Text.Encoding.decodeUtf8' byteKey of
                      Prelude.Right textKey -> Ok textKey
                      Prelude.Left _ -> Err (Internal.LibraryError "key exists but not parsable text")
                  )
                |> map (\textKeys -> (nextCursor, textKeys))
          )
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
    Internal.Sadd key vals ->
      Database.Redis.sadd (toB key) (NonEmpty.toList vals)
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Scard key ->
      Database.Redis.scard (toB key)
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Srem key vals ->
      Database.Redis.srem (toB key) (NonEmpty.toList vals)
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Smembers key ->
      Database.Redis.smembers (toB key)
        |> PreparedQuery
        |> map Ok
    Internal.Zadd key vals ->
      Dict.toList vals
        |> List.map (\(a, b) -> (b, a))
        |> Database.Redis.zadd (toB key)
        |> PreparedQuery
        |> map (Ok << Prelude.fromIntegral)
    Internal.Zrange key start stop ->
      Database.Redis.zrange
        (toB key)
        (Prelude.fromIntegral start)
        (Prelude.fromIntegral stop)
        |> PreparedQuery
        |> map Ok
    Internal.ZrangeByScoreWithScores key start stop ->
      Database.Redis.zrangebyscoreWithscores
        (toB key)
        start
        stop
        |> PreparedQuery
        |> map Ok
    Internal.Zrank key member ->
      Database.Redis.zrank (toB key) member
        |> PreparedQuery
        |> map (Ok << map Prelude.fromIntegral)
    Internal.Zrevrank key member ->
      Database.Redis.zrevrank (toB key) member
        |> PreparedQuery
        |> map (Ok << map Prelude.fromIntegral)
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

releaseHandler :: (Internal.Handler' x, Connection) -> IO ()
releaseHandler (_, Connection {connectionHedis}) = Database.Redis.disconnect connectionHedis

data Connection = Connection
  { connectionHedis :: Database.Redis.Connection,
    connectionHost :: Text,
    connectionPort :: Maybe Int
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
            |> Text.fromList
            |> Internal.LibraryError
            |> Err
            |> pure
      )
    |> Platform.doAnything anything
    |> Stack.withFrozenCallStack Internal.traceQuery cmds (connectionHost connection) (connectionPort connection)

toResult :: Either Database.Redis.Reply a -> Result Internal.Error a
toResult reply =
  case reply of
    Left (Database.Redis.Error err) -> Err (Internal.RedisError <| Data.Text.Encoding.decodeUtf8 err)
    Left err -> Err (Internal.RedisError ("Redis library got back a value with a type it didn't expect: " ++ Text.fromList (Prelude.show err)))
    Right r -> Ok r

toB :: Text -> Data.ByteString.ByteString
toB = Data.Text.Encoding.encodeUtf8
