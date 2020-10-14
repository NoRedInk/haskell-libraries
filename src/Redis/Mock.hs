{-# LANGUAGE GADTs #-}

module Redis.Mock
  ( handler,
  )
where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Database.Redis
import qualified List
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Tuple
import Prelude (IO, error, pure)
import qualified Prelude

handler :: Text -> IO Internal.Handler
handler namespace = do
  modelRef <- init
  anything <- Platform.doAnythingHandler
  Internal.InternalHandler
    { Internal.doQuery = \query ->
        atomicModifyIORef'
          modelRef
          ( \model ->
              let (newHash, res) = doQuery query (hash model)
               in ( model {hash = newHash},
                    res
                  )
          )
          |> Platform.doAnything anything,
      Internal.doTransaction = \query ->
        atomicModifyIORef'
          modelRef
          ( \Model {hash, watchedKeys} ->
              let (newHash, res) =
                    if snapshotKeys (HM.keys watchedKeys) hash == watchedKeys
                      then doQuery query hash
                      else (hash, Err Internal.TransactionAborted)
               in ( Model {hash = newHash, watchedKeys = HM.empty},
                    res
                  )
          )
          |> Platform.doAnything anything,
      Internal.watch = \keys ->
        atomicModifyIORef'
          modelRef
          ( \model ->
              ( watchKeys keys model,
                Ok ()
              )
          )
          |> Platform.doAnything anything
    }
    |> Internal.namespacedHandler namespace
    |> Prelude.pure

-- | This is our mock implementation of the Redis state. Our mock implementation
-- will store a single value of this type, and redis commands will modify it.
data Model
  = Model
      { hash :: HM.HashMap ByteString RedisType,
        watchedKeys :: HM.HashMap ByteString (Maybe RedisType)
      }

-- | Redis supports a small number of types and most of its commands expect a
-- particular type in the keys the command is used on.
--
-- The type below contains a subset of the types supported by Redis, just those
-- we currently have commands for.
data RedisType
  = RedisByteString ByteString
  | RedisHash (HM.HashMap ByteString ByteString)
  deriving (Eq)

expectByteString :: RedisType -> Result Internal.Error ByteString
expectByteString val =
  case val of
    RedisByteString bytestring -> Ok bytestring
    RedisHash _ -> Err (Internal.RedisError "WRONGTYPE Operation against a key holding the wrong kind of value")

watchKeys :: [ByteString] -> Model -> Model
watchKeys keys model =
  model
    { watchedKeys =
        -- If this command watches a key that already was watched,
        -- keep the value from the original snapshot: We want to
        -- ensure transactions don't change keys since they were
        -- first watched.
        snapshotKeys keys (hash model) ++ watchedKeys model
    }

snapshotKeys :: [ByteString] -> HM.HashMap ByteString RedisType -> HM.HashMap ByteString (Maybe RedisType)
snapshotKeys keys hm =
  Prelude.foldMap
    (\key -> HM.singleton key (HM.lookup key hm))
    keys

init :: IO (IORef Model)
init = newIORef (Model HM.empty HM.empty)

doQuery ::
  Internal.Query a ->
  HM.HashMap ByteString RedisType ->
  (HM.HashMap ByteString RedisType, Result Internal.Error a)
doQuery query hm =
  case query of
    Internal.Ping ->
      ( hm,
        Ok Database.Redis.Pong
      )
    Internal.Get key ->
      ( hm,
        HM.lookup key hm
          |> Prelude.traverse expectByteString
      )
    Internal.Set key value ->
      ( HM.insert key (RedisByteString value) hm,
        Ok ()
      )
    Internal.Getset key value ->
      ( HM.insert key (RedisByteString value) hm,
        HM.lookup key hm
          |> Prelude.traverse expectByteString
      )
    Internal.Mget keys ->
      ( hm,
        Prelude.traverse
          (\key -> HM.lookup key hm |> Prelude.traverse expectByteString)
          keys
      )
    Internal.Mset assocs ->
      ( List.foldl
          (\(key, val) hm' -> HM.insert key val hm')
          hm
          (List.map (\(k, v) -> (k, RedisByteString v)) assocs),
        Ok ()
      )
    Internal.Del keys ->
      List.foldl
        ( \key (hm', count) ->
            if HM.member key hm'
              then (HM.delete key hm', count + 1)
              else (hm', count)
        )
        (hm, 0 :: Int)
        keys
        |> Tuple.mapSecond Ok
    Internal.Hgetall _key -> error "No mock implementation implemented yet for hgetall"
    Internal.Hset _key _field _val -> error "No mock implementation implemented yet for hset"
    Internal.Hmset _key _vals -> error "No mock implementation implemented yet for hset"
    Internal.Pure x -> (hm, Ok x)
    Internal.Apply fQuery xQuery ->
      let (hm1, f) = doQuery fQuery hm
          (hm2, x) = doQuery xQuery hm1
       in (hm2, map2 (\f' x' -> f' x') f x)
    Internal.WithResult f q ->
      doQuery q hm
        |> map
          ( \result ->
              case result of
                Err a -> Err a
                Ok res -> f res
          )
