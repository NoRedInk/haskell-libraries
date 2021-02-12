{-# LANGUAGE GADTs #-}

module Redis.Mock
  ( handler,
    handlerIO,
  )
where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as TE
import qualified Database.Redis
import qualified List
import NriPrelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified System.IO.Unsafe
import qualified Text
import qualified Tuple
import Prelude (IO, pure)
import qualified Prelude

-- | This functions returns a task that you can run in each test to retrieve a
-- fresh mock handler
handler :: Task e Internal.Handler
handler =
  handlerIO
    |> map Ok
    |> Platform.doAnything testDoAnything

-- | It's better to use handler and create a new mock handler for each test.
-- Tests run in parallel which means that they all share the same hashmap.
handlerIO :: IO Internal.Handler
handlerIO = do
  modelRef <- init
  Internal.Handler
    { Internal.doQuery = \query ->
        atomicModifyIORef'
          modelRef
          ( \model ->
              let (newHash, res) = doQuery query (hash model)
               in ( model {hash = newHash},
                    res
                  )
          )
          |> Platform.doAnything testDoAnything,
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
          |> Platform.doAnything testDoAnything,
      Internal.doWatch = \keys ->
        atomicModifyIORef'
          modelRef
          ( \model ->
              ( watchKeys keys model,
                Ok ()
              )
          )
          |> Platform.doAnything testDoAnything,
      Internal.namespace = "tests"
    }
    |> Prelude.pure

-- | This is our mock implementation of the Redis state. Our mock implementation
-- will store a single value of this type, and redis commands will modify it.
data Model = Model
  { hash :: HM.HashMap Text RedisType,
    watchedKeys :: HM.HashMap Text (Maybe RedisType)
  }

-- | Redis supports a small number of types and most of its commands expect a
-- particular type in the keys the command is used on.
--
-- The type below contains a subset of the types supported by Redis, just those
-- we currently have commands for.
data RedisType
  = RedisByteString ByteString
  | RedisHash (HM.HashMap Text ByteString)
  | RedisList [ByteString]
  deriving (Eq)

expectByteString :: RedisType -> Result Internal.Error ByteString
expectByteString val =
  case val of
    RedisByteString bytestring -> Ok bytestring
    RedisHash _ -> Err wrongTypeErr
    RedisList _ -> Err wrongTypeErr

expectHash :: RedisType -> Result Internal.Error (HM.HashMap Text ByteString)
expectHash val =
  case val of
    RedisByteString _ -> Err wrongTypeErr
    RedisHash hash -> Ok hash
    RedisList _ -> Err wrongTypeErr

expectInt :: RedisType -> Result Internal.Error Int
expectInt val =
  case val of
    RedisByteString val' ->
      case TE.decodeUtf8' val' of
        Prelude.Left _ -> Err wrongTypeErr
        Prelude.Right str ->
          case Text.toInt str of
            Nothing -> Err wrongTypeErr
            Just int -> Ok int
    RedisHash _ -> Err wrongTypeErr
    RedisList _ -> Err wrongTypeErr

watchKeys :: [Text] -> Model -> Model
watchKeys keys model =
  model
    { watchedKeys =
        -- If this command watches a key that already was watched,
        -- keep the value from the original snapshot: We want to
        -- ensure transactions don't change keys since they were
        -- first watched.
        snapshotKeys keys (hash model) ++ watchedKeys model
    }

snapshotKeys :: [Text] -> HM.HashMap Text RedisType -> HM.HashMap Text (Maybe RedisType)
snapshotKeys keys hm =
  Prelude.foldMap
    (\key -> HM.singleton key (HM.lookup key hm))
    keys

init :: IO (IORef Model)
init = newIORef (Model HM.empty HM.empty)

doQuery ::
  Internal.Query a ->
  HM.HashMap Text RedisType ->
  (HM.HashMap Text RedisType, Result Internal.Error a)
doQuery query hm =
  case query of
    Internal.Apply fQuery xQuery ->
      let (hm1, f) = doQuery fQuery hm
          (hm2, x) = doQuery xQuery hm1
       in (hm2, map2 (\f' x' -> f' x') f x)
    Internal.Del keys ->
      List.foldl
        ( \key (hm', count) ->
            if HM.member key hm'
              then (HM.delete key hm', count + 1)
              else (hm', count)
        )
        (hm, 0 :: Int)
        (NonEmpty.toList keys)
        |> Tuple.mapSecond Ok
    Internal.Exists key ->
      ( hm,
        Ok (HM.member key hm)
      )
    Internal.Expire _ _ ->
      -- Expiring is an intentional no-op in `Redis.Mock`. Implementing it would
      -- likely be a lot of effort, and only support writing slow tests.
      ( hm,
        Ok ()
      )
    Internal.Get key ->
      ( hm,
        HM.lookup key hm
          |> Prelude.traverse expectByteString
      )
    Internal.Getset key value ->
      ( HM.insert key (RedisByteString value) hm,
        HM.lookup key hm
          |> Prelude.traverse expectByteString
      )
    Internal.Hdel key fields ->
      case HM.lookup key hm of
        Nothing ->
          ( hm,
            Ok 0
          )
        Just (RedisHash hm') ->
          let hmAfterDeletions = Prelude.foldr HM.delete hm' fields
           in ( HM.insert key (RedisHash hmAfterDeletions) hm,
                HM.size hm' - HM.size hmAfterDeletions
                  |> Prelude.fromIntegral
                  |> Ok
              )
        Just _ ->
          ( hm,
            Err wrongTypeErr
          )
    Internal.Hget key field ->
      case HM.lookup key hm of
        Nothing ->
          ( hm,
            Ok Nothing
          )
        Just (RedisHash hm') ->
          ( hm,
            Ok (HM.lookup field hm')
          )
        Just _ ->
          ( hm,
            Err wrongTypeErr
          )
    Internal.Hgetall key ->
      ( hm,
        HM.lookup key hm
          |> Prelude.traverse expectHash
          |> map
            ( \res ->
                case res of
                  Just hm' -> HM.toList hm'
                  Nothing -> []
            )
      )
    Internal.Hkeys key ->
      ( hm,
        HM.lookup key hm
          |> Prelude.traverse expectHash
          |> map
            ( \res ->
                case res of
                  Just hm' -> HM.keys hm'
                  Nothing -> []
            )
      )
    Internal.Hmget key fields ->
      case HM.lookup key hm of
        Nothing ->
          ( hm,
            Ok []
          )
        Just (RedisHash hm') ->
          ( hm,
            map (\field -> HM.lookup field hm') fields
              |> NonEmpty.toList
              |> Ok
          )
        Just _ ->
          ( hm,
            Err wrongTypeErr
          )
    Internal.Hmset key vals' ->
      let vals = NonEmpty.toList vals'
       in case HM.lookup key hm of
            Nothing ->
              ( HM.insert key (RedisHash (HM.fromList vals)) hm,
                Ok ()
              )
            Just (RedisHash hm') ->
              ( HM.insert key (RedisHash (HM.fromList vals ++ hm')) hm,
                Ok ()
              )
            Just _ ->
              ( hm,
                Err wrongTypeErr
              )
    Internal.Hset key field val ->
      case HM.lookup key hm of
        Nothing ->
          ( HM.insert key (RedisHash (HM.singleton field val)) hm,
            Ok ()
          )
        Just (RedisHash hm') ->
          ( HM.insert key (RedisHash (HM.insert field val hm')) hm,
            Ok ()
          )
        Just _ ->
          ( hm,
            Err wrongTypeErr
          )
    Internal.Hsetnx key field val ->
      case HM.lookup key hm of
        Nothing ->
          ( HM.insert key (RedisHash (HM.singleton field val)) hm,
            Ok True
          )
        Just (RedisHash hm') ->
          if HM.member field hm'
            then
              ( hm,
                Ok False
              )
            else
              ( HM.insert key (RedisHash (HM.insert field val hm')) hm,
                Ok True
              )
        Just _ ->
          ( hm,
            Err wrongTypeErr
          )
    Internal.Incr key ->
      doQuery (Internal.Incrby key 1) hm
    Internal.Incrby key amount ->
      let encodeInt = RedisByteString << TE.encodeUtf8 << Text.fromInt
       in case HM.lookup key hm of
            Nothing ->
              ( HM.insert key (encodeInt amount) hm,
                Ok 1
              )
            Just val ->
              case expectInt val of
                Err err -> (hm, Err err)
                Ok x ->
                  ( HM.insert key (encodeInt (x + amount)) hm,
                    Ok (x + amount)
                  )
    Internal.Lrange key lower' upper' ->
      ( hm,
        case HM.lookup key hm of
          Nothing ->
            Ok []
          Just (RedisList elems) ->
            let length = List.length elems
                lower = if lower' >= 0 then lower' else length + lower'
                upper = if upper' >= 0 then upper' else length + upper'
             in elems
                  |> Data.List.splitAt (Prelude.fromIntegral (upper + 1))
                  |> Tuple.first
                  |> List.drop lower
                  |> Ok
          Just _ ->
            Err wrongTypeErr
      )
    Internal.Mget keys ->
      ( hm,
        Prelude.traverse
          (\key -> HM.lookup key hm |> Prelude.traverse expectByteString)
          (NonEmpty.toList keys)
      )
    Internal.Mset assocs ->
      ( List.foldl
          (\(key, val) hm' -> HM.insert key val hm')
          hm
          (List.map (\(k, v) -> (k, RedisByteString v)) (NonEmpty.toList assocs)),
        Ok ()
      )
    Internal.Ping ->
      ( hm,
        Ok Database.Redis.Pong
      )
    Internal.Pure x -> (hm, Ok x)
    Internal.Rpush key vals' ->
      let vals = NonEmpty.toList vals'
       in case HM.lookup key hm of
            Nothing ->
              ( HM.insert key (RedisList vals) hm,
                Ok (List.length vals)
              )
            Just (RedisList prev) ->
              let combined = prev ++ vals
               in ( HM.insert key (RedisList combined) hm,
                    Ok (List.length combined)
                  )
            Just _ ->
              ( hm,
                Err wrongTypeErr
              )
    Internal.Set key value ->
      ( HM.insert key (RedisByteString value) hm,
        Ok ()
      )
    Internal.Setex key _ value ->
      ( HM.insert key (RedisByteString value) hm,
        Ok ()
      )
    Internal.Setnx key value ->
      if HM.member key hm
        then (hm, Ok False)
        else (HM.insert key (RedisByteString value) hm, Ok True)
    Internal.WithResult f q ->
      doQuery q hm
        |> map
          ( \result ->
              case result of
                Err a -> Err a
                Ok res -> f res
          )

wrongTypeErr :: Internal.Error
wrongTypeErr = Internal.RedisError "WRONGTYPE Operation against a key holding the wrong kind of value"

-- | Creates a unpacked `DoAnythingHandler`, allowing us to use it without
-- to turn `IO` into `Task` types without needing to pass it in as an argument,
-- in the context of this test helper.
{-# NOINLINE testDoAnything #-}
testDoAnything :: Platform.DoAnythingHandler
testDoAnything = System.IO.Unsafe.unsafePerformIO Platform.doAnythingHandler
