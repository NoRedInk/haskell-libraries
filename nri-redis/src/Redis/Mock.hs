{-# LANGUAGE GADTs #-}

-- | Redis.Mock is useful for writing tests without Redis running
module Redis.Mock
  ( handler,
    handlerIO,
  )
where

import qualified Array
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as TE
import qualified Database.Redis
import qualified Dict
import qualified Expect
import qualified List
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Text
import qualified Text.Regex.PCRE.Light as Regex
import qualified Tuple
import Prelude (IO, pure)
import qualified Prelude

-- | This functions returns a task that you can run in each test to retrieve a
-- fresh mock handler
handler :: Expect.Expectation' (Internal.Handler' x)
handler =
  handlerIO
    |> Expect.fromIO

-- | It's better to use handler and create a new mock handler for each test.
-- Tests run in parallel which means that they all share the same hashmap.
handlerIO :: IO (Internal.Handler' x)
handlerIO = do
  modelRef <- init
  doAnything <- Platform.doAnythingHandler
  Internal.Handler'
    { Internal.doQuery = doQuery' modelRef doAnything,
      Internal.doTransaction = doQuery' modelRef doAnything,
      Internal.namespace = "tests",
      Internal.maxKeySize = Settings.NoMaxKeySize
    }
    |> Prelude.pure
  where
    doQuery' modelRef doAnything = \query ->
      atomicModifyIORef' modelRef (doQuery query)
        |> Platform.doAnything doAnything
        |> Internal.traceQuery (Internal.cmds query) "Redis.Mock" Nothing

-- | This is our mock implementation of the Redis state. Our mock implementation
-- will store a single value of this type, and redis commands will modify it.
data Model = Model
  { hash :: HM.HashMap Text RedisType,
    scans :: Scans
  }

type Scans = Array.Array (HM.HashMap Text ())

-- | Redis supports a small number of types and most of its commands expect a
-- particular type in the keys the command is used on.
--
-- The type below contains a subset of the types supported by Redis, just those
-- we currently have commands for.
data RedisType
  = RedisByteString ByteString
  | RedisHash (HM.HashMap Text ByteString)
  | RedisList [ByteString]
  | RedisSet (HS.HashSet ByteString)
  | RedisSortedSet (Dict.Dict ByteString Float)
  deriving (Eq)

expectByteString :: RedisType -> Result Internal.Error ByteString
expectByteString val =
  case val of
    RedisByteString bytestring -> Ok bytestring
    RedisHash _ -> Err wrongTypeErr
    RedisList _ -> Err wrongTypeErr
    RedisSet _ -> Err wrongTypeErr
    RedisSortedSet _ -> Err wrongTypeErr

expectHash :: RedisType -> Result Internal.Error (HM.HashMap Text ByteString)
expectHash val =
  case val of
    RedisByteString _ -> Err wrongTypeErr
    RedisHash hash -> Ok hash
    RedisList _ -> Err wrongTypeErr
    RedisSet _ -> Err wrongTypeErr
    RedisSortedSet _ -> Err wrongTypeErr

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
    RedisSet _ -> Err wrongTypeErr
    RedisSortedSet _ -> Err wrongTypeErr

init :: IO (IORef Model)
init =
  newIORef
    Model
      { hash = HM.empty,
        scans = Array.initialize 1 (always HM.empty)
      }

doQuery :: Internal.Query a -> Model -> (Model, Result Internal.Error a)
doQuery query model =
  let hm = hash model
      updateHash = \h -> model {hash = h}
   in case query of
        Internal.Apply fQuery xQuery ->
          let (model1, f) = doQuery fQuery model
              (model2, x) = doQuery xQuery model1
           in (model2, map2 (\f' x' -> f' x') f x)
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
            |> Tuple.mapFirst updateHash
        Internal.Exists key ->
          ( model,
            Ok (HM.member key hm)
          )
        Internal.Expire _ _ ->
          -- Expiring is an intentional no-op in `Redis.Mock`. Implementing it would
          -- likely be a lot of effort, and only support writing slow tests.
          ( model,
            Ok ()
          )
        Internal.Get key ->
          ( model,
            HM.lookup key hm
              |> Prelude.traverse expectByteString
          )
        Internal.Getset key value ->
          ( updateHash <| HM.insert key (RedisByteString value) hm,
            HM.lookup key hm
              |> Prelude.traverse expectByteString
          )
        Internal.Hdel key fields ->
          case HM.lookup key hm of
            Nothing ->
              ( model,
                Ok 0
              )
            Just (RedisHash hm') ->
              let hmAfterDeletions = Prelude.foldr HM.delete hm' fields
               in ( updateHash <| HM.insert key (RedisHash hmAfterDeletions) hm,
                    HM.size hm' - HM.size hmAfterDeletions
                      |> Prelude.fromIntegral
                      |> Ok
                  )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Hget key field ->
          case HM.lookup key hm of
            Nothing ->
              ( model,
                Ok Nothing
              )
            Just (RedisHash hm') ->
              ( model,
                Ok (HM.lookup field hm')
              )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Hgetall key ->
          ( model,
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
          ( model,
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
              ( model,
                Ok []
              )
            Just (RedisHash hm') ->
              ( model,
                map (\field -> HM.lookup field hm') fields
                  |> NonEmpty.toList
                  |> Ok
              )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Hmset key vals' ->
          let vals = NonEmpty.toList vals'
           in case HM.lookup key hm of
                Nothing ->
                  ( updateHash <| HM.insert key (RedisHash (HM.fromList vals)) hm,
                    Ok ()
                  )
                Just (RedisHash hm') ->
                  ( updateHash <| HM.insert key (RedisHash (HM.fromList vals ++ hm')) hm,
                    Ok ()
                  )
                Just _ ->
                  ( model,
                    Err wrongTypeErr
                  )
        Internal.Hset key field val ->
          case HM.lookup key hm of
            Nothing ->
              ( updateHash <| HM.insert key (RedisHash (HM.singleton field val)) hm,
                Ok ()
              )
            Just (RedisHash hm') ->
              ( updateHash <| HM.insert key (RedisHash (HM.insert field val hm')) hm,
                Ok ()
              )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Hsetnx key field val ->
          case HM.lookup key hm of
            Nothing ->
              ( updateHash <| HM.insert key (RedisHash (HM.singleton field val)) hm,
                Ok True
              )
            Just (RedisHash hm') ->
              if HM.member field hm'
                then
                  ( model,
                    Ok False
                  )
                else
                  ( updateHash <| HM.insert key (RedisHash (HM.insert field val hm')) hm,
                    Ok True
                  )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Incr key ->
          doQuery (Internal.Incrby key 1) model
        Internal.Incrby key amount ->
          let encodeInt = RedisByteString << TE.encodeUtf8 << Text.fromInt
           in case HM.lookup key hm of
                Nothing ->
                  ( updateHash <| HM.insert key (encodeInt amount) hm,
                    Ok 1
                  )
                Just val ->
                  case expectInt val of
                    Err err -> (model, Err err)
                    Ok x ->
                      ( updateHash <| HM.insert key (encodeInt (x + amount)) hm,
                        Ok (x + amount)
                      )
        Internal.Lrange key lower' upper' ->
          ( model,
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
          ( model,
            Prelude.traverse
              (\key -> HM.lookup key hm |> Prelude.traverse expectByteString)
              (NonEmpty.toList keys)
          )
        Internal.Mset assocs ->
          ( updateHash
              ( List.foldl
                  (\(key, val) hm' -> HM.insert key val hm')
                  hm
                  (List.map (\(k, v) -> (k, RedisByteString v)) (NonEmpty.toList assocs))
              ),
            Ok ()
          )
        Internal.Ping ->
          ( model,
            Ok Database.Redis.Pong
          )
        Internal.Pure x -> (model, Ok x)
        Internal.Rpush key vals' ->
          let vals = NonEmpty.toList vals'
           in case HM.lookup key hm of
                Nothing ->
                  ( updateHash <| HM.insert key (RedisList vals) hm,
                    Ok (List.length vals)
                  )
                Just (RedisList prev) ->
                  let combined = prev ++ vals
                   in ( updateHash <| HM.insert key (RedisList combined) hm,
                        Ok (List.length combined)
                      )
                Just _ ->
                  ( model,
                    Err wrongTypeErr
                  )
        Internal.Scan prevCursor maybeMatch maybeCount ->
          let count =
                Maybe.withDefault 10 maybeCount

              -- Do the scan in roughly the same way the Redis docs describe, stopping when enough matches have been found
              doScan :: Regex.Regex -> List Text -> HM.HashMap Text () -> List Text -> Int -> (HM.HashMap Text (), List Text)
              doScan regex keys scannedKeys matchedKeys numMatched =
                if numMatched == count
                  then (scannedKeys, matchedKeys)
                  else case keys of
                    [] -> (scannedKeys, matchedKeys)
                    currentKey : remainingKeys ->
                      let (newMatchedKeys, newNumMatched) =
                            if matchesRegex regex currentKey
                              then (currentKey : matchedKeys, numMatched + 1)
                              else (matchedKeys, numMatched)
                          newScannedKeys =
                            HM.insert currentKey () scannedKeys
                       in doScan regex remainingKeys newScannedKeys newMatchedKeys newNumMatched

              prevScans =
                scans model
              currentScanIndex =
                Array.length prevScans

              result :: Result Internal.Error (Scans, Database.Redis.Cursor, [Text])
              result = do
                prevScannedKeys <-
                  if prevCursor == Database.Redis.cursor0
                    then Ok HM.empty
                    else do
                      index <- decodeCursor prevCursor
                      Result.fromMaybe
                        (Internal.LibraryError ("Mock scan state not found for cursor " ++ Text.fromInt index))
                        (Array.get index prevScans)
                matchRegex <-
                  regexFromGlobStylePattern (Maybe.withDefault "*" maybeMatch)
                let keysToSearch =
                      HM.difference hm prevScannedKeys
                        |> HM.keys
                if List.isEmpty keysToSearch
                  then Ok (prevScans, Database.Redis.cursor0, [])
                  else do
                    let (scannedKeys, matchedKeys) =
                          doScan matchRegex keysToSearch prevScannedKeys [] 0
                    let newScans =
                          Array.push scannedKeys prevScans
                    newCursor <-
                      if HM.size scannedKeys == HM.size hm
                        then Ok Database.Redis.cursor0
                        else encodeCursor currentScanIndex
                    Ok (newScans, newCursor, matchedKeys)
           in case result of
                Ok (newScans, newCursor, matchedKeys) ->
                  ( model {scans = newScans},
                    Ok (newCursor, matchedKeys)
                  )
                Err err ->
                  ( model,
                    Err err
                  )
        Internal.Set key value ->
          ( updateHash <| HM.insert key (RedisByteString value) hm,
            Ok ()
          )
        Internal.Setex key _ value ->
          ( updateHash <| HM.insert key (RedisByteString value) hm,
            Ok ()
          )
        Internal.Setnx key value ->
          if HM.member key hm
            then (model, Ok False)
            else
              ( updateHash <| HM.insert key (RedisByteString value) hm,
                Ok True
              )
        Internal.WithResult f q ->
          doQuery q model
            |> map
              ( \result ->
                  case result of
                    Err a -> Err a
                    Ok res -> f res
              )
        Internal.Sadd key vals ->
          let valsSet = HS.fromList (NonEmpty.toList vals)
           in case HM.lookup key hm of
                Nothing ->
                  ( updateHash <| HM.insert key (RedisSet valsSet) hm,
                    Ok (Prelude.fromIntegral (HS.size valsSet))
                  )
                Just (RedisSet set) ->
                  let newSet = valsSet ++ set
                   in ( updateHash <| HM.insert key (RedisSet newSet) hm,
                        Ok (Prelude.fromIntegral (HS.size newSet - HS.size set))
                      )
                Just _ ->
                  ( model,
                    Err wrongTypeErr
                  )
        Internal.Scard key ->
          ( model,
            case HM.lookup key hm of
              Nothing -> Ok 0
              Just (RedisSet set) -> Ok (Prelude.fromIntegral (HS.size set))
              Just _ -> Err wrongTypeErr
          )
        Internal.Srem key vals ->
          let valsSet = HS.fromList (NonEmpty.toList vals)
           in case HM.lookup key hm of
                Nothing ->
                  ( model,
                    Ok 0
                  )
                Just (RedisSet set) ->
                  let newSet = HS.difference set valsSet
                   in ( updateHash <| HM.insert key (RedisSet newSet) hm,
                        Ok (Prelude.fromIntegral (HS.size set - HS.size newSet))
                      )
                Just _ ->
                  ( model,
                    Err wrongTypeErr
                  )
        Internal.Smembers key ->
          ( model,
            case HM.lookup key hm of
              Nothing -> Ok []
              Just (RedisSet set) -> Ok (HS.toList set)
              Just _ -> Err wrongTypeErr
          )
        Internal.Zadd key vals ->
          case HM.lookup key hm of
            Nothing ->
              ( updateHash <| HM.insert key (RedisSortedSet vals) hm,
                Ok (Prelude.fromIntegral (Dict.size vals))
              )
            Just (RedisSortedSet sortedSet) ->
              let newSet = Dict.union sortedSet vals
               in ( updateHash <| HM.insert key (RedisSortedSet newSet) hm,
                    Ok (Prelude.fromIntegral (Dict.size newSet - Dict.size sortedSet))
                  )
            Just _ ->
              ( model,
                Err wrongTypeErr
              )
        Internal.Zrange key start stop ->
          ( model,
            case HM.lookup key hm of
              Nothing -> Ok []
              Just (RedisSortedSet sortedSet) ->
                let items =
                      sortedSet
                        |> Dict.toList
                        |> List.sortBy (\(val, score) -> (score, val))
                        |> List.map Tuple.first
                        |> Array.fromList

                    -- `stop` is inclusive for zrange, whereas with slice it's
                    -- exclusive
                    --
                    -- we attempt to normalize here, by appropriate modulo-math
                    -- and offsetting by 1
                    (sliceStart, sliceStop) =
                      ( modBy (Array.length items) start,
                        1 + modBy (Array.length items + 1) stop
                      )
                 in items
                      |> Array.slice sliceStart sliceStop
                      |> Array.toList
                      |> Ok
              Just _ ->
                Err wrongTypeErr
          )
        Internal.Zrank key member ->
          ( model,
            case HM.lookup key hm of
              Nothing -> Ok Nothing
              Just (RedisSortedSet sortedSet) ->
                let find [] = Nothing
                    find ((idx, v) : rest) = if v == member then Just idx else find rest

                    items =
                      sortedSet
                        |> Dict.toList
                        |> List.sortBy (\(val, score) -> (score, val))
                        |> List.indexedMap (\idx (val, _) -> (idx, val))
                 in Ok (find items)
              Just _ -> Err wrongTypeErr
          )
        Internal.Zrevrank key member ->
          ( model,
            case HM.lookup key hm of
              Nothing -> Ok Nothing
              Just (RedisSortedSet sortedSet) ->
                let find [] = Nothing
                    find ((idx, v) : rest) = if v == member then Just idx else find rest

                    items =
                      sortedSet
                        |> Dict.toList
                        |> List.sortBy (\(val, score) -> (score, val))
                        |> List.reverse
                        |> List.indexedMap (\idx (val, _) -> (idx, val))
                 in Ok (find items)
              Just _ -> Err wrongTypeErr
          )

wrongTypeErr :: Internal.Error
wrongTypeErr = Internal.RedisError "WRONGTYPE Operation against a key holding the wrong kind of value"

-- https://redis.io/commands/keys/
--    h?llo matches hello, hallo and hxllo
--    h*llo matches hllo and heeeello
--    h[ae]llo matches hello and hallo, but not hillo
--    h[^e]llo matches hallo, hbllo, ... but not hello
--    h[a-b]llo matches hallo and hbllo
regexFromGlobStylePattern :: Text -> Result Internal.Error Regex.Regex
regexFromGlobStylePattern globStylePattern =
  let regexBytes =
        globStylePattern
          |> Text.replace "?" "."
          |> Text.replace "*" ".*"
          |> TE.encodeUtf8
   in case Regex.compileM regexBytes [] of
        Prelude.Right regex -> Ok regex
        Prelude.Left errChars -> Err <| Internal.LibraryError <| "INVALID MATCH PATTERN" ++ Text.fromList errChars

matchesRegex :: Regex.Regex -> Text -> Bool
matchesRegex regex text =
  case Regex.match regex (TE.encodeUtf8 text) [] of
    Just _ -> True
    Nothing -> False

-- Cursor is an opaque newtype of ByteString. Can't deconstruct, so cheat using `Show`.
decodeCursor :: Database.Redis.Cursor -> Result Internal.Error Int
decodeCursor opaqueCursor =
  case opaqueCursor |> Prelude.show |> Text.fromList |> Text.filter Char.isDigit |> Text.toInt of
    Just scanIndex -> Ok scanIndex
    Nothing -> Err (Internal.LibraryError "Mock Cursor could not be parsed as an integer")

-- Cursor is opaque. To construct it, pretend we received it as a ByteString over the wire.
encodeCursor :: Int -> Result Internal.Error Database.Redis.Cursor
encodeCursor scanIndex =
  let decoded =
        Text.fromInt scanIndex
          |> TE.encodeUtf8
          |> Just
          |> Database.Redis.Bulk
          |> Database.Redis.decode
   in case decoded of
        Prelude.Right ok -> Ok ok
        Prelude.Left _ -> Err (Internal.LibraryError ("Failed to encode Mock cursor: " ++ Text.fromInt scanIndex))
