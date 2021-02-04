module Main (main) where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Dict
import qualified Environment
import qualified Expect
import qualified Expect.Task
import qualified List
import qualified NonEmptyDict
import NriPrelude hiding (map)
import qualified NriPrelude
import qualified Platform
import Redis hiding (map2)
import qualified Redis.Internal as Internal
import qualified Redis.List
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task
import qualified Test
import Prelude (IO, pure, uncurry)

buildSpecs :: TestHandlers -> Test.Test
buildSpecs TestHandlers {redisHandlers} =
  redisHandlers
    |> List.map (uncurry specs)
    |> Test.describe "Redis Library"

specs :: Text -> Handler -> Test.Test
specs whichHandler redisHandler =
  Test.describe
    (whichHandler ++ " Redis")
    [ Test.task "get and set" <| do
        set api "bob" "hello!" |> query testNS |> Expect.Task.succeeds
        result <- get api "bob" |> query testNS |> Expect.Task.succeeds
        Expect.equal result (Just "hello!")
          |> Expect.Task.check,
      Test.task "namespaces namespace" <| do
        let nsHandler1 = addNamespace "NS1" redisHandler
        let nsHandler2 = addNamespace "NS2" redisHandler
        set api "bob" "hello!" |> query nsHandler1 |> Expect.Task.succeeds
        set api "bob" "goodbye" |> query nsHandler2 |> Expect.Task.succeeds
        result1 <- get api "bob" |> query nsHandler1 |> Expect.Task.succeeds
        result2 <- get api "bob" |> query nsHandler2 |> Expect.Task.succeeds
        Expect.all
          [ \() -> Expect.notEqual result1 result2,
            \() -> Expect.equal (Just "hello!") result1,
            \() -> Expect.equal (Just "goodbye") result2
          ]
          ()
          |> Expect.Task.check,
      Test.task "getset" <| do
        set api "getset" "1" |> query testNS |> Expect.Task.succeeds
        result1 <- getset api "getset" "2" |> query testNS |> Expect.Task.succeeds
        result2 <- get api "getset" |> query testNS |> Expect.Task.succeeds
        Expect.all
          [ \() -> Expect.equal (Just "1") result1,
            \() -> Expect.equal (Just "2") result2
          ]
          ()
          |> Expect.Task.check,
      Test.task "del dels" <| do
        set api "del" "mistake..." |> query testNS |> Expect.Task.succeeds
        _ <- del api ("del" :| []) |> query testNS |> Expect.Task.succeeds
        result <- get api "del" |> query testNS |> Expect.Task.succeeds
        Expect.equal Nothing result |> Expect.Task.check,
      Test.task "del counts" <| do
        set api "delCount" "A thing" |> query testNS |> Expect.Task.succeeds
        result <- del api ("delCount" :| ["key that doesn't exist"]) |> query testNS |> Expect.Task.succeeds
        Expect.equal 1 result |> Expect.Task.check,
      Test.task "json roundtrip" <| do
        let testData :: [Int] = [1, 2, 3]
        set jsonApi' "JSON list" testData |> query testNS |> Expect.Task.succeeds
        result <- get jsonApi' "JSON list" |> query testNS |> Expect.Task.succeeds
        Expect.equal (Just testData) result |> Expect.Task.check,
      Test.task "atomic modify with no value" <| do
        _ <- del api ("Empty Atom" :| []) |> query testNS |> Expect.Task.succeeds
        result <-
          atomicModify
            (experimental api)
            testNS
            "Empty Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
            |> Expect.Task.succeeds
        Expect.equal "Nothing" result |> Expect.Task.check,
      Test.task "mget retrieves a mapping of the requested keys and their corresponding values" <| do
        set api "mgetTest::key1" "value 1" |> query testNS |> Expect.Task.succeeds
        set api "mgetTest::key3" "value 3" |> query testNS |> Expect.Task.succeeds
        result <-
          mget api ("mgetTest::key1" :| ["mgetTest::key2", "mgetTest::key3"]) |> query testNS
            |> Expect.Task.succeeds
        Expect.equal
          (Dict.toList result)
          [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")]
          |> Expect.Task.check,
      Test.task "mget json roundtrip" <| do
        set jsonApi' "Json.mgetTest::key1" ([1, 2] :: [Int]) |> query testNS |> Expect.Task.succeeds
        set jsonApi' "Json.mgetTest::key2" ([3, 4] :: [Int]) |> query testNS |> Expect.Task.succeeds
        result <-
          mget jsonApi' ("Json.mgetTest::key1" :| ["Json.mgetTest::key2"])
            |> query testNS
            |> Expect.Task.succeeds
        Expect.equal
          (Dict.toList result)
          [ ("Json.mgetTest::key1", [1, 2]),
            ("Json.mgetTest::key2", [3, 4])
          ]
          |> Expect.Task.check,
      Test.task "mset allows setting multiple values at once" <| do
        let firstKey = "msetTest::key1"
        let firstValue = "value 1"
        let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("msetTest::key2", "value 2")])
        let dict = NonEmptyDict.toDict nonEmptyDict
        mset api nonEmptyDict |> query testNS |> Expect.Task.succeeds
        result <- mget api (firstKey :| Dict.keys dict) |> query testNS |> Expect.Task.succeeds
        Expect.equal result dict |> Expect.Task.check,
      Test.task "Json.mset allows setting multiple JSON values at once" <| do
        let firstKey = "Json.msetTest::key1"
        let firstValue = [1, 2]
        let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("Json.msetTest::key2", [3, 4] :: [Int])])
        let dict = NonEmptyDict.toDict nonEmptyDict
        mset jsonApi' nonEmptyDict |> query testNS |> Expect.Task.succeeds
        result <- mget jsonApi' (firstKey :| Dict.keys dict) |> query testNS |> Expect.Task.succeeds
        Expect.equal result dict |> Expect.Task.check,
      Test.task "atomic modify with value" <| do
        _ <- del api ("Full Atom" :| []) |> query testNS |> Expect.Task.succeeds
        set api "Full Atom" "Something" |> query testNS |> Expect.Task.succeeds
        result <-
          atomicModify
            (experimental api)
            testNS
            "Full Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
            |> Expect.Task.succeeds
        Expect.equal "Prefix:Something" result |> Expect.Task.check,
      Test.task "atomicModifyWithContext works empty" <| do
        _ <- del api ("Atom With Context" :| []) |> query testNS |> Expect.Task.succeeds
        result <-
          atomicModifyWithContext
            (experimental api)
            testNS
            "Atom With Context"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
            |> Expect.Task.succeeds
        Expect.equal ("after", "Nothing") result |> Expect.Task.check,
      Test.task "atomicModifyWithContext works full" <| do
        set api "Atom With Context (full)" "A piece of text" |> query testNS |> Expect.Task.succeeds
        result <-
          atomicModifyWithContext
            (experimental api)
            testNS
            "Atom With Context (full)"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
            |> Expect.Task.succeeds
        Expect.equal ("after", "Just") result |> Expect.Task.check,
      Test.task "Json.atomicModifyWithContext works" <| do
        _ <- del api ("JSON Atom With Context" :| []) |> query testNS |> Expect.Task.succeeds
        result <-
          atomicModifyWithContext
            (experimental jsonApi')
            testNS
            "JSON Atom With Context"
            ( \v -> case v of
                Just _ -> ([10], Just ())
                Nothing -> ([10], Nothing)
            )
            |> Expect.Task.succeeds
        Expect.equal ([10], Nothing) result |> Expect.Task.check,
      Test.task "transaction preserves order" <| do
        [ Redis.List.del listApi ("order" :| []),
          Redis.List.rpush listApi "order" ("1" :| []),
          Redis.List.rpush listApi "order" ("2" :| []),
          Redis.List.rpush listApi "order" ("3" :| [])
          ]
          |> Redis.sequence
          |> map (\_ -> ())
          |> transaction testNS
          |> Expect.Task.succeeds
        result <- Redis.List.lrange listApi "order" 0 (-1) |> query testNS |> Expect.Task.succeeds
        Expect.equal result ["1", "2", "3"] |> Expect.Task.check
    ]
  where
    testNS = addNamespace "testNamespace" redisHandler

main :: IO ()
main =
  Conduit.withAcquire getHandlers <| \testHandlers ->
    Test.run <| buildSpecs testHandlers

newtype TestHandlers = TestHandlers
  { redisHandlers :: [(Text, Handler)]
  }

getRedisHandlers :: Settings.Settings -> Conduit.Acquire [(Text, Handler)]
getRedisHandlers settings = do
  let realHandler = Real.handler "tests" settings
  log <- Conduit.liftIO Platform.silentHandler
  mockHandler <- Conduit.liftIO <| Mock.handlerIO
  redisAvailable <-
    Conduit.withAcquire realHandler (\h -> query h (get api "foo") |> Task.attempt log)
      |> NriPrelude.map (\_ -> True)
      |> Exception.handleAny (\_ -> pure False)
      |> Conduit.liftIO
  if redisAvailable
    then NriPrelude.map2 (\real mock -> [("Real", real), ("Mock", mock)]) realHandler (pure mockHandler)
    else NriPrelude.map (\mock -> [("Mock", mock)]) (pure mockHandler)

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- getRedisHandlers settings
  pure (TestHandlers rh)

addNamespace :: Text -> Handler -> Handler
addNamespace namespace handler' =
  handler' {Internal.namespace = Internal.namespace handler' ++ ":" ++ namespace}

api :: Api Text Text
api = textApi identity

listApi :: Redis.List.Api Text Text
listApi = Redis.List.textApi identity

jsonApi' :: Api Text [Int]
jsonApi' = jsonApi identity
