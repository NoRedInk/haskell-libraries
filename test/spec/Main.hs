module Main (main) where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Dict
import qualified Environment
import qualified Expect
import qualified Expect.Task
import qualified NonEmptyDict
import NriPrelude
import qualified Platform
import qualified Redis
import qualified Redis.Internal as Internal
import qualified Redis.List
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Conduit.withAcquire getHandlers (Test.run << tests)

tests :: TestHandlers -> Test.Test
tests TestHandlers {realHandler, mockHandler} =
  Test.describe
    "Redis Library"
    [ Test.describe "using mock handler" (queryTests mockHandler),
      Test.describe
        "using real handler"
        ( case realHandler of
            Nothing -> [] -- No real redis running, so we skip these tests
            Just real -> queryTests real
        )
    ]

queryTests :: Redis.Handler -> List Test.Test
queryTests redisHandler =
  [ Test.task "get and set" <| do
      Redis.set api "bob" "hello!" |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.get api "bob" |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal result (Just "hello!")
        |> Expect.Task.check,
    Test.task "namespaces namespace" <| do
      let nsHandler1 = addNamespace "NS1" redisHandler
      let nsHandler2 = addNamespace "NS2" redisHandler
      Redis.set api "bob" "hello!" |> Redis.query nsHandler1 |> Expect.Task.succeeds
      Redis.set api "bob" "goodbye" |> Redis.query nsHandler2 |> Expect.Task.succeeds
      result1 <- Redis.get api "bob" |> Redis.query nsHandler1 |> Expect.Task.succeeds
      result2 <- Redis.get api "bob" |> Redis.query nsHandler2 |> Expect.Task.succeeds
      Expect.all
        [ \() -> Expect.notEqual result1 result2,
          \() -> Expect.equal (Just "hello!") result1,
          \() -> Expect.equal (Just "goodbye") result2
        ]
        ()
        |> Expect.Task.check,
    Test.task "getset" <| do
      Redis.set api "getset" "1" |> Redis.query testNS |> Expect.Task.succeeds
      result1 <- Redis.getset api "getset" "2" |> Redis.query testNS |> Expect.Task.succeeds
      result2 <- Redis.get api "getset" |> Redis.query testNS |> Expect.Task.succeeds
      Expect.all
        [ \() -> Expect.equal (Just "1") result1,
          \() -> Expect.equal (Just "2") result2
        ]
        ()
        |> Expect.Task.check,
    Test.task "del dels" <| do
      Redis.set api "del" "mistake..." |> Redis.query testNS |> Expect.Task.succeeds
      _ <- Redis.del api ("del" :| []) |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.get api "del" |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal Nothing result |> Expect.Task.check,
    Test.task "del counts" <| do
      Redis.set api "delCount" "A thing" |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.del api ("delCount" :| ["key that doesn't exist"]) |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal 1 result |> Expect.Task.check,
    Test.task "json roundtrip" <| do
      let testData :: [Int] = [1, 2, 3]
      Redis.set jsonApi' "JSON list" testData |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.get jsonApi' "JSON list" |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal (Just testData) result |> Expect.Task.check,
    Test.task "atomic modify with no value" <| do
      _ <- Redis.del api ("Empty Atom" :| []) |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.atomicModify
          (Redis.experimental api)
          testNS
          "Empty Atom"
          ( \v -> case v of
              Just v' -> "Prefix:" ++ v'
              Nothing -> "Nothing"
          )
          |> Expect.Task.succeeds
      Expect.equal "Nothing" result |> Expect.Task.check,
    Test.task "mget retrieves a mapping of the requested keys and their corresponding values" <| do
      Redis.set api "mgetTest::key1" "value 1" |> Redis.query testNS |> Expect.Task.succeeds
      Redis.set api "mgetTest::key3" "value 3" |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.mget api ("mgetTest::key1" :| ["mgetTest::key2", "mgetTest::key3"]) |> Redis.query testNS
          |> Expect.Task.succeeds
      Expect.equal
        (Dict.toList result)
        [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")]
        |> Expect.Task.check,
    Test.task "mget json roundtrip" <| do
      Redis.set jsonApi' "Json.mgetTest::key1" ([1, 2] :: [Int]) |> Redis.query testNS |> Expect.Task.succeeds
      Redis.set jsonApi' "Json.mgetTest::key2" ([3, 4] :: [Int]) |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.mget jsonApi' ("Json.mgetTest::key1" :| ["Json.mgetTest::key2"])
          |> Redis.query testNS
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
      Redis.mset api nonEmptyDict |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.mget api (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal result dict |> Expect.Task.check,
    Test.task "Json.mset allows setting multiple JSON values at once" <| do
      let firstKey = "Json.msetTest::key1"
      let firstValue = [1, 2]
      let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("Json.msetTest::key2", [3, 4] :: [Int])])
      let dict = NonEmptyDict.toDict nonEmptyDict
      Redis.mset jsonApi' nonEmptyDict |> Redis.query testNS |> Expect.Task.succeeds
      result <- Redis.mget jsonApi' (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal result dict |> Expect.Task.check,
    Test.task "atomic modify with value" <| do
      _ <- Redis.del api ("Full Atom" :| []) |> Redis.query testNS |> Expect.Task.succeeds
      Redis.set api "Full Atom" "Something" |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.atomicModify
          (Redis.experimental api)
          testNS
          "Full Atom"
          ( \v -> case v of
              Just v' -> "Prefix:" ++ v'
              Nothing -> "Nothing"
          )
          |> Expect.Task.succeeds
      Expect.equal "Prefix:Something" result |> Expect.Task.check,
    Test.task "atomicModifyWithContext works empty" <| do
      _ <- Redis.del api ("Atom With Context" :| []) |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental api)
          testNS
          "Atom With Context"
          ( \v -> case v of
              Just _ -> ("after", "Just" :: Text)
              Nothing -> ("after", "Nothing")
          )
          |> Expect.Task.succeeds
      Expect.equal ("after", "Nothing") result |> Expect.Task.check,
    Test.task "atomicModifyWithContext works full" <| do
      Redis.set api "Atom With Context (full)" "A piece of text" |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental api)
          testNS
          "Atom With Context (full)"
          ( \v -> case v of
              Just _ -> ("after", "Just" :: Text)
              Nothing -> ("after", "Nothing")
          )
          |> Expect.Task.succeeds
      Expect.equal ("after", "Just") result |> Expect.Task.check,
    Test.task "Json.atomicModifyWithContext works" <| do
      _ <- Redis.del api ("JSON Atom With Context" :| []) |> Redis.query testNS |> Expect.Task.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental jsonApi')
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
        |> Redis.transaction testNS
        |> Expect.Task.succeeds
      result <- Redis.List.lrange listApi "order" 0 (-1) |> Redis.query testNS |> Expect.Task.succeeds
      Expect.equal result ["1", "2", "3"] |> Expect.Task.check
  ]
  where
    testNS = addNamespace "testNamespace" redisHandler

data TestHandlers = TestHandlers
  { mockHandler :: Redis.Handler,
    realHandler :: Maybe Redis.Handler
  }

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  let realHandler = Real.handler "tests" settings
  log <- Conduit.liftIO Platform.silentHandler
  mockHandler <- Conduit.liftIO <| Mock.handlerIO
  redisAvailable <-
    Conduit.withAcquire realHandler (\h -> Redis.query h (Redis.get api "foo") |> Task.attempt log)
      |> NriPrelude.map (\_ -> True)
      |> Exception.handleAny (\_ -> Prelude.pure False)
      |> Conduit.liftIO
  if redisAvailable
    then NriPrelude.map (TestHandlers mockHandler << Just) realHandler
    else Prelude.pure (TestHandlers mockHandler Nothing)

addNamespace :: Text -> Redis.Handler -> Redis.Handler
addNamespace namespace handler' =
  handler' {Internal.namespace = Internal.namespace handler' ++ ":" ++ namespace}

api :: Redis.Api Text Text
api = Redis.textApi identity

listApi :: Redis.List.Api Text Text
listApi = Redis.List.textApi identity

jsonApi' :: Redis.Api Text [Int]
jsonApi' = Redis.jsonApi identity
