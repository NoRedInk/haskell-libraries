module Main (main) where

import qualified Conduit
import qualified Control.Concurrent.MVar as MVar
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Debug
import qualified Dict
import qualified Environment
import qualified Expect
import qualified NonEmptyDict
import qualified Platform
import qualified Redis
import qualified Redis.Counter
import qualified Redis.Hash
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

-- put this at the top of the file so that adding tests doesn't push
-- the line number of the source location of this file down, which would
-- change golden test results
spanForTask :: Show e => Task e () -> Expect.Expectation' Platform.TracingSpan
spanForTask task =
  Expect.fromIO <| do
    spanVar <- MVar.newEmptyMVar
    res <-
      Platform.rootTracingSpanIO
        "test-request"
        (MVar.putMVar spanVar)
        "test-root"
        (\log -> Task.attempt log task)
    case res of
      Err err -> Prelude.fail <| Text.toList (Debug.toString err)
      Ok _ ->
        MVar.takeMVar spanVar
          |> map constantValuesForVariableFields

tests :: TestHandlers -> Test.Test
tests TestHandlers {realHandler, mockHandler} =
  Test.describe
    "Redis Library"
    [ Test.describe "query tests using mock handler" (queryTests mockHandler),
      Test.describe "query tests using real handler" (queryTests realHandler),
      Test.describe "observability tests" (observabilityTests realHandler)
    ]

-- We want to test all of our potential makeApi alternatives because it's easy
-- to break. Right now they all share code but if we change that, we would
-- break the observability usability without noticing.
--
-- All the `srcLocFile` fields in the golden result files should contain the
-- value "test/Main.hs". If it points to one of the src files of the redis
-- library it means stack frames for redis query in bugsnag, newrelic, etc will
-- not point to the application code making the query!
observabilityTests :: Redis.Handler -> List Test.Test
observabilityTests handler =
  [ Test.test "Redis.query reports the span data we expect" <| do
      Redis.query handler (Redis.ping api)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-query"),
    Test.test "Redis.transaction reports the span data we expect" <| do
      Redis.transaction handler (Redis.ping api)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-transaction"),
    Test.test "Redis.Hash.query reports the span data we expect" <| do
      Redis.Hash.query handler (Redis.Hash.ping hashApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-hash-query"),
    Test.test "Redis.Hash.transaction reports the span data we expect" <| do
      Redis.Hash.transaction handler (Redis.Hash.ping hashApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-hash-transaction"),
    Test.test "Redis.List.query reports the span data we expect" <| do
      Redis.List.query handler (Redis.List.ping listApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-list-query"),
    Test.test "Redis.List.transaction reports the span data we expect" <| do
      Redis.List.transaction handler (Redis.List.ping listApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-list-transaction"),
    Test.test "Redis.Counter.query reports the span data we expect" <| do
      Redis.Counter.query handler (Redis.Counter.ping counterApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-counter-query"),
    Test.test "Redis.Counter.transaction reports the span data we expect" <| do
      Redis.Counter.transaction handler (Redis.Counter.ping counterApi)
        |> Expect.succeeds
        |> spanForTask
        |> Expect.withIO (Debug.toString >> Expect.equalToContentsOf "test/golden-results/observability-spec-reporting-redis-counter-transaction")
  ]

queryTests :: Redis.Handler -> List Test.Test
queryTests redisHandler =
  [ Test.test "get and set" <| do
      Redis.set api "bob" "hello!" |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get api "bob" |> Redis.query testNS |> Expect.succeeds
      Expect.equal result (Just "hello!"),
    Test.test "namespaces namespace" <| do
      let nsHandler1 = addNamespace "NS1" redisHandler
      let nsHandler2 = addNamespace "NS2" redisHandler
      Redis.set api "bob" "hello!" |> Redis.query nsHandler1 |> Expect.succeeds
      Redis.set api "bob" "goodbye" |> Redis.query nsHandler2 |> Expect.succeeds
      result1 <- Redis.get api "bob" |> Redis.query nsHandler1 |> Expect.succeeds
      result2 <- Redis.get api "bob" |> Redis.query nsHandler2 |> Expect.succeeds
      Expect.all
        [ \() -> Expect.notEqual result1 result2,
          \() -> Expect.equal (Just "hello!") result1,
          \() -> Expect.equal (Just "goodbye") result2
        ]
        (),
    Test.test "getset" <| do
      Redis.set api "getset" "1" |> Redis.query testNS |> Expect.succeeds
      result1 <- Redis.getset api "getset" "2" |> Redis.query testNS |> Expect.succeeds
      result2 <- Redis.get api "getset" |> Redis.query testNS |> Expect.succeeds
      Expect.all
        [ \() -> Expect.equal (Just "1") result1,
          \() -> Expect.equal (Just "2") result2
        ]
        (),
    Test.test "del dels" <| do
      Redis.set api "del" "mistake..." |> Redis.query testNS |> Expect.succeeds
      _ <- Redis.del api ("del" :| []) |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get api "del" |> Redis.query testNS |> Expect.succeeds
      Expect.equal Nothing result,
    Test.test "del counts" <| do
      Redis.set api "delCount" "A thing" |> Redis.query testNS |> Expect.succeeds
      result <- Redis.del api ("delCount" :| ["key that doesn't exist"]) |> Redis.query testNS |> Expect.succeeds
      Expect.equal 1 result,
    Test.test "json roundtrip" <| do
      let testData :: [Int] = [1, 2, 3]
      Redis.set jsonApi' "JSON list" testData |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get jsonApi' "JSON list" |> Redis.query testNS |> Expect.succeeds
      Expect.equal (Just testData) result,
    Test.test "atomic modify with no value" <| do
      _ <- Redis.del api ("Empty Atom" :| []) |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.atomicModify
          (Redis.experimental api)
          testNS
          "Empty Atom"
          ( \v -> case v of
              Just v' -> "Prefix:" ++ v'
              Nothing -> "Nothing"
          )
          |> Expect.succeeds
      Expect.equal "Nothing" result,
    Test.test "mget retrieves a mapping of the requested keys and their corresponding values" <| do
      Redis.set api "mgetTest::key1" "value 1" |> Redis.query testNS |> Expect.succeeds
      Redis.set api "mgetTest::key3" "value 3" |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.mget api ("mgetTest::key1" :| ["mgetTest::key2", "mgetTest::key3"]) |> Redis.query testNS
          |> Expect.succeeds
      Expect.equal
        (Dict.toList result)
        [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")],
    Test.test "mget json roundtrip" <| do
      Redis.set jsonApi' "Json.mgetTest::key1" ([1, 2] :: [Int]) |> Redis.query testNS |> Expect.succeeds
      Redis.set jsonApi' "Json.mgetTest::key2" ([3, 4] :: [Int]) |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.mget jsonApi' ("Json.mgetTest::key1" :| ["Json.mgetTest::key2"])
          |> Redis.query testNS
          |> Expect.succeeds
      Expect.equal
        (Dict.toList result)
        [ ("Json.mgetTest::key1", [1, 2]),
          ("Json.mgetTest::key2", [3, 4])
        ],
    Test.test "mset allows setting multiple values at once" <| do
      let firstKey = "msetTest::key1"
      let firstValue = "value 1"
      let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("msetTest::key2", "value 2")])
      let dict = NonEmptyDict.toDict nonEmptyDict
      Redis.mset api nonEmptyDict |> Redis.query testNS |> Expect.succeeds
      result <- Redis.mget api (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.succeeds
      Expect.equal result dict,
    Test.test "Json.mset allows setting multiple JSON values at once" <| do
      let firstKey = "Json.msetTest::key1"
      let firstValue = [1, 2]
      let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("Json.msetTest::key2", [3, 4] :: [Int])])
      let dict = NonEmptyDict.toDict nonEmptyDict
      Redis.mset jsonApi' nonEmptyDict |> Redis.query testNS |> Expect.succeeds
      result <- Redis.mget jsonApi' (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.succeeds
      Expect.equal result dict,
    Test.test "atomic modify with value" <| do
      _ <- Redis.del api ("Full Atom" :| []) |> Redis.query testNS |> Expect.succeeds
      Redis.set api "Full Atom" "Something" |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.atomicModify
          (Redis.experimental api)
          testNS
          "Full Atom"
          ( \v -> case v of
              Just v' -> "Prefix:" ++ v'
              Nothing -> "Nothing"
          )
          |> Expect.succeeds
      Expect.equal "Prefix:Something" result,
    Test.test "atomicModifyWithContext works empty" <| do
      _ <- Redis.del api ("Atom With Context" :| []) |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental api)
          testNS
          "Atom With Context"
          ( \v -> case v of
              Just _ -> ("after", "Just" :: Text)
              Nothing -> ("after", "Nothing")
          )
          |> Expect.succeeds
      Expect.equal ("after", "Nothing") result,
    Test.test "atomicModifyWithContext works full" <| do
      Redis.set api "Atom With Context (full)" "A piece of text" |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental api)
          testNS
          "Atom With Context (full)"
          ( \v -> case v of
              Just _ -> ("after", "Just" :: Text)
              Nothing -> ("after", "Nothing")
          )
          |> Expect.succeeds
      Expect.equal ("after", "Just") result,
    Test.test "Json.atomicModifyWithContext works" <| do
      _ <- Redis.del api ("JSON Atom With Context" :| []) |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.atomicModifyWithContext
          (Redis.experimental jsonApi')
          testNS
          "JSON Atom With Context"
          ( \v -> case v of
              Just _ -> ([10], Just ())
              Nothing -> ([10], Nothing)
          )
          |> Expect.succeeds
      Expect.equal ([10], Nothing) result,
    Test.test "transaction preserves order" <| do
      [ Redis.List.del listApi ("order" :| []),
        Redis.List.rpush listApi "order" ("1" :| []),
        Redis.List.rpush listApi "order" ("2" :| []),
        Redis.List.rpush listApi "order" ("3" :| [])
        ]
        |> Redis.sequence
        |> map (\_ -> ())
        |> Redis.transaction testNS
        |> Expect.succeeds
      result <- Redis.List.lrange listApi "order" 0 (-1) |> Redis.query testNS |> Expect.succeeds
      Expect.equal result ["1", "2", "3"],
    Test.test "sequence is happy doing nothing"
      <| ( [ Redis.sequence [] |> Redis.transaction testNS,
             Redis.sequence [] |> Redis.query testNS
           ]
             |> Task.sequence
             |> Expect.succeeds
         ),
    Test.test "hmset inserts at least one field" <| do
      Redis.Hash.hmset hashApi "hmset-insert-test" (NonEmptyDict.init "field" "val" Dict.empty)
        |> Redis.query redisHandler
        |> Expect.succeeds
      Redis.Hash.hget hashApi "hmset-insert-test" "field"
        |> Redis.query redisHandler
        |> Task.map (Expect.equal (Just "val"))
        |> Expect.succeeds
        |> Task.andThen Expect.check,
    Test.test "hmset overwrites at least existing field" <| do
      Redis.Hash.hset hashApi "hmset-overwrite-test" "field" "old-val"
        |> Redis.query redisHandler
        |> Expect.succeeds
      Redis.Hash.hmset hashApi "hmset-overwrite-test" (NonEmptyDict.init "field" "val" Dict.empty)
        |> Redis.query redisHandler
        |> Expect.succeeds
      Redis.Hash.hget hashApi "hmset-overwrite-test" "field"
        |> Redis.query redisHandler
        |> Task.map (Expect.equal (Just "val"))
        |> Expect.succeeds
        |> Task.andThen Expect.check,
    Test.test "hmset inserts multiple fields" <| do
      NonEmptyDict.init
        "field"
        "val"
        (Dict.fromList [("field2", "val2")])
        |> Redis.Hash.hmset hashApi "hmset-insert-multiple-test"
        |> Redis.query redisHandler
        |> Expect.succeeds
      Redis.Hash.hget hashApi "hmset-insert-multiple-test" "field"
        |> Redis.query redisHandler
        |> Task.map (Expect.equal (Just "val"))
        |> Expect.succeeds
        |> Task.andThen Expect.check
      Redis.Hash.hget hashApi "hmset-insert-multiple-test" "field2"
        |> Redis.query redisHandler
        |> Task.map (Expect.equal (Just "val2"))
        |> Expect.succeeds
        |> Task.andThen Expect.check,
    Test.test "lock works without errors"
      <| let lock =
               Redis.Lock
                 { Redis.lockKey = "test-lock-key",
                   Redis.lockTimeoutInMs = 100,
                   Redis.lockMaxTries = 1,
                   Redis.lockHandleError = Task.fail
                 }
          in Redis.lock redisHandler lock (Task.succeed ())
               |> Expect.succeeds
  ]
  where
    testNS = addNamespace "testNamespace" redisHandler

data TestHandlers = TestHandlers
  { mockHandler :: Redis.Handler,
    realHandler :: Redis.Handler
  }

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  let realHandler = Real.handler "tests" settings {Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 1}
  mockHandler <- Conduit.liftIO <| Mock.handlerIO
  NriPrelude.map (TestHandlers mockHandler) realHandler

addNamespace :: Text -> Redis.Handler -> Redis.Handler
addNamespace namespace handler' =
  handler' {Internal.namespace = Internal.namespace handler' ++ ":" ++ namespace}

api :: Redis.Api Text Text
api = Redis.textApi identity

hashApi :: Redis.Hash.Api Text Text Text
hashApi = Redis.Hash.textApi identity identity Just

listApi :: Redis.List.Api Text Text
listApi = Redis.List.textApi identity

counterApi :: Redis.Counter.Api Text
counterApi = Redis.Counter.makeApi identity

jsonApi' :: Redis.Api Text [Int]
jsonApi' = Redis.jsonApi identity

-- | Timestamps recorded in spans would make each test result different from the
-- last. This helper sets all timestamps to zero to prevent this.
--
-- Similarly the db URI changes in each test, because we create temporary test
-- database. To prevent this from failing tests we set the URI to a standard
-- value.
constantValuesForVariableFields :: Platform.TracingSpan -> Platform.TracingSpan
constantValuesForVariableFields span =
  span
    { Platform.started = 0,
      Platform.finished = 0,
      Platform.allocated = 0,
      Platform.children = map constantValuesForVariableFields (Platform.children span)
    }
