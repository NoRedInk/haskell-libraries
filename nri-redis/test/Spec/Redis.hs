{-# LANGUAGE QuasiQuotes #-}

module Spec.Redis (tests) where

import qualified Control.Concurrent.MVar as MVar
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Dict
import qualified Expect
import Helpers
import qualified NonEmptyDict
import qualified Platform
import qualified Redis
import qualified Redis.Counter
import qualified Redis.Hash
import qualified Redis.Internal as Internal
import qualified Redis.List
import qualified Redis.SortedSet
import qualified Set
import qualified Task
import qualified Test
import qualified Prelude

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
tests TestHandlers {handler, autoExtendExpireHandler} =
  Test.describe
    "Redis Library"
    [ Test.describe "query tests using handler" (queryTests handler),
      Test.describe "query tests using auto extend expire handler" (queryTests autoExtendExpireHandler),
      Test.describe "observability tests" (observabilityTests handler)
    ]

-- We want to test all of our potential makeApi alternatives because it's easy
-- to break. Right now they all share code but if we change that, we would
-- break the observability usability without noticing.
--
-- All the `srcLocFile` fields in the golden result files should contain the
-- value "test/Main.hs". If it points to one of the src files of the redis
-- library it means stack frames for redis query in bugsnag, newrelic, etc will
-- not point to the application code making the query!
observabilityTests :: Redis.Handler' x -> List Test.Test
observabilityTests handler =
  [ Test.test "Redis.query reports the span data we expect" <| \() -> do
      span <-
        Redis.query handler (Redis.ping api)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-query"),
    Test.test "Redis.transaction reports the span data we expect" <| \() -> do
      span <-
        Redis.transaction handler (Redis.ping api)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-transaction"),
    Test.test "Redis.Hash.query reports the span data we expect" <| \() -> do
      span <-
        Redis.Hash.query handler (Redis.Hash.ping hashApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-hash-query"),
    Test.test "Redis.Hash.transaction reports the span data we expect" <| \() -> do
      span <-
        Redis.Hash.transaction handler (Redis.Hash.ping hashApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-hash-transaction"),
    Test.test "Redis.List.query reports the span data we expect" <| \() -> do
      span <-
        Redis.List.query handler (Redis.List.ping listApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-list-query"),
    Test.test "Redis.List.transaction reports the span data we expect" <| \() -> do
      span <-
        Redis.List.transaction handler (Redis.List.ping listApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-list-transaction"),
    Test.test "Redis.Counter.query reports the span data we expect" <| \() -> do
      span <-
        Redis.Counter.query handler (Redis.Counter.ping counterApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-counter-query"),
    Test.test "Redis.Counter.transaction reports the span data we expect" <| \() -> do
      span <-
        Redis.Counter.transaction handler (Redis.Counter.ping counterApi)
          |> spanForTask
      span
        |> Debug.toString
        |> Expect.equalToContentsOf (goldenResultsDir ++ "/observability-spec-reporting-redis-counter-transaction")
  ]

queryTests :: Redis.Handler' x -> List Test.Test
queryTests redisHandler =
  [ Test.test "get and set" <| \() -> do
      Redis.set api "bob" "hello!" |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get api "bob" |> Redis.query testNS |> Expect.succeeds
      Expect.equal result (Just "hello!"),
    Test.test "namespaces namespace" <| \() -> do
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
    Test.test "getset" <| \() -> do
      Redis.set api "getset" "1" |> Redis.query testNS |> Expect.succeeds
      result1 <- Redis.getset api "getset" "2" |> Redis.query testNS |> Expect.succeeds
      result2 <- Redis.get api "getset" |> Redis.query testNS |> Expect.succeeds
      Expect.all
        [ \() -> Expect.equal (Just "1") result1,
          \() -> Expect.equal (Just "2") result2
        ]
        (),
    Test.test "del dels" <| \() -> do
      Redis.set api "del" "mistake..." |> Redis.query testNS |> Expect.succeeds
      _ <- Redis.del api ("del" :| []) |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get api "del" |> Redis.query testNS |> Expect.succeeds
      Expect.equal Nothing result,
    Test.test "del counts" <| \() -> do
      Redis.set api "delCount" "A thing" |> Redis.query testNS |> Expect.succeeds
      result <- Redis.del api ("delCount" :| ["key that doesn't exist"]) |> Redis.query testNS |> Expect.succeeds
      Expect.equal 1 result,
    Test.test "json roundtrip" <| \() -> do
      let testData :: [Int] = [1, 2, 3]
      Redis.set jsonApi' "JSON list" testData |> Redis.query testNS |> Expect.succeeds
      result <- Redis.get jsonApi' "JSON list" |> Redis.query testNS |> Expect.succeeds
      Expect.equal (Just testData) result,
    Test.test "mget retrieves a mapping of the requested keys and their corresponding values" <| \() -> do
      Redis.set api "mgetTest::key1" "value 1" |> Redis.query testNS |> Expect.succeeds
      Redis.set api "mgetTest::key3" "value 3" |> Redis.query testNS |> Expect.succeeds
      result <-
        Redis.mget api ("mgetTest::key1" :| ["mgetTest::key2", "mgetTest::key3"])
          |> Redis.query testNS
          |> Expect.succeeds
      Expect.equal
        (Dict.toList result)
        [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")],
    Test.test "mget json roundtrip" <| \() -> do
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
    Test.test "mset allows setting multiple values at once" <| \() -> do
      let firstKey = "msetTest::key1"
      let firstValue = "value 1"
      let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("msetTest::key2", "value 2")])
      let dict = NonEmptyDict.toDict nonEmptyDict
      Redis.mset api nonEmptyDict |> Redis.query testNS |> Expect.succeeds
      result <- Redis.mget api (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.succeeds
      Expect.equal result dict,
    Test.test "Json.mset allows setting multiple JSON values at once" <| \() -> do
      let firstKey = "Json.msetTest::key1"
      let firstValue = [1, 2]
      let nonEmptyDict = NonEmptyDict.init firstKey firstValue (Dict.fromList [("Json.msetTest::key2", [3, 4] :: [Int])])
      let dict = NonEmptyDict.toDict nonEmptyDict
      Redis.mset jsonApi' nonEmptyDict |> Redis.query testNS |> Expect.succeeds
      result <- Redis.mget jsonApi' (firstKey :| Dict.keys dict) |> Redis.query testNS |> Expect.succeeds
      Expect.equal result dict,
    Test.test "transaction preserves order" <| \() -> do
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
    Test.test "sequence is happy doing nothing" <| \() -> do
      _ <-
        Redis.sequence []
          |> Redis.transaction testNS
          |> Expect.succeeds
      Redis.sequence []
        |> Redis.query testNS
        |> Expect.succeeds
        |> map (\_ -> ()),
    Test.test "hmset inserts at least one field" <| \() -> do
      Redis.Hash.hmset hashApi "hmset-insert-test" (NonEmptyDict.init "field" "val" Dict.empty)
        |> Redis.query redisHandler
        |> Expect.succeeds
      response <-
        Redis.Hash.hget hashApi "hmset-insert-test" "field"
          |> Redis.query redisHandler
          |> Expect.succeeds
      response
        |> Expect.equal (Just "val"),
    Test.test "hmset overwrites at least existing field" <| \() -> do
      Redis.Hash.hset hashApi "hmset-overwrite-test" "field" "old-val"
        |> Redis.query redisHandler
        |> Expect.succeeds
      Redis.Hash.hmset hashApi "hmset-overwrite-test" (NonEmptyDict.init "field" "val" Dict.empty)
        |> Redis.query redisHandler
        |> Expect.succeeds
      response <-
        Redis.Hash.hget hashApi "hmset-overwrite-test" "field"
          |> Redis.query redisHandler
          |> Expect.succeeds
      response
        |> Expect.equal (Just "val"),
    Test.test "hmset inserts multiple fields" <| \() -> do
      NonEmptyDict.init
        "field"
        "val"
        (Dict.fromList [("field2", "val2")])
        |> Redis.Hash.hmset hashApi "hmset-insert-multiple-test"
        |> Redis.query redisHandler
        |> Expect.succeeds
      field <-
        Redis.Hash.hget hashApi "hmset-insert-multiple-test" "field"
          |> Redis.query redisHandler
          |> Expect.succeeds
      field
        |> Expect.equal (Just "val")
      field2 <-
        Redis.Hash.hget hashApi "hmset-insert-multiple-test" "field2"
          |> Redis.query redisHandler
          |> Expect.succeeds
      field2
        |> Expect.equal (Just "val2"),
    Test.test "zadd returns count of stored values" <| \() -> do
      _ <-
        Redis.SortedSet.del sortedSetApi ("zadd-returns-count-of-stored-values" :| [])
          |> Redis.query redisHandler
          |> Expect.succeeds
      NonEmptyDict.init "foo" 1 Dict.empty
        |> Redis.SortedSet.zadd sortedSetApi "zadd-returns-count-of-stored-values"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal 1),
    Test.test "zrange works as expected" <| \() -> do
      _ <-
        Redis.SortedSet.del sortedSetApi ("zrange-works" :| [])
          |> Redis.query redisHandler
          |> Expect.succeeds
      _ <-
        NonEmptyDict.init "one" 1 (Dict.fromList [("two", 2), ("three", 3)])
          |> Redis.SortedSet.zadd sortedSetApi "zrange-works"
          |> Redis.query redisHandler
          |> Expect.succeeds
      Redis.SortedSet.zrange sortedSetApi "zrange-works" 0 (-1)
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal ["one", "two", "three"])
      Redis.SortedSet.zrange sortedSetApi "zrange-works" 2 3
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal ["three"])
      Redis.SortedSet.zrange sortedSetApi "zrange-works" (-2) (-1)
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal ["two", "three"]),
    Test.test "zrank works as expected" <| \() -> do
      _ <-
        Redis.SortedSet.del sortedSetApi ("zrank-works" :| [])
          |> Redis.query redisHandler
          |> Expect.succeeds
      _ <-
        NonEmptyDict.init "one" 1 (Dict.fromList [("two", 2), ("twobis", 2), ("three", 3)])
          |> Redis.SortedSet.zadd sortedSetApi "zrank-works"
          |> Redis.query redisHandler
          |> Expect.succeeds
      Redis.SortedSet.zrank sortedSetApi "zrank-works" "one"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal (Just 0))
      Redis.SortedSet.zrank sortedSetApi "zrank-works" "two"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal (Just 1))
      Redis.SortedSet.zrank sortedSetApi "zrank-works" "twobis"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal (Just 2))
      Redis.SortedSet.zrank sortedSetApi "zrank-works" "three"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal (Just 3))
      Redis.SortedSet.zrank sortedSetApi "zrank-works" "foobar"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal Nothing)
      Redis.SortedSet.zrank sortedSetApi "zrank-works-nothing-stored" "foobar"
        |> Redis.query redisHandler
        |> Expect.andCheck (Expect.equal Nothing),
    Test.test "scan iterates over all matching keys in batches" <| \() -> do
      let firstKey = "scanTest::key1"
      let firstValue = "value 1"
      let nonEmptyDict =
            NonEmptyDict.init firstKey firstValue
              <| Dict.fromList
                [ ("scanTest::key2", "value 2"),
                  ("scanTest::key3", "value 3"),
                  ("scanTest::key4", "value 4")
                ]
      let expectedKeys =
            NonEmptyDict.toDict nonEmptyDict
              |> Dict.keys
      Redis.mset api nonEmptyDict
        |> Redis.query testNS
        |> Expect.succeeds
      let processBatch = \batchKeys acc ->
            Task.succeed (List.foldl Set.insert acc batchKeys)
      keySet <-
        Redis.foldWithScan testNS (Just "scanTest::*") (Just 2) processBatch Set.empty
          |> Expect.succeeds
      keySet
        |> Set.toList
        |> Expect.equal expectedKeys,
    Test.test "scan works correctly when deleting keys" <| \() -> do
      let firstKey = "scanDeleteTest::key1"
      let firstValue = "value 1"
      let nonEmptyDict =
            NonEmptyDict.init firstKey firstValue
              <| Dict.fromList
                [ ("scanDeleteTest::key2", "value 2"),
                  ("scanDeleteTest::key3", "value 3"),
                  ("scanDeleteTest::key4", "value 4")
                ]
      let expectedKeys =
            NonEmptyDict.toDict nonEmptyDict
              |> Dict.keys
      Redis.mset api nonEmptyDict
        |> Redis.query testNS
        |> Expect.succeeds
      let processBatch = \batchKeys (accDeleted, accKeys) ->
            case batchKeys of
              [] -> Task.succeed (accDeleted, accKeys)
              first : rest -> do
                nDel <-
                  Redis.del api (first :| rest)
                    |> Redis.query testNS
                Task.succeed (accDeleted + nDel, List.foldl Set.insert accKeys batchKeys)
      (totalDeleted, keySet) <-
        Redis.foldWithScan testNS (Just "scanDeleteTest::*") (Just 2) processBatch (0, Set.empty)
          |> Expect.succeeds
      totalDeleted
        |> Expect.equal (List.length expectedKeys)
      keySet
        |> Set.toList
        |> Expect.equal expectedKeys,
    Test.test "eval runs and returns something" <| \() -> do
      let script = [Redis.script|return 1|]
      result <- Redis.eval intJsonApi script |> Redis.query testNS |> Expect.succeeds
      Expect.equal result 1,
    Test.test "eval with arguments runs and returns something" <| \() -> do
      let script =
            [Redis.script|
      local a = ${Redis.Key "hi"}
      local b = ${Redis.Literal "hello"}
      return 1|]
      result <- Redis.eval intJsonApi script |> Redis.query testNS |> Expect.succeeds
      Expect.equal result 1,
    Test.test "eval with arguments returns argument" <| \() -> do
      let script =
            [Redis.script|
      local a = ${Redis.Key 2}
      local b = ${Redis.Literal 3}
      return b|]
      result <- Redis.eval intJsonApi script |> Redis.query testNS |> Expect.succeeds
      Expect.equal result 3,
    Test.test "eval with arguments namespaces key" <| \() -> do
      let script = [Redis.script|return ${Redis.Key "hi"}|]
      result <- Redis.eval api script |> Redis.query testNS |> Expect.succeeds
      Expect.true
        ( List.member
            result
            -- All tests here run twice:
            -- - once with the auto-extend-expire handler
            -- - once with the normal handler
            -- each run generates a different namespace
            [ "tests-auto-extend-expire:testNamespace:hi",
              "tests:testNamespace:hi"
            ]
        )
  ]
  where
    testNS = addNamespace "testNamespace" redisHandler

addNamespace :: Text -> Redis.Handler' x -> Redis.Handler' x
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

sortedSetApi :: Redis.SortedSet.Api Text Text
sortedSetApi = Redis.SortedSet.textApi identity

jsonApi' :: Redis.Api Text [Int]
jsonApi' = Redis.jsonApi identity

intJsonApi :: Redis.Api Text Prelude.Integer
intJsonApi = Redis.jsonApi identity

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
