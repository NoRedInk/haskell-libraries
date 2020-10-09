module Main (main) where

import qualified Conduit
import qualified Control.Concurrent.Async
import qualified Control.Exception.Safe as Exception
import qualified Database.Redis
import qualified Dict
import Dict (Dict)
import qualified Environment
import qualified Expect
import qualified List
import Nri.Prelude
import qualified Platform
import Redis (Error, NamespacedHandler, changeNamespace)
import qualified Redis.Internal as Internal
import qualified Redis.Json as Json
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import Redis.Text
import qualified Task
import Test
import qualified Test.Runner.Tasty
import qualified Tuple
import Prelude (IO, fst, pure, uncurry)

buildSpecs :: TestHandlers -> Test
buildSpecs TestHandlers {logHandler, redisHandlers} =
  redisHandlers
    |> map (uncurry (specs logHandler))
    |> describe "Redis Library"

specs :: Platform.LogHandler -> Text -> NamespacedHandler -> Test
specs logHandler whichHandler redisHandler =
  describe
    (whichHandler ++ " Redis")
    [ redisTest "get and set" <| do
        set testNS "bob" "hello!"
        result <- get testNS "bob"
        pure <| Expect.equal result (Just "hello!"),
      redisTest "namespaces namespace" <| do
        let nsHandler1 = changeNamespace "NS1" redisHandler
        let nsHandler2 = changeNamespace "NS2" redisHandler
        set nsHandler1 "bob" "hello!"
        set nsHandler2 "bob" "goodbye"
        result1 <- get nsHandler1 "bob"
        result2 <- get nsHandler2 "bob"
        pure
          <| Expect.all
            [ \() -> Expect.notEqual result1 result2,
              \() -> Expect.just (Expect.equal "hello!") result1,
              \() -> Expect.just (Expect.equal "goodbye") result2
            ]
            (),
      redisTest "getset" <| do
        set testNS "getset" "1"
        result1 <- getset testNS "getset" "2"
        result2 <- get testNS "getset"
        pure
          <| Expect.all
            [ \() -> Expect.just (Expect.equal "1") result1,
              \() -> Expect.just (Expect.equal "2") result2
            ]
            (),
      redisTest "del dels" <| do
        set testNS "del" "mistake..."
        _ <- del testNS ["del"]
        result <- get testNS "del"
        pure <| Expect.nothing result,
      redisTest "del counts" <| do
        set testNS "delCount" "A thing"
        result <- del testNS ["delCount", "key that doesn't exist"]
        pure <| Expect.equal 1 result,
      redisTest "json roundtrip" <| do
        let testData :: [Text] = ["one", "two", "three"]
        Json.set testNS "JSON list" testData
        result <- Json.get testNS "JSON list"
        pure <| Expect.just (Expect.equal testData) result,
      redisTest "atomic modify with no value" <| do
        _ <- del testNS ["Empty Atom"]
        result <-
          atomicModify
            testNS
            "Empty Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Nothing" result,
      redisTest "mget retrieves a mapping of the requested keys and their corresponding values" <| do
        set testNS "mgetTest::key1" "value 1"
        set testNS "mgetTest::key3" "value 3"
        result <- mget testNS ["mgetTest::key1", "mgetTest::key2", "mgetTest::key3"]
        pure
          ( Expect.equal
              (Dict.toList result)
              [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")]
          ),
      redisTest "mget json roundtrip" <| do
        Json.set testNS "Json.mgetTest::key1" ([1, 2] :: [Int])
        Json.set testNS "Json.mgetTest::key2" ([3, 4] :: [Int])
        result <- Json.mget testNS ["Json.mgetTest::key1", "Json.mgetTest::key2"] :: Task Error (Dict Text [Int])
        pure
          ( Expect.equal
              (Dict.toList result)
              [ ("Json.mgetTest::key1", [1, 2]),
                ("Json.mgetTest::key2", [3, 4])
              ]
          ),
      redisTest "mset allows setting multiple values at once" <| do
        let dict =
              Dict.fromList
                [ ("msetTest::key1", "value 1"),
                  ("msetTest::key2", "value 2")
                ]
        mset testNS dict
        result <- mget testNS (Dict.keys dict)
        pure (Expect.equal result dict),
      redisTest "Json.mset allows setting multiple JSON values at once" <| do
        let dict =
              Dict.fromList
                [ ("Json.msetTest::key1", [1, 2] :: [Int]),
                  ("Json.msetTest::key2", [3, 4] :: [Int])
                ]
        Json.mset testNS dict
        result <- Json.mget testNS (Dict.keys dict)
        pure (Expect.equal result dict),
      redisTest "atomic modify with value" <| do
        _ <- del testNS ["Full Atom"]
        set testNS "Full Atom" "Something"
        result <-
          atomicModify
            testNS
            "Full Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Prefix:Something" result,
      test "Concurrent atomicModify works" <| \() ->
        let ioTest = do
              _ <- del testNS ["Concurrent Atom"] |> Task.attempt logHandler
              let ops =
                    List.repeat
                      1000
                      ( Json.atomicModify
                          testNS
                          "Concurrent Atom"
                          ( \i -> case i of
                              Just i' -> i' + 1
                              Nothing -> 1 :: Int
                          )
                      )
              _ <- Control.Concurrent.Async.mapConcurrently (Task.attempt logHandler) ops
              Json.get testNS "Concurrent Atom" |> Task.attempt logHandler
         in Expect.withIO (Expect.ok <| Expect.just <| Expect.equal (1000 :: Int)) ioTest,
      redisTest "atomicModifyWithContext works empty" <| do
        _ <- del testNS ["Atom With Context"]
        result <-
          atomicModifyWithContext
            testNS
            "Atom With Context"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
        pure <| Expect.equal ("after", "Nothing") result,
      redisTest "atomicModifyWithContext works full" <| do
        set testNS "Atom With Context (full)" "A piece of text"
        result <-
          atomicModifyWithContext
            testNS
            "Atom With Context (full)"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
        pure <| Expect.equal ("after", "Just") result,
      redisTest "Json.atomicModifyWithContext works" <| do
        _ <- del testNS ["JSON Atom With Context"]
        result <-
          Json.atomicModifyWithContext
            testNS
            "JSON Atom With Context"
            ( \(v :: Maybe Int) -> case v of
                Just _ -> (10, Just ())
                Nothing -> (10, Nothing)
            )
        pure <| Expect.equal (10, Nothing) result
    ]
  where
    testNS = changeNamespace "testNamespace" redisHandler
    redisTest name test' =
      test name <| \() ->
        Task.attempt logHandler test'
          |> Expect.withIO (Expect.ok identity)

main :: IO ()
main =
  Conduit.withAcquire getHandlers <| \testHandlers ->
    Test.Runner.Tasty.main <| buildSpecs testHandlers

data TestHandlers
  = TestHandlers
      { logHandler :: Platform.LogHandler,
        redisHandlers :: [(Text, NamespacedHandler)]
      }

getRedisHandlers :: Settings.Settings -> Conduit.Acquire [(Text, NamespacedHandler)]
getRedisHandlers settings =
  Conduit.mkAcquire acquire release
    |> map fst
  where
    release (_, Just connection) = Database.Redis.disconnect (Real.connectionHedis connection)
    release (_, Nothing) = pure ()
    acquire =
      Exception.catchAny
        ( Real.acquireHandler settings
            |> map (Tuple.mapSecond Just)
            |> andThen
              ( \(h, c) -> do
                  h' <- Mock.handler "tests"
                  pure ([("Real", Internal.namespacedHandler "test" h), ("Mock", h')], c)
              )
        )
        ( \_ -> do
            h <- Mock.handler "tests"
            pure ([("Mock", h)], Nothing)
        )

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  lh <- Conduit.liftIO Platform.silentHandler
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- getRedisHandlers settings
  pure (TestHandlers lh rh)
