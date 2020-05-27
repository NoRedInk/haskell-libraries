module Main (main) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Concurrent.Async
import qualified Control.Monad.Catch
import qualified Database.Redis
import qualified Dict
import Dict (Dict)
import qualified Environment
import qualified Expect
import qualified List
import qualified Platform
import Redis
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
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

specs :: Platform.LogHandler -> Text -> Handler -> Test
specs logHandler whichHandler redisHandler =
  describe
    (whichHandler ++ " Redis")
    [ redisTest "get and set" <| do
        set testNS "bob" "hello!"
        result <- get testNS "bob"
        pure <| Expect.equal result (Just "hello!"),
      redisTest "namespaces namespace" <| do
        let nsHandler1 = namespacedHandler redisHandler "NS1"
        let nsHandler2 = namespacedHandler redisHandler "NS2"
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
        result1 <- getSet testNS "getset" "2"
        result2 <- get testNS "getset"
        pure
          <| Expect.all
            [ \() -> Expect.just (Expect.equal "1") result1,
              \() -> Expect.just (Expect.equal "2") result2
            ]
            (),
      redisTest "delete deletes" <| do
        set testNS "delete" "mistake..."
        _ <- delete testNS ["delete"]
        result <- get testNS "delete"
        pure <| Expect.nothing result,
      redisTest "delete counts" <| do
        set testNS "deleteCount" "A thing"
        result <- delete testNS ["deleteCount", "key that doesn't exist"]
        pure <| Expect.equal 1 result,
      redisTest "json roundtrip" <| do
        let testData :: [Text] = ["one", "two", "three"]
        setJSON testNS "JSON list" testData
        result <- getJSON testNS "JSON list"
        pure <| Expect.just (Expect.equal testData) result,
      redisTest "atomic modify with no value" <| do
        _ <- delete testNS ["Empty Atom"]
        result <-
          atomicModify
            testNS
            "Empty Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Nothing" result,
      redisTest "getMany retrieves a mapping of the requested keys and their corresponding values" <| do
        set testNS "key 1" "value 1"
        set testNS "key 3" "value 3"
        result <- getMany testNS ["key 1", "key 2", "key 3"]
        pure
          ( Expect.equal
              (Dict.toList result)
              [("key 1", "value 1"), ("key 3", "value 3")]
          ),
      redisTest "getMany json roundtrip" <| do
        setJSON testNS "JSON key 1" ([1, 2] :: [Int])
        setJSON testNS "JSON key 2" ([3, 4] :: [Int])
        result <- getManyJSON testNS ["JSON key 1", "JSON key 2"] :: Task Error (Dict Text [Int])
        pure
          ( Expect.equal
              (Dict.toList result)
              [ ("JSON key 1", [1, 2]),
                ("JSON key 2", [3, 4])
              ]
          ),
      redisTest "atomic modify with value" <| do
        _ <- delete testNS ["Full Atom"]
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
              _ <- delete testNS ["Concurrent Atom"] |> Task.attempt logHandler
              let ops =
                    List.repeat
                      1000
                      ( atomicModifyJSON
                          testNS
                          "Concurrent Atom"
                          ( \i -> case i of
                              Just i' -> i' + 1
                              Nothing -> 1 :: Int
                          )
                      )
              _ <- Control.Concurrent.Async.mapConcurrently (Task.attempt logHandler) ops
              getJSON testNS "Concurrent Atom" |> Task.attempt logHandler
         in Expect.withIO (Expect.ok <| Expect.just <| Expect.equal (1000 :: Int)) ioTest,
      redisTest "atomicModifyWithContext works empty" <| do
        _ <- delete testNS ["Atom With Context"]
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
      redisTest "atomicModifyWithContextJSON works" <| do
        _ <- delete testNS ["JSON Atom With Context"]
        result <-
          atomicModifyWithContextJSON
            testNS
            "JSON Atom With Context"
            ( \(v :: Maybe Int) -> case v of
                Just _ -> (10, Just ())
                Nothing -> (10, Nothing)
            )
        pure <| Expect.equal (10, Nothing) result
    ]
  where
    testNS = namespacedHandler redisHandler "TestNamespace"
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
        redisHandlers :: [(Text, Handler)]
      }

getLogHandler :: Conduit.Acquire Platform.LogHandler
getLogHandler =
  Platform.mkLogHandler "redis" (Platform.Environment "test") [] Platform.nullTracer

getRedisHandlers :: Settings.Settings -> Conduit.Acquire [(Text, Handler)]
getRedisHandlers settings =
  Conduit.mkAcquire acquire release
    |> map fst
  where
    release (_, Just conn) = Database.Redis.disconnect conn
    release (_, Nothing) = pure ()
    acquire =
      Control.Monad.Catch.catchAll
        ( Real.acquireHandler settings
            |> map (Tuple.mapSecond Just)
            |> andThen
              ( \(h, c) -> do
                  h' <- Mock.handler
                  pure ([("Real", h), ("Mock", h')], c)
              )
        )
        ( \_ -> do
            h <- Mock.handler
            pure ([("Mock", h)], Nothing)
        )

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  lh <- getLogHandler
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- getRedisHandlers settings
  pure (TestHandlers lh rh)
