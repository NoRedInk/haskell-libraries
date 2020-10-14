module Main (main) where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Dict
import Dict (Dict)
import qualified Environment
import qualified Expect
import Nri.Prelude
import qualified Platform
import Redis (Error, Handler, addNamespace)
import qualified Redis.Json as Json
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import Redis.Text
import qualified Task
import Test
import qualified Test.Runner.Tasty
import Prelude (IO, pure, uncurry)

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
        set "bob" "hello!" |> query testNS
        result <- get "bob" |> query testNS
        pure <| Expect.equal result (Just "hello!"),
      redisTest "namespaces namespace" <| do
        let nsHandler1 = addNamespace "NS1" redisHandler
        let nsHandler2 = addNamespace "NS2" redisHandler
        set "bob" "hello!" |> query nsHandler1
        set "bob" "goodbye" |> query nsHandler2
        result1 <- get "bob" |> query nsHandler1
        result2 <- get "bob" |> query nsHandler2
        pure
          <| Expect.all
            [ \() -> Expect.notEqual result1 result2,
              \() -> Expect.just (Expect.equal "hello!") result1,
              \() -> Expect.just (Expect.equal "goodbye") result2
            ]
            (),
      redisTest "getset" <| do
        set "getset" "1" |> query testNS
        result1 <- getset "getset" "2" |> query testNS
        result2 <- get "getset" |> query testNS
        pure
          <| Expect.all
            [ \() -> Expect.just (Expect.equal "1") result1,
              \() -> Expect.just (Expect.equal "2") result2
            ]
            (),
      redisTest "del dels" <| do
        set "del" "mistake..." |> query testNS
        _ <- del ["del"] |> query testNS
        result <- get "del" |> query testNS
        pure <| Expect.nothing result,
      redisTest "del counts" <| do
        set "delCount" "A thing" |> query testNS
        result <- del ["delCount", "key that doesn't exist"] |> query testNS
        pure <| Expect.equal 1 result,
      redisTest "json roundtrip" <| do
        let testData :: [Text] = ["one", "two", "three"]
        Json.set "JSON list" testData |> query testNS
        result <- Json.get "JSON list" |> query testNS
        pure <| Expect.just (Expect.equal testData) result,
      redisTest "atomic modify with no value" <| do
        _ <- del ["Empty Atom"] |> query testNS
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
        set "mgetTest::key1" "value 1" |> query testNS
        set "mgetTest::key3" "value 3" |> query testNS
        result <- mget ["mgetTest::key1", "mgetTest::key2", "mgetTest::key3"] |> query testNS
        pure
          ( Expect.equal
              (Dict.toList result)
              [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")]
          ),
      redisTest "mget json roundtrip" <| do
        Json.set "Json.mgetTest::key1" ([1, 2] :: [Int]) |> query testNS
        Json.set "Json.mgetTest::key2" ([3, 4] :: [Int]) |> query testNS
        result <-
          Json.mget ["Json.mgetTest::key1", "Json.mgetTest::key2"] |> query testNS ::
            Task Error (Dict Text [Int])
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
        mset dict |> query testNS
        result <- mget (Dict.keys dict) |> query testNS
        pure (Expect.equal result dict),
      redisTest "Json.mset allows setting multiple JSON values at once" <| do
        let dict =
              Dict.fromList
                [ ("Json.msetTest::key1", [1, 2] :: [Int]),
                  ("Json.msetTest::key2", [3, 4] :: [Int])
                ]
        Json.mset dict |> query testNS
        result <- Json.mget (Dict.keys dict) |> query testNS
        pure (Expect.equal result dict),
      redisTest "atomic modify with value" <| do
        _ <- del ["Full Atom"] |> query testNS
        set "Full Atom" "Something" |> query testNS
        result <-
          atomicModify
            testNS
            "Full Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Prefix:Something" result,
      redisTest "atomicModifyWithContext works empty" <| do
        _ <- del ["Atom With Context"] |> query testNS
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
        set "Atom With Context (full)" "A piece of text" |> query testNS
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
        _ <- del ["JSON Atom With Context"] |> query testNS
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
    testNS = addNamespace "testNamespace" redisHandler
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

getRedisHandlers :: Settings.Settings -> Conduit.Acquire [(Text, Handler)]
getRedisHandlers settings = do
  let realHandler = Real.handler "tests" settings
  let mockHandler = Conduit.liftIO <| Mock.handler "tests"
  log <- Conduit.liftIO Platform.silentHandler
  redisAvailable <-
    Conduit.withAcquire realHandler (\h -> query h (get "foo") |> Task.attempt log)
      |> map (\_ -> True)
      |> Exception.handleAny (\_ -> pure False)
      |> Conduit.liftIO
  if redisAvailable
    then map2 (\real mock -> [("Real", real), ("Mock", mock)]) realHandler mockHandler
    else map (\mock -> [("Mock", mock)]) mockHandler

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  lh <- Conduit.liftIO Platform.silentHandler
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- getRedisHandlers settings
  pure (TestHandlers lh rh)
