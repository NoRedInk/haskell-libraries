module Main (main) where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Debug
import qualified Dict
import Dict (Dict)
import qualified Environment
import qualified Expect
import qualified List
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
import Test
import Prelude (IO, pure, uncurry)

buildSpecs :: TestHandlers -> Test
buildSpecs TestHandlers {logHandler, redisHandlers} =
  redisHandlers
    |> List.map (uncurry (specs logHandler))
    |> describe "Redis Library"

specs :: Platform.LogHandler -> Text -> Handler -> Test
specs logHandler whichHandler redisHandler =
  describe
    (whichHandler ++ " Redis")
    [ redisTest "get and set" <| do
        set api "bob" "hello!" |> query testNS
        result <- get api "bob" |> query testNS
        pure <| Expect.equal result (Just "hello!"),
      redisTest "namespaces namespace" <| do
        let nsHandler1 = addNamespace "NS1" redisHandler
        let nsHandler2 = addNamespace "NS2" redisHandler
        set api "bob" "hello!" |> query nsHandler1
        set api "bob" "goodbye" |> query nsHandler2
        result1 <- get api "bob" |> query nsHandler1
        result2 <- get api "bob" |> query nsHandler2
        pure
          <| Expect.all
            [ \() -> Expect.notEqual result1 result2,
              \() -> Expect.equal (Just "hello!") result1,
              \() -> Expect.equal (Just "goodbye") result2
            ]
            (),
      redisTest "getset" <| do
        set api "getset" "1" |> query testNS
        result1 <- getset api "getset" "2" |> query testNS
        result2 <- get api "getset" |> query testNS
        pure
          <| Expect.all
            [ \() -> Expect.equal (Just "1") result1,
              \() -> Expect.equal (Just "2") result2
            ]
            (),
      redisTest "del dels" <| do
        set api "del" "mistake..." |> query testNS
        _ <- del api ("del" :| []) |> query testNS
        result <- get api "del" |> query testNS
        pure <| Expect.equal Nothing result,
      redisTest "del counts" <| do
        set api "delCount" "A thing" |> query testNS
        result <- del api ("delCount" :| ["key that doesn't exist"]) |> query testNS
        pure <| Expect.equal 1 result,
      redisTest "json roundtrip" <| do
        let testData :: [Int] = [1, 2, 3]
        set jsonApi' "JSON list" testData |> query testNS
        result <- get jsonApi' "JSON list" |> query testNS
        pure <| Expect.equal (Just testData) result,
      redisTest "atomic modify with no value" <| do
        _ <- del api ("Empty Atom" :| []) |> query testNS
        result <-
          atomicModify
            (experimental api)
            testNS
            "Empty Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Nothing" result,
      redisTest "mget retrieves a mapping of the requested keys and their corresponding values" <| do
        set api "mgetTest::key1" "value 1" |> query testNS
        set api "mgetTest::key3" "value 3" |> query testNS
        result <- mget api ("mgetTest::key1" :| ["mgetTest::key2", "mgetTest::key3"]) |> query testNS
        pure
          ( Expect.equal
              (Dict.toList result)
              [("mgetTest::key1", "value 1"), ("mgetTest::key3", "value 3")]
          ),
      redisTest "mget json roundtrip" <| do
        set jsonApi' "Json.mgetTest::key1" ([1, 2] :: [Int]) |> query testNS
        set jsonApi' "Json.mgetTest::key2" ([3, 4] :: [Int]) |> query testNS
        result <-
          mget jsonApi' ("Json.mgetTest::key1" :| ["Json.mgetTest::key2"]) |> query testNS ::
            Task Error (Dict Text [Int])
        pure
          ( Expect.equal
              (Dict.toList result)
              [ ("Json.mgetTest::key1", [1, 2]),
                ("Json.mgetTest::key2", [3, 4])
              ]
          ),
      redisTest "mset allows setting multiple values at once" <| do
        let firstKey = "msetTest::key1"
        let firstValue = "value 1"
        let dict = Dict.fromList [(firstKey, firstValue), ("msetTest::key2", "value 2")]
        mset api firstKey firstValue dict |> query testNS
        result <- mget api (firstKey :| Dict.keys dict) |> query testNS
        pure (Expect.equal result dict),
      redisTest "Json.mset allows setting multiple JSON values at once" <| do
        let firstKey = "Json.msetTest::key1"
        let firstValue = [1, 2]
        let dict = Dict.fromList [(firstKey, firstValue), ("Json.msetTest::key2", [3, 4] :: [Int])]
        mset jsonApi' firstKey firstValue dict |> query testNS
        result <- mget jsonApi' (firstKey :| Dict.keys dict) |> query testNS
        pure (Expect.equal result dict),
      redisTest "atomic modify with value" <| do
        _ <- del api ("Full Atom" :| []) |> query testNS
        set api "Full Atom" "Something" |> query testNS
        result <-
          atomicModify
            (experimental api)
            testNS
            "Full Atom"
            ( \v -> case v of
                Just v' -> "Prefix:" ++ v'
                Nothing -> "Nothing"
            )
        pure <| Expect.equal "Prefix:Something" result,
      redisTest "atomicModifyWithContext works empty" <| do
        _ <- del api ("Atom With Context" :| []) |> query testNS
        result <-
          atomicModifyWithContext
            (experimental api)
            testNS
            "Atom With Context"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
        pure <| Expect.equal ("after", "Nothing") result,
      redisTest "atomicModifyWithContext works full" <| do
        set api "Atom With Context (full)" "A piece of text" |> query testNS
        result <-
          atomicModifyWithContext
            (experimental api)
            testNS
            "Atom With Context (full)"
            ( \v -> case v of
                Just _ -> ("after", "Just" :: Text)
                Nothing -> ("after", "Nothing")
            )
        pure <| Expect.equal ("after", "Just") result,
      redisTest "Json.atomicModifyWithContext works" <| do
        _ <- del api ("JSON Atom With Context" :| []) |> query testNS
        result <-
          atomicModifyWithContext
            (experimental jsonApi')
            testNS
            "JSON Atom With Context"
            ( \v -> case v of
                Just _ -> ([10], Just ())
                Nothing -> ([10], Nothing)
            )
        pure <| Expect.equal ([10], Nothing) result,
      redisTest "transaction preserves order" <| do
        [ Redis.List.del listApi ("order" :| []),
          Redis.List.rpush listApi "order" ["1"],
          Redis.List.rpush listApi "order" ["2"],
          Redis.List.rpush listApi "order" ["3"]
          ]
          |> Redis.sequence
          |> map (\_ -> ())
          |> transaction testNS
        result <- Redis.List.lrange listApi "order" 0 (-1) |> query testNS
        pure <| Expect.equal result ["1", "2", "3"]
    ]
  where
    testNS = addNamespace "testNamespace" redisHandler
    redisTest name test' =
      test name <| \() ->
        Task.attempt logHandler test'
          |> Expect.withIO
            ( \res ->
                case res of
                  Ok x -> x
                  Err err -> Expect.fail ("Expected Ok but got Err: " ++ Debug.toString err)
            )

main :: IO ()
main =
  Conduit.withAcquire getHandlers <| \testHandlers ->
    run <| buildSpecs testHandlers

data TestHandlers
  = TestHandlers
      { logHandler :: Platform.LogHandler,
        redisHandlers :: [(Text, Handler)]
      }

getRedisHandlers :: Settings.Settings -> Conduit.Acquire [(Text, Handler)]
getRedisHandlers settings = do
  let realHandler = Real.handler "tests" settings
  log <- Conduit.liftIO Platform.silentHandler
  mockHandlerMaker <- Conduit.liftIO <| Mock.mkHandler "tests"
  let mockHandler =
        Task.perform log mockHandlerMaker
          |> Conduit.liftIO
  redisAvailable <-
    Conduit.withAcquire realHandler (\h -> query h (get api "foo") |> Task.attempt log)
      |> NriPrelude.map (\_ -> True)
      |> Exception.handleAny (\_ -> pure False)
      |> Conduit.liftIO
  if redisAvailable
    then NriPrelude.map2 (\real mock -> [("Real", real), ("Mock", mock)]) realHandler mockHandler
    else NriPrelude.map (\mock -> [("Mock", mock)]) mockHandler

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  lh <- Conduit.liftIO Platform.silentHandler
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- getRedisHandlers settings
  pure (TestHandlers lh rh)

addNamespace :: Text -> Handler -> Handler
addNamespace namespace handler' =
  handler' {Internal.namespace = Internal.namespace handler' ++ ":" ++ namespace}

api :: Api Text Text
api = textApi identity

listApi :: Redis.List.Api Text Text
listApi = Redis.List.textApi identity

jsonApi' :: Api Text [Int]
jsonApi' = jsonApi identity
