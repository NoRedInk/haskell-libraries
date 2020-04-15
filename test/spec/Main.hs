module Main (main) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Monad.Catch
import qualified Database.Redis
import qualified Environment
import qualified Expect
import qualified Platform
import Redis
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task
import Test
import qualified Test.Runner.Tasty
import qualified Tuple
import Prelude (IO, fst, pure)

buildSpecs :: TestHandlers -> Test
buildSpecs TestHandlers {logHandler, redisHandlers} =
  redisHandlers
    |> map (\(whichHandler, redisHandler) -> specs logHandler whichHandler redisHandler)
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
        pure <| Expect.just (Expect.equal testData) result
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
