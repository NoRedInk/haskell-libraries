module Main (main) where

import Cherry.Prelude
import qualified Conduit
import qualified Environment
import qualified Expect
import qualified Platform
import Redis
import qualified Redis.Settings as Settings
import qualified Task
import Test
import qualified Test.Runner.Tasty
import Prelude (IO, pure)

specs :: TestHandlers -> Test
specs TestHandlers {logHandler, redisHandler} =
  describe
    "Redis"
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
main = do
  Conduit.withAcquire getHandlers <| \testHandlers -> do
    Test.Runner.Tasty.main <| specs testHandlers

data TestHandlers
  = TestHandlers
      { logHandler :: Platform.LogHandler,
        redisHandler :: Handler
      }

getLogHandler :: Conduit.Acquire Platform.LogHandler
getLogHandler =
  Platform.mkLogHandler "redis" (Platform.Environment "test") [] Platform.nullTracer

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  lh <- getLogHandler
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  rh <- handler settings
  pure (TestHandlers lh rh)
