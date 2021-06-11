module Main (main) where

import qualified Conduit
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Debug
import qualified Expect
import qualified Http
import qualified Http.Mock
import qualified Log.HttpRequest as HttpRequest
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Platform
import qualified Task
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test
tests =
  describe
    "Http"
    [ test "Given a request made using `get` when the response has a 200 status code the task return succeeds" <| \() ->
        withServer
          (constant "" Status.ok200)
          ( \http url -> do
              Http.get http url Http.expectWhatever
                |> Expect.succeeds
          ),
      test "Given a request made using `get` when the response has a 400 status code the task fails with a BadStatus error" <| \() ->
        withServer
          (constant "" Status.badRequest400)
          ( \http url -> do
              err <-
                Http.get http url Http.expectWhatever
                  |> Expect.fails
              err
                |> Expect.equal (Http.BadStatus 400)
          ),
      test "Given a request made using `get` that expects a JSON response when the response includes the right JSON it is returned decoded" <| \() ->
        withServer
          (constant "[1,2,3]" Status.ok200)
          ( \http url -> do
              response <-
                Http.get http url Http.expectJson
                  |> Expect.succeeds
              response
                |> Expect.equal [1, 2, 3 :: Int]
          ),
      test "Given a request made using `get` that expects a JSON response when the JSON in the responses can not be decoded we fail with a BadBody error" <| \() ->
        withServer
          (constant "12" Status.ok200)
          ( \http url -> do
              err <-
                Http.get http url (Http.expectJson :: Http.Expect Text)
                  |> Expect.fails
              err
                |> Expect.equal (Http.BadBody "Error in $: parsing Text failed, expected String, but encountered Number")
          ),
      test "When a request is made using `get` to an invalid URL we fail with a BadUrl error" <| \() ->
        withServer
          (constant "" Status.ok200)
          ( \http _ -> do
              err <-
                Http.get http "ceci n'est pas un URL" Http.expectWhatever
                  |> Expect.fails
              err
                |> Expect.equal (Http.BadUrl "Invalid URL")
          ),
      test "When a request is made using `get` with a json body the `Content-Type` header is set to `application/json`" <| \() -> do
        request <-
          expectRequest
            ( \http url ->
                Http.post http url (Http.jsonBody ()) Http.expectWhatever
            )
        request
          |> Wai.requestHeaders
          |> Data.List.lookup "content-type"
          |> Expect.equal (Just "application/json"),
      test "When a request is made using `get` with a json body the JSON is encoded correctly" <| \() -> do
        request <-
          expectRequest
            ( \http url ->
                Http.post http url (Http.jsonBody [1, 2, 3 :: Int]) Http.expectWhatever
            )
        body <- Expect.fromIO (Wai.strictRequestBody request)
        Expect.equal "[1,2,3]" body,
      test "When a request is made using `get` with a string body the `Content-Type` header is set to provided mime type" <| \() -> do
        request <-
          expectRequest
            ( \http url ->
                Http.post http url (Http.stringBody "element/fire" "WOOSH") Http.expectWhatever
            )
        request
          |> Wai.requestHeaders
          |> Data.List.lookup "content-type"
          |> Expect.equal (Just "element/fire"),
      test "Http requests report the span data we expect" <| \_ ->
        withServer
          (constant "" Status.ok200)
          ( \http url -> do
              span <-
                Http.get http url Http.expectWhatever
                  |> spanForTask
              Debug.toString span
                |> Expect.equalToContentsOf "test/golden-results/expected-http-span"
          ),
      test "Http.Mock.stub" <| \_ -> do
        urlsAccessed <-
          Http.Mock.stub
            [Http.Mock.mkStub (\req -> Task.succeed (Http.url req, "Response!" :: Text))]
            ( \http ->
                Expect.succeeds <| do
                  _ <- Http.get http "example.com/one" Http.expectText
                  _ <- Http.get http "example.com/two" Http.expectText
                  Task.succeed ()
            )
        urlsAccessed
          |> Expect.equal ["example.com/one", "example.com/two"]
    ]

-- # Wai applications to test against
-- WAI NOT?

-- | A web application that returns the same response to every request.
constant :: Data.ByteString.Lazy.ByteString -> Status.Status -> Wai.Application
constant body status _ respond =
  Wai.responseLBS status [] body
    |> respond

-- | Run a temporary web application to send requests to.
withServer ::
  Wai.Application ->
  (Http.Handler -> Text -> Expect.Expectation) ->
  Expect.Expectation
withServer app run = do
  log <- Expect.succeeds Platform.logHandler
  doAnything <- Expect.fromIO Platform.doAnythingHandler
  Expect.around
    ( \runTask ->
        withServerIO log app (\http host -> runTask (http, host))
          |> Platform.doAnything doAnything
    )
    (\(http, host) -> run http host)

-- | Run a temporary web application to send requests to.
withServerIO ::
  Platform.LogHandler ->
  Wai.Application ->
  (Http.Handler -> Text -> Task e a) ->
  Prelude.IO (Result e a)
withServerIO log app run = do
  Conduit.withAcquire Http.handler <| \http ->
    Warp.testWithApplication
      (Prelude.pure app)
      ( \port ->
          run http ("http://localhost:" ++ Text.fromInt (Prelude.fromIntegral port))
            |> Task.attempt log
      )

-- | Run a temporary web application that handles a single request, and then
-- immediately returns that request so you can run expectations against it.
--
-- Useful if you want to check properties of requests you send.
expectRequest :: Show e => (Http.Handler -> Text -> Task e a) -> Expect.Expectation' Wai.Request
expectRequest run = do
  let app req _respond = Exception.throwIO (FirstRequest req)
  log <- Expect.succeeds Platform.logHandler
  either <- Expect.fromIO <| Exception.try (withServerIO log app run)
  Expect.succeeds
    <| case either of
      Prelude.Left (FirstRequest req) -> Task.succeed req
      Prelude.Right (Ok _) -> Task.fail "Expected a request, but none was received."
      Prelude.Right (Err err) -> Task.fail (Debug.toString err)

newtype FirstRequest = FirstRequest Wai.Request deriving (Show)

instance Exception.Exception FirstRequest

spanForTask :: Show e => Task e () -> Expect.Expectation' Platform.TracingSpan
spanForTask task = do
  spanVar <- Expect.fromIO MVar.newEmptyMVar
  res <-
    Expect.fromIO <| do
      Platform.rootTracingSpanIO
        "test-request"
        (MVar.putMVar spanVar)
        "test-root"
        (\log -> Task.attempt log task)
  Expect.ok res
  span <- Expect.fromIO (MVar.takeMVar spanVar)
  constantValuesForVariableFields span
    |> Task.succeed
    |> Expect.succeeds

-- | Timestamps recorded in spans would make each test result different from the
-- last. This helper sets all timestamps to zero to prevent this.
--
-- Similarly the host URI changes in each test, because `warp` pickes a random
-- free port to run a test webserver on. To prevent this from failing tests we
-- set the URI to a standard value.
constantValuesForVariableFields :: Platform.TracingSpan -> Platform.TracingSpan
constantValuesForVariableFields span =
  span
    { Platform.started = 0,
      Platform.finished = 0,
      Platform.details =
        Platform.details span
          |> andThen
            ( \details ->
                Platform.renderTracingSpanDetails
                  [ Platform.Renderer
                      ( \(HttpRequest.Outgoing httpDetails) ->
                          httpDetails {HttpRequest.host = Just "mock-uri"}
                            |> HttpRequest.Outgoing
                            |> Platform.toTracingSpanDetails
                      )
                  ]
                  details
            ),
      Platform.allocated = 0,
      Platform.summary = Just "mock-uri",
      Platform.children = map constantValuesForVariableFields (Platform.children span)
    }
