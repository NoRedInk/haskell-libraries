module Main (main) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Text
import qualified Debug
import qualified Expect
import qualified Http
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Platform
import qualified Task
import Test (Test, describe, skip, test)
import qualified Test.Runner.Tasty
import qualified Prelude

main :: Prelude.IO ()
main = Test.Runner.Tasty.main tests

tests :: Test
tests =
  describe
    "Http"
    [ test "Given a request made using `get` when the response has a 200 status code the task return succeeds" <| \() ->
        withServer
          (constant "" Status.ok200)
          (\http url -> Http.get http url Http.expectWhatever)
          |> Expect.withIO Expect.ok,
      test "Given a request made using `get` when the response has a 400 status code the task fails with a BadStatus error" <| \() ->
        withServer
          (constant "" Status.badRequest400)
          (\http url -> Http.get http url Http.expectWhatever)
          |> Expect.withIO (Expect.equal (Err (Http.BadStatus 400))),
      test "Given a request made using `get` that expects a JSON response when the response includes the right JSON it is returned decoded" <| \() ->
        withServer
          (constant "[1,2,3]" Status.ok200)
          (\http url -> Http.get http url Http.expectJson)
          |> Expect.withIO (Expect.equal (Ok [1, 2, 3 :: Int])),
      test "Given a request made using `get` that expects a JSON response when the JSON in the responses can not be decoded we fail with a BadBody error" <| \() ->
        withServer
          (constant "12" Status.ok200)
          (\http url -> Http.get http url (Http.expectJson :: Http.Expect Text))
          |> Expect.withIO (Expect.equal (Err (Http.BadBody "Error in $: expected Text, encountered Number"))),
      test "When a request is made using `get` to an invalid URL we fail with a BadUrl error" <| \() ->
        withServer
          (constant "" Status.ok200)
          (\http _ -> Http.get http "ceci n'est pas un URL" Http.expectWhatever)
          |> Expect.withIO (Expect.equal (Err (Http.BadUrl "Invalid URL"))),
      test "When a request is made using `get` with a json body the `Content-Type` header is set to `application/json`" <| \() ->
        expectRequest (\http url -> Http.post http url (Http.jsonBody ()) Http.expectWhatever)
          |> map (map (Data.List.lookup "content-type" << Wai.requestHeaders))
          |> Expect.withIO (Expect.equal (Ok (Just "application/json"))),
      test "When a request is made using `get` with a json body the JSON is encoded correctly" <| \() ->
        expectRequest (\http url -> Http.post http url (Http.jsonBody [1, 2, 3 :: Int]) Http.expectWhatever)
          |> andThen
            ( \result ->
                case result of
                  Ok req -> map Ok (Wai.strictRequestBody req)
                  Err err -> Prelude.pure (Err err)
            )
          |> Expect.withIO (Expect.equal (Ok "[1,2,3]")),
      skip <| test "When a request is made using `get` with a string body the `Content-Type` header is set to provided mime type" <| \() ->
        expectRequest (\http url -> Http.post http url (Http.stringBody "element/fire" "WOOSH") Http.expectWhatever)
          |> map (map (Data.List.lookup "content-type" << Wai.requestHeaders))
          |> Expect.withIO (Expect.equal (Ok (Just "element/fire")))
    ]

-- # Wai applications to test against
-- WAI NOT?

-- | A web application that returns the same response to every request.
constant :: Data.ByteString.Lazy.ByteString -> Status.Status -> Wai.Application
constant body status _ respond =
  Wai.responseLBS status [] body
    |> respond

-- | Run a temporary web application to send requests to.
withServer :: Wai.Application -> (Http.Handler -> Text -> Task e a) -> Prelude.IO (Result e a)
withServer app run = do
  log <- Platform.silentContext
  Conduit.withAcquire Http.handler <| \http ->
    Warp.testWithApplication (Prelude.pure app) <| \port ->
      run http ("http://localhost:" ++ Debug.toString port)
        |> Task.attempt identity
        |> Platform.runCmd log

-- | Run a temporary web application that handles a single request, and then
-- immediately returns that request so you can run expectations against it.
--
-- Useful if you want to check properties of requests you send.
expectRequest :: Show e => (Http.Handler -> Text -> Task e a) -> Prelude.IO (Result Text Wai.Request)
expectRequest run =
  let app req _respond = Exception.throwIO (FirstRequest req)
   in Exception.try (withServer app run)
        |> map
          ( \either ->
              case either of
                Prelude.Left (FirstRequest req) -> Ok req
                Prelude.Right (Ok _) -> Err "Expected a request, but none was received."
                Prelude.Right (Err err) -> Err (Data.Text.pack (Prelude.show err))
          )

newtype FirstRequest = FirstRequest Wai.Request deriving (Show)

instance Exception.Exception FirstRequest
