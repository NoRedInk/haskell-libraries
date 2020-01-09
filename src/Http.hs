module Http
  ( Handler,
    handler,
    withThirdParty,
    withThirdPartyIO,
    Http.get,
    post,
    request,
    Settings (..),
    Body,
    emptyBody,
    stringBody,
    jsonBody,
    bytesBody,
    Expect,
    expectJson,
    expectText,
    expectWhatever,
    Error (..),
  )
where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Log
import qualified Maybe
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Mime as Mime
import qualified Network.URI
import qualified Platform
import qualified Task
import Prelude (Either (Left, Right), IO, fromIntegral, pure)

-- |
data Handler
  = Handler Platform.DoAnythingHandler HTTP.Manager

-- |
handler :: Conduit.Acquire Handler
handler =
  map2 Handler (liftIO Platform.doAnythingHandler) TLS.newTlsManager

-- | This is for external libraries only!
withThirdParty :: Handler -> (HTTP.Manager -> Task e a) -> Task e a
withThirdParty (Handler _ manager) library = do
  requestManager <- prepareManagerForRequest manager
  library requestManager

-- | Like `withThirdParty`, but runs in `IO`. We'd rather this function didn't
-- exist, and as we move more of our code to run in `Task` rather than `IO`
-- there will should come a point we will be able to delete it.
withThirdPartyIO :: Platform.LogHandler -> Handler -> (HTTP.Manager -> IO a) -> IO a
withThirdPartyIO log (Handler _ manager) library = do
  requestManager <-
    prepareManagerForRequest manager
      |> Task.attempt
        ( \result ->
            case result of
              Err err -> never err
              Ok x -> x
        )
      |> Platform.runCmd log
  library requestManager

-- QUICKS

-- |
get :: Handler -> Text -> Expect a -> Task Error a
get handler' url expect =
  request handler' Settings
    { _method = "GET",
      _headers = [],
      _url = url,
      _body = emptyBody,
      _timeout = Nothing,
      _expect = expect
    }

-- |
post :: Handler -> Text -> Body -> Expect a -> Task Error a
post handler' url body expect =
  request handler' Settings
    { _method = "POST",
      _headers = [],
      _url = url,
      _body = body,
      _timeout = Nothing,
      _expect = expect
    }

-- REQUEST

-- |
data Settings a
  = Settings
      { _method :: Text,
        _headers :: [Header.Header],
        _url :: Text,
        _body :: Body,
        _timeout :: Maybe Int,
        _expect :: Expect a
      }

-- |  Represents the body of a Request.
data Body
  = Body
      { bodyContents :: Data.ByteString.Lazy.ByteString,
        bodyContentType :: Maybe Mime.MimeType
      }

-- | Create an empty body for your Request. This is useful for GET requests and
-- POST requests where you are not sending any data.
emptyBody :: Body
emptyBody =
  Body
    { bodyContents = "",
      bodyContentType = Nothing
    }

-- | Put some string in the body of your Request.
--
-- The first argument is a MIME type of the body. Some servers are strict about
-- this!
stringBody :: Text -> Text -> Body
stringBody mimeType text =
  Body
    { bodyContents = Data.Text.Encoding.encodeUtf8 text |> Data.ByteString.Lazy.fromStrict,
      bodyContentType = Just (Data.Text.Encoding.encodeUtf8 mimeType)
    }

-- | Put some JSON value in the body of your Request. This will automatically
-- add the Content-Type: application/json header.
jsonBody :: Aeson.ToJSON body => body -> Body
jsonBody json =
  Body
    { bodyContents = Aeson.encode json,
      bodyContentType = Just "application/json"
    }

-- | Put some Bytes in the body of your Request. This allows you to use
-- ByteString to have full control over the binary representation of the data
-- you are sending.
--
-- The first argument is a MIME type of the body. In other scenarios you may
-- want to use MIME types like image/png or image/jpeg instead.
bytesBody :: Text -> ByteString -> Body
bytesBody mimeType bytes =
  Body
    { bodyContents = Data.ByteString.Lazy.fromStrict bytes,
      bodyContentType = Just (Data.Text.Encoding.encodeUtf8 mimeType)
    }

-- |
request :: Handler -> Settings expect -> Task Error expect
request (Handler doAnythingHandler manager) settings = do
  requestManager <- prepareManagerForRequest manager
  Platform.doAnything doAnythingHandler <| do
    basicRequest <-
      HTTP.parseUrlThrow <| Data.Text.unpack (_url settings)
    let finalRequest =
          basicRequest
            { HTTP.method = Data.Text.Encoding.encodeUtf8 (_method settings),
              HTTP.requestHeaders = case bodyContentType (_body settings) of
                Nothing ->
                  _headers settings
                Just mimeType ->
                  ("content-type", mimeType) : _headers settings,
              HTTP.requestBody = HTTP.RequestBodyLBS <| bodyContents (_body settings),
              HTTP.responseTimeout = HTTP.responseTimeoutMicro <| fromIntegral <| Maybe.withDefault (30 * 1000 * 1000) (_timeout settings)
            }
    response <- Exception.try (HTTP.httpLbs finalRequest requestManager)
    pure <| case response of
      Right okResponse ->
        case decode (_expect settings) (HTTP.responseBody okResponse) of
          Ok decodedBody ->
            Ok decodedBody
          Err message ->
            Err (BadBody message)
      Left (HTTP.HttpExceptionRequest _ content) ->
        case content of
          HTTP.StatusCodeException res _ ->
            let statusCode = fromIntegral << Status.statusCode << HTTP.responseStatus
             in Err (BadStatus (statusCode res))
          HTTP.ResponseTimeout ->
            Err Timeout
          HTTP.ConnectionTimeout ->
            Err NetworkError
          HTTP.ConnectionFailure _ ->
            Err NetworkError
          _ ->
            Err BadResponse
      Left (HTTP.InvalidUrlException _ message) ->
        Err (BadUrl (Data.Text.pack message))

-- |
-- Logic for interpreting a response body.
newtype Expect a = Expect {decode :: Data.ByteString.Lazy.ByteString -> Result Text a}

-- |
-- Expect the response body to be JSON.
expectJson :: Aeson.FromJSON a => Expect a
expectJson =
  Expect <| \bytestring ->
    case Aeson.eitherDecode bytestring of
      Left err -> Err (Data.Text.pack err)
      Right x -> Ok x

-- |
-- Expect the response body to be a `Text`.
expectText :: Expect Text
expectText = Expect (Ok << Data.Text.Lazy.toStrict << Data.Text.Lazy.Encoding.decodeUtf8)

-- |
-- Expect the response body to be whatever. It does not matter. Ignore it!
expectWhatever :: Expect ()
expectWhatever = Expect (\_ -> Ok ())

-- |
data Error
  = BadUrl Text
  | BadStatus Int
  | BadBody Text
  | BadResponse
  | Timeout
  | NetworkError

-- Our Task type carries around some context values which should influence in
-- minor ways the logic of sending a request. In this function we modify a
-- manager to apply these modifications (see the comments below for the exact
-- nature of the modifications).
--
-- We're changing settings on the manager that originally get set during the
-- creation of the manager. We cannot set these settings once during creation
-- because they will be different for each outgoing request, and for performance
-- reasons we're encouraged to reuse a manager as much as possible. Modifying a
-- manager in this way does require use of the `Network.HTTP.Client.Internal`
-- module, which on account of being an internal module increases the risk of
-- this code breaking in future versions of the `http-client` package. There's
-- an outstanding PR for motivating these Manager modification functions are
-- moved to the stable API: https://github.com/snoyberg/http-client/issues/426
prepareManagerForRequest :: HTTP.Manager -> Task e HTTP.Manager
prepareManagerForRequest manager = do
  log <- Platform.logHandler
  contexts <- Platform.getContext
  pure
    manager
      { -- To be able to correlate events and logs belonging to a single
        -- original user request we pass around a request ID on HTTP requests
        -- between services. Below we add this request ID to all outgoing HTTP
        -- requests.
        HTTP.Internal.mModifyRequest = modifyRequest contexts,
        -- We trace outgoing HTTP requests. This comes down to measuring how
        -- long they take and passing that information to some dashboard. This
        -- dashboard can then draw nice graphs showing how the time responding
        -- to a request it divided between different activities, such as sending
        -- HTTP requests. We can use the `mWrapException` for this purpose,
        -- although in our case we're not wrapping because of exceptions.
        HTTP.Internal.mWrapException = wrapException log
      }
  where
    modifyRequest :: Platform.Context -> HTTP.Request -> IO HTTP.Request
    modifyRequest contexts req =
      case Platform.requestId contexts of
        Nothing -> pure req
        Just requestId ->
          pure
            req
              { HTTP.requestHeaders =
                  ("x-request-id", Data.Text.Encoding.encodeUtf8 requestId)
                    : HTTP.requestHeaders req
              }
    wrapException :: forall a. Platform.LogHandler -> HTTP.Request -> IO a -> IO a
    wrapException log req io = do
      doAnything <- Platform.doAnythingHandler
      Log.withContext
        "outoing http request"
        [ Platform.httpRequestContext Platform.HttpRequestInfo
            { Platform.requestUri =
                HTTP.getUri req
                  |> Network.URI.uriToString (\_ -> "*****")
                  |> (\showS -> Data.Text.pack (showS "")),
              Platform.requestMethod =
                HTTP.method req
                  |> Data.Text.Encoding.decodeUtf8
            }
        ]
        (Platform.doAnything doAnything (map Ok io))
        |> Task.attempt
          ( \result ->
              case result of
                Err err -> never err
                Ok x -> x
          )
        |> Platform.runCmd log
