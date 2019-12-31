module Http
  ( Handler,
    handler,
    withThirdParty,
    Http.get,
    post,
    request,
    Settings (..),
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
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Maybe
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Status as Status
import qualified Platform
import Prelude (Either (Left, Right), IO, fromIntegral, pure)

-- |
data Handler
  = Handler Platform.DoAnythingHandler HTTP.Manager

-- |
handler :: Conduit.Acquire Handler
handler =
  map2 Handler (liftIO Platform.doAnythingHandler) TLS.newTlsManager

-- | This is for external libraries only!
withThirdParty :: Handler -> (HTTP.Manager -> a) -> a
withThirdParty (Handler _ manager) library =
  library manager

-- QUICKS

-- |
get :: Handler -> Text -> Expect a -> Task Error a
get handler' url expect =
  request handler' Settings
    { _method = "GET",
      _headers = [],
      _url = url,
      _body = (),
      _timeout = Nothing,
      _expect = expect
    }

-- |
post :: Handler -> Text -> Expect a -> Task Error a
post handler' url expect =
  request handler' Settings
    { _method = "POST",
      _headers = [],
      _url = url,
      _body = (),
      _timeout = Nothing,
      _expect = expect
    }

-- REQUEST

-- |
data Settings body a
  = Settings
      { _method :: Text,
        _headers :: [Header.Header],
        _url :: Text,
        _body :: body,
        _timeout :: Maybe Int,
        _expect :: Expect a
      }

-- |
request :: (Aeson.ToJSON body) => Handler -> Settings body expect -> Task Error expect
request (Handler doAnythingHandler manager) settings = do
  requestManager <- prepareManagerForRequest manager
  Platform.doAnything doAnythingHandler <| do
    basicRequest <-
      HTTP.parseUrlThrow <| Data.Text.unpack (_url settings)
    let finalRequest =
          basicRequest
            { HTTP.method = Data.Text.Encoding.encodeUtf8 (_method settings),
              HTTP.requestHeaders = _headers settings,
              HTTP.requestBody = HTTP.RequestBodyLBS <| Aeson.encode (_body settings),
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
  contexts <- Platform.getContexts
  pure
    manager
      { -- To be able to correlate events and logs belonging to a single
        -- original user request we pass around a request ID on HTTP requests
        -- between services. Below we add this request ID to all outgoing HTTP
        -- requests.
        HTTP.Internal.mModifyRequest = modifyRequest contexts
      }
  where
    modifyRequest :: Platform.Contexts -> HTTP.Request -> IO HTTP.Request
    modifyRequest contexts request =
      case Platform.requestId contexts of
        Nothing -> pure request
        Just requestId ->
          pure
            request
              { HTTP.requestHeaders =
                  ("x-request-id", Data.Text.Encoding.encodeUtf8 requestId)
                    : HTTP.requestHeaders request
              }
