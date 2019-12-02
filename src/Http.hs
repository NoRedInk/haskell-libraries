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

import qualified Conduit
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Maybe
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Status as Status
import Nri.Prelude
import qualified Platform

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
request (Handler doAnythingHandler manager) settings =
  Platform.doAnything doAnythingHandler <| do
    basicRequest <-
      HTTP.parseRequest <| toS (_url settings)
    let finalRequest =
          basicRequest
            { HTTP.method = toS (_method settings),
              HTTP.requestHeaders = _headers settings,
              HTTP.requestBody = HTTP.RequestBodyLBS <| Aeson.encode (_body settings),
              HTTP.responseTimeout = HTTP.responseTimeoutMicro <| fromIntegral <| Maybe.withDefault (30 * 1000 * 1000) (_timeout settings)
            }
    response <- try (HTTP.httpLbs finalRequest manager)
    pure <| case response of
      Right okResponse ->
        case decode (_expect settings) (HTTP.responseBody okResponse) of
          Ok decodedBody ->
            Ok decodedBody
          Err message ->
            Err (BadBody (toS message))
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
        Err (BadUrl (toS message))

-- |
-- Logic for interpreting a response body.
newtype Expect a = Expect {decode :: Data.ByteString.Lazy.ByteString -> Result Text a}

-- |
-- Expect the response body to be JSON.
expectJson :: Aeson.FromJSON a => Expect a
expectJson =
  Expect <| \bytestring ->
    case Aeson.eitherDecode bytestring of
      Left err -> Err (toS err)
      Right x -> Ok x

-- |
-- Expect the response body to be a `Text`.
expectText :: Expect Text
expectText = Expect (Ok << toS)

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
