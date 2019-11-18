module Http (Handler, handler, withThirdParty, Anything (..), Http.get, post, request, Settings, Error (..)) where

import Nri.Prelude
import qualified Maybe
import qualified Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Header as Header
import qualified Conduit


{-| -}
newtype Handler =
  Handler HTTP.Manager


{-| -}
handler :: Conduit.Acquire Handler
handler = do
  manager <- TLS.newTlsManager
  pure (Handler manager)


{-| This is for external libraries only! -}
withThirdParty :: Handler -> (HTTP.Manager -> a) -> a
withThirdParty (Handler manager) library =
  library manager



-- QUICKS


{-| Use this if your response body should succeed no matter what it contains. -}
data Anything
  = Anything

instance Aeson.FromJSON Anything where
  parseJSON _ = pure Anything


{-| -}
get :: Aeson.FromJSON expect => Handler -> Text -> IO (Result Error expect)
get handler url =
  request handler Settings
    { _method = "GET"
    , _headers = []
    , _url = url
    , _body = ()
    , _timeout = Nothing
    }


{-| -}
post :: Aeson.FromJSON expect => Handler -> Text -> IO (Result Error expect)
post handler url =
  request handler Settings
    { _method = "POST"
    , _headers = []
    , _url = url
    , _body = ()
    , _timeout = Nothing
    }



-- REQUEST


{-| -}
data Settings body =
  Settings
    { _method :: Text
    , _headers :: [Header.Header]
    , _url :: Text
    , _body :: body
    , _timeout :: Maybe Int
    }


{-| -}
request :: (Aeson.ToJSON body, Aeson.FromJSON expect) => Handler -> Settings body -> IO (Result Error expect)
request (Handler manager) settings = do
  basicRequest <-
    HTTP.parseRequest <| toS (_url settings)

  let finalRequest = basicRequest
        { HTTP.method = toS (_method settings)
        , HTTP.requestHeaders = _headers settings
        , HTTP.requestBody = HTTP.RequestBodyLBS <| Aeson.encode (_body settings)
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro <| fromIntegral <| Maybe.withDefault 30 (_timeout settings)
        }

  response <-
    try (HTTP.httpLbs finalRequest manager)

  pure <|
    case response of
      Right okResponse ->
        case Aeson.eitherDecode (HTTP.responseBody okResponse) of
          Right decodedBody ->
            Ok decodedBody

          Left message ->
            Err (BadBody (toS message))

      Left (HTTP.HttpExceptionRequest _ content) ->
        case content of
          HTTP.StatusCodeException res _ ->
            let statusCode = fromIntegral << Status.statusCode << HTTP.responseStatus in
            Err (BadStatus (statusCode res))

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


{-| -}
data Error
  = BadUrl Text
  | BadStatus Int
  | BadBody Text
  | BadResponse
  | Timeout
  | NetworkError
