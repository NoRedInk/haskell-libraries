{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Making HTTP requests using an API inspired by Elm's elm/http.
module Http
  ( -- * Handlers
    Handler,
    handler,

    -- * Requests
    get,
    post,
    request,
    Internal.Http.Request (..),
    Internal.Http.Error (..),

    -- * Header,
    Internal.Http.Header,
    header,

    -- * Body
    Internal.Http.Body,
    emptyBody,
    stringBody,
    jsonBody,
    bytesBody,

    -- * Expect
    Expect,
    expectJson,
    expectText,
    expectWhatever,

    -- * Use with external libraries
    withThirdParty,
    withThirdPartyIO,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.String (fromString)
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Internal.Http (Body, Expect, Handler)
import qualified Internal.Http
import qualified Log.HttpRequest as HttpRequest
import qualified Maybe
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import qualified Network.URI
import qualified Platform
import qualified Task
import Prelude (Either (Left, Right), IO, fromIntegral, pure, show)

-- | Create a 'Handler' for making HTTP requests.
handler :: Conduit.Acquire Handler
handler = do
  doAnything <- liftIO Platform.doAnythingHandler
  manager <- TLS.newTlsManager
  pure
    <| Internal.Http.Handler
      (_request doAnything manager)
      (_withThirdParty manager)
      (_withThirdPartyIO manager)

-- | Third party libraries that make HTTP requests often take a 'HTTP.Manager'.
-- This helper allows us to call such a library using a 'Handler'.
--
-- The benefit over using this over using a separate 'HTTP.Manager' for the
-- external library, is that 'withThirdParty' will ensure HTTP requests made
-- by the external library will get logged.
withThirdParty :: Handler -> (HTTP.Manager -> Task e a) -> Task e a
withThirdParty Internal.Http.Handler {Internal.Http.handlerWithThirdParty = wtp} library =
  wtp library

_withThirdParty :: HTTP.Manager -> (HTTP.Manager -> Task e a) -> Task e a
_withThirdParty manager library = do
  requestManager <- prepareManagerForRequest manager
  library requestManager

-- | Like `withThirdParty`, but runs in `IO`.
withThirdPartyIO :: Platform.LogHandler -> Handler -> (HTTP.Manager -> IO a) -> IO a
withThirdPartyIO log Internal.Http.Handler {Internal.Http.handlerWithThirdPartyIO = wtp} library =
  wtp log library

_withThirdPartyIO :: HTTP.Manager -> Platform.LogHandler -> (HTTP.Manager -> IO a) -> IO a
_withThirdPartyIO manager log library = do
  requestManager <- prepareManagerForRequest manager |> Task.perform log
  library requestManager

-- QUICKS

-- | Create a @GET@ request.
get :: Handler -> Text -> Expect a -> Task Error a
get handler' url expect =
  request
    handler'
    Internal.Http.Request
      { Internal.Http.method = "GET",
        Internal.Http.headers = [],
        Internal.Http.url = url,
        Internal.Http.body = emptyBody,
        Internal.Http.timeout = Nothing,
        Internal.Http.expect = expect
      }

-- | Create a @POST@ request.
post :: Handler -> Text -> Body -> Expect a -> Task Error a
post handler' url body expect =
  request
    handler'
    Internal.Http.Request
      { Internal.Http.method = "POST",
        Internal.Http.headers = [],
        Internal.Http.url = url,
        Internal.Http.body = body,
        Internal.Http.timeout = Nothing,
        Internal.Http.expect = expect
      }

-- REQUEST

-- | Create a 'Header'.
header :: Text -> Text -> Internal.Http.Header
header key val =
  Internal.Http.Header
    (fromString (Text.toList key), fromString (Text.toList val))

-- | Create an empty body for your Request. This is useful for GET requests and
-- POST requests where you are not sending any data.
emptyBody :: Body
emptyBody =
  Internal.Http.Body
    { Internal.Http.bodyContents = "",
      Internal.Http.bodyContentType = Nothing
    }

-- | Put some string in the body of your Request.
--
-- The first argument is a MIME type of the body. Some servers are strict about
-- this!
stringBody :: Text -> Text -> Body
stringBody mimeType text =
  Internal.Http.Body
    { Internal.Http.bodyContents = Data.Text.Encoding.encodeUtf8 text |> Data.ByteString.Lazy.fromStrict,
      Internal.Http.bodyContentType = Just (Data.Text.Encoding.encodeUtf8 mimeType)
    }

-- | Put some JSON value in the body of your Request. This will automatically
-- add the Content-Type: application/json header.
jsonBody :: Aeson.ToJSON body => body -> Body
jsonBody json =
  Internal.Http.Body
    { Internal.Http.bodyContents = Aeson.encode json,
      Internal.Http.bodyContentType = Just "application/json"
    }

-- | Put some Bytes in the body of your Request. This allows you to use
-- ByteString to have full control over the binary representation of the data
-- you are sending.
--
-- The first argument is a MIME type of the body. In other scenarios you may
-- want to use MIME types like image/png or image/jpeg instead.
bytesBody :: Text -> ByteString -> Body
bytesBody mimeType bytes =
  Internal.Http.Body
    { Internal.Http.bodyContents = Data.ByteString.Lazy.fromStrict bytes,
      Internal.Http.bodyContentType = Just (Data.Text.Encoding.encodeUtf8 mimeType)
    }

-- | Create a custom request.
request :: Handler -> Internal.Http.Request expect -> Task Error expect
request Internal.Http.Handler {Internal.Http.handlerRequest} settings = handlerRequest settings

_request :: Platform.DoAnythingHandler -> HTTP.Manager -> Internal.Http.Request expect -> Task Error expect
_request doAnythingHandler manager settings = do
  requestManager <- prepareManagerForRequest manager
  Platform.doAnything doAnythingHandler <| do
    response <-
      Exception.try <| do
        basicRequest <-
          HTTP.parseUrlThrow <| Text.toList (Internal.Http.url settings)
        let finalRequest =
              basicRequest
                { HTTP.method = Data.Text.Encoding.encodeUtf8 (Internal.Http.method settings),
                  HTTP.requestHeaders = case Internal.Http.bodyContentType (Internal.Http.body settings) of
                    Nothing ->
                      Internal.Http.headers settings
                        |> List.map Internal.Http.unHeader
                    Just mimeType ->
                      ("content-type", mimeType) :
                      List.map Internal.Http.unHeader (Internal.Http.headers settings),
                  HTTP.requestBody = HTTP.RequestBodyLBS <| Internal.Http.bodyContents (Internal.Http.body settings),
                  HTTP.responseTimeout =
                    Internal.Http.timeout settings
                      |> Maybe.withDefault (30 * 1000)
                      |> (*) 1000
                      |> fromIntegral
                      |> HTTP.responseTimeoutMicro
                }
        HTTP.httpLbs finalRequest requestManager
    pure <| case response of
      Right okResponse ->
        case decode (Internal.Http.expect settings) (HTTP.responseBody okResponse) of
          Ok decodedBody ->
            Ok decodedBody
          Err message ->
            Err (Internal.Http.BadBody message)
      Left (HTTP.HttpExceptionRequest _ content) ->
        case content of
          HTTP.StatusCodeException res _ ->
            let statusCode = fromIntegral << Status.statusCode << HTTP.responseStatus
             in Err (Internal.Http.BadStatus (statusCode res))
          HTTP.ResponseTimeout ->
            Err Internal.Http.Timeout
          HTTP.ConnectionTimeout ->
            Err (Internal.Http.NetworkError "ConnectionTimeout")
          HTTP.ConnectionFailure err ->
            Err (Internal.Http.NetworkError (Text.fromList (Exception.displayException err)))
          err ->
            Err (Internal.Http.NetworkError (Text.fromList (show err)))
      Left (HTTP.InvalidUrlException _ message) ->
        Err (Internal.Http.BadUrl (Text.fromList message))

decode :: Expect a -> Data.ByteString.Lazy.ByteString -> Result Text a
decode Internal.Http.ExpectJson bytes =
  case Aeson.eitherDecode bytes of
    Left err -> Err (Text.fromList err)
    Right x -> Ok x
decode Internal.Http.ExpectText bytes = (Ok << Data.Text.Lazy.toStrict << Data.Text.Lazy.Encoding.decodeUtf8) bytes
decode Internal.Http.ExpectWhatever _ = Ok ()

-- |
-- Expect the response body to be JSON.
expectJson :: Aeson.FromJSON a => Expect a
expectJson = Internal.Http.ExpectJson

-- |
-- Expect the response body to be a `Text`.
expectText :: Expect Text
expectText = Internal.Http.ExpectText

-- |
-- Expect the response body to be whatever. It does not matter. Ignore it!
expectWhatever :: Expect ()
expectWhatever = Internal.Http.ExpectWhatever

-- |
type Error = Internal.Http.Error

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
  requestId <- Platform.requestId
  pure
    manager
      { -- To be able to correlate events and logs belonging to a single
        -- original user request we pass around a request ID on HTTP requests
        -- between services. Below we add this request ID to all outgoing HTTP
        -- requests.
        HTTP.Internal.mModifyRequest = \req ->
          HTTP.Internal.mModifyRequest manager req
            |> andThen (modifyRequest requestId),
        -- We trace outgoing HTTP requests. This comes down to measuring how
        -- long they take and passing that information to some dashboard. This
        -- dashboard can then draw nice graphs showing how the time responding
        -- to a request it divided between different activities, such as sending
        -- HTTP requests. We can use the `mWrapException` for this purpose,
        -- although in our case we're not wrapping because of exceptions.
        HTTP.Internal.mWrapException = \req io ->
          HTTP.Internal.mWrapException manager req io
            |> wrapException log req
      }
  where
    modifyRequest :: Text -> HTTP.Request -> IO HTTP.Request
    modifyRequest requestId req =
      case requestId of
        "" -> pure req
        _ ->
          pure
            req
              { HTTP.requestHeaders =
                  ("x-request-id", Data.Text.Encoding.encodeUtf8 requestId) :
                  HTTP.requestHeaders req
              }
    wrapException :: forall a. Platform.LogHandler -> HTTP.Request -> IO a -> IO a
    wrapException log req io =
      let uri = HTTP.getUri req
          host =
            Network.URI.uriScheme uri
              ++ ( Network.URI.uriAuthority uri
                     |> Network.URI.uriAuthToString (\_ -> "*****")
                     |> (\showS -> showS "")
                 )
              |> Text.fromList
          method =
            HTTP.method req
              |> Data.Text.Encoding.decodeUtf8
          spanDetails =
            HttpRequest.Outgoing
              HttpRequest.emptyDetails
                { HttpRequest.host = Just host,
                  HttpRequest.path =
                    Network.URI.uriPath uri
                      |> Text.fromList
                      |> Just,
                  HttpRequest.queryString =
                    Network.URI.uriQuery uri
                      |> Text.fromList
                      |> Just,
                  HttpRequest.method = Just method
                }
          uriStr =
            HTTP.getUri req
              |> Network.URI.uriToString (\_ -> "*****")
              |> (\showS -> Text.fromList (showS ""))
       in Platform.tracingSpanIO
            log
            "Outoing HTTP Request"
            ( \log' ->
                Exception.finally
                  io
                  ( do
                      Platform.setTracingSpanDetailsIO log' spanDetails
                      Platform.setTracingSpanSummaryIO
                        log'
                        (method ++ " " ++ uriStr)
                  )
            )
