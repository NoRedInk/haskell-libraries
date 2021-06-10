{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Http
  ( Handler,
    handler,
    handlerWithCustomManager,
    withThirdParty,
    withThirdPartyIO,
    get,
    post,
    request,
    emptyBody,
    stringBody,
    jsonBody,
    bytesBody,
    Expect,
    expectJson,
    expectText,
    expectWhatever,
    Internal.Http.Error (..),
    Internal.Http.Settings,
    Internal.Http.Body,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
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

-- |
type Handler = Internal.Http.Handler

-- |
handler :: Conduit.Acquire Handler
handler = do
  doAnything <- liftIO Platform.doAnythingHandler
  manager <- TLS.newTlsManager
  pure
    <| Internal.Http.Handler
      (_request doAnything manager)
      (_withThirdParty manager)
      (_withThirdPartyIO manager)

handlerWithCustomManager :: HTTP.Manager -> Conduit.Acquire Handler
handlerWithCustomManager m = do
  doAnything <- liftIO Platform.doAnythingHandler
  pure <| Internal.Http.Handler (_request doAnything m) (_withThirdParty m) (_withThirdPartyIO m)

-- | This is for external libraries only!
withThirdParty :: Handler -> (HTTP.Manager -> Task e a) -> Task e a
withThirdParty Internal.Http.Handler {Internal.Http.handlerWithThirdParty = wtp} library =
  wtp library

_withThirdParty :: HTTP.Manager -> (HTTP.Manager -> Task e a) -> Task e a
_withThirdParty manager library = do
  requestManager <- prepareManagerForRequest manager
  library requestManager

-- | Like `withThirdParty`, but runs in `IO`. We'd rather this function didn't
-- exist, and as we move more of our code to run in `Task` rather than `IO`
-- there will should come a point we will be able to delete it.
withThirdPartyIO :: Platform.LogHandler -> Handler -> (HTTP.Manager -> IO a) -> IO a
withThirdPartyIO log Internal.Http.Handler {Internal.Http.handlerWithThirdPartyIO = wtp} library =
  wtp log library

_withThirdPartyIO :: HTTP.Manager -> Platform.LogHandler -> (HTTP.Manager -> IO a) -> IO a
_withThirdPartyIO manager log library = do
  requestManager <- prepareManagerForRequest manager |> Task.perform log
  library requestManager

-- QUICKS

-- |
get :: Handler -> Text -> Expect a -> Task Error a
get handler' url expect =
  request
    handler'
    Internal.Http.Settings
      { Internal.Http._method = "GET",
        Internal.Http._headers = [],
        Internal.Http._url = url,
        Internal.Http._body = emptyBody,
        Internal.Http._timeout = Nothing,
        Internal.Http._expect = expect
      }

-- |
post :: Handler -> Text -> Body -> Expect a -> Task Error a
post handler' url body expect =
  request
    handler'
    Internal.Http.Settings
      { Internal.Http._method = "POST",
        Internal.Http._headers = [],
        Internal.Http._url = url,
        Internal.Http._body = body,
        Internal.Http._timeout = Nothing,
        Internal.Http._expect = expect
      }

-- REQUEST

-- |
type Settings a = Internal.Http.Settings a

-- |  Represents the body of a Request.
type Body = Internal.Http.Body

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

-- |
request :: Handler -> Settings expect -> Task Error expect
request Internal.Http.Handler {Internal.Http.handlerRequest} settings = handlerRequest settings

-- |
_request :: Platform.DoAnythingHandler -> HTTP.Manager -> Settings expect -> Task Error expect
_request doAnythingHandler manager settings = do
  requestManager <- prepareManagerForRequest manager
  Platform.doAnything doAnythingHandler <| do
    response <-
      Exception.try <| do
        basicRequest <-
          HTTP.parseUrlThrow <| Text.toList (Internal.Http._url settings)
        let finalRequest =
              basicRequest
                { HTTP.method = Data.Text.Encoding.encodeUtf8 (Internal.Http._method settings),
                  HTTP.requestHeaders = case Internal.Http.bodyContentType (Internal.Http._body settings) of
                    Nothing ->
                      Internal.Http._headers settings
                    Just mimeType ->
                      ("content-type", mimeType) : Internal.Http._headers settings,
                  HTTP.requestBody = HTTP.RequestBodyLBS <| Internal.Http.bodyContents (Internal.Http._body settings),
                  HTTP.responseTimeout = HTTP.responseTimeoutMicro <| fromIntegral <| Maybe.withDefault (30 * 1000 * 1000) (Internal.Http._timeout settings)
                }
        HTTP.httpLbs finalRequest requestManager
    pure <| case response of
      Right okResponse ->
        case decode (Internal.Http._expect settings) (HTTP.responseBody okResponse) of
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
            Err (Internal.Http.BadResponse (Text.fromList (show err)))
      Left (HTTP.InvalidUrlException _ message) ->
        Err (Internal.Http.BadUrl (Text.fromList message))

-- |
-- Logic for interpreting a response body.
type Expect a = Internal.Http.Expect a

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
