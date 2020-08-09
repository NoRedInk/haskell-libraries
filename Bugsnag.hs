module Observability.Bugsnag
  ( reporter,
    Settings,
    decoder,
    readiness,
    toEvent,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Proxy as Proxy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified Data.Typeable as Typeable
import qualified Environment
import qualified GHC.Stack as Stack
import qualified Health
import qualified Http
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client
import qualified Network.HostName
import qualified Platform
import qualified Prelude

-- | Reporting to Bugsnag.
--
-- This function takes the root span of a completed request and reports it to
-- Bugsnag, if there has been a failure. A request that completed succesfully
-- is not reported.
--
-- If we squint a bit, the rough shape of data that Bugsnag expects of us is:
--
--    event {attributes} [breadcrumbs]
--
-- Meaning: we can use various attributes to describe an event and in addition
-- pass a list of "breadcrumbs", other events that took place before the one the
-- report we're making is about.
--
-- The root span we pass in is a tree structure. It can have child spans, which
-- in turn can have child spans, etc. Each span is marked with whether it
-- succeeded or failed. If one of the children of a span failed, the span itself
-- failed too.
--
-- To turn this tree structure into the data that Bugsnag expects we're going to
-- take the following approach. First we're going to find the 'root cause span'.
-- This is the most recently started span that failed. The data in this span and
-- it's parents is going to make up the main event to Bugsnag. All other spans
-- that completed before the root cause span started we'll turn into
-- breadcrumbs. For some span tree it might look like this:
--
--     ^     failed span, a = 1            -> event { a = 1,
--     t         succeeded span
--     i         failed span, b = 2        ->         b = 2,
--     m             failed span, c = 3    ->         c = 3 }
--     e                 succeeded span    ->       [ breadcrumb1
--     ^         succeeded span            ->       , breadcrumb2 ]
--
-- A span that happened _after_ the root cause event completed we're not
-- reporting.
reporter :: Http.Handler -> Settings -> Platform.Span -> Prelude.IO ()
reporter http settings span = do
  defaultEvent <- mkDefaultEvent settings
  if failed span
    then send http settings (toEvent defaultEvent span)
    else Prelude.pure ()

send :: Http.Handler -> Settings -> Bugsnag.Event -> Prelude.IO ()
send http settings event = do
  log <- Platform.silentHandler
  Http.withThirdPartyIO log http <| \manager -> do
    -- Logging to Bugsnag might fail, but if it does we can't very well send the
    -- error to Bugsnag. This is the end of the line, these errors disappear
    -- into the aether.
    _ <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) [event]
    Prelude.pure ()

toEvent :: Bugsnag.Event -> Platform.Span -> Bugsnag.Event
toEvent = rootCause [] Prelude.mempty

-- | Find the most recently started span that failed. This span is closest to
-- the failure and we'll use the data in it and its parents to build the
-- exception we send to Bugsnag. We'll send information about spans that ran
-- before the root cause span started as breadcrumbs.
rootCause :: [Bugsnag.StackFrame] -> Breadcrumbs -> Bugsnag.Event -> Platform.Span -> Bugsnag.Event
rootCause frames breadcrumbs event span =
  let newFrames =
        case Platform.frame span of
          Nothing -> frames
          Just (name, src) -> toStackFrame name src : frames
      newEvent = decorateEventWithSpanData span event
      childSpans = Platform.children span
   in -- We're not interested in child spans that happened _after_ the root
      -- cause took place. These are not breadcrumbs (leading up to the error)
      -- nor can they have caused the error itself because they happened after.
      -- Since child spans are ordered most-recent first we can keep dropping
      -- child spans until we hit the one where the most recent error happened.
      case Data.List.dropWhile (not << failed) childSpans of
        child : preErrorSpans ->
          rootCause
            newFrames
            (addCrumbs preErrorSpans ++ breadcrumbs)
            newEvent
            child
        [] ->
          newEvent
            { Bugsnag.event_exceptions = [toException newFrames span],
              Bugsnag.event_breadcrumbs =
                -- This is the innermost span that failed, so all it's children
                -- succeeded. We're going to assume that the error happened
                -- after the last of these child spans, making all child spans
                -- breadcrumbs.
                addCrumbs childSpans ++ breadcrumbs
                  |> breadcrumbsAsList
                  |> Just,
              Bugsnag.event_unhandled = case Platform.succeeded span of
                Platform.Succeeded -> Nothing
                -- `Failed` indicates a span was marked as failed by the application
                -- author. Something went wrong, but we wrote logic to handle it.
                Platform.Failed -> Just False
                -- `FailedWith` indicates a Haskell exception was thrown. We don't throw
                -- in our applications, so this indicates a library is doing something
                -- we didn't expect.
                Platform.FailedWith _ -> Just True
            }

-- | This function is passed a list of spans and outputs a type representing a
-- flat list of breadcrumbs.
--
-- It looks like this function should be able to do a map, to turn each span
-- into a breadcrumb. Each span can contain child breadcrumbs though, requiring
-- us to recurse.
--
-- Our Bugsnag library asks for a value of type `[Bugsnag.Breadcrumb]`, so a
-- list. It's very performant to add single items to the front of a list, but
-- appending two lists is costly. So we want to avoid appending in our
-- breadcrumb collection, because if the span tree gets large we'd be doing a
-- lot of it.
--
-- To help us avoid doing appends we create a helper type `Breadcrumbs a`, which
-- is a wrapper around a state monad. The only helper function it exposes for
-- adding a breadcrumb is one that cons that breadcrumb to the front of the
-- list, so no appends.
addCrumbs :: [Platform.Span] -> Breadcrumbs
addCrumbs spans =
  case spans of
    [] -> Prelude.mempty
    span : after ->
      addCrumbsForSpan span
        ++ addCrumbs after

addCrumbsForSpan :: Platform.Span -> Breadcrumbs
addCrumbsForSpan span =
  case Platform.children span of
    [] ->
      addBreadcrumb (toBreadcrumb DoSpan span)
    children ->
      addBreadcrumb (toBreadcrumb EndSpan span)
        ++ addCrumbs children
        ++ addBreadcrumb (toBreadcrumb StartSpan span)

newtype Breadcrumbs = Breadcrumbs ([Bugsnag.Breadcrumb] -> [Bugsnag.Breadcrumb])
  deriving (Prelude.Semigroup, Prelude.Monoid)

breadcrumbsAsList :: Breadcrumbs -> [Bugsnag.Breadcrumb]
breadcrumbsAsList (Breadcrumbs f) = f []

addBreadcrumb :: Bugsnag.Breadcrumb -> Breadcrumbs
addBreadcrumb crumb = Breadcrumbs (crumb :)

data BreadcrumbType = StartSpan | DoSpan | EndSpan

toBreadcrumb :: BreadcrumbType -> Platform.Span -> Bugsnag.Breadcrumb
toBreadcrumb breadcrumbType span =
  Bugsnag.defaultBreadcrumb
    { Bugsnag.breadcrumb_name = case breadcrumbType of
        DoSpan -> Platform.name span
        StartSpan -> "Starting: " ++ Platform.name span
        EndSpan -> "Finished: " ++ Platform.name span
    }

decorateEventWithSpanData :: Platform.Span -> Bugsnag.Event -> Bugsnag.Event
decorateEventWithSpanData span event =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderSpanDetails
          [ Platform.Renderer (renderIncomingHttpRequest event),
            Platform.Renderer (renderRemainingSpanDetails span event)
          ]
      )
    |> Maybe.withDefault event

renderRemainingSpanDetails :: Platform.Span -> Bugsnag.Event -> Platform.SomeSpanDetails -> Bugsnag.Event
renderRemainingSpanDetails span event details =
  event
    { Bugsnag.event_metaData =
        Aeson.toJSON details
          |> HashMap.singleton (Platform.name span)
          |> Just
          |> (++) (Bugsnag.event_metaData event)
    }

renderIncomingHttpRequest :: Bugsnag.Event -> Monitoring.RequestDetails -> Bugsnag.Event
renderIncomingHttpRequest event request =
  event
    { Bugsnag.event_context = Just (Monitoring.method request ++ " " ++ Monitoring.endpoint request),
      Bugsnag.event_request =
        Just
          Bugsnag.defaultRequest
            { Bugsnag.request_httpMethod = Just (Monitoring.method request),
              Bugsnag.request_headers =
                Monitoring.requestHeaders request
                  |> Monitoring.unHeaders
                  |> map
                    ( \(key, value) ->
                        ( Data.Text.Encoding.decodeUtf8 (CI.original key),
                          Data.Text.Encoding.decodeUtf8 value
                        )
                    )
                  |> HashMap.fromList
                  |> Just
            },
      -- Extra request data that Bugsnag doesn't ask for in its API, but which
      -- we can make appear on the 'request' tab anyway by logging it on the
      -- 'request' key of the event metadata.
      Bugsnag.event_metaData =
        [ "endpoint" .= Monitoring.endpoint request,
          "http version" .= Monitoring.httpVersion request,
          "response status" .= Monitoring.responseStatus request,
          "path" .= Monitoring.path request,
          "query string" .= Monitoring.queryString request
        ]
          |> Aeson.object
          |> HashMap.singleton "request"
          |> Just
          |> (++) (Bugsnag.event_metaData event)
    }

failed :: Platform.Span -> Bool
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> False
    Platform.Failed -> True
    Platform.FailedWith _ -> True

toException :: [Bugsnag.StackFrame] -> Platform.Span -> Bugsnag.Exception
toException frames span =
  case Platform.succeeded span of
    Platform.Succeeded -> Bugsnag.defaultException
    Platform.Failed ->
      Bugsnag.defaultException
        { Bugsnag.exception_errorClass = "Failed: " ++ Platform.name span,
          Bugsnag.exception_stacktrace = frames
        }
    Platform.FailedWith (Exception.SomeException exception) ->
      Bugsnag.defaultException
        { Bugsnag.exception_errorClass = typeName exception,
          Bugsnag.exception_stacktrace = frames,
          Bugsnag.exception_message =
            Exception.displayException exception
              |> Data.Text.pack
              |> Just
        }

toStackFrame :: Text -> Stack.SrcLoc -> Bugsnag.StackFrame
toStackFrame functionName frame =
  Bugsnag.defaultStackFrame
    { Bugsnag.stackFrame_file = Data.Text.pack (Stack.srcLocFile frame),
      Bugsnag.stackFrame_lineNumber = Stack.srcLocStartLine frame,
      Bugsnag.stackFrame_columnNumber = Just (Stack.srcLocStartCol frame),
      Bugsnag.stackFrame_method = functionName,
      Bugsnag.stackFrame_inProject = Just True
    }

typeName :: forall a. Typeable.Typeable a => a -> Text
typeName _ =
  Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy a)
    |> Prelude.show
    |> Data.Text.pack

data Settings
  = Settings
      { apiKey :: Log.Secret Bugsnag.ApiKey,
        appName :: Namespace,
        appEnvironment :: Environment
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap apiKeyDecoder
    |> andMap namespaceDecoder
    |> andMap environmentDecoder

apiKeyDecoder :: Environment.Decoder (Log.Secret Bugsnag.ApiKey)
apiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "BUGSNAG_API_KEY",
        Environment.description = "The API key of the Bugsnag project we should send items too.",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> map Bugsnag.apiKey |> Environment.secret)

newtype Namespace = Namespace {unNamespace :: Text}

namespaceDecoder :: Environment.Decoder Namespace
namespaceDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ROOT_NAMESPACE",
        Environment.description = "Root of the log namespace. This should be the name of the application.",
        Environment.defaultValue = "your-application-name-here"
      }
    (map Namespace Environment.text)

newtype Environment = Environment {unEnvironment :: Text}

environmentDecoder :: Environment.Decoder Environment
environmentDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ENVIRONMENT",
        Environment.description = "Environment to display in logs.",
        Environment.defaultValue = "development"
      }
    (map Environment Environment.text)

-- |
-- Check if Bugsnag is ready to receive requests.
readiness :: Settings -> Network.HTTP.Client.Manager -> Health.Check
readiness settings manager =
  Health.mkCheck "bugsnag" <| do
    result <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) []
    Prelude.pure <| case result of
      Prelude.Right () -> Health.Good
      Prelude.Left err ->
        "HTTP request to Bugsnag failed: " ++ Exception.displayException err
          |> Data.Text.pack
          |> Health.Bad

_getRevision :: Prelude.IO Text
_getRevision = do
  eitherRevision <- Exception.tryAny <| Prelude.readFile "revision"
  case eitherRevision of
    Prelude.Left _err -> Prelude.pure "no revision file found"
    Prelude.Right version -> Prelude.pure <| Data.Text.pack version

mkDefaultEvent :: Settings -> Prelude.IO Bugsnag.Event
mkDefaultEvent settings = do
  revision <- getRevision
  hostname <- Network.HostName.getHostName
  let appId = unNamespace (appName settings)
  let app =
        Bugsnag.defaultApp
          { Bugsnag.app_id = Just appId,
            -- Same format as what bugsnag-build-notify uses for appVersion
            Bugsnag.app_version = Just (appId ++ "-" ++ unRevision revision),
            Bugsnag.app_releaseStage = Just (unEnvironment (appEnvironment settings)),
            Bugsnag.app_type = Just "haskell"
          }
  let device =
        Bugsnag.defaultDevice
          { Bugsnag.device_hostname = Just (Data.Text.pack hostname)
          }
  Prelude.pure
    Bugsnag.defaultEvent
      { Bugsnag.event_app = Just app,
        Bugsnag.event_device = Just device
      }

newtype Revision = Revision {unRevision :: Text}

getRevision :: Prelude.IO Revision
getRevision = do
  eitherRevision <- Exception.tryAny <| Data.Text.IO.readFile "revision"
  case eitherRevision of
    Prelude.Left _err -> Prelude.pure (Revision "no revision file found")
    Prelude.Right version -> Prelude.pure (Revision version)
