-- | Reporting to Bugsnag.
--
-- This reporter reports failures to Bugsnag. It does nothing for requests that
-- completed without error.
module Reporter.Bugsnag.Internal where

import qualified Control.Exception.Safe as Exception
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Proxy as Proxy
import qualified Data.Text.IO
import qualified Data.Typeable as Typeable
import qualified Dict
import qualified Environment
import qualified GHC.Stack as Stack
import qualified List
import qualified Log.HttpRequest as HttpRequest
import qualified Log.RedisCommands as RedisCommands
import qualified Log.SqlQuery as SqlQuery
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP.TLS
import qualified Network.HostName
import qualified Platform
import qualified Platform.AesonHelpers as AesonHelpers
import qualified Platform.ReporterHelpers as Helpers
import qualified Platform.Timer as Timer
import qualified Prelude

-- | This function takes the root span of a completed request and reports it to
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
--
-- Example usage:
--
-- > settings <- Bugsnag.decode
-- > handler <- Bugsnag.handler settings
-- > Bugsnag.report handler "request-id" span
report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report Handler {http, timer, defaultEvent, apiKey'} requestId span =
  if failed span
    then send http apiKey' (toEvent requestId timer defaultEvent span)
    else Prelude.pure ()

-- | Contextual information this reporter needs to do its work. You can create
-- one using 'handler'.
data Handler = Handler
  { http :: HTTP.Manager,
    timer :: Timer.Timer,
    defaultEvent :: Bugsnag.Event,
    apiKey' :: Log.Secret Bugsnag.ApiKey
  }

-- | Create a 'Handler' for a specified set of 'Settings'. Do this once when
-- your application starts and reuse the 'Handler' you get.
handler :: Settings -> Prelude.IO Handler
handler settings = do
  http <- HTTP.TLS.getGlobalManager
  defaultEvent <- mkDefaultEvent settings
  timer <- Timer.mkTimer
  Prelude.pure
    Handler
      { http,
        timer,
        defaultEvent,
        apiKey' = apiKey settings
      }

send :: HTTP.Manager -> Log.Secret Bugsnag.ApiKey -> Bugsnag.Event -> Prelude.IO ()
send manager key event = do
  result <- Bugsnag.sendEvents manager (Log.unSecret key) [event]
  case result of
    Prelude.Left err -> Exception.throwIO err
    Prelude.Right _ -> Prelude.pure ()

toEvent :: Text -> Timer.Timer -> Bugsnag.Event -> Platform.TracingSpan -> Bugsnag.Event
toEvent requestId timer defaultEvent span =
  rootCause [] emptyCrumbs timer rootEvent span
  where
    rootEvent =
      defaultEvent
        { Bugsnag.event_metaData =
            mergeMetaData
              (Bugsnag.event_metaData defaultEvent)
              (Just rootMetaData)
        }
    rootMetaData =
      [ "response time in ms"
          .= ( Timer.durationInUs span
                 |> Prelude.fromIntegral
                 |> (*) 1e-3 ::
                 Float
             ),
        "megabytes allocated"
          .= ( Prelude.fromIntegral (Platform.allocated span)
                 / (1024 * 1024) ::
                 Float
             ),
        "request id" .= requestId
      ]
        |> Aeson.object
        |> AesonHelpers.singleton "request"

-- | Find the most recently started span that failed. This span is closest to
-- the failure and we'll use the data in it and its parents to build the
-- exception we send to Bugsnag. We'll send information about spans that ran
-- before the root cause span started as breadcrumbs.
rootCause ::
  [Bugsnag.StackFrame] ->
  Crumbs ->
  Timer.Timer ->
  Bugsnag.Event ->
  Platform.TracingSpan ->
  Bugsnag.Event
rootCause frames breadcrumbs timer event span =
  let newFrames =
        case Platform.frame span of
          Nothing -> frames
          Just (name, src) -> toStackFrame name src : frames
      newEvent = decorateEventWithTracingSpanData span event
      childTracingSpans = Platform.children span
   in -- We're not interested in child spans that happened _after_ the root
      -- cause took place. These are not breadcrumbs (leading up to the error)
      -- nor can they have caused the error itself because they happened after.
      -- Since child spans are ordered most-recent first we can keep dropping
      -- child spans until we hit the one where the most recent error happened.
      case Data.List.dropWhile (not << failed) childTracingSpans of
        child : preErrorTracingSpans ->
          rootCause
            newFrames
            ( breadcrumbs
                |> followedBy (addCrumb (startBreadcrumb timer span))
                |> followedBy (addCrumbs timer preErrorTracingSpans)
            )
            timer
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
                breadcrumbs
                  |> followedBy (addCrumbs timer childTracingSpans)
                  |> crumbsAsList
                  |> List.reverse
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
-- Each span can contain child spans requiring us to recurse.
--
-- Our Bugsnag library asks for a value of type `[Bugsnag.Breadcrumb]`, so a
-- list. It's very performant to add single items to the front of a list, but
-- appending two lists is costly. So we want to avoid appending in our
-- breadcrumb collection, because if the span tree gets large we'd be doing a
-- lot of it.
--
-- To help us avoid doing appends we create a helper type `Crumbs a`. The only
-- helper function it exposes for adding a breadcrumb is one that cons that
-- breadcrumb to the front of the list, ensuring no appends take place.
addCrumbs :: Timer.Timer -> [Platform.TracingSpan] -> Crumbs
addCrumbs timer spans =
  case spans of
    [] -> emptyCrumbs
    span : after ->
      addCrumbs timer after
        |> followedBy (addCrumbsForTracingSpan timer span)

addCrumbsForTracingSpan :: Timer.Timer -> Platform.TracingSpan -> Crumbs
addCrumbsForTracingSpan timer span =
  case Platform.children span of
    [] ->
      addCrumb (doBreadcrumb timer span)
    children ->
      addCrumb (startBreadcrumb timer span)
        |> followedBy (addCrumbs timer children)
        |> followedBy (addCrumb (endBreadcrumb timer span))

-- | A type representing a list of breadcrumbs. We're not using just a list
-- directly, because then in constructing the full list of breadcrumbs we'd have
-- to do list appends often, which aren't very efficient. Instead we store a
-- function that describes creation of the eventual list of breadcrumbs from an
-- initially empty list.
newtype Crumbs = Crumbs ([Bugsnag.Breadcrumb] -> [Bugsnag.Breadcrumb])

emptyCrumbs :: Crumbs
emptyCrumbs = Crumbs identity

-- | Combine breadcrumbs, placing one set after the other.
--
--     earlyCrumbs
--       |> followedBy laterCrumbs
followedBy :: Crumbs -> Crumbs -> Crumbs
followedBy (Crumbs f) (Crumbs g) = Crumbs (f << g)

crumbsAsList :: Crumbs -> [Bugsnag.Breadcrumb]
crumbsAsList (Crumbs f) = f []

addCrumb :: Bugsnag.Breadcrumb -> Crumbs
addCrumb crumb = Crumbs (crumb :)

endBreadcrumb :: Timer.Timer -> Platform.TracingSpan -> Bugsnag.Breadcrumb
endBreadcrumb timer span =
  Bugsnag.defaultBreadcrumb
    { Bugsnag.breadcrumb_name = "Finished: " ++ Platform.name span,
      Bugsnag.breadcrumb_type = Bugsnag.logBreadcrumbType,
      Bugsnag.breadcrumb_timestamp = Timer.toISO8601 timer (Platform.finished span)
    }

startBreadcrumb :: Timer.Timer -> Platform.TracingSpan -> Bugsnag.Breadcrumb
startBreadcrumb timer span =
  (doBreadcrumb timer span)
    { Bugsnag.breadcrumb_name = "Starting: " ++ Platform.name span
    }

doBreadcrumb :: Timer.Timer -> Platform.TracingSpan -> Bugsnag.Breadcrumb
doBreadcrumb timer span =
  let defaultBreadcrumb =
        Bugsnag.defaultBreadcrumb
          { Bugsnag.breadcrumb_name = Platform.name span,
            Bugsnag.breadcrumb_type = Bugsnag.manualBreadcrumbType,
            Bugsnag.breadcrumb_timestamp = Timer.toISO8601 timer (Platform.started span),
            Bugsnag.breadcrumb_metaData = stackFrameMetaData ++ durationMetaData
          }
      stackFrameMetaData =
        case Platform.frame span of
          Nothing -> Nothing
          Just (_, frame) ->
            Stack.srcLocFile frame ++ ":" ++ Prelude.show (Stack.srcLocStartLine frame)
              |> Text.fromList
              |> HashMap.singleton "stack frame"
              |> Just
      durationMetaData =
        Just
          ( HashMap.singleton
              "duration in milliseconds"
              ( Timer.durationInUs span
                  |> Prelude.fromIntegral
                  |> (*) 1e-3
                  |> Text.fromFloat
              )
          )
   in case Platform.details span of
        Nothing -> defaultBreadcrumb
        Just details -> customizeBreadcrumb span details defaultBreadcrumb

customizeBreadcrumb :: Platform.TracingSpan -> Platform.SomeTracingSpanDetails -> Bugsnag.Breadcrumb -> Bugsnag.Breadcrumb
customizeBreadcrumb span details breadcrumb =
  details
    |> Platform.renderTracingSpanDetails
      [ Platform.Renderer (outgoingHttpRequestAsBreadcrumb breadcrumb),
        Platform.Renderer (sqlQueryAsBreadcrumb breadcrumb),
        Platform.Renderer (redisQueryAsBreadcrumb breadcrumb),
        Platform.Renderer (logAsBreadcrumb span breadcrumb),
        Platform.Renderer (unknownAsBreadcrumb breadcrumb)
      ]
    |> Maybe.withDefault breadcrumb

outgoingHttpRequestAsBreadcrumb :: Bugsnag.Breadcrumb -> HttpRequest.Outgoing -> Bugsnag.Breadcrumb
outgoingHttpRequestAsBreadcrumb breadcrumb (HttpRequest.Outgoing details) =
  breadcrumb
    { Bugsnag.breadcrumb_type = Bugsnag.requestBreadcrumbType,
      Bugsnag.breadcrumb_metaData =
        Bugsnag.breadcrumb_metaData breadcrumb ++ Just (Helpers.toHashMap details)
    }

sqlQueryAsBreadcrumb :: Bugsnag.Breadcrumb -> SqlQuery.Details -> Bugsnag.Breadcrumb
sqlQueryAsBreadcrumb breadcrumb details =
  breadcrumb
    { Bugsnag.breadcrumb_type = Bugsnag.requestBreadcrumbType,
      Bugsnag.breadcrumb_metaData =
        Bugsnag.breadcrumb_metaData breadcrumb ++ Just (Helpers.toHashMap details)
    }

redisQueryAsBreadcrumb :: Bugsnag.Breadcrumb -> RedisCommands.Details -> Bugsnag.Breadcrumb
redisQueryAsBreadcrumb breadcrumb details =
  breadcrumb
    { Bugsnag.breadcrumb_type = Bugsnag.requestBreadcrumbType,
      Bugsnag.breadcrumb_metaData =
        Bugsnag.breadcrumb_metaData breadcrumb ++ Just (Helpers.toHashMap details)
    }

logAsBreadcrumb :: Platform.TracingSpan -> Bugsnag.Breadcrumb -> Log.LogContexts -> Bugsnag.Breadcrumb
logAsBreadcrumb span breadcrumb details =
  breadcrumb
    { Bugsnag.breadcrumb_type =
        if List.isEmpty (Platform.children span)
          then Bugsnag.logBreadcrumbType
          else Bugsnag.processBreadcrumbType,
      Bugsnag.breadcrumb_metaData =
        Bugsnag.breadcrumb_metaData breadcrumb ++ Just (Helpers.toHashMap details)
    }

unknownAsBreadcrumb :: Bugsnag.Breadcrumb -> Platform.SomeTracingSpanDetails -> Bugsnag.Breadcrumb
unknownAsBreadcrumb breadcrumb details =
  breadcrumb
    { Bugsnag.breadcrumb_type = Bugsnag.manualBreadcrumbType,
      Bugsnag.breadcrumb_metaData =
        Bugsnag.breadcrumb_metaData breadcrumb ++ Just (Helpers.toHashMap details)
    }

decorateEventWithTracingSpanData :: Platform.TracingSpan -> Bugsnag.Event -> Bugsnag.Event
decorateEventWithTracingSpanData span event =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (renderIncomingHttpRequest event),
            Platform.Renderer (renderLog event),
            Platform.Renderer (renderRemainingTracingSpanDetails span event)
          ]
      )
    |> Maybe.withDefault event

renderRemainingTracingSpanDetails :: Platform.TracingSpan -> Bugsnag.Event -> Platform.SomeTracingSpanDetails -> Bugsnag.Event
renderRemainingTracingSpanDetails span event details =
  event
    { Bugsnag.event_metaData =
        Aeson.toJSON details
          |> AesonHelpers.singleton (Platform.name span)
          |> Just
          |> (++) (Bugsnag.event_metaData event)
    }

renderLog :: Bugsnag.Event -> Log.LogContexts -> Bugsnag.Event
renderLog event details =
  event
    { Bugsnag.event_metaData =
        Aeson.toJSON details
          |> AesonHelpers.singleton "custom"
          |> KeyMap.unionWith
            mergeJson
            (Bugsnag.event_metaData event |> Maybe.withDefault KeyMap.empty)
          |> Just
    }

mergeJson :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeJson (Aeson.Object x) (Aeson.Object y) = Aeson.Object (KeyMap.unionWith mergeJson x y)
mergeJson _ last = last

mergeMetaData ::
  Maybe Aeson.Object ->
  Maybe Aeson.Object ->
  Maybe Aeson.Object
mergeMetaData Nothing x = x
mergeMetaData x Nothing = x
mergeMetaData (Just x) (Just y) = Just (KeyMap.unionWith mergeJson x y)

renderIncomingHttpRequest ::
  Bugsnag.Event ->
  HttpRequest.Incoming ->
  Bugsnag.Event
renderIncomingHttpRequest event (HttpRequest.Incoming request) =
  event
    { Bugsnag.event_context = HttpRequest.endpoint request,
      Bugsnag.event_request =
        Just
          Bugsnag.defaultRequest
            { Bugsnag.request_httpMethod = HttpRequest.method request,
              Bugsnag.request_headers =
                HttpRequest.headers request
                  |> Dict.toList
                  |> HashMap.fromList
                  |> Just
            },
      -- Extra request data that Bugsnag doesn't ask for in its API, but which
      -- we can make appear on the 'request' tab anyway by logging it on the
      -- 'request' key of the event metadata.
      Bugsnag.event_metaData =
        mergeMetaData
          (Bugsnag.event_metaData event)
          ( [ Key.fromText "endpoint" .= HttpRequest.endpoint request,
              Key.fromText "http version" .= HttpRequest.httpVersion request,
              Key.fromText "response status" .= HttpRequest.status request,
              Key.fromText "path" .= HttpRequest.path request,
              Key.fromText "query string" .= HttpRequest.queryString request
            ]
              |> Aeson.object
              |> AesonHelpers.singleton "request"
              |> Just
          )
    }

failed :: Platform.TracingSpan -> Bool
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> False
    Platform.Failed -> True
    Platform.FailedWith _ -> True

toException :: [Bugsnag.StackFrame] -> Platform.TracingSpan -> Bugsnag.Exception
toException frames span =
  case Platform.succeeded span of
    Platform.Succeeded -> Bugsnag.defaultException
    Platform.Failed ->
      Bugsnag.defaultException
        { Bugsnag.exception_errorClass = Platform.name span,
          Bugsnag.exception_stacktrace = frames
        }
    Platform.FailedWith (Exception.SomeException exception) ->
      Bugsnag.defaultException
        { Bugsnag.exception_errorClass = typeName exception,
          Bugsnag.exception_stacktrace = frames,
          Bugsnag.exception_message =
            Exception.displayException exception
              |> Text.fromList
              |> Just
        }

toStackFrame :: Text -> Stack.SrcLoc -> Bugsnag.StackFrame
toStackFrame functionName frame =
  Bugsnag.defaultStackFrame
    { Bugsnag.stackFrame_file = Text.fromList (Stack.srcLocFile frame),
      Bugsnag.stackFrame_lineNumber = Stack.srcLocStartLine frame,
      Bugsnag.stackFrame_columnNumber = Just (Stack.srcLocStartCol frame),
      Bugsnag.stackFrame_method = functionName,
      Bugsnag.stackFrame_inProject = Just True
    }

typeName :: forall a. Typeable.Typeable a => a -> Text
typeName _ =
  Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy a)
    |> Prelude.show
    |> Text.fromList

-- | Configuration settings for this reporter. A value of this type can be read
-- from the environment using the 'decoder' function.
data Settings = Settings
  { -- | The Bugsnag API key to use. This determines which Bugsnag project your
    -- errors will end up in.
    --
    -- [@environment variable@] BUGSNAG_API_KEY
    -- [@default value@] *****
    apiKey :: Log.Secret Bugsnag.ApiKey,
    -- | The name of this application. This will be attached to all bugsnag
    -- reports.
    --
    -- [@environment variable@] LOG_ROOT_NAMESPACE
    -- [@default value@] your-application-name-here
    appName :: Namespace,
    -- | The environment this application is running in. This will be attached
    -- to all bugsnage reports.
    --
    -- [@environment variable@] ENVIRONMENT
    -- [@default value@] development
    appEnvironment :: Environment
  }

-- | Read 'Settings' from environment variables. Default variables will be used
-- in case no environment variable is set for an option.
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
          { Bugsnag.device_hostname = Just (Text.fromList hostname)
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
