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
import qualified Data.List
import qualified Data.Proxy as Proxy
import qualified Data.Text
import qualified Data.Typeable as Typeable
import qualified Environment
import qualified GHC.Stack as Stack
import qualified Health
import qualified Http
import qualified Log
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client
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
  let send' = send http settings
  case Platform.succeeded span of
    Platform.Succeeded -> Prelude.pure ()
    Platform.Failed -> send' (toEvent span)
    Platform.FailedWith _ -> send' (toEvent span)

send :: Http.Handler -> Settings -> Bugsnag.Event -> Prelude.IO ()
send http settings event = do
  log <- Platform.silentHandler
  Http.withThirdPartyIO log http <| \manager -> do
    -- Logging to Bugsnag might fail, but if it does we can't very well send the
    -- error to Bugsnag. This is the end of the line, these errors disappear
    -- into the aether.
    _ <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) [event]
    Prelude.pure ()

toEvent :: Platform.Span -> Bugsnag.Event
toEvent span =
  Bugsnag.defaultEvent
    { Bugsnag.event_exceptions = [rootCause [] span],
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

-- | Find the most recently started span that failed. This span is closest to
-- the failure and we'll use the data in it and its parents to build the
-- exception we send to Bugsnag. We'll send information about spans that ran
-- before the root cause span started as breadcrumbs.
rootCause :: [Bugsnag.StackFrame] -> Platform.Span -> Bugsnag.Exception
rootCause frames span =
  let newFrames =
        case Platform.frame span of
          Nothing -> frames
          Just (name, src) -> toStackFrame name src : frames
   in case Data.List.find failed (Platform.children span) of
        Nothing -> toException newFrames span
        Just child -> rootCause newFrames child

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

newtype Settings
  = Settings
      { apiKey :: Log.Secret Bugsnag.ApiKey
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap apiKeyDecoder

apiKeyDecoder :: Environment.Decoder (Log.Secret Bugsnag.ApiKey)
apiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "BUGSNAG_API_KEY",
        Environment.description = "The API key of the Bugsnag project we should send items too.",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> map Bugsnag.apiKey |> Environment.secret)

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
    Prelude.Left _err ->
      Prelude.pure "no revision file found"
    Prelude.Right version -> Prelude.pure <| Data.Text.pack version
