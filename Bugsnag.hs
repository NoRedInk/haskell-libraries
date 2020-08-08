module Observability.Bugsnag
  ( logger,
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

logger :: Http.Handler -> Settings -> Platform.Span -> Prelude.IO ()
logger http settings span = do
  let send' = send http settings
  case Platform.succeeded span of
    Platform.Succeeded -> Prelude.pure ()
    Platform.Failed -> send' (toEvent Nothing span)
    Platform.FailedWith err -> send' (toEvent (Just err) span)

send :: Http.Handler -> Settings -> Bugsnag.Event -> Prelude.IO ()
send http settings event = do
  log <- Platform.silentHandler
  Http.withThirdPartyIO log http <| \manager -> do
    -- Logging to Bugsnag might fail, but if it does we can't very well send the
    -- error to Bugsnag. This is the end of the line, these errors disappear
    -- into the aether.
    _ <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) [event]
    Prelude.pure ()

toEvent :: Maybe Exception.SomeException -> Platform.Span -> Bugsnag.Event
toEvent _ span =
  Bugsnag.defaultEvent
    { Bugsnag.event_exceptions = [rootCause [] span]
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
