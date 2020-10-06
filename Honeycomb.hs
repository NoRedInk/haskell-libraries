-- | Reporting to a file.
--
-- This reporter logs debugging information about completed requests to a file
-- or `stdout` (in that case, pass in `/dev/stdout` as the file to log to).
--
-- Every line this reporter logs is a JSON string. This 'structured logging'
-- output is optimized for external logging platforms that display these logs in
-- a pretty UI.
--
-- This logger supports sampling of successful requests, to help us save money.
--
-- This reporter is based on Katip for historical reasons. Katip used to run all
-- of what is now called 'reporting' in our apps, not it's just the file logger.
-- We maybe be able to remove it entirely at this point.
module Observability.Honeycomb
  ( report,
    handler,
    Handler,
    Settings (..),
    decoder,
  )
where

import Cherry.Prelude
import qualified Conduit
-- import qualified Data.Text

-- import qualified GHC.Stack as Stack

-- import qualified Language.Haskell.TH as TH
-- import qualified Maybe

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Debug
import qualified Environment
import qualified Http
import qualified List
import qualified Log
import Observability.Timer (Timer, toISO8601)
import qualified Platform
import qualified Task
import qualified Prelude

batchApiEndpoint :: Text -> Text
batchApiEndpoint datasetName = "https://api.honeycomb.io/1/batch/" ++ datasetName

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' requestId span = do
  spans <- toBatchEvents handler' requestId Nothing span
  let body = Http.jsonBody spans
  silentHandler' <- Platform.silentHandler
  let datasetName = (handler_serviceName handler') ++ "-" ++ (handler_environment handler')
  let url = batchApiEndpoint datasetName
  let requestSettings = Http.Settings
        { Http._method = "POST",
          Http._headers = [("X-Honeycomb-Team", Encoding.encodeUtf8 <| Log.unSecret <| handler_honeycombApiKey handler')],
          Http._url = url,
          Http._body = body,
          Http._timeout = Nothing,
          Http._expect = Http.expectText
        }
  result <-
    Http.request (handler_http handler') requestSettings
      |> Task.attempt (silentHandler')
  case result of
    Ok body' -> do
      Prelude.putStrLn "OK"
      Prelude.putStrLn <| Text.unpack body'
      Prelude.pure ()
    Err err -> do
      _ <- Prelude.fail <| Prelude.show err
      Prelude.putStrLn "ERROR"
      Prelude.pure ()

toBatchEvents :: Handler -> Text -> Maybe SpanId -> Platform.TracingSpan -> Prelude.IO [BatchEvent]
toBatchEvents handler' requestId parentSpanId span = do
  thisSpansId <- map SpanId nextRandom
  children <- Prelude.traverse (toBatchEvents handler' requestId (Just thisSpansId)) (Platform.children span)
  let duration = (Platform.finished span) - (Platform.started span) |> Platform.inMicroseconds
  let timestamp = toISO8601 (handler_timer handler') (Platform.started span)
  let hcSpan = Span
        { name = Platform.name span,
          spanId = thisSpansId,
          parentId = parentSpanId,
          traceId = requestId,
          serviceName = handler_serviceName handler',
          environment = handler_environment handler',
          durationMs = (Prelude.fromIntegral duration) / 1000,
          details = Platform.details span
        }
  Prelude.pure <| BatchEvent
    { batchevent_time = timestamp,
      batchevent_data = hcSpan
    }
    : (List.concat children)

data BatchEvent
  = BatchEvent
      { batchevent_time :: Text,
        batchevent_data :: Span
      }
  deriving (Generic)

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { -- Drop the batchevent_ prefix
      Aeson.fieldLabelModifier = List.drop 1 << Prelude.dropWhile ('_' /=)
    }

instance Aeson.ToJSON BatchEvent where
  toJSON = Aeson.genericToJSON options

data Span
  = Span
      { name :: Text,
        spanId :: SpanId,
        parentId :: Maybe SpanId,
        traceId :: Text,
        serviceName :: Text,
        environment :: Text,
        durationMs :: Float,
        details :: Maybe Platform.SomeTracingSpanDetails
      }
  deriving (Generic)

instance Aeson.ToJSON Span where
  toJSON span =
    -- Use honeycomb's field names, srcs:
    -- https://docs.honeycomb.io/getting-data-in/tracing/send-trace-data/#manual-tracing
    -- https://docs.honeycomb.io/working-with-your-data/managing-your-data/definitions/
    let basePairs =
          [ "name" .= name span,
            "trace.span_id" .= spanId span,
            "trace.parent_id" .= parentId span,
            "trace.trace_id" .= traceId span,
            "service_name" .= serviceName span,
            "duration_ms" .= durationMs span
          ]
        detailsPairs =
          span
            |> details
            |> toHashMap
            |> HashMap.mapWithKey (\key value -> ("details." ++ key) .= value)
            |> HashMap.elems
     in Aeson.object (basePairs ++ detailsPairs)

newtype SpanId = SpanId UUID.UUID
  deriving (Aeson.ToJSON)

data Handler
  = Handler
      { -- | A bit of state that can be used to turn the clock values attached
        -- to spans into real timestamps.
        handler_timer :: Timer,
        handler_http :: Http.Handler,
        handler_serviceName :: Text,
        handler_environment :: Text,
        handler_honeycombApiKey :: Log.Secret Text
      }

handler :: Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  http <- Http.handler
  Prelude.pure Handler
    { handler_timer = timer,
      handler_http = http,
      handler_serviceName = appName settings,
      handler_environment = appEnvironment settings,
      handler_honeycombApiKey = honeycombApiKey settings
    }

data Settings
  = Settings
      { appName :: Text,
        appEnvironment :: Text,
        honeycombApiKey :: Log.Secret Text
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap appNameDecoder
    |> andMap appEnvironmentDecoder
    |> andMap honeycombApiKeyDecoder

honeycombApiKeyDecoder :: Environment.Decoder (Log.Secret Text)
honeycombApiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_API_KEY",
        Environment.description = "The API key for Honeycomb",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> Environment.secret)

appEnvironmentDecoder :: Environment.Decoder (Text)
appEnvironmentDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ENVIRONMENT",
        Environment.description = "Environment to display in logs.",
        Environment.defaultValue = "development"
      }
    (Environment.text)

appNameDecoder :: Environment.Decoder Text
appNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ROOT_NAMESPACE",
        Environment.description = "Root of the log namespace. This should be the name of the application.",
        Environment.defaultValue = "your-application-name-here"
      }
    (Environment.text)
