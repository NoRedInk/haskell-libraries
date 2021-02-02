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
    -- for tests
    toBatchEvents,
    CommonFields (..),
  )
where

import qualified Conduit
import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Encoding as Encoding
import qualified Environment
import qualified Http
import qualified List
import qualified Log
import qualified Network.HostName
import NriPrelude
import Observability.Helpers (toHashMap)
import Observability.Timer (Timer, toISO8601)
import qualified Platform
import qualified System.Random as Random
import qualified Task
import qualified Text as NriText
import qualified Prelude

batchApiEndpoint :: Text -> Text
batchApiEndpoint datasetName = "https://api.honeycomb.io/1/batch/" ++ datasetName

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' requestId span = do
  -- This is an initial implementation of sampling, based on
  -- https://docs.honeycomb.io/working-with-your-data/best-practices/sampling/
  -- using Dynamic Sampling based on whether the request was successful or not.
  --
  -- We can go further and:
  --
  --  * Not sample requests above a certain configurable threshold, to replicate
  --    NewRelic's slow request tracing.
  --  * Apply some sampling rate to errors
  --  * Apply different sample rates depending on traffic (easiest approximation
  --    is basing it off of time of day) so we sample less at low traffic

  (skipLogging, sampleRate) <-
    case Platform.succeeded span of
      Platform.Succeeded -> do
        roll <- Random.randomRIO (0, 1)
        let fractionOfSuccessRequestsLogged' = handler_fractionOfSuccessRequestsLogged handler'
        Prelude.pure (roll > fractionOfSuccessRequestsLogged', round (1 / fractionOfSuccessRequestsLogged'))
      Platform.Failed -> Prelude.pure (False, 1)
      Platform.FailedWith _ -> Prelude.pure (False, 1)
  hostname' <- Network.HostName.getHostName
  let commonFields =
        CommonFields
          (handler_timer handler')
          (handler_serviceName handler')
          (handler_environment handler')
          requestId
          (Data.Text.pack hostname')
  let (_, spans) = toBatchEvents commonFields sampleRate Nothing 0 span
  let body = Http.jsonBody spans
  silentHandler' <- Platform.silentHandler
  let datasetName = handler_serviceName handler' ++ "-" ++ handler_environment handler'
  let url = batchApiEndpoint datasetName
  let requestSettings =
        Http.Settings
          { Http._method = "POST",
            Http._headers = [("X-Honeycomb-Team", Encoding.encodeUtf8 <| Log.unSecret <| handler_honeycombApiKey handler')],
            Http._url = url,
            Http._body = body,
            Http._timeout = Nothing,
            Http._expect = Http.expectText
          }
  Http.request (handler_http handler') requestSettings
    |> Task.attempt silentHandler'
    |> map (\_ -> ())
    |> unless skipLogging

toBatchEvents :: CommonFields -> Int -> Maybe SpanId -> Int -> Platform.TracingSpan -> (Int, [BatchEvent])
toBatchEvents commonFields sampleRate parentSpanId spanIndex span = do
  let thisSpansId = SpanId (common_requestId commonFields ++ "-" ++ NriText.fromInt spanIndex)
  let (lastSpanIndex, children) = Data.List.mapAccumL (toBatchEvents commonFields sampleRate (Just thisSpansId)) (spanIndex + 1) (Platform.children span)
  let duration = Platform.finished span - Platform.started span |> Platform.inMicroseconds
  let timestamp = toISO8601 (common_timer commonFields) (Platform.started span)
  let hcSpan =
        Span
          { name = Platform.name span,
            spanId = thisSpansId,
            parentId = parentSpanId,
            traceId = common_requestId commonFields,
            serviceName = common_serviceName commonFields,
            environment = common_environment commonFields,
            durationMs = Prelude.fromIntegral duration / 1000,
            allocatedBytes = Platform.allocated span,
            hostname = common_hostname commonFields,
            details = Platform.details span
          }
  ( lastSpanIndex,
    BatchEvent
      { batchevent_time = timestamp,
        batchevent_data = hcSpan,
        batchevent_samplerate = sampleRate
      } :
    List.concat children
    )

data BatchEvent = BatchEvent
  { batchevent_time :: Text,
    batchevent_data :: Span,
    batchevent_samplerate :: Int
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

data CommonFields = CommonFields
  { common_timer :: Timer,
    common_serviceName :: Text,
    common_environment :: Text,
    common_requestId :: Text,
    common_hostname :: Text
  }

data Span = Span
  { name :: Text,
    spanId :: SpanId,
    parentId :: Maybe SpanId,
    traceId :: Text,
    serviceName :: Text,
    environment :: Text,
    durationMs :: Float,
    allocatedBytes :: Int,
    hostname :: Text,
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
            "duration_ms" .= durationMs span,
            "allocated_bytes" .= allocatedBytes span
          ]
        detailsPairs =
          span
            |> details
            |> toHashMap
            |> HashMap.mapWithKey (\key value -> ("details." ++ key) .= value)
            |> HashMap.elems
     in Aeson.object (basePairs ++ detailsPairs)

newtype SpanId = SpanId Text
  deriving (Aeson.ToJSON)

data Handler = Handler
  { -- | A bit of state that can be used to turn the clock values attached
    -- to spans into real timestamps.
    handler_timer :: Timer,
    handler_http :: Http.Handler,
    handler_serviceName :: Text,
    handler_environment :: Text,
    handler_honeycombApiKey :: Log.Secret Text,
    handler_fractionOfSuccessRequestsLogged :: Float
  }

handler :: Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  http <- Http.handler
  Prelude.pure
    Handler
      { handler_timer = timer,
        handler_http = http,
        handler_serviceName = appName settings,
        handler_environment = appEnvironment settings,
        handler_honeycombApiKey = honeycombApiKey settings,
        handler_fractionOfSuccessRequestsLogged = fractionOfSuccessRequestsLogged settings
      }

data Settings = Settings
  { appName :: Text,
    appEnvironment :: Text,
    honeycombApiKey :: Log.Secret Text,
    fractionOfSuccessRequestsLogged :: Float
  }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap appNameDecoder
    |> andMap appEnvironmentDecoder
    |> andMap honeycombApiKeyDecoder
    |> andMap fractionOfSuccessRequestsLoggedDecoder

honeycombApiKeyDecoder :: Environment.Decoder (Log.Secret Text)
honeycombApiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_API_KEY",
        Environment.description = "The API key for Honeycomb",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> Environment.secret)

appEnvironmentDecoder :: Environment.Decoder Text
appEnvironmentDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ENVIRONMENT",
        Environment.description = "Environment to display in logs.",
        Environment.defaultValue = "development"
      }
    Environment.text

appNameDecoder :: Environment.Decoder Text
appNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "LOG_ROOT_NAMESPACE",
        Environment.description = "Root of the log namespace. This should be the name of the application.",
        Environment.defaultValue = "your-application-name-here"
      }
    Environment.text

fractionOfSuccessRequestsLoggedDecoder :: Environment.Decoder Float
fractionOfSuccessRequestsLoggedDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_FRACTION_OF_SUCCESS_REQUESTS_LOGGED",
        Environment.description = "The fraction of successful requests logged. Defaults to logging all successful requests.",
        Environment.defaultValue = "1"
      }
    Environment.float
