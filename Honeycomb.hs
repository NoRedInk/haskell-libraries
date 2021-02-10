{-# LANGUAGE TransformListComp #-}

-- | Honeycomb
--
-- This reporter logs execution to https://honeycomb.io.
--
-- It does some custom stuff compared to other reporters:
--
-- * Sample requests based on
--   * Response type
--   * Endpoint (log fewer healthchecks)
-- * Calculates statistics over child spans per type
-- * Enriches child spans with data to help track problems
--   * Server (Pod, in k8s' case) hostname
--   * Http endpoint
module Observability.Honeycomb
  ( report,
    handler,
    Handler,
    Settings (..),
    decoder,
    -- for tests
    toBatchEvents,
    enrich,
    CommonFields (..),
    BatchEvent (..),
    Span (..),
  )
where

import qualified Conduit
import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Environment
import GHC.Exts (groupWith, the)
import qualified Http
import qualified List
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified Network.HostName
import NriPrelude
import Observability.Helpers (toHashMap)
import Observability.Timer (Timer, toISO8601)
import qualified Platform
import qualified Redis
import qualified System.Random as Random
import qualified Task
import qualified Text as NriText
import qualified Prelude

batchApiEndpoint :: Text -> Text
batchApiEndpoint datasetName = "https://api.honeycomb.io/1/batch/" ++ datasetName

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span = do
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
        deriveSampleRate span (handler_fractionOfSuccessRequestsLogged handler')
      Platform.Failed -> Prelude.pure (False, 1)
      Platform.FailedWith _ -> Prelude.pure (False, 1)
  hostname' <- Network.HostName.getHostName
  uuid <- Data.UUID.V4.nextRandom
  let commonFields =
        CommonFields
          (handler_timer handler')
          (handler_serviceName handler')
          (handler_environment handler')
          -- Don't use requestId if we don't do Distributed Tracing
          -- Else, it will create traces with no parent sharing the same TraceId
          -- Which makes Honeycomb's UI confused
          (Data.UUID.toText uuid)
          (Data.Text.pack hostname')
  let (_, events) = toBatchEvents commonFields sampleRate Nothing 0 span
  let enrichedEvents = enrich events
  let body = Http.jsonBody enrichedEvents
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

getRootSpanEndpoint :: Platform.TracingSpan -> Maybe Text
getRootSpanEndpoint rootSpan =
  Platform.details rootSpan
    |> Maybe.andThen (Platform.renderTracingSpanDetails [Platform.Renderer Monitoring.endpoint])

getBatchEventEndpoint :: BatchEvent -> Maybe Text
getBatchEventEndpoint event =
  event
    |> batchevent_data
    |> details
    |> Maybe.andThen (Platform.renderTracingSpanDetails [Platform.Renderer Monitoring.endpoint])

deriveSampleRate :: Platform.TracingSpan -> Float -> Prelude.IO (Bool, Int)
deriveSampleRate rootSpan fractionOfSuccessRequestsLogged' = do
  let isNonAppEndpoint =
        case getRootSpanEndpoint rootSpan of
          Nothing -> False
          Just endpoint -> List.any (endpoint ==) ["GET /health/readiness", "GET /metrics", "GET /health/liveness"]
  let probability =
        if isNonAppEndpoint
          then --
          -- We have 2678400 seconds in a month
          -- We health-check once per second per Pod in Haskell
          -- We have 2-3 pods at idle per service
          -- We have some 5 services
          -- We have up to 4 environments (staging, prod, demo, backyard)
          --
          -- Healthchecks would be 107,136,000 / sampleRate traces per month
          --
          -- But we also don't wanna never log them, who knows, they might cause
          -- problems
          --
          -- High sample rates might make honeycomb make ridiculous assumptions
          -- about the actual request rate tho. Adjust if that's the case.
            1 / 500
          else fractionOfSuccessRequestsLogged'
  roll <- Random.randomRIO (0, 1)
  Prelude.pure (roll > probability, round (1 / probability))

toBatchEvents :: CommonFields -> Int -> Maybe SpanId -> Int -> Platform.TracingSpan -> (Int, [BatchEvent])
toBatchEvents commonFields sampleRate parentSpanId spanIndex span = do
  let thisSpansId = SpanId (common_requestId commonFields ++ "-" ++ NriText.fromInt spanIndex)
  let (lastSpanIndex, children) = Data.List.mapAccumL (toBatchEvents commonFields sampleRate (Just thisSpansId)) (spanIndex + 1) (Platform.children span)
  let duration = Platform.finished span - Platform.started span |> Platform.inMicroseconds
  let timestamp = toISO8601 (common_timer commonFields) (Platform.started span)
  let isError = case Platform.succeeded span of
        Platform.Succeeded -> False
        Platform.Failed -> True
        Platform.FailedWith _ -> True
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
            failed = isError,
            enrichedData = [],
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

enrich :: [BatchEvent] -> [BatchEvent]
enrich [] = []
enrich [x] = [x]
-- Ensure we have a root and a rest to enrich
enrich (root : rest) =
  let maybeEndpoint = getBatchEventEndpoint root
      -- Grab all durations by span name (e.g. "MySQL Query") while tagging them
      -- with the root's endpoint, and shaving some excess columns in `details`
      crunch x (acc, xs) =
        let duration = x |> batchevent_data |> durationMs
            updateFn (Just durations) = Just (duration : durations)
            updateFn Nothing = Just [duration]
            span = batchevent_data x
            key = name span
            acc' = HashMap.alter updateFn key acc
            x' = case maybeEndpoint of
              Just endpoint ->
                x
                  { batchevent_data =
                      span
                        { enrichedData =
                            ("details.endpoint", endpoint) : enrichedData span,
                          details = span |> details |> deNoise
                        }
                  }
              Nothing ->
                x
                  { batchevent_data =
                      span {details = span |> details |> deNoise}
                  }
         in (acc', x' : xs)
      -- chose foldr to preserve order, not super important tho
      (durationsByName, newRest) = List.foldr crunch (HashMap.empty, []) rest
      stats (name, durations) =
        let total = List.sum durations
            calls = List.length durations
            average = total / Prelude.fromIntegral calls
            saneName = name |> NriText.toLower |> NriText.replace " " "_"
         in [ ("stats.total_time_ms." ++ saneName, NriText.fromFloat total),
              ("stats.average_time_ms." ++ saneName, NriText.fromFloat average),
              ("stats.count." ++ saneName, NriText.fromInt calls)
            ]
      perSpanNameStats = List.concatMap stats (HashMap.toList durationsByName)
      rootSpan = batchevent_data root
      rootSpanWithStats = rootSpan {enrichedData = perSpanNameStats}
      newRoot = root {batchevent_data = rootSpanWithStats}
   in newRoot : newRest

-- Some of our TracingSpanDetails instances create a lot of noise in the column
-- space of Honeycomb.
--
-- If we ever hit 10k unique column names (and we were past the thousands when
-- this code was introduced) Honeycomb will stop accepting traces from us.
--
-- "Unique column names" means different column names that Honeycomb has seen us
-- report on a span.
--
-- It is manual labor, but we must ensure our TracingSpanDetails don't serialize
-- to an unbounded number of column names.
deNoise :: Maybe Platform.SomeTracingSpanDetails -> Maybe Platform.SomeTracingSpanDetails
deNoise details =
  details
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer deNoiseLog,
            Platform.Renderer deNoiseRedis
          ]
      )

-- LogContext is an unbounded list of key value pairs with possibly nested
-- stuff in them. Aeson flatens the nesting, so:
--
-- {error: [{quiz: [{"some-quiz-id": "some context"}]}]}
--
-- becomes
--
-- {"error.0.quiz.0.some-quiz-id": "some context"}
--
-- - With "some-quiz-id" in the example above, we have an unbounded number of
--   unique columns.
-- - With long lists, the `.0` parts helps boost our unique column name growth.
--
-- We don't need Honeycomb to collect rich error information.
-- That's what we pay Bugsnag for.
deNoiseLog :: Log.LogContexts -> Platform.SomeTracingSpanDetails
deNoiseLog context@(Log.LogContexts contexts) =
  let tojson thing = case thing |> Aeson.toJSON of
                      Aeson.String txt -> txt
                      value -> value |> Aeson.encode |> Lazy.Encoding.decodeUtf8 |> LazyText.toStrict
      deets = if List.length contexts > 5 then
                HashMap.singleton "context" (tojson context)
              else
                contexts
                |> map (\(Log.Context key val) -> (key, tojson val))
                |> HashMap.fromList
  in Platform.toTracingSpanDetails (GenericTracingSpanDetails deets)

-- Redis creates one column per command for batches
-- Let's trace what matters:
-- - How many of each command
-- - The full blob in a single column
-- - The rest of our Info record
deNoiseRedis :: Redis.Info -> Platform.SomeTracingSpanDetails
deNoiseRedis redisInfo =
  let commandsCount =
        redisInfo
          |> Redis.infoCommands
          |> List.filterMap (NriText.words >> List.head)
          |> (\x -> [(the key ++ ".count", key |> List.length |> NriText.fromInt) | key <- x, then group by key using groupWith])
      fullBlob =
        redisInfo
          |> Redis.infoCommands
          |> NriText.join "\n"
   in HashMap.fromList
        ( ("commands", fullBlob) :
          ("infoHost", Redis.infoHost redisInfo) :
          ("infoPort", Redis.infoPort redisInfo) : commandsCount
        )
        |> GenericTracingSpanDetails
        |> Platform.toTracingSpanDetails

newtype GenericTracingSpanDetails = GenericTracingSpanDetails (HashMap.HashMap Text Text)
  deriving (Generic)

instance Aeson.ToJSON GenericTracingSpanDetails

instance Platform.TracingSpanDetails GenericTracingSpanDetails

data BatchEvent = BatchEvent
  { batchevent_time :: Text,
    batchevent_data :: Span,
    batchevent_samplerate :: Int
  }
  deriving (Generic, Show)

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
    failed :: Bool,
    enrichedData :: [(Text, Text)],
    details :: Maybe Platform.SomeTracingSpanDetails
  }
  deriving (Generic, Show)

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
            "allocated_bytes" .= allocatedBytes span,
            "hostname" .= hostname span,
            "failed" .= failed span
          ]
        detailsPairs =
          span
            |> details
            |> toHashMap
            |> HashMap.mapWithKey (\key value -> ("details." ++ key) .= value)
            |> HashMap.elems
        enrichedData' = map (\(key, value) -> key .= value) (enrichedData span)
     in Aeson.object (basePairs ++ detailsPairs ++ enrichedData')

newtype SpanId = SpanId Text
  deriving (Aeson.ToJSON, Show)

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
