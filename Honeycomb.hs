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
    sampleRateForDuration,
  )
where

import qualified Conduit
import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Environment
import GHC.Exts (groupWith, the)
import qualified GHC.Stack as Stack
import qualified Http
import qualified List
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.RedisCommands as RedisCommands
import qualified Maybe
import qualified Network.HostName
import Observability.Helpers (toHashMap)
import qualified Observability.Timer as Timer
import qualified Platform
import qualified System.Random as Random
import qualified Task
import qualified Text
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
        let probability = deriveSampleRate span handler'
        roll <- Random.randomRIO (0.0, 1.0)
        Prelude.pure (roll > probability, round (1 / probability))
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
          (Text.fromList hostname')
          (calculateApdex handler' span)
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

getRootSpanRequestPath :: Platform.TracingSpan -> Maybe Text
getRootSpanRequestPath rootSpan =
  Platform.details rootSpan
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (\(HttpRequest.Incoming details) -> HttpRequest.endpoint details)
          ]
      )
    |> Maybe.andThen identity

getBatchEventEndpoint :: BatchEvent -> Maybe Text
getBatchEventEndpoint event =
  event
    |> batchevent_data
    |> details
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (\(HttpRequest.Incoming details) -> HttpRequest.endpoint details)
          ]
      )
    |> Maybe.andThen identity

deriveSampleRate :: Platform.TracingSpan -> Handler -> Float
deriveSampleRate rootSpan handler' =
  let isNonAppRequestPath =
        case getRootSpanRequestPath rootSpan of
          Nothing -> False
          -- You might be tempted to use `endpoint` instead of `path`, but
          -- healthcheck endpoints don't populate `HttpRequest.endpoint`.
          -- Fix that first before trying this.
          Just requestPath -> List.any (requestPath ==) ["/health/readiness", "/metrics", "/health/liveness"]
      baseRate = handler_fractionOfSuccessRequestsLogged handler'
      requestDurationMs =
        Timer.difference (Platform.started rootSpan) (Platform.finished rootSpan)
          |> Platform.inMicroseconds
          |> Prelude.fromIntegral
          |> (*) 1e-3
      apdexTMs = Prelude.fromIntegral (handler_apdexTimeMs handler')
   in if isNonAppRequestPath
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
          baseRate / 500
        else sampleRateForDuration baseRate requestDurationMs apdexTMs

-- For every increase of apdexTU in the request duration we double the chance of
-- a request getting logged, up to a maximum of 1.
--
-- An example plot of this function, for:
--  * apdex 30ms
--  * baseRate 1/1000
--  * from 1ms to 300ms

-- https://www.wolframalpha.com/input/?i=plot+1%2Fmax%281%2F1000%2C+min%281%2C+%281%2F1000%29+*+%281.5+%5E+%28x+%2F+30%29%29%29%29+from+x%3D1+to+x%3D300
sampleRateForDuration :: Float -> Float -> Float -> Float
sampleRateForDuration baseRate requestDurationMs apdexTMs =
  baseRate * (1.5 ^ (requestDurationMs / apdexTMs))
    |> clamp baseRate 1

calculateApdex :: Handler -> Platform.TracingSpan -> Float
calculateApdex handler' span =
  case Platform.succeeded span of
    Platform.Failed -> 0
    Platform.FailedWith _ -> 0
    Platform.Succeeded ->
      let duration =
            Timer.difference (Platform.started span) (Platform.finished span)
              |> Platform.inMicroseconds
              |> Prelude.fromIntegral
          apdexTUs = 1000 * handler_apdexTimeMs handler'
       in if duration < apdexTUs
            then 1
            else
              if duration < (4 * apdexTUs)
                then 0.5
                else 0

toBatchEvents :: CommonFields -> Int -> Maybe SpanId -> Int -> Platform.TracingSpan -> (Int, [BatchEvent])
toBatchEvents commonFields sampleRate parentSpanId spanIndex span = do
  let thisSpansId = SpanId (common_requestId commonFields ++ "-" ++ NriText.fromInt spanIndex)
  let (lastSpanIndex, children) = Data.List.mapAccumL (toBatchEvents commonFields sampleRate (Just thisSpansId)) (spanIndex + 1) (Platform.children span)
  let duration =
        Timer.difference (Platform.started span) (Platform.finished span)
          |> Platform.inMicroseconds
  let timestamp = Timer.toISO8601 (common_timer commonFields) (Platform.started span)
  let sourceLocation =
        Platform.frame span
          |> Maybe.map
            ( \(_, frame) ->
                Text.fromList (Stack.srcLocFile frame)
                  ++ ":"
                  ++ Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine frame))
            )
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
            sourceLocation = sourceLocation,
            apdex = common_apdex commonFields,
            enrichedData = [],
            details = Platform.details span,
            sampleRate
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
deNoise maybeDetails =
  case maybeDetails of
    Just originalDetails ->
      originalDetails
        |> Platform.renderTracingSpanDetails
          [ Platform.Renderer deNoiseLog,
            Platform.Renderer deNoiseRedis
          ]
        -- `renderTracingSpanDetails` returns Nothing when type of details
        -- doesn't match any in our list of functions above.
        --
        -- Default to the original details then so we don't lose data
        |> Maybe.withDefault originalDetails
        |> Just
    Nothing -> Nothing

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
      deets =
        if List.length contexts > 5
          then HashMap.singleton "context" (tojson context)
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
deNoiseRedis :: RedisCommands.Details -> Platform.SomeTracingSpanDetails
deNoiseRedis redisInfo =
  let commandsCount =
        redisInfo
          |> RedisCommands.commands
          |> List.filterMap (NriText.words >> List.head)
          |> (\x -> [(the key ++ ".count", key |> List.length |> NriText.fromInt) | key <- x, then group by key using groupWith])
      fullBlob =
        redisInfo
          |> RedisCommands.commands
          |> NriText.join "\n"
   in HashMap.fromList
        ( ("commands", fullBlob) :
          ("host", RedisCommands.host redisInfo |> Maybe.withDefault "unknown") :
          ("port", RedisCommands.port redisInfo |> Maybe.map Text.fromInt |> Maybe.withDefault "unknown") :
          commandsCount
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
  { common_timer :: Timer.Timer,
    common_serviceName :: Text,
    common_environment :: Text,
    common_requestId :: Text,
    common_hostname :: Text,
    common_apdex :: Float
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
    sourceLocation :: Maybe Text,
    apdex :: Float,
    enrichedData :: [(Text, Text)],
    details :: Maybe Platform.SomeTracingSpanDetails,
    sampleRate :: Int
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
            "source_location" .= sourceLocation span,
            "hostname" .= hostname span,
            "failed" .= failed span,
            "apdex" .= apdex span
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
    handler_timer :: Timer.Timer,
    handler_http :: Http.Handler,
    handler_serviceName :: Text,
    handler_environment :: Text,
    handler_honeycombApiKey :: Log.Secret Text,
    handler_fractionOfSuccessRequestsLogged :: Float,
    handler_apdexTimeMs :: Int
  }

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  http <- Http.handler
  Prelude.pure
    Handler
      { handler_timer = timer,
        handler_http = http,
        handler_serviceName = appName settings,
        handler_environment = appEnvironment settings,
        handler_honeycombApiKey = honeycombApiKey settings,
        handler_fractionOfSuccessRequestsLogged = fractionOfSuccessRequestsLogged settings,
        handler_apdexTimeMs = appdexTimeMs settings
      }

data Settings = Settings
  { appName :: Text,
    appEnvironment :: Text,
    honeycombApiKey :: Log.Secret Text,
    fractionOfSuccessRequestsLogged :: Float,
    appdexTimeMs :: Int
  }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap appNameDecoder
    |> andMap appEnvironmentDecoder
    |> andMap honeycombApiKeyDecoder
    |> andMap fractionOfSuccessRequestsLoggedDecoder
    |> andMap apdexTimeMsDecoder

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

apdexTimeMsDecoder :: Environment.Decoder Int
apdexTimeMsDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_APDEX_TIME_IN_MILLISECONDS",
        Environment.description = "The T value in the apdex, the time in milliseconds in which a health request should complete.",
        Environment.defaultValue = "100"
      }
    Environment.int
