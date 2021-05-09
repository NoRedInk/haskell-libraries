{-# LANGUAGE GADTs #-}
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
    CommonFields (..),
    BatchEvent (..),
    Span,
    emptySpan,
    addField,
    Revision (..),
    sampleRateForDuration,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Data.Word
import qualified Dict
import qualified Environment
import GHC.Exts (groupWith, the)
import qualified GHC.Stack as Stack
import qualified Http
import qualified List
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.Kafka as Kafka
import qualified Log.RedisCommands as RedisCommands
import qualified Maybe
import qualified Network.HostName
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
  uuid <- Data.UUID.V4.nextRandom
  let commonFields =
        CommonFields
          (handler_timer handler')
          (Data.UUID.toText uuid)
          ( handler_initSpan handler'
              |> addField "apdex" (calculateApdex handler' span)
              -- Don't use requestId if we don't do Distributed Tracing
              -- Else, it will create traces with no parent sharing the same TraceId
              -- Which makes Honeycomb's UI confused
              |> addField "trace.trace_id" (Data.UUID.toText uuid)
          )
  let events = toBatchEvents commonFields sampleRate span
  let body = Http.jsonBody events
  silentHandler' <- Platform.silentHandler
  let url = batchApiEndpoint (handler_datasetName handler')
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

getSpanEndpoint :: Platform.TracingSpan -> Maybe Text
getSpanEndpoint span =
  span
    |> Platform.details
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (\(HttpRequest.Incoming details) -> HttpRequest.endpoint details),
            Platform.Renderer (Just << Kafka.topic)
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
            spanDuration span
              |> Platform.inMicroseconds
              |> Prelude.fromIntegral
          apdexTUs = 1000 * handler_apdexTimeMs handler'
       in if duration < apdexTUs
            then 1
            else
              if duration < (4 * apdexTUs)
                then 0.5
                else 0

toBatchEvents :: CommonFields -> Int -> Platform.TracingSpan -> List BatchEvent
toBatchEvents commonFields sampleRate span =
  let (_, events) =
        batchEventsHelper
          commonFields
          (getSpanEndpoint span)
          sampleRate
          Nothing
          (emptyStatsByName, 0)
          span
   in events

batchEventsHelper ::
  CommonFields ->
  Maybe Text ->
  Int ->
  Maybe SpanId ->
  (StatsByName, Int) ->
  Platform.TracingSpan ->
  ((StatsByName, Int), [BatchEvent])
batchEventsHelper commonFields maybeEndpoint sampleRate parentSpanId (statsByName, spanIndex) span = do
  let thisSpansId = SpanId (requestId commonFields ++ "-" ++ NriText.fromInt spanIndex)
  let ((lastStatsByName, lastSpanIndex), nestedChildren) =
        Data.List.mapAccumL
          (batchEventsHelper commonFields maybeEndpoint sampleRate (Just thisSpansId))
          ( -- Don't record the root span. We have only one of those per trace,
            -- so there's no statistics we can do with it.
            if spanIndex == 0
              then statsByName
              else recordStats span statsByName,
            spanIndex + 1
          )
          (Platform.children span)
  let children = List.concat nestedChildren
  let duration =
        Timer.difference (Platform.started span) (Platform.finished span)
          |> Platform.inMicroseconds
  let timestamp = Timer.toISO8601 (timer commonFields) (Platform.started span)
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
  let addStats span' =
        if spanIndex == 0
          then perSpanNameStats lastStatsByName span'
          else span'
  let addEndpoint span' =
        case maybeEndpoint of
          Nothing -> span'
          Just endpoint -> addField "details.endpoint" endpoint span'
  let addDetails span' =
        HashMap.foldrWithKey
          (\key value -> addField ("details." ++ key) value)
          span'
          (renderDetails (Platform.details span))
  let hcSpan =
        initSpan commonFields
          |> addField "name" (Platform.name span)
          |> addField "trace.span_id" thisSpansId
          |> addField "trace.parent_id" parentSpanId
          |> addField "duration_ms" (Prelude.fromIntegral duration / 1000)
          |> addField "allocated_bytes" (Platform.allocated span)
          |> addField "failed" isError
          |> addField "source_location" sourceLocation
          |> addDetails
          |> addEndpoint
          |> addStats
  ( (lastStatsByName, lastSpanIndex),
    BatchEvent
      { batchevent_time = timestamp,
        batchevent_data = hcSpan,
        batchevent_samplerate = sampleRate
      } :
    children
    )

data Stats = Stats
  { count :: Int,
    totalTimeMicroseconds :: Data.Word.Word64
  }

newtype StatsByName = StatsByName (Dict.Dict Text Stats)

emptyStatsByName :: StatsByName
emptyStatsByName = StatsByName Dict.empty

recordStats :: Platform.TracingSpan -> StatsByName -> StatsByName
recordStats span (StatsByName statsByName) =
  let name = Platform.name span
      duration = Platform.inMicroseconds (spanDuration span)
      newStats =
        case Dict.get name statsByName of
          Nothing ->
            Stats
              { count = 1,
                totalTimeMicroseconds = duration
              }
          Just stats ->
            Stats
              { count = 1 + count stats,
                totalTimeMicroseconds = duration + totalTimeMicroseconds stats
              }
   in StatsByName (Dict.insert name newStats statsByName)

spanDuration :: Platform.TracingSpan -> Platform.MonotonicTime
spanDuration span =
  Timer.difference (Platform.started span) (Platform.finished span)

perSpanNameStats :: StatsByName -> Span -> Span
perSpanNameStats (StatsByName statsByName) span =
  let -- chose foldr to preserve order, not super important tho
      statsForCategory (name, stats) acc =
        let calls = count stats
            total = (Prelude.fromIntegral (totalTimeMicroseconds stats)) / 1000
            average = total / Prelude.fromIntegral calls
            saneName = name |> NriText.toLower |> NriText.replace " " "_"
         in acc
              |> addField ("stats.total_time_ms." ++ saneName) total
              |> addField ("stats.average_time_ms." ++ saneName) average
              |> addField ("stats.count." ++ saneName) calls
   in List.foldl statsForCategory span (Dict.toList statsByName)

-- Customize how we render span details for different kinds of spans to
-- Honeycomb.
--
-- In the past we used the toHashMap helper to generate the JSON representations
-- we send to honeycomb (see its documentation in the Helpers module to learn
-- more about it). This turned out to be a poor fit because it creates a ton of
-- top-level JSON keys, each of which Honeycomb will create a unique column for.
--
-- If we ever hit 10k unique column names (and we were past the thousands when
-- this code was introduced) Honeycomb will stop accepting traces from us.
--
-- "Unique column names" means different column names that Honeycomb has seen us
-- report on a span.
renderDetails :: Maybe Platform.SomeTracingSpanDetails -> HashMap.HashMap Text Aeson.Value
renderDetails maybeDetails =
  case maybeDetails of
    Just originalDetails ->
      originalDetails
        |> Platform.renderTracingSpanDetails
          [ Platform.Renderer renderDetailsLog,
            Platform.Renderer renderDetailsRedis
          ]
        -- `renderTracingSpanDetails` returns Nothing when type of details
        -- doesn't match any in our list of functions above.
        --
        -- We'll default to using the default JSON encoding of the span.
        -- Assuming it encodes into a JSON object with multiple keys (every
        -- known details object we have does this) we'll use that object
        -- directly.
        |> Maybe.withDefault
          ( case Aeson.toJSON originalDetails of
              Aeson.Object hashMap -> hashMap
              jsonVal -> HashMap.singleton "val" jsonVal
          )
    Nothing -> HashMap.empty

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
renderDetailsLog :: Log.LogContexts -> HashMap.HashMap Text Aeson.Value
renderDetailsLog context@(Log.LogContexts contexts) =
  if List.length contexts > 5
    then HashMap.singleton "context" (Aeson.toJSON context)
    else
      contexts
        |> map (\(Log.Context key val) -> (key, Aeson.toJSON val))
        |> HashMap.fromList

-- Redis creates one column per command for batches
-- Let's trace what matters:
-- - How many of each command
-- - The full blob in a single column
-- - The rest of our Info record
renderDetailsRedis :: RedisCommands.Details -> HashMap.HashMap Text Aeson.Value
renderDetailsRedis redisInfo =
  let commandsCount =
        redisInfo
          |> RedisCommands.commands
          |> List.filterMap (NriText.words >> List.head)
          |> (\x -> [(the key ++ ".count", key |> List.length |> Aeson.toJSON) | key <- x, then group by key using groupWith])
      fullBlob =
        redisInfo
          |> RedisCommands.commands
          |> Aeson.toJSON
   in HashMap.fromList
        ( ("commands", fullBlob) :
          ("host", RedisCommands.host redisInfo |> Aeson.toJSON) :
          ("port", RedisCommands.port redisInfo |> Aeson.toJSON) :
          commandsCount
        )

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
  { timer :: Timer.Timer,
    requestId :: Text,
    initSpan :: Span
  }

-- | Honeycomb defines a span to be a list of key-value pairs, which we model
-- using a dictionary. Honeycomb expects as values anything that's valid JSON.
--
-- We could use the `Aeson.Value` type to model values, but that mean we'd be
-- encoding spans for honeycomb in two steps: first from our original types to
-- `Aeson.Value`, then to the `ByteString` we send in the network request.
-- `Aeson` has a more efficient encoding strategy that is able to encode types
-- into JSON in one go. To use that we accept as keys any value we know we'll be
-- able to encode into JSON later, once we got the whole payload we want to send
-- to honeycomb together.
newtype Span = Span (Dict.Dict Text JsonEncodable)
  deriving (Aeson.ToJSON, Show)

data JsonEncodable where
  JsonEncodable :: Aeson.ToJSON a => a -> JsonEncodable

instance Aeson.ToJSON JsonEncodable where
  toEncoding (JsonEncodable x) = Aeson.toEncoding x
  toJSON (JsonEncodable x) = Aeson.toJSON x

instance Show JsonEncodable where
  show (JsonEncodable x) = Prelude.show (Aeson.toJSON x)

emptySpan :: Span
emptySpan = Span Dict.empty

addField :: Aeson.ToJSON a => Text -> a -> Span -> Span
addField key val (Span span) = Span (Dict.insert key (JsonEncodable val) span)

newtype SpanId = SpanId Text
  deriving (Aeson.ToJSON, Show)

data Handler = Handler
  { -- | A bit of state that can be used to turn the clock values attached
    -- to spans into real timestamps.
    handler_timer :: Timer.Timer,
    handler_http :: Http.Handler,
    handler_datasetName :: Text,
    handler_honeycombApiKey :: Log.Secret Text,
    handler_fractionOfSuccessRequestsLogged :: Float,
    handler_apdexTimeMs :: Int,
    handler_initSpan :: Span
  }

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  http <- Http.handler
  revision <- liftIO getRevision
  hostname' <- liftIO Network.HostName.getHostName
  let serviceName =
        case optionalServiceName settings of
          "" -> appName settings
          name -> name
  Prelude.pure
    Handler
      { handler_timer = timer,
        handler_http = http,
        handler_datasetName = appName settings ++ "-" ++ appEnvironment settings,
        handler_honeycombApiKey = honeycombApiKey settings,
        handler_fractionOfSuccessRequestsLogged = fractionOfSuccessRequestsLogged settings,
        handler_apdexTimeMs = appdexTimeMs settings,
        handler_initSpan =
          emptySpan
            |> addField "service_name" serviceName
            |> addField "hostname" (Text.fromList hostname')
            |> addField "revision" revision
      }

newtype Revision = Revision Text
  deriving (Show, Aeson.ToJSON)

-- | Get the GIT revision of the current code. We do this by reading a file that
-- our K8S setup is supposed to provide.
getRevision :: Prelude.IO Revision
getRevision = do
  eitherRevision <- Exception.tryAny <| Data.Text.IO.readFile "revision"
  case eitherRevision of
    Prelude.Left _err -> Prelude.pure (Revision "no revision file found")
    Prelude.Right version -> Prelude.pure (Revision version)

data Settings = Settings
  { appName :: Text,
    optionalServiceName :: Text,
    appEnvironment :: Text,
    honeycombApiKey :: Log.Secret Text,
    fractionOfSuccessRequestsLogged :: Float,
    appdexTimeMs :: Int
  }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap appNameDecoder
    |> andMap honeycombAppNameDecoder
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

honeycombAppNameDecoder :: Environment.Decoder Text
honeycombAppNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_SERVICE_NAME",
        Environment.description = "Variable that sets the honeycomb service name without using an. If not set the LOG_ROOT_NAMESPACE value is used instead.",
        Environment.defaultValue = ""
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
