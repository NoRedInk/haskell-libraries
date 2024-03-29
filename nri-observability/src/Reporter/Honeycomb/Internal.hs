{-# LANGUAGE GADTs #-}

module Reporter.Honeycomb.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Data.Word
import qualified Dict
import qualified Environment
import qualified GHC.Stack as Stack
import qualified List
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.Kafka as Kafka
import qualified Log.RedisCommands as RedisCommands
import qualified Log.SqlQuery as SqlQuery
import qualified Maybe
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP.TLS
import qualified Network.HostName
import qualified Platform
import qualified Platform.AesonHelpers as AesonHelpers
import qualified Platform.Timer as Timer
import qualified System.Random as Random
import qualified Text
import qualified Text as NriText
import qualified Prelude

batchApiEndpoint :: Text -> Text
batchApiEndpoint datasetName = "https://api.honeycomb.io/1/batch/" ++ datasetName

-- | Report a tracing span to Honeycomb.
report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span = do
  sendOrSample <- makeSharedTraceData handler' span
  case sendOrSample of
    SampledOut -> Prelude.pure ()
    SendToHoneycomb sharedTraceData -> sendToHoneycomb handler' sharedTraceData span

sendToHoneycomb :: Handler -> SharedTraceData -> Platform.TracingSpan -> Prelude.IO ()
sendToHoneycomb handler' sharedTraceData span = do
  let events = toBatchEvents sharedTraceData span
  baseRequest <-
    settings handler'
      |> datasetName
      |> batchApiEndpoint
      |> Text.toList
      |> HTTP.parseRequest
  let req =
        baseRequest
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ( "X-Honeycomb-Team",
                  settings handler'
                    |> apiKey
                    |> Log.unSecret
                )
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode events)
          }
  _ <- HTTP.httpNoBody req (http handler')
  Prelude.pure ()

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
            Platform.Renderer Kafka.topic
          ]
      )
    |> Maybe.andThen identity

deriveSampleRate :: Platform.TracingSpan -> Settings -> Float
deriveSampleRate rootSpan settings =
  let isNonAppRequestPath =
        case getRootSpanRequestPath rootSpan of
          Nothing -> False
          -- You might be tempted to use `endpoint` instead of `path`, but
          -- healthcheck endpoints don't populate `HttpRequest.endpoint`.
          -- Fix that first before trying this.
          Just requestPath -> List.any (requestPath ==) ["/health/readiness", "/metrics", "/health/liveness"]
      baseRate = modifyFractionOfSuccessRequestsLogged settings (fractionOfSuccessRequestsLogged settings) rootSpan
      requestDurationMs =
        Timer.difference (Platform.started rootSpan) (Platform.finished rootSpan)
          |> Platform.inMicroseconds
          |> Prelude.fromIntegral
          |> (*) 1e-3
      apdexTMs =
        modifyApdexTimeMs settings (apdexTimeMs settings) rootSpan
          |> Prelude.fromIntegral
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

calculateApdex :: Settings -> Platform.TracingSpan -> Float
calculateApdex settings span =
  case Platform.succeeded span of
    Platform.Failed -> 0
    Platform.FailedWith _ -> 0
    Platform.Succeeded ->
      let duration =
            spanDuration span
              |> Platform.inMicroseconds
              |> Prelude.fromIntegral
          apdexTUs = 1000 * apdexTimeMs settings
       in if duration < apdexTUs
            then 1
            else
              if duration < (4 * apdexTUs)
                then 0.5
                else 0

toBatchEvents :: SharedTraceData -> Platform.TracingSpan -> List BatchEvent
toBatchEvents sharedTraceData span =
  let (_, events) =
        batchEventsHelper
          sharedTraceData
          Nothing
          (emptyStatsByName, 0)
          span
   in events

batchEventsHelper ::
  SharedTraceData ->
  Maybe SpanId ->
  (StatsByName, Int) ->
  Platform.TracingSpan ->
  ((StatsByName, Int), [BatchEvent])
batchEventsHelper sharedTraceData parentSpanId (statsByName, spanIndex) span = do
  let isRootSpan = parentSpanId == Nothing
  let thisSpansId = SpanId (requestId sharedTraceData ++ "-" ++ NriText.fromInt spanIndex)
  let ((lastStatsByName, lastSpanIndex), nestedChildren) =
        Data.List.mapAccumL
          (batchEventsHelper sharedTraceData (Just thisSpansId))
          ( -- Don't record the root span. We have only one of those per trace,
            -- so there's no statistics we can do with it.
            if isRootSpan
              then statsByName
              else recordStats span statsByName,
            spanIndex + 1
          )
          (Platform.children span)
  let children = List.concat nestedChildren
  let duration =
        Timer.difference (Platform.started span) (Platform.finished span)
          |> Platform.inMicroseconds
  let timestamp = Timer.toISO8601 (timer sharedTraceData) (Platform.started span)
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
        if isRootSpan
          then perSpanNameStats lastStatsByName span'
          else span'
  let addEndpoint span' =
        case endpoint sharedTraceData of
          Nothing -> span'
          Just endpoint -> addField "endpoint" endpoint span'
  let hcSpan =
        initSpan sharedTraceData
          |> addField "name" (Platform.name span)
          |> addField "trace.span_id" thisSpansId
          |> addField "trace.parent_id" parentSpanId
          |> addField "duration_ms" (Prelude.fromIntegral duration / 1000)
          |> addField "allocated_bytes" (Platform.allocated span)
          |> addField "failed" isError
          |> addField "contains_failures" (Platform.containsFailures span)
          |> addField "source_location" sourceLocation
          |> addDetails span
          |> addEndpoint
          |> addStats
  ( (lastStatsByName, lastSpanIndex),
    BatchEvent
      { batchevent_time = timestamp,
        batchevent_data = hcSpan,
        batchevent_samplerate = sampleRate sharedTraceData
      }
      : children
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
            total = Prelude.fromIntegral (totalTimeMicroseconds stats) / 1000
            average = total / Prelude.fromIntegral calls
            saneName = useAsKey name
         in acc
              |> addField ("stats.total_time_ms." ++ saneName) total
              |> addField ("stats.average_time_ms." ++ saneName) average
              |> addField ("stats.count." ++ saneName) calls
   in List.foldl statsForCategory span (Dict.toList statsByName)

useAsKey :: Text -> Text
useAsKey str =
  str
    |> NriText.toLower
    |> NriText.replace " " "_"

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
addDetails :: Platform.TracingSpan -> Span -> Span
addDetails tracingSpan honeycombSpan =
  case Platform.details tracingSpan of
    Just details ->
      details
        |> Platform.renderTracingSpanDetails
          [ Platform.Renderer (renderDetailsLog honeycombSpan),
            Platform.Renderer (renderDetailsRedis honeycombSpan),
            Platform.Renderer
              ( \(_ :: HttpRequest.Incoming) ->
                  renderDetailsGeneric "http" details honeycombSpan
              ),
            Platform.Renderer
              ( \(_ :: SqlQuery.Details) ->
                  renderDetailsGeneric "sql" details honeycombSpan
              ),
            Platform.Renderer
              ( \(_ :: Kafka.Details) ->
                  renderDetailsGeneric "kafka" details honeycombSpan
              )
          ]
        -- `renderTracingSpanDetails` returns Nothing when type of details
        -- doesn't match any in our list of functions above.
        --
        -- We'll default to using the default JSON encoding of the honeycombSpan.
        -- Assuming it encodes into a JSON object with multiple keys (every
        -- known details object we have does this) we'll use that object
        -- directly.
        |> Maybe.withDefault
          (renderDetailsGeneric (Platform.name tracingSpan) details honeycombSpan)
    Nothing -> honeycombSpan

renderDetailsGeneric :: Text -> Platform.SomeTracingSpanDetails -> Span -> Span
renderDetailsGeneric keyPrefix details honeycombSpan =
  case Aeson.toJSON details of
    Aeson.Object object ->
      AesonHelpers.foldObject
        (\key value -> addField (useAsKey (keyPrefix ++ "." ++ key)) value)
        honeycombSpan
        object
    jsonVal -> addField keyPrefix jsonVal honeycombSpan

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
renderDetailsLog :: Span -> Log.LogContexts -> Span
renderDetailsLog span context@(Log.LogContexts contexts) =
  if List.length contexts > 5
    then addField "log.context" context span
    else
      List.foldl
        (\(Log.Context key val) -> addField ("log." ++ key) val)
        span
        contexts

-- Redis creates one column per command for batches
-- Let's trace what matters:
-- - How many of each command
-- - The full blob in a single column
-- - The rest of our Info record
renderDetailsRedis :: Span -> RedisCommands.Details -> Span
renderDetailsRedis span redisInfo =
  let addCommandCounts span' =
        redisInfo
          |> RedisCommands.commands
          |> List.filterMap (NriText.words >> List.head)
          |> NonEmpty.groupWith identity
          |> List.foldr
            ( \xs ->
                addField
                  ("redis." ++ NonEmpty.head xs ++ ".count")
                  (NonEmpty.length xs)
            )
            span'
      fullBlob =
        redisInfo
          |> RedisCommands.commands
          |> Aeson.toJSON
   in span
        |> addField "redis.commands" fullBlob
        |> addField "redis.host" (RedisCommands.host redisInfo)
        |> addField "redis.port" (RedisCommands.port redisInfo)
        |> addCommandCounts

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

data SharedTraceData = SharedTraceData
  { -- | We use this to turn GHC.Clock-produced timestamps into regular times.
    timer :: Timer.Timer,
    -- | Each request has a unique id, for correlating spans for the same request.
    requestId :: Text,
    -- | A honeycomb span with the common fields for this request pre-applied.
    initSpan :: Span,
    -- | The 'endpoint' of the request this trace describes. Honeycomb uses
    -- this for a breakdown-by-endpoint on the dataset home.
    endpoint :: Maybe Text,
    -- | The amount of similar traces this one trace represents. For example,
    -- if we send this trace but sampled out 9 similar ones, sample rate will be
    -- 10. This will let honeycomb know it should count this trace as 10.
    sampleRate :: Int
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
  deriving (Aeson.ToJSON, Eq, Show)

-- | Contextual information this reporter needs to do its work. You can create
-- one using 'handler'.
data Handler = Handler
  { http :: HTTP.Manager,
    settings :: Settings,
    makeSharedTraceData :: Platform.TracingSpan -> Prelude.IO SendOrSample
  }

data SendOrSample
  = SendToHoneycomb SharedTraceData
  | SampledOut

-- | Create a 'Handler' for a specified set of 'Settings'. Do this once when
-- your application starts and reuse the 'Handler' you get.
handler :: Settings -> Prelude.IO Handler
handler settings = do
  timer <- Timer.mkTimer
  http <- HTTP.TLS.getGlobalManager
  revision <- getRevision
  hostname' <- Network.HostName.getHostName
  let baseSpan' =
        emptySpan
          |> addField "service_name" (serviceName settings)
          |> addField "hostname" (Text.fromList hostname')
          |> addField "revision" revision
  let baseSpan =
        case k8sNode settings of
          Nothing -> baseSpan'
          Just node -> addField "k8s_node" node baseSpan'
  Prelude.pure
    Handler
      { http = http,
        settings = settings,
        makeSharedTraceData = \span -> do
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
            if Platform.containsFailures span
              then Prelude.pure (False, 1)
              else do
                let probability = deriveSampleRate span settings
                roll <- Random.randomRIO (0.0, 1.0)
                Prelude.pure (roll > probability, round (1 / probability))
          uuid <- Data.UUID.V4.nextRandom
          if skipLogging
            then Prelude.pure SampledOut
            else
              Prelude.pure
                <| SendToHoneycomb
                  SharedTraceData
                    { timer,
                      sampleRate,
                      requestId = Data.UUID.toText uuid,
                      endpoint = getSpanEndpoint span,
                      initSpan =
                        baseSpan
                          |> addField "apdex" (calculateApdex settings span)
                          -- Don't use requestId if we don't do Distributed Tracing
                          -- Else, it will create traces with no parent sharing the same TraceId
                          -- Which makes Honeycomb's UI confused
                          |> addField "trace.trace_id" (Data.UUID.toText uuid)
                    }
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

-- | Configuration settings for ths reporter. A value of this type can be read
-- from the environment using the 'decoder' function.
data Settings = Settings
  { -- | The Honeycomb API key to use.
    apiKey :: Log.Secret ByteString.ByteString,
    -- | The name of the honeycomb dataset to report to. If the dataset does not
    -- exist yet, Honeycomb will create it when you first send a request for it.
    --
    -- [@environment variable@] HONEYCOMB_API_KEY
    -- [@default value@] *****
    datasetName :: Text,
    -- | The name of the service we're reporting for.
    --
    -- [@environment variable@] HONEYCOMB_SERVICE_NAME
    -- [@default value@] service
    serviceName :: Text,
    -- | The fraction of successfull requests that will be reported. If your
    -- service receives a lot of requests you might want reduce this to safe
    -- cost.
    --
    -- [@environment variable@] HONEYCOMB_FRACTION_OF_SUCCESS_REQUESTS_LOGGED
    -- [@default value@] 1
    fractionOfSuccessRequestsLogged :: Float,
    -- | The apdex time for this service in ms. Requests handled faster than
    -- this time will be sampled according to the
    -- @HONEYCOMB_FRACTION_OF_SUCCESS_REQUESTS_LOGGED@ variable. Slower request
    -- will have a larger chance to be reported.
    --
    -- [@environment variable@] HONEYCOMB_APDEX_TIME_IN_MILLISECONDS
    -- [@default value@] 100
    apdexTimeMs :: Int,
    -- | Allows overriding the default sample rates for given spans.
    -- This allows us to change the sample rate for certain endpoints within an
    -- application, for example if a path is critical but low volume we may choose
    -- to increase the rate.
    -- [@default value@] the input float
    modifyFractionOfSuccessRequestsLogged :: Float -> Platform.TracingSpan -> Float,
    -- | Allows overriding the default apdex rates for given spans.
    -- This allows us to change the apdex for certain endpoints within an
    -- application, for example if a path is significantly lower volume than
    -- another the apdex may require tuning.
    -- [@default value@] the input int
    modifyApdexTimeMs :: Int -> Platform.TracingSpan -> Int,
    -- | When not @Nothing@, an extra @k8s_label@ label is applied to every logged
    -- span.
    -- [@default value@] Nothing
    k8sNode :: Maybe Text
  }

-- | Read 'Settings' from environment variables. Default variables will be used
-- in case no environment variable is set for an option.
decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap honeycombApiKeyDecoder
    |> andMap datasetNameDecoder
    |> andMap honeycombAppNameDecoder
    |> andMap fractionOfSuccessRequestsLoggedDecoder
    |> andMap apdexTimeMsDecoder
    |> andMap (Prelude.pure always)
    |> andMap (Prelude.pure always)
    |> andMap k8sNodeDecoder

honeycombApiKeyDecoder :: Environment.Decoder (Log.Secret ByteString.ByteString)
honeycombApiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_API_KEY",
        Environment.description = "The API key for Honeycomb",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> map Encoding.encodeUtf8 |> Environment.secret)

datasetNameDecoder :: Environment.Decoder Text
datasetNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_DATASET",
        Environment.description = "Name of the dataset honeycomb should log to.",
        Environment.defaultValue = "dataset"
      }
    Environment.text

honeycombAppNameDecoder :: Environment.Decoder Text
honeycombAppNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_SERVICE_NAME",
        Environment.description = "Variable that sets the honeycomb service name.",
        Environment.defaultValue = "service"
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
        Environment.description = "The T value in the apdex, the time in milliseconds in which a healthy request should complete.",
        Environment.defaultValue = "100"
      }
    Environment.int

k8sNodeDecoder :: Environment.Decoder (Maybe Text)
k8sNodeDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "HONEYCOMB_K8S_NODE",
        Environment.description = "The Kubernetes node this service instance is running on.",
        Environment.defaultValue = ""
      }
    ( Environment.custom Environment.text <| \str ->
        if Text.isEmpty str
          then Ok Nothing
          else Ok (Just str)
    )
