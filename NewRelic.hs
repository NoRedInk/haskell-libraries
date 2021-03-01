-- | Report request tracing data to NewRelic APM.
--
-- NewRelic helps us get statistics about requests such as average response
-- times, error rates, and break downs of how much time is spent doing what
-- when responding to a request.
--
-- We report data about each individual request to NewRelic.
module Observability.NewRelic
  ( report,
    handler,
    Handler,
    Settings,
    decoder,
  )
where

import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.Int
import qualified Data.Proxy as Proxy
import qualified Data.Scientific
import qualified Data.Text.Encoding
import qualified Data.Typeable as Typeable
import qualified Environment
import qualified List
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.SqlQuery as SqlQuery
import qualified Maybe
import qualified Observability.Timer as Timer
import qualified Platform
import qualified Redis
import qualified Text
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span =
  case Platform.details span |> andThen Platform.fromTracingSpanDetails of
    Nothing -> Prelude.pure ()
    Just details -> reportWebTransaction handler' span details

data Handler = Handler Timer.Timer NewRelic.App

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer settings =
  Conduit.mkAcquire
    ( do
        appConfig <- NewRelic.createAppConfig (appName settings) (Log.unSecret (licenseKey settings))
        NewRelic.createApp appConfig (timeout settings)
          |> map (Handler timer)
    )
    (\_ -> Prelude.pure ())

reportWebTransaction :: Handler -> Platform.TracingSpan -> HttpRequest.Incoming -> Prelude.IO ()
reportWebTransaction (Handler timer app) span (HttpRequest.Incoming details) = do
  let endpoint =
        -- We're always going to get some requests to routes that don't exist.
        -- Users might mystype a URL, or hackers might try to find admin login
        -- endpoints. If we report these paths to NewRelic directly it's going
        -- to create a new entry in the list of transactions for each
        -- non-existing path anyone in the world thinks of request of our
        -- services. Those quickly add up.
        --
        -- So instead we group all these requests for unknown routes together.
        case (HttpRequest.endpoint details, HttpRequest.status details) of
          -- Some unknown routes are from middlewares. That's why we also check
          -- the response code.
          (Nothing, Just 401) -> "401"
          (Nothing, Just 403) -> "403"
          (Nothing, Just 404) -> "404"
          _ ->
            HttpRequest.endpoint details
              |> Maybe.withDefault
                ( Maybe.map2
                    (\method path -> method ++ " " ++ path)
                    (HttpRequest.method details)
                    (HttpRequest.path details)
                    |> Maybe.withDefault "Unknown route"
                )
  Exception.bracketWithError
    (startWebTransaction app endpoint)
    ( \maybeException tx -> do
        -- If we encountered any sort of failure ignore the transaction. We
        -- don't know which bits of the transaction we managed to apply
        -- successfully and which not, so to prevent skewing the data best leave
        -- this one out entirely.
        case maybeException of
          Nothing -> Prelude.pure ()
          Just _ -> ignoreTransaction tx
        endTransaction tx
    )
    ( \tx -> do
        setTransactionTiming tx (startTime timer span) (toDuration span)
        case Platform.succeeded span of
          Platform.Succeeded -> Prelude.pure ()
          Platform.Failed -> reportError tx "Error logged during request" details
          Platform.FailedWith err -> reportError tx (typeName err) details
        Platform.children span
          |> Foldable.traverse_ (reportTracingSpan (Platform.started span) timer tx Nothing)
    )

reportError :: NewRelic.Transaction -> Text -> HttpRequest.Details -> Prelude.IO ()
reportError tx msg details =
  noticeError tx 1 msg
    <| case HttpRequest.status details of
      Nothing -> "Unknown"
      Just status -> Text.fromInt status

reportTracingSpan ::
  Platform.MonotonicTime ->
  Timer.Timer ->
  NewRelic.Transaction ->
  Maybe NewRelic.Segment ->
  Platform.TracingSpan ->
  Prelude.IO ()
reportTracingSpan txStartTime timer tx maybeParent span =
  Exception.bracket
    (chooseAndStartSegment tx span)
    endSegment
    ( \segment -> do
        setSegmentTiming segment (segmentStartTime txStartTime span) (toDuration span)
        case maybeParent of
          Just parent -> setSegmentParent segment parent
          Nothing -> setSegmentParentRoot segment
        addAttributesForSpan tx span
        Platform.children span
          |> Foldable.traverse_ (reportTracingSpan txStartTime timer tx (Just segment))
    )

chooseAndStartSegment :: NewRelic.Transaction -> Platform.TracingSpan -> Prelude.IO NewRelic.Segment
chooseAndStartSegment tx span =
  Platform.details span
    |> andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (startDatastoreSegment tx << sqlToDatastore),
            Platform.Renderer (startDatastoreSegment tx << redisToDatastore),
            Platform.Renderer (startExternalSegment tx << httpToExternalSegment)
          ]
      )
    |> Maybe.withDefault
      (startSegment tx (Platform.name span) (category span))

sqlToDatastore :: SqlQuery.Details -> NewRelic.DatastoreSegment
sqlToDatastore details =
  NewRelic.DatastoreSegment
    { NewRelic.datastoreSegmentProduct =
        case SqlQuery.databaseType details of
          Just product ->
            if product == SqlQuery.postgresql
              then NewRelic.Postgres
              else
                if product == SqlQuery.mysql
                  then NewRelic.MySQL
                  else NewRelic.OtherDatastoreSegmentProduct product
          Nothing -> NewRelic.OtherDatastoreSegmentProduct "unknown",
      NewRelic.datastoreSegmentCollection = SqlQuery.queriedRelation details,
      NewRelic.datastoreSegmentOperation = SqlQuery.sqlOperation details,
      NewRelic.datastoreSegmentHost = SqlQuery.host details,
      NewRelic.datastoreSegmentPortPathOrId = Maybe.map Text.fromInt (SqlQuery.port details),
      NewRelic.datastoreSegmentDatabaseName = SqlQuery.database details,
      NewRelic.datastoreSegmentQuery = Maybe.map Log.unSecret (SqlQuery.query details)
    }

redisToDatastore :: Redis.Info -> NewRelic.DatastoreSegment
redisToDatastore info =
  NewRelic.DatastoreSegment
    { NewRelic.datastoreSegmentProduct = NewRelic.Redis,
      NewRelic.datastoreSegmentCollection = Nothing,
      NewRelic.datastoreSegmentOperation = firstCommand (Redis.infoCommands info),
      NewRelic.datastoreSegmentHost = Just (Redis.infoHost info),
      NewRelic.datastoreSegmentPortPathOrId = Just (Redis.infoPort info),
      NewRelic.datastoreSegmentDatabaseName = Nothing,
      NewRelic.datastoreSegmentQuery = Just (Text.join "\n" (Redis.infoCommands info))
    }

firstCommand :: [Text] -> Maybe Text
firstCommand cmds =
  case cmds of
    [] -> Nothing
    first : _ -> List.head (Text.split " " first)

httpToExternalSegment :: HttpRequest.Outgoing -> NewRelic.ExternalSegment
httpToExternalSegment (HttpRequest.Outgoing details) =
  NewRelic.ExternalSegment
    { NewRelic.externalSegmentUri =
        HttpRequest.host details ++ HttpRequest.path details
          |> Maybe.withDefault "unknown-uri",
      NewRelic.externalSegmentProcedure = HttpRequest.method details,
      NewRelic.externalSegmentLibrary = Nothing
    }

startTime :: Timer.Timer -> Platform.TracingSpan -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> Timer.toPosixMicroseconds timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

segmentStartTime :: Platform.MonotonicTime -> Platform.TracingSpan -> NewRelic.StartTimeUsSinceUnixEpoch
segmentStartTime txStartTime span =
  Timer.difference txStartTime (Platform.started span)
    |> Platform.inMicroseconds
    |> NewRelic.StartTimeUsSinceUnixEpoch

toDuration :: Platform.TracingSpan -> NewRelic.DurationUs
toDuration span =
  Timer.difference (Platform.started span) (Platform.finished span)
    |> Platform.inMicroseconds
    -- When we tell NewRelic a segment lasted 0 microseconds it seems to ignore
    -- that value and uses other values instead. No idea where it's getting
    -- those alternative numbers from but they're wrong. Ensuring each duration
    -- is at least 1 microsecond seems to circumvent this problem.
    |> Prelude.max 1
    |> NewRelic.DurationUs

category :: Platform.TracingSpan -> Text
category span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer
              ( \(details :: SqlQuery.Details) ->
                  SqlQuery.databaseType details
                    |> Maybe.withDefault "unknown"
                    |> Text.toLower
              ),
            Platform.Renderer (\(_ :: HttpRequest.Outgoing) -> "http"),
            Platform.Renderer (\(_ :: HttpRequest.Incoming) -> "request"),
            Platform.Renderer (\(_ :: Log.LogContexts) -> "haskell")
          ]
      )
    |> Maybe.withDefault "unknown"

addAttributesForSpan :: NewRelic.Transaction -> Platform.TracingSpan -> Prelude.IO ()
addAttributesForSpan tx span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (\contexts -> addAttributes tx contexts)
          ]
      )
    |> Maybe.withDefault (Prelude.pure ())

addAttributes :: NewRelic.Transaction -> Log.LogContexts -> Prelude.IO ()
addAttributes tx (Log.LogContexts contexts) =
  Foldable.traverse_
    (\(Log.Context key val) -> addAttribute tx key (Aeson.toJSON val))
    contexts

addAttribute :: NewRelic.Transaction -> Text -> Aeson.Value -> Prelude.IO ()
addAttribute tx key value =
  case value of
    Aeson.String text -> addAttributeText tx key text
    Aeson.Number number ->
      case Data.Scientific.floatingOrInteger number of
        Prelude.Left double -> addAttributeDouble tx key double
        Prelude.Right long -> addAttributeLong tx key long
    Aeson.Bool bool' ->
      addAttributeText tx key (if bool' then "True" else "False")
    Aeson.Null -> addAttributeText tx key "null"
    _ ->
      addAttributeText
        tx
        key
        (Data.Text.Encoding.decodeUtf8 (Data.ByteString.Lazy.toStrict (Aeson.encode value)))

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  case text of
    "" -> Nothing
    _ -> Just text

--
-- NewRelic API
--
-- Wrappers around the functions from `tracing-newrelic` that throw in case of
-- failure.
--

startWebTransaction :: NewRelic.App -> Text -> Prelude.IO NewRelic.Transaction
startWebTransaction app name =
  NewRelic.startWebTransaction app name
    |> andThen expectJust

endTransaction :: NewRelic.Transaction -> Prelude.IO ()
endTransaction tx =
  NewRelic.endTransaction tx
    |> andThen expectSuccess

ignoreTransaction :: NewRelic.Transaction -> Prelude.IO ()
ignoreTransaction tx =
  NewRelic.ignoreTransaction tx
    |> andThen expectSuccess

setTransactionTiming ::
  NewRelic.Transaction ->
  NewRelic.StartTimeUsSinceUnixEpoch ->
  NewRelic.DurationUs ->
  Prelude.IO ()
setTransactionTiming tx start duration =
  NewRelic.setTransactionTiming tx start duration
    |> andThen expectSuccess

startSegment ::
  NewRelic.Transaction ->
  Text ->
  Text ->
  Prelude.IO NewRelic.Segment
startSegment tx name cat =
  NewRelic.startSegment tx (nonEmptyText name) (nonEmptyText cat)
    |> andThen expectJust

startDatastoreSegment ::
  NewRelic.Transaction ->
  NewRelic.DatastoreSegment ->
  Prelude.IO NewRelic.Segment
startDatastoreSegment tx config =
  NewRelic.startDatastoreSegment tx config
    |> andThen expectJust

startExternalSegment ::
  NewRelic.Transaction ->
  NewRelic.ExternalSegment ->
  Prelude.IO NewRelic.Segment
startExternalSegment tx config =
  NewRelic.startExternalSegment tx config
    |> andThen expectJust

endSegment :: NewRelic.Segment -> Prelude.IO ()
endSegment segment =
  NewRelic.endSegment segment
    |> andThen expectSuccess

setSegmentTiming ::
  NewRelic.Segment ->
  NewRelic.StartTimeUsSinceUnixEpoch ->
  NewRelic.DurationUs ->
  Prelude.IO ()
setSegmentTiming tx start duration =
  NewRelic.setSegmentTiming tx start duration
    |> andThen expectSuccess

setSegmentParent :: NewRelic.Segment -> NewRelic.Segment -> Prelude.IO ()
setSegmentParent segment parent =
  NewRelic.setSegmentParent segment parent
    |> andThen expectSuccess

setSegmentParentRoot :: NewRelic.Segment -> Prelude.IO ()
setSegmentParentRoot segment =
  NewRelic.setSegmentParentRoot segment
    |> andThen expectSuccess

noticeError :: NewRelic.Transaction -> Data.Int.Int32 -> Text -> Text -> Prelude.IO ()
noticeError = NewRelic.noticeError

addAttributeText :: NewRelic.Transaction -> Text -> Text -> Prelude.IO ()
addAttributeText tx key value =
  NewRelic.addAttributeText tx key value
    |> andThen allowFailure

addAttributeLong :: NewRelic.Transaction -> Text -> Int -> Prelude.IO ()
addAttributeLong tx key value =
  NewRelic.addAttributeLong tx key value
    |> andThen allowFailure

addAttributeDouble :: NewRelic.Transaction -> Text -> Float -> Prelude.IO ()
addAttributeDouble tx key value =
  NewRelic.addAttributeDouble tx key value
    |> andThen allowFailure

-- | Don't report the transaction if reporting
expectSuccess :: Bool -> Prelude.IO ()
expectSuccess success =
  if success
    then Prelude.pure ()
    else Exception.throwIO NewRelicFailure

-- | Don't fail the report for the transaction as a whole if this specific
-- NewRelic reporting step fails.
allowFailure :: Bool -> Prelude.IO ()
allowFailure _ = Prelude.pure ()

expectJust :: Maybe a -> Prelude.IO a
expectJust maybe =
  case maybe of
    Nothing -> Exception.throwIO NewRelicFailure
    Just val -> Prelude.pure val

data NewRelicFailure = NewRelicFailure deriving (Show)

instance Exception.Exception NewRelicFailure

typeName :: forall a. Typeable.Typeable a => a -> Text
typeName _ =
  Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy a)
    |> Prelude.show
    |> Text.fromList

data Settings = Settings
  { appName :: NewRelic.AppName,
    licenseKey :: Log.Secret NewRelic.LicenseKey,
    timeout :: NewRelic.TimeoutMs
  }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap appNameDecoder
    |> andMap licenseKeyDecoder
    |> andMap timeoutDecoder

appNameDecoder :: Environment.Decoder NewRelic.AppName
appNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_APP_NAME",
        Environment.description = "The NewRelic applicat name to connect to.",
        Environment.defaultValue = ""
      }
    (Environment.text |> map NewRelic.AppName)

licenseKeyDecoder :: Environment.Decoder (Log.Secret NewRelic.LicenseKey)
licenseKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_LICENSE_KEY",
        Environment.description = "The NewRelic license key to connect with..",
        Environment.defaultValue = ""
      }
    (Environment.text |> map NewRelic.LicenseKey |> Environment.secret)

timeoutDecoder :: Environment.Decoder NewRelic.TimeoutMs
timeoutDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_TIMEOUT_MS",
        Environment.description = "The timeout when connecting to NewRelic",
        Environment.defaultValue = "10000"
      }
    (Environment.int |> map NewRelic.TimeoutMs)
