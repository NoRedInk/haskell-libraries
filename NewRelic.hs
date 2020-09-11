-- | Report request tracing data to NewRelic.
--
-- NewRelic helps us get statistics about requests, such as average response
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

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.Int
import qualified Data.Proxy as Proxy
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Typeable as Typeable
import qualified Environment
import qualified Http
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified MySQL
import Observability.Timer (Timer, toPosixMicroseconds)
import qualified Platform
import qualified Postgres
import qualified Text
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' _requestId span =
  case Platform.details span |> andThen Platform.fromTracingSpanDetails of
    Nothing -> Prelude.pure ()
    Just details -> reportWebTransaction handler' span details

data Handler = Handler Timer NewRelic.App

handler :: Timer -> Settings -> Conduit.Acquire Handler
handler timer settings =
  Conduit.mkAcquire
    ( do
        appConfig <- NewRelic.createAppConfig (appName settings) (Log.unSecret (licenseKey settings))
        NewRelic.createApp appConfig (timeout settings)
          |> map (Handler timer)
    )
    (\_ -> Prelude.pure ())

reportWebTransaction :: Handler -> Platform.TracingSpan -> Monitoring.RequestDetails -> Prelude.IO ()
reportWebTransaction (Handler timer app) span details =
  Exception.bracketWithError
    (startWebTransaction app (Monitoring.endpoint details))
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

reportError :: NewRelic.Transaction -> Text -> Monitoring.RequestDetails -> Prelude.IO ()
reportError tx msg details =
  noticeError tx 1 msg (Text.fromInt (Monitoring.responseStatus details))

reportTracingSpan ::
  Platform.MonotonicTime ->
  Timer ->
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
          [ Platform.Renderer (startDatastoreSegment tx << mysqlToDatastore),
            Platform.Renderer (startDatastoreSegment tx << postgresToDatastore),
            Platform.Renderer (startExternalSegment tx << httpToExternalSegment)
          ]
      )
    |> Maybe.withDefault
      (startSegment tx (Platform.name span) (category span))

mysqlToDatastore :: MySQL.Info -> NewRelic.DatastoreSegment
mysqlToDatastore info =
  NewRelic.DatastoreSegment
    { NewRelic.datastoreSegmentProduct = NewRelic.MySQL,
      NewRelic.datastoreSegmentCollection = Just (MySQL.infoQueriedRelation info),
      NewRelic.datastoreSegmentOperation = Just (MySQL.infoSqlOperation info),
      NewRelic.datastoreSegmentHost = case MySQL.infoConnection info of
        MySQL.TcpSocket host _ _ -> Just host
        MySQL.UnixSocket _ _ -> Nothing,
      NewRelic.datastoreSegmentPortPathOrId = case MySQL.infoConnection info of
        MySQL.TcpSocket _ port _ -> Just port
        MySQL.UnixSocket path _ -> Just path,
      NewRelic.datastoreSegmentDatabaseName = case MySQL.infoConnection info of
        MySQL.TcpSocket _ _ dbname -> Just dbname
        MySQL.UnixSocket _ dbname -> Just dbname,
      NewRelic.datastoreSegmentQuery = Just (MySQL.infoQueryTemplate info)
    }

postgresToDatastore :: Postgres.Info -> NewRelic.DatastoreSegment
postgresToDatastore info =
  NewRelic.DatastoreSegment
    { NewRelic.datastoreSegmentProduct = NewRelic.Postgres,
      NewRelic.datastoreSegmentCollection = Just (Postgres.infoQueriedRelation info),
      NewRelic.datastoreSegmentOperation = Just (Postgres.infoSqlOperation info),
      NewRelic.datastoreSegmentHost = case Postgres.infoConnection info of
        Postgres.TcpSocket host _ _ -> Just host
        Postgres.UnixSocket _ _ -> Nothing,
      NewRelic.datastoreSegmentPortPathOrId = case Postgres.infoConnection info of
        Postgres.TcpSocket _ port _ -> Just port
        Postgres.UnixSocket path _ -> Just path,
      NewRelic.datastoreSegmentDatabaseName = case Postgres.infoConnection info of
        Postgres.TcpSocket _ _ dbname -> Just dbname
        Postgres.UnixSocket _ dbname -> Just dbname,
      NewRelic.datastoreSegmentQuery = Just (Postgres.infoQueryTemplate info)
    }

httpToExternalSegment :: Http.Info -> NewRelic.ExternalSegment
httpToExternalSegment info =
  NewRelic.ExternalSegment
    { NewRelic.externalSegmentUri = Http.infoUri info,
      NewRelic.externalSegmentProcedure = Just (Http.infoRequestMethod info),
      NewRelic.externalSegmentLibrary = Nothing
    }

startTime :: Timer -> Platform.TracingSpan -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> toPosixMicroseconds timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

segmentStartTime :: Platform.MonotonicTime -> Platform.TracingSpan -> NewRelic.StartTimeUsSinceUnixEpoch
segmentStartTime txStartTime span =
  timeDiff txStartTime (Platform.started span)
    |> Platform.inMicroseconds
    |> NewRelic.StartTimeUsSinceUnixEpoch

toDuration :: Platform.TracingSpan -> NewRelic.DurationUs
toDuration span =
  timeDiff (Platform.started span) (Platform.finished span)
    |> Platform.inMicroseconds
    -- When we tell NewRelic a segment lasted 0 microseconds it seems to ignore
    -- that value and uses other values instead. No idea where it's getting
    -- those alternative numbers from but they're wrong. Ensuring each duration
    -- is at least 1 microsecond seems to circumvent this problem.
    |> Prelude.max 1
    |> NewRelic.DurationUs

-- | We have to be careful when calculating the difference between two times.
-- Because they are unsigned (don't allow negative numbers), subtracting times
-- in the wrong order is going to result in very large numbers:
--
--     ghci> import GHC.Word
--     ghci> 5 - 2 :: Word64
--     3
--     ghci> 2 - 5 :: Word64
--     18446744073709551613
--
-- The span data we get from Platform should ensure end times always come
-- before start times. If they're not though one of these extremely long span
-- durations can have a major effect on request duration statistics.
--
-- This function performs some defensive programming to prevent flukes from
-- doing major damage.
timeDiff :: Platform.MonotonicTime -> Platform.MonotonicTime -> Platform.MonotonicTime
timeDiff start end =
  if end > start
    then end - start
    else 0

category :: Platform.TracingSpan -> Text
category span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderTracingSpanDetails
          [ Platform.Renderer (\(_ :: MySQL.Info) -> "mysql"),
            Platform.Renderer (\(_ :: Postgres.Info) -> "postgres"),
            Platform.Renderer (\(_ :: Http.Info) -> "http"),
            Platform.Renderer (\(_ :: Monitoring.RequestDetails) -> "request"),
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
  NewRelic.startSegment tx (Just name) (Just cat)
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
    |> Data.Text.pack

data Settings
  = Settings
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
