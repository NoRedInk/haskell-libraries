module Observability.NewRelic (reporter, handler, Handler) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Foldable as Foldable
import qualified Data.Int
import qualified Data.Proxy as Proxy
import qualified Data.Text
import qualified Data.Typeable as Typeable
import qualified Http
import qualified Log
import qualified Maybe
import qualified Monitoring
import qualified MySQL
import qualified Observability.NewRelic.Settings as Settings
import Observability.Timer (Timer, toWord64)
import qualified Platform
import qualified Postgres
import qualified Text
import qualified Tracing.NewRelic as NewRelic
import qualified Prelude

reporter :: Handler -> Platform.Span -> Prelude.IO ()
reporter handler' span =
  case Platform.details span |> andThen Platform.fromSpanDetails of
    Nothing -> Prelude.pure ()
    Just details -> reportWebTransaction handler' span details

data Handler = Handler Timer NewRelic.App

handler :: Timer -> Settings.Settings -> Conduit.Acquire Handler
handler timer settings =
  Conduit.mkAcquire
    ( do
        appConfig <- NewRelic.createAppConfig (Settings.appName settings) (Log.unSecret (Settings.licenseKey settings))
        NewRelic.createApp appConfig (Settings.timeout settings)
          |> map (Handler timer)
    )
    (\_ -> Prelude.pure ())

reportWebTransaction :: Handler -> Platform.Span -> Monitoring.RequestDetails -> Prelude.IO ()
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
        reportSpan timer tx Nothing span
    )

reportError :: NewRelic.Transaction -> Text -> Monitoring.RequestDetails -> Prelude.IO ()
reportError tx msg details =
  noticeError tx 1 msg (Text.fromInt (Monitoring.responseStatus details))

reportSpan ::
  Timer ->
  NewRelic.Transaction ->
  Maybe NewRelic.Segment ->
  Platform.Span ->
  Prelude.IO ()
reportSpan timer tx maybeParent span =
  Exception.bracket
    (chooseAndStartSegment tx span)
    endSegment
    ( \segment -> do
        setSegmentTiming segment (startTime timer span) (toDuration span)
        case maybeParent of
          Just parent -> setSegmentParent segment parent
          Nothing -> setSegmentParentRoot segment
        Platform.children span
          |> Foldable.traverse_ (reportSpan timer tx (Just segment))
    )

chooseAndStartSegment :: NewRelic.Transaction -> Platform.Span -> Prelude.IO NewRelic.Segment
chooseAndStartSegment tx span =
  Platform.details span
    |> andThen
      ( Platform.renderSpanDetails
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

startTime :: Timer -> Platform.Span -> NewRelic.StartTimeUsSinceUnixEpoch
startTime timer span =
  Platform.started span
    |> toWord64 timer
    |> NewRelic.StartTimeUsSinceUnixEpoch

toDuration :: Platform.Span -> NewRelic.DurationUs
toDuration span =
  Platform.finished span - Platform.started span `Prelude.div` 1000
    |> NewRelic.DurationUs

category :: Platform.Span -> Text
category span =
  Platform.details span
    |> Maybe.andThen
      ( Platform.renderSpanDetails
          [ Platform.Renderer (\(_ :: MySQL.Info) -> "mysql"),
            Platform.Renderer (\(_ :: Postgres.Info) -> "postgres"),
            Platform.Renderer (\(_ :: Http.Info) -> "http"),
            Platform.Renderer (\(_ :: Monitoring.RequestDetails) -> "request"),
            Platform.Renderer (\(_ :: Log.LogContexts) -> "haskell")
          ]
      )
    |> Maybe.withDefault "unknown"

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

expectSuccess :: Bool -> Prelude.IO ()
expectSuccess success =
  if success
    then Prelude.pure ()
    else Exception.throwIO NewRelicFailure

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
