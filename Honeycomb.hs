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
import qualified Debug
import qualified List
import qualified Data.Aeson as Aeson
-- import qualified Data.Text
import qualified Environment
-- import qualified GHC.Stack as Stack
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Http
-- import qualified Language.Haskell.TH as TH
-- import qualified Maybe
import Observability.Timer (Timer, toISO8601)
import qualified Platform
import qualified Prelude
import qualified Task

report :: Handler -> Text -> Platform.TracingSpan -> Prelude.IO ()
report handler' requestId span = do
  spans <- toSpans handler' requestId Nothing span
  let body = Http.jsonBody spans
  silentHandler' <- Platform.silentHandler
  result <- Http.post (handler_http handler') "idk" body Http.expectWhatever
              |> Task.attempt (silentHandler')
  case result of
    Ok () -> Prelude.pure ()
    Err _wedontcareyet -> Prelude.pure ()

toSpans :: Handler -> Text -> Maybe SpanId -> Platform.TracingSpan -> Prelude.IO [Span]
toSpans handler' requestId parentSpanId span = do
  thisSpansId <- map SpanId nextRandom
  children <- Prelude.traverse (toSpans handler' requestId (Just thisSpansId)) (Platform.children span)
  let duration = (Platform.finished span) - (Platform.started span) |> Platform.inMicroseconds
  let timestamp' = toISO8601 (handler_timer handler') (Platform.started span)
  Prelude.pure <| Span {
    timestamp = timestamp',
    name = Platform.name span,
    spanId = thisSpansId,
    parentId = parentSpanId,
    traceId = requestId,
    serviceName = handler_serviceName handler',
    environment = handler_environment handler',
    durationMs = (Prelude.fromIntegral duration) / 1000
  } : (List.concat children)

data Span = Span {
  timestamp :: Text,
  name :: Text,
  spanId :: SpanId,
  parentId :: Maybe SpanId,
  traceId :: Text,
  serviceName :: Text,
  environment :: Text,
  durationMs :: Float
}
  deriving (Generic)

instance Aeson.ToJSON Span

newtype SpanId = SpanId UUID.UUID
  deriving Aeson.ToJSON

data Handler
  = Handler
      { -- | A bit of state that can be used to turn the clock values attached
        -- to spans into real timestamps.
        handler_timer :: Timer,
        handler_http :: Http.Handler,
        handler_serviceName :: Text,
        handler_environment :: Text
      }

handler :: Timer -> Settings -> Conduit.Acquire Handler
handler timer settings = do
  http <- Http.handler
  Prelude.pure Handler {
    handler_timer = timer,
    handler_http = http,
    handler_serviceName = appName settings,
    handler_environment = appEnvironment settings
  }

data Settings
  = Settings
      { appName :: Text,
        appEnvironment :: Text
      }

decoder :: Environment.Decoder Settings
decoder =
  Debug.todo "hihi"
  -- Prelude.pure Settings
  --   |> andMap logFileDecoder
  --   |> andMap namespaceDecoder
  --   |> andMap environmentDecoder
  --   |> andMap fractionOfSuccessRequestsLoggedDecoder
  --   -- We don't define decoders for the mock* fields used by tests. Tests can
  --   -- construct a Settings object directly without decoding it from env vars.
  --   |> andMap (Prelude.pure Nothing)
  --   |> andMap (Prelude.pure Nothing)

