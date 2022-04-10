{-# LANGUAGE NumericUnderscores #-}

module Spec.Reporter.Honeycomb (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified Data.Time.LocalTime as LocalTime
import qualified Dict
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.Kafka as Kafka
import qualified Log.RedisCommands as RedisCommands
import qualified Log.SqlQuery as SqlQuery
import qualified Platform
import qualified Platform.Timer as Timer
import qualified Reporter.Honeycomb.Internal as Honeycomb
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "Observability.Honeycomb"
    [ test "encodes span marked as failed as an exception" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.succeeded = Platform.Failed
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-without-exception",
      test "if an exception was thrown use that exceptions name" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.frame = Just ("function1", Stack.SrcLoc "package1" "Module2" "Filename1.hs" 0 1 2 3)
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-source-location",
      test "encodes span with source location" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.succeeded =
              Platform.FailedWith (Exception.SomeException (CustomException "something went wrong"))
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-with-exception",
      test "encodes information about an incoming http request" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Incoming HTTP Request",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              HttpRequest.emptyDetails
                { HttpRequest.httpVersion = Just "1",
                  HttpRequest.method = Just "GET",
                  HttpRequest.path = Just "/hats/5",
                  HttpRequest.queryString = Just "?top=flat",
                  HttpRequest.endpoint = Just "GET /hats/:hat_id",
                  HttpRequest.headers = Dict.fromList [("Accept", "application/json")],
                  HttpRequest.status = Just 500
                }
                |> HttpRequest.Incoming
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-incoming-http-request",
      test "encodes information about an processing kafka message" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Processing Kafka Message",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              Kafka.emptyDetails
                { Kafka.topic = Just "topic",
                  Kafka.partitionId = Just 12,
                  Kafka.key = Just "key",
                  Kafka.contents = Just (Kafka.mkContents ()),
                  Kafka.createTime = Just (Clock.POSIX.posixSecondsToUTCTime 0),
                  Kafka.logAppendTime = Just (Clock.POSIX.posixSecondsToUTCTime 0),
                  Kafka.timeSinceLastRebalance = Just 0,
                  Kafka.processAttempt = Just 0,
                  Kafka.assignedPartitions = Just 1,
                  Kafka.pausedPartitions = Just 2,
                  Kafka.requestId = Just "requestId"
                }
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-processing-kafka-message",
      test "renders sql query failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.name = "MySQL Query",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              SqlQuery.emptyDetails
                { SqlQuery.databaseType = Just SqlQuery.postgresql,
                  SqlQuery.query = Just (Log.mkSecret "SELECT name FROM ants WHERE id = ?"),
                  SqlQuery.queryTemplate = Just "SELECT name FROM ants WHERE id = ${id}",
                  SqlQuery.sqlOperation = Just "SELECT",
                  SqlQuery.queriedRelation = Just "ants",
                  SqlQuery.host = Just "http://antsonline.com",
                  SqlQuery.port = Just 5432,
                  SqlQuery.database = Just "nri",
                  SqlQuery.rowsReturned = Just 5
                }
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-sql",
      test "renders redis query failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Make Redis Query",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              RedisCommands.emptyDetails
                { RedisCommands.host = Just "cache.noredink.com",
                  RedisCommands.port = Just 6379,
                  RedisCommands.commands = ["GET somekey"]
                }
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-redis",
      test "renders outgoing http request failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Making HTTP request",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              HttpRequest.emptyDetails
                { HttpRequest.host = Just "http://antsonline.com",
                  HttpRequest.path = Just "/bulletin",
                  HttpRequest.method = Just "GET"
                }
                |> HttpRequest.Outgoing
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-http",
      test "renders log failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.name = "log message",
            Platform.details =
              Log.LogContexts
                [ Log.context "number" (4 :: Int),
                  Log.context "text" ("Hi!" :: Text),
                  Log.context "advisory" "close the windows!"
                ]
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-log",
      test "renders withContext failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.name = "some context",
            Platform.details =
              Log.LogContexts
                [ Log.context "number" (4 :: Int)
                ]
                |> Platform.toTracingSpanDetails
                |> Just,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded
                  }
              ]
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-with-context",
      test "merges unknown span details from nested spans into metadata" <| \_ ->
        emptyTracingSpan
          { Platform.name = "measure weather",
            Platform.succeeded = Platform.Failed,
            Platform.details =
              CustomDetails "32 C"
                |> Platform.toTracingSpanDetails
                |> Just,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.name = "measure humidity",
                    Platform.succeeded = Platform.Failed,
                    Platform.details =
                      CustomDetails "25 %"
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-failure-unknown",
      test "tags all child spans with the root's endpoint" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.children =
              [ emptyTracingSpan
                  { Platform.children = [emptyTracingSpan]
                  }
              ],
            Platform.details =
              HttpRequest.emptyDetails
                { HttpRequest.httpVersion = Just "1",
                  HttpRequest.method = Just "GET",
                  HttpRequest.path = Just "/hats/5",
                  HttpRequest.queryString = Just "?top=flat",
                  HttpRequest.endpoint = Just "GET /hats/:hat_id",
                  HttpRequest.headers = Dict.fromList [("Accept", "application/json")],
                  HttpRequest.status = Just 500
                }
                |> HttpRequest.Incoming
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-child-spans-endpoint",
      test "de-noises nested log events at enrichment time" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Failed,
                    Platform.name = "log message",
                    Platform.details =
                      Log.LogContexts
                        [ Log.context "number" (4 :: Int),
                          Log.context "text" ("Hi!" :: Text),
                          Log.context
                            "nested"
                            ( HashMap.fromList
                                [ ("a", HashMap.fromList [("b", 1)])
                                ] ::
                                HashMap.HashMap Text (HashMap.HashMap Text Int)
                            )
                        ]
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-denoising-log",
      test "de-noises redis queries at enrichment time" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Make Redis query",
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      RedisCommands.emptyDetails
                        { RedisCommands.host = Just "cache.noredink.com",
                          RedisCommands.port = Just 6379,
                          RedisCommands.commands =
                            ["GET somekey", "GET otherkey", "HGETALL idk"]
                        }
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-denoising-redis",
      test "doesn't touch sql spans" <| \_ ->
        emptyTracingSpan
          { Platform.name = "Make Postgres query",
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      SqlQuery.emptyDetails
                        { SqlQuery.databaseType = Just SqlQuery.postgresql,
                          SqlQuery.query = Just (Log.mkSecret "SELECT name FROM ants WHERE id = ?"),
                          SqlQuery.queryTemplate = Just "SELECT name FROM ants WHERE id = ${id}",
                          SqlQuery.sqlOperation = Just "SELECT",
                          SqlQuery.queriedRelation = Just "ants",
                          SqlQuery.host = Just "http://antsonline.com",
                          SqlQuery.port = Just 5432,
                          SqlQuery.database = Just "nri",
                          SqlQuery.rowsReturned = Just 5
                        }
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> toBatchEvents
          |> encodesTo "honeycomb-denoising-sql",
      describe
        "sampleRateForDuration"
        [ test "request duration affects sample rate as expected" <| \_ -> do
            let baseRate = 0.001
            let apdexTMs = 10
            let durations = List.map (\n -> round (2 ^ toFloat n)) (List.range 1 8)
            let sampleRates =
                  List.map
                    ( \duration ->
                        Honeycomb.sampleRateForDuration
                          baseRate
                          (toFloat duration)
                          apdexTMs
                    )
                    durations
            List.map2
              ( \duration sampleRate ->
                  Text.fromInt duration ++ "ms: " ++ Text.fromFloat sampleRate
              )
              durations
              sampleRates
              |> (:) ("apdex T: " ++ Text.fromFloat apdexTMs ++ "ms")
              |> Text.join "\n"
              |> Expect.equalToContentsOf "test/golden-results/observability-spec-honeycomb-sampling"
        ],
      describe
        "deriveSampleRate"
        [ test "modifies configured sample rate by route" <| \_ -> do
            let matchingSpan =
                  emptyTracingSpan
                    { Platform.name = "matching-span"
                    }
            let customRate = 1.0
            let defaultRate = 0.1
            let settings =
                  emptySettings
                    { Honeycomb.fractionOfSuccessRequestsLogged = defaultRate,
                      Honeycomb.modifyFractionOfSuccessRequestsLogged =
                        \rate span ->
                          case Platform.name span of
                            "matching-span" -> customRate
                            _ -> rate
                    }
            Honeycomb.deriveSampleRate matchingSpan settings
              |> Expect.within (Expect.Absolute 0.001) customRate

            Honeycomb.deriveSampleRate emptyTracingSpan settings
              |> Expect.within (Expect.Absolute 0.001) defaultRate,
          test "modifies configured apdex by route" <| \_ -> do
            let matchingSpan =
                  emptyTracingSpan
                    { Platform.name = "matching-span",
                      Platform.started = 0,
                      Platform.finished = 1000
                    }
            let settings =
                  emptySettings
                    { Honeycomb.fractionOfSuccessRequestsLogged = 0.1,
                      Honeycomb.apdexTimeMs = Prelude.maxBound,
                      Honeycomb.modifyApdexTimeMs =
                        \defaultApdex span ->
                          case Platform.name span of
                            "matching-span" -> 10
                            _ -> defaultApdex
                    }
            Honeycomb.deriveSampleRate matchingSpan settings
              |> Expect.notWithin (Expect.Absolute 0.001) 0.1

            Honeycomb.deriveSampleRate emptyTracingSpan settings
              |> Expect.within (Expect.Absolute 0.001) 0.1
        ]
    ]

newtype CustomException = CustomException Text deriving (Show)

instance Exception.Exception CustomException

newtype CustomDetails = CustomDetails Text
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails CustomDetails

timer :: Timer.Timer
timer = Timer.Timer 0 LocalTime.utc

emptyTracingSpan :: Platform.TracingSpan
emptyTracingSpan =
  Platform.emptyTracingSpan
    { Platform.name = "some-span",
      Platform.started = 0,
      Platform.finished = 0,
      Platform.frame = Nothing,
      Platform.details = Nothing,
      Platform.succeeded = Platform.Succeeded,
      Platform.allocated = 0,
      Platform.children = []
    }

emptySettings :: Honeycomb.Settings
emptySettings =
  Honeycomb.Settings
    { Honeycomb.apiKey = Log.mkSecret "api-key",
      Honeycomb.datasetName = "dataset",
      Honeycomb.serviceName = "service",
      Honeycomb.fractionOfSuccessRequestsLogged = 0.0,
      Honeycomb.apdexTimeMs = 10,
      Honeycomb.modifyFractionOfSuccessRequestsLogged = always,
      Honeycomb.modifyApdexTimeMs = always
    }

toBatchEvents :: Platform.TracingSpan -> [Honeycomb.BatchEvent]
toBatchEvents span =
  let commonFields =
        Honeycomb.SharedTraceData
          { Honeycomb.timer = timer,
            Honeycomb.requestId = "request-id-123",
            Honeycomb.endpoint = Just "GET /hats/:hat_id",
            Honeycomb.sampleRate = 1,
            Honeycomb.initSpan =
              Honeycomb.emptySpan
                |> Honeycomb.addField "service_name" "some service"
                |> Honeycomb.addField "hostname" "some-service-oaiefiowh-aoidawi"
                |> Honeycomb.addField "apdex" 1
                |> Honeycomb.addField "revision" (Honeycomb.Revision "gitref")
                -- Don't use requestId if we don't do Distributed Tracing
                -- Else, it will create traces with no parent sharing the same TraceId
                -- Which makes Honeycomb's UI confused
                |> Honeycomb.addField "trace.trace_id" "request-id-123"
          }
   in Honeycomb.toBatchEvents commonFields span

encodesTo :: Text -> [Honeycomb.BatchEvent] -> Expect.Expectation
encodesTo filename events =
  let config =
        Data.Aeson.Encode.Pretty.defConfig
          { Data.Aeson.Encode.Pretty.confCompare = Data.Aeson.Encode.Pretty.compare
          }
   in events
        |> Data.Aeson.Encode.Pretty.encodePretty' config
        |> Data.ByteString.Lazy.toStrict
        |> Data.Text.Encoding.decodeUtf8
        |> Expect.equalToContentsOf ("test/golden-results/" ++ filename ++ ".json")
