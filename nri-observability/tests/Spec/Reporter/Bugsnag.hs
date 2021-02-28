module Spec.Reporter.Bugsnag (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Data.Time.LocalTime as LocalTime
import qualified Dict
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Log
import qualified Log.HttpRequest as HttpRequest
import qualified Log.RedisCommands as RedisCommands
import qualified Log.SqlQuery as SqlQuery
import qualified Network.Bugsnag
import qualified Platform
import qualified Platform.Timer as Timer
import qualified Reporter.Bugsnag.Internal as Bugsnag
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "Observability.Bugsnag"
    [ test "encodes span marked as failed as an exception" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root span",
            Platform.succeeded = Platform.Failed
          }
          |> encodesTo "bugsnag-failure-without-exception",
      test "if an exception was thrown use that exceptions name" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded =
              Platform.FailedWith (Exception.SomeException (CustomException "something went wrong"))
          }
          |> encodesTo "bugsnag-failure-with-exception",
      test "identifies the most recently started failing span as the root cause failure" <| \_ ->
        emptyTracingSpan
          { Platform.name = "parent of root cause",
            Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.name = "after root cause",
                    Platform.succeeded = Platform.Succeeded
                  },
                emptyTracingSpan
                  { Platform.name = "root cause",
                    Platform.succeeded = Platform.Failed,
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.name = "child of root cause",
                            Platform.succeeded = Platform.Succeeded
                          }
                      ]
                  },
                emptyTracingSpan
                  { Platform.name = "before root cause",
                    Platform.succeeded = Platform.Succeeded
                  }
              ]
          }
          |> encodesTo "bugsnag-failure-with-root-cause",
      test "shows stack trace from the root span to the root cause span" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.frame = Just ("function1", Stack.SrcLoc "package1" "Module2" "Filename1.hs" 0 1 2 3),
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.frame = Just ("function2", Stack.SrcLoc "package2" "Module2" "Filename2.hs" 10 11 12 13)
                  },
                emptyTracingSpan
                  { Platform.succeeded = Platform.Failed,
                    Platform.frame = Just ("function3", Stack.SrcLoc "package3" "Module3" "Filename3.hs" 20 21 22 23),
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.succeeded = Platform.Succeeded,
                            Platform.frame = Just ("function4", Stack.SrcLoc "package4" "Module4" "Filename4.hs" 30 31 32 33)
                          }
                      ]
                  },
                emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.frame = Just ("function5", Stack.SrcLoc "package5" "Module5" "Filename5.hs" 40 41 42 43)
                  }
              ]
          }
          |> encodesTo "bugsnag-failure-stack-trace",
      test "encodes information about an incoming http request" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
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
          |> encodesTo "bugsnag-failure-incoming-http-request",
      test "renders sql query failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.details =
              SqlQuery.emptyDetails
                { SqlQuery.query = Just (Log.mkSecret "SELECT name FROM ants WHERE id = 3"),
                  SqlQuery.databaseType = Just SqlQuery.postgresql,
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
          |> encodesTo "bugsnag-failure-sql",
      test "renders redis query failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.details =
              RedisCommands.emptyDetails
                { RedisCommands.host = Just "cache.noredink.com",
                  RedisCommands.port = Just 6379,
                  RedisCommands.commands = ["GET somekey"]
                }
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> encodesTo "bugsnag-failure-redis",
      test "renders outgoing http request failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Failed,
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
              ]
          }
          |> encodesTo "bugsnag-failure-http",
      test "renders log failures correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.name = "log message",
            Platform.details =
              Log.LogContexts
                [ Log.context "number" (4 :: Int),
                  Log.context "text" ("Hi!" :: Text),
                  Log.context
                    "triage"
                    ( Log.TriageInfo Log.UserBlocked "close the windows!"
                    )
                ]
                |> Platform.toTracingSpanDetails
                |> Just
          }
          |> encodesTo "bugsnag-failure-log",
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
          |> encodesTo "bugsnag-failure-with-context",
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
          |> encodesTo "bugsnag-failure-unknown",
      test "non-failing spans are encoded as breadcrumbs in the right order" <| \_ ->
        emptyTracingSpan
          { Platform.name = "root-span (failed)",
            Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.name = "child-3 (leaf)",
                    Platform.succeeded = Platform.Succeeded
                  },
                emptyTracingSpan
                  { Platform.name = "child-2 (failed)",
                    Platform.succeeded = Platform.Failed,
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.name = "child-3-grandchild-3 (failed, root cause)",
                            Platform.succeeded = Platform.Failed,
                            Platform.children =
                              [ emptyTracingSpan
                                  { Platform.name = "child-3-grandchild-3-greatgrandchild-2 (leaf)",
                                    Platform.succeeded = Platform.Succeeded
                                  },
                                emptyTracingSpan
                                  { Platform.name = "child-3-grandchild-3-greatgrandchild-1 (leaf)",
                                    Platform.succeeded = Platform.Succeeded
                                  }
                              ]
                          },
                        emptyTracingSpan
                          { Platform.name = "child-2-grandchild-2",
                            Platform.succeeded = Platform.Succeeded,
                            Platform.children =
                              [ emptyTracingSpan
                                  { Platform.name = "child-2-grandchild-2-greatgrandchild-2 (leaf)",
                                    Platform.succeeded = Platform.Failed
                                  },
                                emptyTracingSpan
                                  { Platform.name = "child-2-grandchild-2-greatgrandchild-1 (leaf)",
                                    Platform.succeeded = Platform.Succeeded
                                  }
                              ]
                          },
                        emptyTracingSpan
                          { Platform.name = "child-2-grandchild-1 (leaf)",
                            Platform.succeeded = Platform.Succeeded
                          }
                      ]
                  },
                emptyTracingSpan
                  { Platform.name = "child-1 (leaf)",
                    Platform.succeeded = Platform.Succeeded
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-right-order",
      test "renders sql query breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      SqlQuery.emptyDetails
                        { SqlQuery.query = Just (Log.mkSecret "SELECT name FROM ants WHERE id = 3"),
                          SqlQuery.databaseType = Just SqlQuery.postgresql,
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
          |> encodesTo "bugsnag-breadcrumbs-sql",
      test "renders redis query breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      RedisCommands.emptyDetails
                        { RedisCommands.host = Just "cache.noredink.com",
                          RedisCommands.port = Just 6379,
                          RedisCommands.commands = ["GET somekey"]
                        }
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-redis",
      test "renders outgoing http request breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.succeeded = Platform.Succeeded,
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
                      ]
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-http",
      test "renders log breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      Log.LogContexts
                        [ Log.context "number" (4 :: Int),
                          Log.context "text" ("Hi!" :: Text),
                          Log.context
                            "triage"
                            ( Log.TriageInfo Log.UserBlocked "close the windows!"
                            )
                        ]
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-log",
      test "renders withContext breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
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
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-with-context",
      test "renders unknown spans breadcrumbs correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.details =
                      CustomDetails "The deets!"
                        |> Platform.toTracingSpanDetails
                        |> Just
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-unknown",
      test "renders timestamps correctly" <| \_ ->
        emptyTracingSpan
          { Platform.succeeded = Platform.Failed,
            Platform.started = ms 4,
            Platform.finished = ms 9, -- 8 ms
            Platform.children =
              [ emptyTracingSpan
                  { Platform.succeeded = Platform.Succeeded,
                    Platform.name = "outer",
                    Platform.started = ms 5,
                    Platform.finished = ms 8, -- 8 ms
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.succeeded = Platform.Succeeded,
                            Platform.name = "inner",
                            Platform.started = ms 6, -- 6 ms
                            Platform.finished = ms 7, -- 7 ms
                            Platform.children =
                              []
                          }
                      ]
                  }
              ]
          }
          |> encodesTo "bugsnag-breadcrumbs-timestamps"
    ]

newtype CustomException = CustomException Text deriving (Show)

instance Exception.Exception CustomException

newtype CustomDetails = CustomDetails Text
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails CustomDetails

ms :: Int -> Platform.MonotonicTime
ms n = Prelude.fromIntegral (n * 1000)

timer :: Timer.Timer
timer = Timer.Timer 0 LocalTime.utc

emptyTracingSpan :: Platform.TracingSpan
emptyTracingSpan =
  Platform.emptyTracingSpan
    { Platform.name = "some span",
      Platform.started = 0,
      Platform.finished = 0,
      Platform.frame = Nothing,
      Platform.details = Nothing,
      Platform.succeeded = Platform.Succeeded,
      Platform.allocated = 0,
      Platform.children = []
    }

encodesTo :: Text -> Platform.TracingSpan -> Expect.Expectation
encodesTo filename span =
  span
    |> Bugsnag.toEvent "request-id-1234" timer Network.Bugsnag.defaultEvent
    |> Data.Aeson.Encode.Pretty.encodePretty
    |> Data.ByteString.Lazy.toStrict
    |> Data.Text.Encoding.decodeUtf8
    |> Expect.equalToContentsOf ("tests/golden-results/" ++ filename ++ ".json")
