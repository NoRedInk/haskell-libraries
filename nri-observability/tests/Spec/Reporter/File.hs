module Spec.Reporter.File (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Time.LocalTime as LocalTime
import qualified Dict
import qualified Expect
import qualified Log.HttpRequest as HttpRequest
import qualified Log.RedisCommands as RedisCommands
import qualified Log.SqlQuery as SqlQuery
import qualified Platform
import qualified Platform.Timer as Timer
import qualified Reporter.File.Internal as File
import qualified Test
import qualified Prelude

tests :: Test.Test
tests =
  Test.describe
    "Observability.FileSpec"
    [ logTest
        "logs span without details or children"
        emptyTracingSpan
          { Platform.name = "root-span",
            Platform.finished = ms 7
          },
      logTest
        "logs span that failed with an exception"
        emptyTracingSpan
          { Platform.name = "root-span",
            Platform.succeeded = Platform.FailedWith (Exception.SomeException (CustomException "help!"))
          },
      logTest
        "logs nested spans"
        emptyTracingSpan
          { Platform.name = "root-span",
            Platform.children =
              [ emptyTracingSpan
                  { Platform.name = "child1",
                    Platform.children =
                      [ emptyTracingSpan
                          { Platform.name = "grandchild",
                            Platform.children = []
                          }
                      ]
                  },
                emptyTracingSpan
                  { Platform.name = "child2",
                    Platform.children = []
                  }
              ]
          },
      logTest
        "logs information about an incoming http request"
        emptyTracingSpan
          { Platform.details =
              HttpRequest.emptyDetails
                { HttpRequest.httpVersion = Just "1",
                  HttpRequest.method = Just "GET",
                  HttpRequest.path = Just "/hats/5",
                  HttpRequest.queryString = Just "?top=flat",
                  HttpRequest.endpoint = Just "/hats/:hat_id",
                  HttpRequest.headers = Dict.fromList [("Accept", "application/json")],
                  HttpRequest.status = Just 500
                }
                |> HttpRequest.Incoming
                |> Platform.toTracingSpanDetails
                |> Just
          },
      logTest
        "logs information about an SQL query"
        emptyTracingSpan
          { Platform.details =
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
          },
      logTest
        "logs information about a redis query"
        emptyTracingSpan
          { Platform.details =
              RedisCommands.emptyDetails
                { RedisCommands.host = Just "cache.noredink.com",
                  RedisCommands.port = Just 6379,
                  RedisCommands.commands = ["GET somekey"]
                }
                |> Platform.toTracingSpanDetails
                |> Just
          },
      logTest
        "logs information about an outoing http request"
        emptyTracingSpan
          { Platform.details =
              HttpRequest.emptyDetails
                { HttpRequest.httpVersion = Just "1",
                  HttpRequest.method = Just "GET",
                  HttpRequest.path = Just "/hats/5",
                  HttpRequest.queryString = Just "?top=flat",
                  HttpRequest.endpoint = Just "/hats/:hat_id",
                  HttpRequest.headers = Dict.fromList [("Accept", "application/json")],
                  HttpRequest.status = Just 500
                }
                |> HttpRequest.Incoming
                |> Platform.toTracingSpanDetails
                |> Just
          },
      logTest
        "logs log messages"
        emptyTracingSpan
          { Platform.details =
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

logTest :: Text -> Platform.TracingSpan -> Test.Test
logTest name span =
  Test.test name <| \_ -> do
    let logContext =
          File.LogContext
            { File.requestId = "request-id",
              File.namespace = ["app"],
              File.environment = "narnia",
              File.hostname = "some-host",
              File.timer = Timer.Timer 0 LocalTime.utc
            }
    let reEncodingResult =
          File.logItemRecursively logContext span []
            -- Pretty-print each log entry to make test files more readable.
            |> Prelude.traverse reEncodePretty
    case reEncodingResult of
      Err err -> Expect.fail err
      Ok reEncoded ->
        Text.join "\n" reEncoded
          |> Expect.equalToContentsOf
            ( Text.fromList "tests/golden-results/file-reporter-"
                ++ Text.replace " " "-" name
            )

reEncodePretty :: Data.ByteString.Lazy.ByteString -> Result Text Text
reEncodePretty str =
  case Aeson.eitherDecode' str of
    Prelude.Left err -> Err (Text.fromList err)
    Prelude.Right (json :: Aeson.Value) ->
      Data.Aeson.Encode.Pretty.encodePretty json
        |> Data.Text.Lazy.Encoding.decodeUtf8
        |> Data.Text.Lazy.toStrict
        |> Ok

newtype CustomException = CustomException Text deriving (Show)

instance Exception.Exception CustomException

ms :: Int -> Platform.MonotonicTime
ms n = Prelude.fromIntegral (1000 * n)

emptyTracingSpan :: Platform.TracingSpan
emptyTracingSpan =
  Platform.emptyTracingSpan
    { Platform.name = "",
      Platform.started = 0,
      Platform.finished = 0,
      Platform.frame = Nothing,
      Platform.details = Nothing,
      Platform.succeeded = Platform.Succeeded,
      Platform.allocated = 0,
      Platform.children = []
    }
