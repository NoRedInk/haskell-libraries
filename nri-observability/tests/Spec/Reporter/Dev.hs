{-# LANGUAGE CPP #-}

module Spec.Reporter.Dev (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Time.LocalTime as LocalTime
import qualified Expect
import GoldenHelpers (goldenResultsDir)
import qualified Log.HttpRequest as HttpRequest
import qualified Platform
import qualified Platform.Timer as Timer
import qualified Reporter.Dev.Internal as Dev
import qualified Test
import qualified Prelude

tests :: Test.Test
tests =
  Test.describe
    "Observability.Dev"
    [ Test.describe "log tests" logTests,
      Test.describe "advertise tests" advertiseTests
    ]

logTests :: [Test.Test]
logTests =
  [ logTest
      "log span without details or children"
      emptyTracingSpan
        { Platform.name = "root-span",
          Platform.started = ms 5
        },
    logTest
      "log span with exception"
      emptyTracingSpan
        { Platform.name = "root-span",
          Platform.succeeded = Platform.FailedWith (Exception.SomeException (CustomException "toast!"))
        },
    logTest
      "logs information about an incoming http request"
      emptyTracingSpan
        { Platform.details =
            HttpRequest.emptyDetails
              { HttpRequest.endpoint = Just "/hats/:hat_id"
              }
              |> HttpRequest.Incoming
              |> Platform.toTracingSpanDetails
              |> Just
        }
  ]

logTest :: Text -> Platform.TracingSpan -> Test.Test
logTest name span =
  Test.test name <| \_ -> do
    let logfile = Text.replace " " "-" name
    let logpath = goldenResultsDir ++ "/dev-reporter-" ++ logfile
    span
      |> Dev.mkLog timer
      |> Data.Text.Lazy.Builder.toLazyText
      |> Data.Text.Lazy.toStrict
      |> Expect.equalToContentsOf logpath

newtype CustomException = CustomException Text deriving (Show)

instance Exception.Exception CustomException

ms :: Int -> Platform.MonotonicTime
ms n = Prelude.fromIntegral (n * 1000)

timer :: Timer.Timer
timer = Timer.Timer 0 LocalTime.utc

emptyTracingSpan :: Platform.TracingSpan
emptyTracingSpan =
  Platform.emptyTracingSpan
    { Platform.name = "Example TracingSpan",
      Platform.started = 0,
      Platform.finished = 0,
      Platform.frame = Nothing,
      Platform.details = Nothing,
      Platform.succeeded = Platform.Succeeded,
      Platform.allocated = 0,
      Platform.children = []
    }

advertiseTests :: [Test.Test]
advertiseTests =
  [ Test.test "does not advertises on launch" <| \_ -> do
      Dev.shouldAdvertise 0 0 Nothing |> Expect.equal False,
    Test.test "does not advertise if still logging (on launch)" <| \_ -> do
      Dev.shouldAdvertise 0 10 Nothing |> Expect.equal False,
    Test.test "does not advertise if still logging" <| \_ -> do
      Dev.shouldAdvertise 0 10 (Just 0) |> Expect.equal False,
    Test.test "does not readvertise" <| \_ -> do
      Dev.shouldAdvertise 1 1 (Just 1) |> Expect.equal False,
    Test.test "does not advertise if still logging (on launch)" <| \_ -> do
      Dev.shouldAdvertise 0 10 Nothing |> Expect.equal False,
    Test.test "does not advertise if still logging" <| \_ -> do
      Dev.shouldAdvertise 0 10 (Just 0) |> Expect.equal False,
    Test.test "only advertises if counter hasn't changed and we haven't advertised it yet" <| \_ -> do
      Dev.shouldAdvertise 10 10 (Just 0) |> Expect.equal True
  ]
