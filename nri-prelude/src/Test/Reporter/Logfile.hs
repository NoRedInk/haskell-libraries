module Test.Reporter.Logfile
  ( report,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Time as Time
import qualified List
import qualified Maybe
import NriPrelude
import qualified Platform.Internal as Platform
import qualified System.IO
import qualified Test.Internal as Internal
import qualified Tuple
import qualified Prelude

report :: Time.UTCTime -> System.IO.Handle -> Internal.SuiteResult -> Prelude.IO ()
report now handle results = do
  let testSpans = spans results
  let rootSpan =
        Platform.TracingSpan
          { Platform.name = "test run",
            Platform.started =
              List.minimum (List.map Platform.started testSpans)
                |> Maybe.withDefault (Platform.MonotonicTime 0),
            Platform.finished =
              List.maximum (List.map Platform.finished testSpans)
                |> Maybe.withDefault (Platform.MonotonicTime 0),
            Platform.frame = Nothing,
            Platform.details = Nothing,
            Platform.succeeded = case results of
              Internal.AllPassed _ -> Platform.Succeeded
              _ -> Platform.Failed,
            Platform.allocated = 0,
            Platform.children = testSpans
          }
  Aeson.encode (now, rootSpan)
    |> Data.ByteString.Lazy.hPut handle
  Data.ByteString.Lazy.hPut handle "\n"

spans :: Internal.SuiteResult -> [Platform.TracingSpan]
spans results =
  case results of
    Internal.AllPassed tests -> List.map Internal.body tests
    Internal.OnlysPassed tests _ -> List.map Internal.body tests
    Internal.PassedWithSkipped tests _ -> List.map Internal.body tests
    Internal.TestsFailed passed _ failed ->
      List.map Internal.body passed
        ++ List.map (Tuple.first << Internal.body) failed
    Internal.NoTestsInSuite -> []
