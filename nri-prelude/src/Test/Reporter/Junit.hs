{-# LANGUAGE NumericUnderscores #-}

-- | Module for presenting test results as a Junit XML file.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Junit
  ( report,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString as BS
import qualified Data.Text
import qualified Data.Text.Encoding as TE
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified Platform
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Internal as Internal
import qualified Test.Reporter.Internal
import qualified Text
import qualified Text.Colour
import qualified Text.XML.JUnit as JUnit
import qualified Prelude

report :: FilePath.FilePath -> Internal.SuiteResult -> Prelude.IO ()
report path result = do
  createPathDirIfMissing path
  results <- testResults result
  JUnit.writeXmlReport path results

testResults :: Internal.SuiteResult -> Prelude.IO (List JUnit.TestSuite)
testResults result =
  case result of
    Internal.AllPassed passed ->
      List.map renderPassed passed
        |> Prelude.pure
    Internal.OnlysPassed passed skipped ->
      Prelude.pure
        ( List.map renderSkipped skipped
            ++ List.map renderPassed passed
        )
    Internal.PassedWithSkipped passed skipped ->
      Prelude.pure
        ( List.map renderSkipped skipped
            ++ List.map renderPassed passed
        )
    Internal.TestsFailed passed skipped failed -> do
      srcLocs <-
        List.map (map (\(Internal.FailedSpan _ failure) -> failure)) failed
          |> Prelude.traverse Test.Reporter.Internal.readSrcLoc
      let renderedFailed = List.map2 renderFailed failed srcLocs
      Prelude.pure
        ( renderedFailed
            ++ List.map renderSkipped skipped
            ++ List.map renderPassed passed
        )
    Internal.NoTestsInSuite -> Prelude.pure []

renderPassed :: Internal.SingleTest Platform.TracingSpan -> JUnit.TestSuite
renderPassed test =
  JUnit.passed (Internal.name test)
    |> JUnit.time (duration (Internal.body test))
    |> JUnit.inSuite (suiteName test)

renderSkipped :: Internal.SingleTest Internal.NotRan -> JUnit.TestSuite
renderSkipped test =
  JUnit.skipped (Internal.name test)
    |> JUnit.inSuite (suiteName test)

renderFailed ::
  Internal.SingleTest Internal.FailedSpan ->
  Maybe (Stack.SrcLoc, BS.ByteString) ->
  JUnit.TestSuite
renderFailed test maybeSrcLoc =
  case Internal.body test of
    Internal.FailedSpan tracingSpan (Internal.FailedAssertion msg _) ->
      let msg' = case maybeSrcLoc of
            Nothing -> msg
            Just (loc, src) ->
              Test.Reporter.Internal.renderSrcLoc loc src
                |> Text.Colour.renderChunksUtf8BS Text.Colour.WithoutColours
                |> TE.decodeUtf8
                |> (\srcStr -> srcStr ++ "\n" ++ msg)
       in JUnit.failed (Internal.name test)
            |> JUnit.stderr msg'
            |> JUnit.failureStackTrace [stackFrame test]
            |> JUnit.time (duration tracingSpan)
            |> JUnit.inSuite (suiteName test)
    Internal.FailedSpan tracingSpan (Internal.ThrewException err) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage "This test threw an exception."
        |> JUnit.stderr (Data.Text.pack (Exception.displayException err))
        |> JUnit.errorStackTrace [stackFrame test]
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
    Internal.FailedSpan tracingSpan (Internal.TookTooLong) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage "This test timed out."
        |> JUnit.errorStackTrace [stackFrame test]
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
    Internal.FailedSpan tracingSpan (Internal.TestRunnerMessedUp msg) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage
          ( Text.join
              "\n"
              [ "Test runner encountered an unexpected error:",
                msg,
                "",
                "This is a bug.",
                "If you have some time to report the bug it would be much appreciated!",
                "You can do so here: https://github.com/NoRedInk/haskell-libraries/issues"
              ]
          )
        |> JUnit.errorStackTrace [stackFrame test]
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)

suiteName :: Internal.SingleTest a -> Text
suiteName test =
  Internal.describes test
    |> Text.join " - "

stackFrame :: Internal.SingleTest a -> Text
stackFrame test =
  let loc = Internal.loc test
   in Data.Text.pack
        ( Stack.srcLocFile loc
            ++ ":"
            ++ Prelude.show (Stack.srcLocStartLine loc)
        )

duration :: Platform.TracingSpan -> Float
duration test =
  let duration' = Platform.finished test - Platform.started test
   in Prelude.fromIntegral (Platform.inMicroseconds duration') / 1000_000

createPathDirIfMissing :: FilePath.FilePath -> Prelude.IO ()
createPathDirIfMissing path = do
  dirPath <- map FilePath.takeDirectory (Directory.canonicalizePath path)
  Directory.createDirectoryIfMissing True dirPath
