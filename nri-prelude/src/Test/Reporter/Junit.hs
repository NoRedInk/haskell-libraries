{-# LANGUAGE NumericUnderscores #-}

-- | Module for presenting test results as a Junit XML file.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Junit
  ( report,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Text
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified Platform
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.FilePath as FilePath
import qualified Test.Internal as Internal
import qualified Text
import qualified Text.XML.JUnit as JUnit
import qualified Prelude

report :: Internal.SuiteResult -> Prelude.IO ()
report result = do
  args <- System.Environment.getArgs
  case getPath args of
    Nothing -> Prelude.pure ()
    Just path -> do
      createPathDirIfMissing path
      JUnit.writeXmlReport path (testResults result)

testResults :: Internal.SuiteResult -> List JUnit.TestSuite
testResults result =
  case result of
    Internal.AllPassed passed ->
      List.map renderPassed passed
    Internal.OnlysPassed passed skipped ->
      List.map renderSkipped skipped
        ++ List.map renderPassed passed
    Internal.PassedWithSkipped passed skipped ->
      List.map renderSkipped skipped
        ++ List.map renderPassed passed
    Internal.TestsFailed passed skipped failed ->
      List.map renderFailed failed
        ++ List.map renderSkipped skipped
        ++ List.map renderPassed passed
    Internal.NoTestsInSuite -> []

renderPassed :: Internal.SingleTest Platform.TracingSpan -> JUnit.TestSuite
renderPassed test =
  JUnit.passed (Internal.name test)
    |> JUnit.time (duration (Internal.body test))
    |> JUnit.inSuite (suiteName test)

renderSkipped :: Internal.SingleTest Internal.Skipped -> JUnit.TestSuite
renderSkipped test =
  JUnit.skipped (Internal.name test)
    |> JUnit.inSuite (suiteName test)

renderFailed :: Internal.SingleTest (Platform.TracingSpan, Internal.Failure) -> JUnit.TestSuite
renderFailed test =
  case Internal.body test of
    (tracingSpan, Internal.FailedAssertion msg) ->
      JUnit.failed (Internal.name test)
        |> JUnit.stderr msg
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.failureStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
    (tracingSpan, Internal.ThrewException err) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage "This test threw an exception."
        |> JUnit.stderr (Data.Text.pack (Exception.displayException err))
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.errorStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
    (tracingSpan, Internal.TookTooLong) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage "This test timed out."
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.errorStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
    (tracingSpan, Internal.TestRunnerMessedUp msg) ->
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
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.errorStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)

suiteName :: Internal.SingleTest a -> Text
suiteName test =
  Internal.describes test
    |> Text.join " - "

stackFrame :: Internal.SingleTest a -> Maybe Text
stackFrame test =
  Internal.loc test
    |> map
      ( \loc ->
          Data.Text.pack
            ( Stack.srcLocFile loc
                ++ ":"
                ++ Prelude.show (Stack.srcLocStartLine loc)
            )
      )

duration :: Platform.TracingSpan -> Float
duration test =
  let duration' = Platform.finished test - Platform.started test
   in Prelude.fromIntegral (Platform.inMicroseconds duration') / 1000_000

getPath :: [Prelude.String] -> Maybe FilePath.FilePath
getPath args =
  case args of
    [] -> Nothing
    "--xml" : path : _ -> Just path
    _ : rest -> getPath rest

createPathDirIfMissing :: FilePath.FilePath -> Prelude.IO ()
createPathDirIfMissing path = do
  dirPath <- map FilePath.takeDirectory (Directory.canonicalizePath path)
  Directory.createDirectoryIfMissing True dirPath
