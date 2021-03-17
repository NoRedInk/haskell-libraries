{-# LANGUAGE NumericUnderscores #-}

-- | Module for presenting test results as a Junit XML file.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Junit
  ( report,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy
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
      renderedFailed <- Prelude.traverse renderFailed failed
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

renderFailed :: Internal.SingleTest (Platform.TracingSpan, Internal.Failure) -> Prelude.IO JUnit.TestSuite
renderFailed test =
  case Internal.body test of
    (tracingSpan, Internal.FailedAssertion msg maybeLoc) -> do
      msg' <- case maybeLoc of
        Nothing -> Prelude.pure msg
        Just loc -> do
          result <- Test.Reporter.Internal.renderSrcLoc (\_ -> identity) loc
          Builder.toLazyByteString result
            |> Data.ByteString.Lazy.toStrict
            |> TE.decodeUtf8
            |> (\src -> src ++ "\n" ++ msg)
            |> Prelude.pure
      JUnit.failed (Internal.name test)
        |> JUnit.stderr msg'
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.failureStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
        |> Prelude.pure
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
        |> Prelude.pure
    (tracingSpan, Internal.TookTooLong) ->
      JUnit.errored (Internal.name test)
        |> JUnit.errorMessage "This test timed out."
        |> ( case stackFrame test of
               Nothing -> identity
               Just frame -> JUnit.errorStackTrace [frame]
           )
        |> JUnit.time (duration tracingSpan)
        |> JUnit.inSuite (suiteName test)
        |> Prelude.pure
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
        |> Prelude.pure

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

createPathDirIfMissing :: FilePath.FilePath -> Prelude.IO ()
createPathDirIfMissing path = do
  dirPath <- map FilePath.takeDirectory (Directory.canonicalizePath path)
  Directory.createDirectoryIfMissing True dirPath
