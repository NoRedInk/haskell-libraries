module TestSpec (tests) where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.IO
import qualified Expect
import qualified Expect.Task
import qualified Fuzz
import qualified GHC.Exts
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform.Internal
import qualified System.IO
import qualified Task
import Test (Test, describe, fuzz, fuzz2, fuzz3, only, skip, task, test, todo)
import qualified Test.Internal as Internal
import qualified Test.Reporter.Logfile
import qualified Test.Reporter.Stdout
import qualified Prelude

tests :: Platform.Internal.LogHandler -> Test
tests log =
  describe
    "Test"
    [ api,
      stdoutReporter log,
      logfileReporter
    ]

api :: Test
api =
  describe
    "Api"
    [ task "suite result is 'AllPassed' when all tests passed" <| do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  test "test 2" (\_ -> Expect.pass)
                ]
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal (AllPassed ["test 1", "test 2"])
          |> Expect.Task.check,
      task "suite result is 'OnlysPassed' when containing an `only`" <| do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  only <| test "test 2" (\_ -> Expect.pass)
                ]
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal (OnlysPassed ["test 2"] ["test 1"])
          |> Expect.Task.check,
      task "suite result is 'PassedWithSkipped' when containing  skipped test" <| do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  skip <| test "test 2" (\_ -> Expect.pass)
                ]
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal (PassedWithSkipped ["test 1"] ["test 2"])
          |> Expect.Task.check,
      task "suite result is 'PassedWithSkipped' when containing a todo test" <| do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  todo "test 2"
                ]
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal (PassedWithSkipped ["test 1"] ["test 2"])
          |> Expect.Task.check,
      task "suite result is 'NoTestsInSuite' when it contains no tests" <| do
        let suite = describe "suite" []
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal NoTestsInSuite
          |> Expect.Task.check,
      task "suite result is 'TestsFailed' when it contains a failing test" <| do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  skip <| test "test 2" (\_ -> Expect.pass),
                  test "test 3" (\_ -> Expect.fail "oops")
                ]
        result <- Internal.run suite
        result
          |> simplify
          |> Expect.equal (TestsFailed ["test 1"] ["test 2"] ["test 3"])
          |> Expect.Task.check,
      test "nested describes are exposed on each test" <| \_ ->
        let suite =
              describe
                "suite"
                [ describe
                    "sub suite"
                    [ test "test 1" (\_ -> Expect.pass)
                    ]
                ]
         in suite
              |> expectSingleTest
                ( \test' ->
                    Internal.describes test'
                      |> Expect.equal ["suite", "sub suite"]
                ),
      test "source location of `test` are the file in which the test is defined" <| \_ ->
        test "test 1" (\_ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `todo` are the file in which the test is defined" <| \_ ->
        todo "test 1"
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `task` are the file in which the test is defined" <| \_ ->
        task "test 1" (Expect.Task.check Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `fuzz` are the file in which the test is defined" <| \_ ->
        fuzz Fuzz.int "test 1" (\_ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `fuzz2` are the file in which the test is defined" <| \_ ->
        fuzz2 Fuzz.int Fuzz.int "test 1" (\_ _ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `fuzz3` are the file in which the test is defined" <| \_ ->
        fuzz3 Fuzz.int Fuzz.int Fuzz.int "test 1" (\_ _ _ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs")
    ]

expectSingleTest ::
  (Internal.SingleTest Internal.Expectation -> Expect.Expectation) ->
  Test ->
  Expect.Expectation
expectSingleTest check (Internal.Test tests') =
  case tests' of
    [test'] -> check test'
    _ -> Expect.fail "I didn't find a single test as I expected."

expectSrcFile :: Text -> Internal.SingleTest a -> Expect.Expectation
expectSrcFile expected test' =
  case Internal.loc test' of
    Nothing ->
      Expect.fail "Expected source file location for test, but none was set."
    Just loc ->
      Stack.srcLocFile loc
        |> Data.Text.pack
        |> Expect.equal expected

-- | A type mirroring `Internal.SuiteResult`, simplified to allow easy
-- comparisons in tests.
data SimplifiedSuiteResult
  = AllPassed [Text]
  | OnlysPassed [Text] [Text]
  | PassedWithSkipped [Text] [Text]
  | TestsFailed [Text] [Text] [Text]
  | NoTestsInSuite
  deriving (Eq, Show)

simplify :: Internal.SuiteResult -> SimplifiedSuiteResult
simplify suiteResult =
  case suiteResult of
    Internal.AllPassed passed ->
      AllPassed
        (map Internal.name passed)
    Internal.OnlysPassed passed skipped ->
      OnlysPassed
        (map Internal.name passed)
        (map Internal.name skipped)
    Internal.PassedWithSkipped passed skipped ->
      PassedWithSkipped
        (map Internal.name passed)
        (map Internal.name skipped)
    Internal.TestsFailed passed skipped failed ->
      TestsFailed
        (map Internal.name passed)
        (map Internal.name skipped)
        (map Internal.name failed)
    Internal.NoTestsInSuite -> NoTestsInSuite

stdoutReporter :: Platform.Internal.LogHandler -> Test
stdoutReporter log =
  describe
    "Stdout Reporter"
    [ task "all passed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.AllPassed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-all-passed"
          |> Expect.Task.check,
      task "onlys passed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.OnlysPassed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-onlys-passed"
          |> Expect.Task.check,
      task "passed with skipped" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.PassedWithSkipped
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-passed-with-skipped"
          |> Expect.Task.check,
      task "no tests in suite" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.NoTestsInSuite
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-no-tests-in-suite"
          |> Expect.Task.check,
      task "tests failed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.TestsFailed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  [ mockTest "test 5" (mockTracingSpan, Internal.FailedAssertion "assertion error" Nothing),
                    mockTest "test 6" (mockTracingSpan, Internal.ThrewException mockException),
                    mockTest "test 7" (mockTracingSpan, Internal.TookTooLong),
                    mockTest "test 7" (mockTracingSpan, Internal.TestRunnerMessedUp "sorry")
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed"
          |> Expect.Task.check,
      task "tests failed (actually running)" <| do
        let suite =
              describe
                "suite loc"
                [ test "test 1" (\_ -> Expect.fail "fail"),
                  test "test 2" (\_ -> Expect.equal True False),
                  test "test 3" (\_ -> Expect.notEqual True True),
                  test
                    "test 4"
                    ( \_ ->
                        True
                          |> Expect.all
                            [ Expect.equal False
                            ]
                    ),
                  test "test 5" (\_ -> Expect.lessThan 1 (2 :: Int)),
                  test "test 6" (\_ -> Expect.atMost 1 (2 :: Int)),
                  test "test 7" (\_ -> Expect.greaterThan 2 (1 :: Int)),
                  test "test 8" (\_ -> Expect.atLeast 2 (1 :: Int)),
                  test "test 9" (\_ -> Expect.atLeast 2 (1 :: Int)),
                  test "test 10" (\_ -> Expect.true False),
                  test "test 11" (\_ -> Expect.false True),
                  test "test 12" (\_ -> Expect.ok (Err ())),
                  test "test 13" (\_ -> Expect.err (Ok ()))
                ]
        contents <-
          withTempFile
            ( \_ handle -> do
                result <-
                  Internal.run suite
                    |> Task.perform log
                Test.Reporter.Stdout.report handle result
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed-loc"
          |> Expect.Task.check
    ]

logfileReporter :: Test
logfileReporter =
  describe
    "Logfile Reporter"
    [ task "all passed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.AllPassed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-all-passed"
          |> Expect.Task.check,
      task "onlys passed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.OnlysPassed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-onlys-passed"
          |> Expect.Task.check,
      task "passed with skipped" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.PassedWithSkipped
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-passed-with-skipped"
          |> Expect.Task.check,
      task "no tests in suite" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.NoTestsInSuite
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-no-tests-in-suite"
          |> Expect.Task.check,
      task "tests failed" <| do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.TestsFailed
                  [ mockTest "test 1" mockTracingSpan,
                    mockTest "test 2" mockTracingSpan
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  [ mockTest "test 5" (mockTracingSpan, Internal.FailedAssertion "assertion error" Nothing),
                    mockTest "test 6" (mockTracingSpan, Internal.ThrewException mockException),
                    mockTest "test 7" (mockTracingSpan, Internal.TookTooLong),
                    mockTest "test 7" (mockTracingSpan, Internal.TestRunnerMessedUp "sorry")
                  ]
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-tests-failed"
          |> Expect.Task.check
    ]

writeSpan :: System.IO.Handle -> Platform.Internal.TracingSpan -> Prelude.IO ()
writeSpan handle span =
  do
    Data.Aeson.Encode.Pretty.encodePretty span
    |> Data.ByteString.Lazy.hPut handle

-- | Provide a temporary file for a test to do some work in, then return the
-- contents of the file when the test is done with it.
withTempFile :: (System.IO.FilePath -> System.IO.Handle -> Prelude.IO ()) -> Task e Text
withTempFile go = do
  Platform.Internal.Task
    ( \_ -> do
        (path, handle) <-
          System.IO.openTempFile "/tmp" "nri-haskell-libraries-test-file"
        go path handle
        System.IO.hClose handle
        Data.Text.IO.readFile path
          |> map Ok
    )

mockTest :: Text -> body -> Internal.SingleTest body
mockTest name body =
  Internal.SingleTest
    { Internal.describes = ["suite", "sub suite"],
      Internal.name = name,
      Internal.label = Internal.None,
      Internal.loc = Just mockSrcLoc,
      Internal.body = body
    }

mockTracingSpan :: Platform.Internal.TracingSpan
mockTracingSpan =
  Platform.Internal.TracingSpan
    { Platform.Internal.name = "name",
      Platform.Internal.started = Platform.Internal.MonotonicTime 0,
      Platform.Internal.finished = Platform.Internal.MonotonicTime 0,
      Platform.Internal.frame = Nothing,
      Platform.Internal.details = Nothing,
      Platform.Internal.summary = Nothing,
      Platform.Internal.succeeded = Platform.Internal.Succeeded,
      Platform.Internal.allocated = 1,
      Platform.Internal.children = []
    }

mockSrcLoc :: Stack.SrcLoc
mockSrcLoc =
  Stack.SrcLoc
    { Stack.srcLocPackage = "package",
      Stack.srcLocModule = "Module",
      Stack.srcLocFile = "Module.hs",
      Stack.srcLocStartLine = 2,
      Stack.srcLocStartCol = 4,
      Stack.srcLocEndLine = 12,
      Stack.srcLocEndCol = 14
    }

mockException :: Exception.SomeException
mockException =
  Exception.StringException "exception" (GHC.Exts.fromList [])
    |> Exception.toException
