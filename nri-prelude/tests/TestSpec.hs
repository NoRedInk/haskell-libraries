module TestSpec (tests) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug
import qualified Expect
import qualified Fuzz
import qualified GHC.Exts
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified Platform
import qualified Platform.Internal
import qualified System.IO
import qualified Task
import Test (Test, describe, fuzz, fuzz2, fuzz3, only, serialize, skip, test, todo)
import qualified Test.CliParser as CliParser
import qualified Test.Internal as Internal
import qualified Test.Reporter.Logfile
import qualified Test.Reporter.Stdout
import qualified Text
import qualified Prelude

tests :: Test
tests =
  describe
    "Test"
    [ api,
      floatComparison,
      stdoutReporter,
      logfileReporter,
      cliParser,
      deadlockPrevention
    ]

api :: Test
api =
  describe
    "Api"
    [ test "suite result is 'AllPassed' when all tests passed" <| \_ -> do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  test "test 2" (\_ -> Expect.pass)
                ]
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal (AllPassed ["test 1", "test 2"]),
      test "suite result is 'OnlysPassed' when containing an `only`" <| \_ -> do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  only <| test "test 2" (\_ -> Expect.pass)
                ]
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal (OnlysPassed ["test 2"] ["test 1"]),
      test "suite result is 'AllPassed' with only the one test when passed a filepath" <| \_ -> do
        let srcLoc =
              Internal.getFrame "subset test"
                |> Stack.srcLocStartLine
                |> Prelude.fromIntegral
            suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  test "test 2" (\_ -> Expect.pass),
                  test
                    "test 3"
                    ( \_ ->
                        Expect.pass
                    )
                ]
        result <-
          suite
            |> Internal.run
              ( Internal.Some
                  [ Internal.SubsetOfTests "tests/TestSpec.hs" (Just (srcLoc + 6)),
                    Internal.SubsetOfTests "tests/TestSpec.hs" (Just (srcLoc + 11))
                  ]
              )
            |> Expect.succeeds

        result
          |> simplify
          |> Expect.equal (AllPassed ["test 1", "test 3"]),
      test "suite result is 'PassedWithSkipped' when containing  skipped test" <| \_ -> do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  skip <| test "test 2" (\_ -> Expect.pass)
                ]
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal (PassedWithSkipped ["test 1"] ["test 2"]),
      test "suite result is 'PassedWithSkipped' when containing a todo test" <| \_ -> do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  todo "test 2"
                ]
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal (PassedWithSkipped ["test 1"] ["test 2"]),
      test "suite result is 'NoTestsInSuite' when it contains no tests" <| \_ -> do
        let suite = describe "suite" []
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal NoTestsInSuite,
      test "suite result is 'TestsFailed' when it contains a failing test" <| \_ -> do
        let suite =
              describe
                "suite"
                [ test "test 1" (\_ -> Expect.pass),
                  skip <| test "test 2" (\_ -> Expect.pass),
                  test "test 3" (\_ -> Expect.fail "oops")
                ]
        result <- Expect.succeeds <| Internal.run Internal.All suite
        result
          |> simplify
          |> Expect.equal (TestsFailed ["test 1"] ["test 2"] ["test 3"]),
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
      test "source location of `fuzz` are the file in which the test is defined" <| \_ ->
        fuzz Fuzz.int "test 1" (\_ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `fuzz2` are the file in which the test is defined" <| \_ ->
        fuzz2 Fuzz.int Fuzz.int "test 1" (\_ _ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "source location of `fuzz3` are the file in which the test is defined" <| \_ ->
        fuzz3 Fuzz.int Fuzz.int Fuzz.int "test 1" (\_ _ _ -> Expect.pass)
          |> expectSingleTest (expectSrcFile "tests/TestSpec.hs"),
      test "Debug.todo gives reasonable stack traces" <| \_ -> do
        contents <-
          Expect.fromIO
            ( do
                Exception.handle
                  ( \(exception :: Exception.SomeException) ->
                      Exception.displayException exception
                        |> Text.fromList
                        |> Prelude.pure
                  )
                  (Debug.todo "foo" :: Prelude.IO Text)
            )
        Expect.equalToContentsOf "tests/golden-results/debug-todo-stacktrace" contents
    ]

floatComparison :: Test
floatComparison =
  describe
    "Float comparison expectations"
    [ test "Expect.within" <| \_ -> do
        Expect.within (Expect.Absolute 1) 0.1 0.5
        Expect.within (Expect.Relative 1) 3 5
        Expect.within (Expect.AbsoluteOrRelative 1 1) 0.1 0.5
        Expect.within (Expect.AbsoluteOrRelative 1 1) 3 5,
      test "Expect.notWithin" <| \_ -> do
        Expect.notWithin (Expect.Absolute 1) 3 5
        Expect.notWithin (Expect.Relative 1) 0.1 0.5
        Expect.notWithin (Expect.AbsoluteOrRelative 1 1) 3 10
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
  test'
    |> Internal.loc
    |> Stack.srcLocFile
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

stdoutReporter :: Test
stdoutReporter =
  describe
    "Stdout Reporter"
    [ test "all passed" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.AllPassed
                  [ mockTest "test 1" (mockTracingSpanWithTimes 0 10),
                    mockTest "test 2" (mockTracingSpanWithTimes 1 1234)
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-all-passed",
      test "onlys passed" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.OnlysPassed
                  [ mockTest "test 1" (mockTracingSpanWithTimes 0 10),
                    mockTest "test 2" (mockTracingSpanWithTimes 1 13)
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-onlys-passed",
      test "passed with skipped" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.PassedWithSkipped
                  [ mockTest "test 1" (mockTracingSpanWithTimes 0 10),
                    mockTest "test 2" (mockTracingSpanWithTimes 1 9)
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-passed-with-skipped",
      test "no tests in suite" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.NoTestsInSuite
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-no-tests-in-suite",
      test "tests failed" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.TestsFailed
                  [ mockTest "test 1" (mockTracingSpanWithTimes 1 2),
                    mockTest "test 2" (mockTracingSpanWithTimes 3 6)
                  ]
                  [ mockTest "test 3" Internal.NotRan,
                    mockTest "test 4" Internal.NotRan
                  ]
                  [ mockTest "test 5" (Internal.FailedSpan (mockTracingSpanWithTimes 1 12) (Internal.FailedAssertion "assertion error" mockSrcLoc)),
                    mockTest "test 6" (Internal.FailedSpan (mockTracingSpanWithTimes 1 13) (Internal.ThrewException mockException)),
                    mockTest "test 7" (Internal.FailedSpan (mockTracingSpanWithTimes 1 14) Internal.TookTooLong),
                    mockTest "test 7" (Internal.FailedSpan (mockTracingSpanWithTimes 1 18) (Internal.TestRunnerMessedUp "sorry"))
                  ]
                  |> Test.Reporter.Stdout.report handle
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed",
      test "tests failed (actually running)" <| \_ -> do
        let suite =
              describe
                "suite loc"
                [ test "test fail" (\_ -> Expect.fail "fail"),
                  test "test equal" (\_ -> Expect.equal True False),
                  test "test notEqual" (\_ -> Expect.notEqual True True),
                  test
                    "test all"
                    ( \_ ->
                        True
                          |> Expect.all
                            [ Expect.equal False
                            ]
                    ),
                  test "test lessThan" (\_ -> Expect.lessThan 1 (2 :: Int)),
                  test "test astMost" (\_ -> Expect.atMost 1 (2 :: Int)),
                  test "test greatherThan" (\_ -> Expect.greaterThan 2 (1 :: Int)),
                  test "test atLeast" (\_ -> Expect.atLeast 2 (1 :: Int)),
                  test "test within" (\_ -> Expect.within (Expect.Absolute 0.1) 1 2),
                  test "test notWithin" (\_ -> Expect.notWithin (Expect.Relative 0.1) 1 1),
                  test "test true" (\_ -> Expect.true False),
                  test "test false" (\_ -> Expect.false True),
                  test "test ok" (\_ -> Expect.ok (Err ())),
                  test "test err" (\_ -> Expect.err (Ok ())),
                  test "test succeeds" (\_ -> Expect.succeeds (Task.fail "oops")),
                  test "test fails" (\_ -> Expect.fails (Task.succeed "oops")),
                  test "test andCheck" (\_ -> Task.succeed (1 :: Int) |> Expect.andCheck (Expect.equal 2) |> map (\_ -> ()))
                ]
        contents <-
          withTempFile
            ( \_ handle -> do
                log <- Platform.silentHandler
                result <-
                  Internal.run Internal.All suite
                    |> Task.perform log
                Test.Reporter.Stdout.report handle result
            )
        contents
          |> withoutDurationLine
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed-loc",
      test "tests failed (actually running) only run subset" <| \_ -> do
        let srcLoc =
              Internal.getFrame "subset test"
                |> Stack.srcLocStartLine
                |> Prelude.fromIntegral
            suite =
              describe
                "suite loc"
                [ test "test fail" (\_ -> Expect.fail "fail"),
                  test "test equal" (\_ -> Expect.equal True True),
                  test "test notEqual" (\_ -> Expect.notEqual True False)
                ]
        contents <-
          withTempFile
            ( \_ handle -> do
                log <- Platform.silentHandler
                result <-
                  suite
                    |> Internal.run
                      ( Internal.Some
                          [ Internal.SubsetOfTests "tests/TestSpec.hs" (Just (srcLoc + 6)),
                            Internal.SubsetOfTests "tests/TestSpec.hs" (Just (srcLoc + 8))
                          ]
                      )
                    |> Task.perform log
                Test.Reporter.Stdout.report handle result
            )
        Expect.true (Text.contains "Passed:    1" contents)
        Expect.true (Text.contains "Failed:    1" contents)
        contents
          |> withoutDurationLine
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed-loc-subset",
      test "tests failed (actually running) only run onefile" <| \_ -> do
        let suite =
              describe
                "suite loc"
                [ test "test fail" (\_ -> Expect.fail "fail"),
                  test "test equal" (\_ -> Expect.equal True True),
                  test "test notEqual" (\_ -> Expect.notEqual True False)
                ]
        contents <-
          withTempFile
            ( \_ handle -> do
                log <- Platform.silentHandler
                result <-
                  suite
                    |> Internal.run
                      (Internal.Some [Internal.SubsetOfTests "tests/TestSpec.hs" Nothing])
                    |> Task.perform log
                Test.Reporter.Stdout.report handle result
            )
        contents
          |> withoutDurationLine
          |> Expect.equalToContentsOf "tests/golden-results/test-report-stdout-tests-failed-loc-one-file"
    ]

logfileReporter :: Test
logfileReporter =
  describe
    "Logfile Reporter"
    [ test "all passed" <| \_ -> do
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
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-all-passed",
      test "onlys passed" <| \_ -> do
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
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-onlys-passed",
      test "passed with skipped" <| \_ -> do
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
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-passed-with-skipped",
      test "no tests in suite" <| \_ -> do
        contents <-
          withTempFile
            ( \_ handle ->
                Internal.NoTestsInSuite
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-no-tests-in-suite",
      test "tests failed" <| \_ -> do
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
                  [ mockTest "test 5" (Internal.FailedSpan mockTracingSpan (Internal.FailedAssertion "assertion error" mockSrcLoc)),
                    mockTest "test 6" (Internal.FailedSpan mockTracingSpan (Internal.ThrewException mockException)),
                    mockTest "test 7" (Internal.FailedSpan mockTracingSpan Internal.TookTooLong),
                    mockTest "test 7" (Internal.FailedSpan mockTracingSpan (Internal.TestRunnerMessedUp "sorry"))
                  ]
                  |> Test.Reporter.Logfile.report (writeSpan handle)
            )
        contents
          |> Expect.equalToContentsOf "tests/golden-results/test-report-logfile-tests-failed"
    ]

writeSpan :: System.IO.Handle -> Platform.Internal.TracingSpan -> Prelude.IO ()
writeSpan handle span =
  do
    Data.Aeson.Encode.Pretty.encodePretty span
    |> Data.ByteString.Lazy.hPut handle

-- | Provide a temporary file for a test to do some work in, then return the
-- contents of the file when the test is done with it.
withTempFile :: (System.IO.FilePath -> System.IO.Handle -> Prelude.IO ()) -> Expect.Expectation' Text
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
    |> Expect.succeeds

mockTest :: Text -> body -> Internal.SingleTest body
mockTest name body =
  Internal.SingleTest
    { Internal.describes = ["suite", "sub suite"],
      Internal.name = name,
      Internal.label = Internal.None,
      Internal.loc = mockSrcLoc,
      Internal.group = Internal.Ungrouped,
      Internal.body = body
    }

mockTracingSpan :: Platform.Internal.TracingSpan
mockTracingSpan =
  mockTracingSpanWithTimes 0 0

mockTracingSpanWithTimes :: Int -> Int -> Platform.Internal.TracingSpan
mockTracingSpanWithTimes startedMs finishedMs =
  Platform.Internal.TracingSpan
    { Platform.Internal.name = "name",
      Platform.Internal.started = Platform.Internal.MonotonicTime (1000 * Prelude.fromIntegral startedMs),
      Platform.Internal.finished = Platform.Internal.MonotonicTime (1000 * Prelude.fromIntegral finishedMs),
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

cliParser :: Test
cliParser =
  describe
    "CLI Parser"
    [ test "All tests" <| \_ ->
        CliParser.parseArgs
          []
          |> Expect.equal (Ok Internal.All),
      test "Missing separator" <| \_ ->
        CliParser.parseArgs
          ["--files"]
          |> Expect.equal (Err "must inform at least one file: not enough input"),
      test "trailing comma" <| \_ ->
        CliParser.parseArgs
          ["--files", "a.hs,"]
          |> Expect.equal (Ok (Internal.Some [Internal.SubsetOfTests "a.hs" Nothing])),
      test "1 file" <| \_ ->
        CliParser.parseArgs
          ["--files", "a.hs"]
          |> Expect.equal (Ok (Internal.Some [Internal.SubsetOfTests "a.hs" Nothing])),
      test "2 files" <| \_ ->
        CliParser.parseArgs
          ["--files", "a.hs,b.hs"]
          |> Expect.equal
            ( Ok
                ( Internal.Some
                    [Internal.SubsetOfTests "a.hs" Nothing, Internal.SubsetOfTests "b.hs" Nothing]
                )
            ),
      test "Doesn't really parse file paths" <| \_ ->
        CliParser.parseArgs
          ["--files", "bla.hs\nble.hs"]
          |> Expect.equal
            ( Ok
                (Internal.Some [Internal.SubsetOfTests "bla.hs\nble.hs" Nothing])
            ),
      test "File with LoC" <| \_ ->
        CliParser.parseArgs
          ["--files", "bla.hs:123"]
          |> Expect.equal
            ( Ok
                (Internal.Some [Internal.SubsetOfTests "bla.hs" (Just 123)])
            ),
      test "File with bad LoC" <| \_ ->
        CliParser.parseArgs
          ["--files", "bla.hs:1asd"]
          |> Expect.equal
            (Err "Failed reading: expected format: --files=bla.hs or --files bla.hs: \"asd\""),
      test "File with bad LoC in first file" <| \_ ->
        CliParser.parseArgs
          ["--files", "bla.hs:1asd,b.hs"]
          |> Expect.equal
            (Err "Failed reading: expected format: --files=bla.hs or --files bla.hs: \"asd,b.hs\"")
    ]

deadlockPrevention :: Test
deadlockPrevention =
  describe
    "Prevent deadlocks in tests"
    [ test "a test that deadlocks" <| \_ -> do
        mvar1 <- Expect.fromIO MVar.newEmptyMVar
        mvar2 <- Expect.fromIO MVar.newEmptyMVar

        _ <-
          deadlockSuite mvar1 mvar2
            |> Internal.run Internal.All
            |> Task.timeout 1000 Internal.TookTooLong
            |> Expect.fails
        Expect.pass,
      test "serial tests don't deadlock" <| \_ -> do
        mvar1 <- Expect.fromIO MVar.newEmptyMVar
        mvar2 <- Expect.fromIO MVar.newEmptyMVar

        _ <-
          deadlockSuite mvar1 mvar2
            |> serialize "groupKey"
            |> Internal.run Internal.All
            |> Expect.succeeds
        Expect.pass
    ]

deadlockSuite :: MVar.MVar () -> MVar.MVar () -> Test
deadlockSuite mvar1 mvar2 =
  describe
    "two deadlocking tests"
    [ test "test 1" <| \_ -> do
        Expect.fromIO (MVar.putMVar mvar1 ())
        Expect.fromIO <| threadDelay 100
        Expect.fromIO (MVar.putMVar mvar2 ())
        _ <- Expect.fromIO (MVar.takeMVar mvar2)
        _ <- Expect.fromIO (MVar.takeMVar mvar1)
        Expect.pass,
      test "test 2" <| \_ -> do
        Expect.fromIO (MVar.putMVar mvar2 ())
        Expect.fromIO <| threadDelay 100
        Expect.fromIO (MVar.putMVar mvar1 ())
        _ <- Expect.fromIO (MVar.takeMVar mvar1)
        _ <- Expect.fromIO (MVar.takeMVar mvar2)
        Expect.pass
    ]

withoutDurationLine :: Text -> Text
withoutDurationLine text =
  text
    |> Text.lines
    |> List.filter (\line -> line |> Text.startsWith "Duration: " |> not)
    |> Text.join "\n"
