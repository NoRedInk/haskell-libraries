module TestSpec (tests) where

import qualified Expect
import qualified Expect.Task
import NriPrelude
import Test (Test, describe, only, skip, task, test, todo)
import qualified Test.Internal as Internal

tests :: Test
tests =
  describe
    "Test"
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
      task "suite result is 'PassedWithSkipped' when containing a skipped test" <| do
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
         in case suite of
              Internal.Test [test'] ->
                Internal.describes test'
                  |> Expect.equal ["suite", "sub suite"]
              _ -> Expect.fail "I didn't find a single test as I expected."
    ]

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
