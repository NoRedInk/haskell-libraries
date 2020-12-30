module TestSpec (tests) where

import qualified Expect
import qualified Expect.Task
import NriPrelude
import Test (Test, describe, only, skip, task, test)
import qualified Test.Internal as Internal

tests :: Test
tests =
  describe
    "Test"
    [ task "suite result is 'AllPassed' when all tests passed" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            test "test 2" (\_ -> Expect.pass)
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> simplify
                  |> Expect.equal AllPassed
            ),
      task "suite result is 'OnlysPassed' when containing an `only`" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            only <| test "test 2" (\_ -> Expect.pass)
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> simplify
                  |> Expect.equal OnlysPassed
            ),
      task "suite result is 'PassedWithSkipped' when containing a skipped test" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            skip <| test "test 2" (\_ -> Expect.pass)
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> simplify
                  |> Expect.equal PassedWithSkipped
            ),
      task "suite result is 'NoTestsInSuite' when it contains no tests" <| do
        describe "suite" []
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> simplify
                  |> Expect.equal NoTestsInSuite
            ),
      task "suite result is 'TestsFailed' when it contains a failing test" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            test "test 2" (\_ -> Expect.fail "oops")
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> simplify
                  |> Expect.equal TestsFailed
            ),
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
  = AllPassed
  | OnlysPassed
  | PassedWithSkipped
  | TestsFailed
  | NoTestsInSuite
  deriving (Eq, Show)

simplify :: Internal.SuiteResult -> SimplifiedSuiteResult
simplify suiteResult =
  case suiteResult of
    Internal.AllPassed _ -> AllPassed
    Internal.OnlysPassed _ _ -> OnlysPassed
    Internal.PassedWithSkipped _ _ -> PassedWithSkipped
    Internal.TestsFailed _ _ _ -> TestsFailed
    Internal.NoTestsInSuite -> NoTestsInSuite
