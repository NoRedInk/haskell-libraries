module TestSpec (tests) where

import qualified Expect
import qualified Expect.Task
import NriPrelude
import Test (Test, describe, only, task, test)
import qualified Test.Internal as Internal

tests :: Test
tests =
  describe
    "Test"
    [ task "suite passes if all tests pass" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            test "test 2" (\_ -> Expect.pass)
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> resultConstructor
                  |> Expect.equal "AllPassed"
            ),
      task "suite passes with onlys if suite contains an only" <| do
        describe
          "suite"
          [ test "test 1" (\_ -> Expect.pass),
            only <| test "test 2" (\_ -> Expect.pass)
          ]
          |> Internal.run
          |> Expect.Task.andCheck
            ( \result ->
                result
                  |> resultConstructor
                  |> Expect.equal "OnlysPassed"
            )
    ]

resultConstructor :: Internal.SuiteResult -> Text
resultConstructor suiteResult =
  case suiteResult of
    Internal.AllPassed _ -> "AllPassed"
    Internal.OnlysPassed _ _ -> "OnlysPassed"
    Internal.PassedWithSkipped _ _ -> "PassedWithSkipped"
    Internal.TestsFailed _ _ _ -> "TestsFailed"
    Internal.NoTestsInSuite -> "NoTestsInSuite"
