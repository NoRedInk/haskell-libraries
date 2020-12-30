-- | Module for presenting test results on the console.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Stdout
  ( report,
  )
where

import qualified Control.Exception as Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding as TE
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified System.Console.ANSI as ANSI
import qualified System.IO
import qualified Test.Internal as Internal
import qualified Prelude

report :: Internal.Test -> Internal.SuiteResult -> Prelude.IO ()
report suite results = do
  color <- ANSI.hSupportsANSIColor System.IO.stdout
  let styled =
        if color
          then (\styles builder -> sgr styles ++ builder ++ sgr [ANSI.Reset])
          else (\_ builder -> builder)
  let reportByteString = renderReport styled suite results
  Builder.hPutBuilder System.IO.stdout reportByteString
  System.IO.hFlush System.IO.stdout

renderReport ::
  ([ANSI.SGR] -> Builder.Builder -> Builder.Builder) ->
  Internal.Test ->
  Internal.SuiteResult ->
  Builder.Builder
renderReport styled (Internal.Test tests) results =
  case results of
    Internal.AllPassed ->
      let amountPassed = List.length tests
       in styled [green, underlined] "TEST RUN PASSED"
            ++ "\n\n"
            ++ styled [black] ("Passed:    " ++ Builder.int64Dec amountPassed)
            ++ "\n"
    Internal.OnlysPassed ->
      let onlys =
            tests
              |> List.filter (\test -> Internal.label test == Internal.Only)
          amountPassed =
            List.length onlys
          amountSkipped = List.length tests - amountPassed
       in Prelude.foldMap
            ( \only ->
                prettyPath styled [yellow] only
                  ++ "This test passed, but there is a `Test.only` in your test."
                  ++ "I failed the test, because it's easy to forget to remove `Test.only`."
                  ++ "\n\n"
            )
            onlys
            ++ styled [yellow, underlined] "TEST RUN INCOMPLETE"
            ++ styled [yellow] " because there is an `only` in your tests."
            ++ "\n\n"
            ++ styled [black] ("Passed:    " ++ Builder.int64Dec amountPassed)
            ++ "\n"
            ++ styled [black] ("Skipped:   " ++ Builder.int64Dec amountSkipped)
            ++ "\n"
    Internal.PassedWithSkipped skipped ->
      let amountSkipped = List.length skipped
          amountPassed = List.length tests - amountSkipped
       in Prelude.foldMap
            ( \only ->
                prettyPath styled [yellow] only
                  ++ "This test was skipped."
                  ++ "\n\n"
            )
            skipped
            ++ styled [yellow, underlined] "TEST RUN INCOMPLETE"
            ++ styled
              [yellow]
              ( case List.length skipped of
                  1 -> " because 1 test was skipped"
                  n -> " because " ++ Builder.int64Dec n ++ " tests were skipped"
              )
            ++ "\n\n"
            ++ styled [black] ("Passed:    " ++ Builder.int64Dec amountPassed)
            ++ "\n"
            ++ styled [black] ("Skipped:   " ++ Builder.int64Dec amountSkipped)
            ++ "\n"
    Internal.TestsFailed failed ->
      let amountOnly =
            tests
              |> List.filter (\test -> Internal.label test == Internal.Only)
              |> List.length
          amountSkipped =
            if amountOnly == 0
              then
                tests
                  |> List.filter (\test -> Internal.label test == Internal.Skip)
                  |> List.length
              else List.length tests - amountOnly
          amountFailed = List.length failed
          amountPassed = List.length tests - amountSkipped - amountFailed
       in Prelude.foldMap
            ( \test ->
                prettyPath styled [red] test
                  ++ testFailure test
                  ++ "\n\n"
            )
            failed
            ++ styled [red, underlined] "TEST RUN FAILED"
            ++ "\n\n"
            ++ styled [black] ("Passed:    " ++ Builder.int64Dec amountPassed)
            ++ "\n"
            ++ ( if amountSkipped == 0
                   then ""
                   else
                     styled [black] ("Skipped:   " ++ Builder.int64Dec amountSkipped)
                       ++ "\n"
               )
            ++ styled [black] ("Failed:    " ++ Builder.int64Dec amountFailed)
            ++ "\n"
    Internal.NoTestsInSuite ->
      styled [yellow, underlined] "TEST RUN INCOMPLETE"
        ++ styled [yellow] (" because the test suite is empty.")
        ++ "\n"

prettyPath ::
  ([ANSI.SGR] -> Builder.Builder -> Builder.Builder) ->
  [ANSI.SGR] ->
  Internal.SingleTest a ->
  Builder.Builder
prettyPath styled styles test =
  ( case Internal.loc test of
      Nothing -> ""
      Just loc ->
        styled
          [grey]
          ( "↓ "
              ++ Builder.stringUtf8 (Stack.srcLocFile loc)
              ++ ":"
              ++ Builder.intDec (Stack.srcLocStartLine loc)
              ++ "\n"
          )
  )
    ++ Prelude.foldMap
      (\text -> styled [grey] ("↓ " ++ TE.encodeUtf8Builder text) ++ "\n")
      (Internal.describes test)
    ++ styled styles ("✗ " ++ TE.encodeUtf8Builder (Internal.name test))
    ++ "\n"

testFailure :: Internal.SingleTest Internal.Failure -> Builder.Builder
testFailure test =
  case Internal.body test of
    Internal.FailedAssertion msg ->
      TE.encodeUtf8Builder msg
    Internal.ThrewException exception ->
      "Test threw an exception\n"
        ++ Builder.stringUtf8 (Exception.displayException exception)
    Internal.TookTooLong ->
      "Test timed out"
    Internal.TestRunnerMessedUp msg ->
      "Test runner encountered an unexpected error:\n"
        ++ TE.encodeUtf8Builder msg
        ++ "\n"
        ++ "This is a bug.\n\n"
        ++ "If you have some time to report the bug it would be much appreciated!\n"
        ++ "You can do so here: https://github.com/NoRedInk/haskell-libraries/issues"

sgr :: [ANSI.SGR] -> Builder.Builder
sgr = Builder.stringUtf8 << ANSI.setSGRCode

red :: ANSI.SGR
red = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red

yellow :: ANSI.SGR
yellow = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow

green :: ANSI.SGR
green = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green

grey :: ANSI.SGR
grey = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black

black :: ANSI.SGR
black = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White

underlined :: ANSI.SGR
underlined = ANSI.SetUnderlining ANSI.SingleUnderline
