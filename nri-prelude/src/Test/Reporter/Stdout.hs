module Test.Reporter.Stdout (report) where

import qualified Data.ByteString.Builder as Builder
import qualified List
import NriPrelude
import qualified System.Console.ANSI as ANSI
import qualified System.IO
import qualified Test.Internal as Internal
import qualified Prelude

report :: Internal.SuiteResult -> Prelude.IO ()
report results = do
  color <- ANSI.hSupportsANSIColor System.IO.stdout
  let styled =
        if color
          then (\styles builder -> sgr styles ++ builder ++ sgr [ANSI.Reset])
          else (\_ builder -> builder)
  let reportByteString = renderReport styled results
  Builder.hPutBuilder System.IO.stdout reportByteString
  System.IO.hFlush System.IO.stdout

renderReport ::
  ([ANSI.SGR] -> Builder.Builder -> Builder.Builder) ->
  Internal.SuiteResult ->
  Builder.Builder
renderReport styled results =
  case results of
    Internal.AllPassed -> styled [green, underlined] "TEST RUN PASSED"
    Internal.OnlysPassed ->
      styled [yellow, underlined] "TEST RUN INCOMPLETE"
        ++ styled [yellow] " because there is an `only` in your tests."
    Internal.PassedWithSkipped skipped ->
      case List.length skipped of
        1 ->
          styled [yellow, underlined] "TEST RUN INCOMPLETE"
            ++ styled [yellow] (" because there was 1 test skipped.")
        amountSkipped ->
          styled [yellow, underlined] "TEST RUN INCOMPLETE"
            ++ styled
              [yellow]
              ( " because there were "
                  ++ Builder.int64Dec amountSkipped
                  ++ " tests skipped."
              )
    Internal.TestsFailed _ -> styled [red, underlined] "TEST RUN FAILED"
    Internal.NoTestsInSuite ->
      styled [yellow, underlined] "TEST RUN INCOMPLETE"
        ++ styled [yellow] (" because no tests were ran.")

sgr :: [ANSI.SGR] -> Builder.Builder
sgr = Builder.stringUtf8 << ANSI.setSGRCode

red :: ANSI.SGR
red = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red

yellow :: ANSI.SGR
yellow = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow

green :: ANSI.SGR
green = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green

_grey :: ANSI.SGR
_grey = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black

_black :: ANSI.SGR
_black = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White

underlined :: ANSI.SGR
underlined = ANSI.SetUnderlining ANSI.SingleUnderline
