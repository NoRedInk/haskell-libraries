-- | Module for presenting test results on the console.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Stdout
  ( report,
  )
where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified System.IO
import qualified Test.Internal as Internal
import Test.Reporter.Internal (black, green, grey, red, yellow)
import qualified Test.Reporter.Internal
import qualified Text
import Text.Colour (chunk)
import qualified Text.Colour
import qualified Tuple
import qualified Prelude

report :: System.IO.Handle -> Internal.SuiteResult -> Prelude.IO ()
report handle results = do
  terminalCapabilities <- Text.Colour.getTerminalCapabilitiesFromHandle handle
  reportChunks <- renderReport results
  Text.Colour.hPutChunksWith terminalCapabilities handle reportChunks
  System.IO.hFlush handle

renderReport :: Internal.SuiteResult -> Prelude.IO (List (Text.Colour.Chunk))
renderReport results =
  case results of
    Internal.AllPassed passed ->
      let amountPassed = List.length passed
       in Prelude.pure
            [ green (Text.Colour.underline "TEST RUN PASSED"),
              "\n\n",
              black (chunk <| "Passed:    " ++ Text.fromInt amountPassed),
              "\n"
            ]
    Internal.OnlysPassed passed skipped ->
      let amountPassed = List.length passed
          amountSkipped = List.length skipped
       in Prelude.pure
            <| List.concat
              [ List.concatMap
                  ( \only ->
                      prettyPath yellow only
                        ++ [ "This test passed, but there is a `Test.only` in your test.\n",
                             "I failed the test, because it's easy to forget to remove `Test.only`.\n",
                             "\n\n"
                           ]
                  )
                  passed,
                [ yellow (Text.Colour.underline ("TEST RUN INCOMPLETE")),
                  yellow " because there is an `only` in your tests.",
                  "\n\n",
                  black (chunk <| "Passed:    " ++ Text.fromInt amountPassed),
                  "\n",
                  black (chunk <| "Skipped:   " ++ Text.fromInt amountSkipped),
                  "\n"
                ]
              ]
    Internal.PassedWithSkipped passed skipped ->
      let amountPassed = List.length passed
          amountSkipped = List.length skipped
       in Prelude.pure
            <| List.concat
              [ List.concatMap
                  ( \only ->
                      prettyPath yellow only
                        ++ [ "This test was skipped.",
                             "\n\n"
                           ]
                  )
                  skipped,
                [ yellow (Text.Colour.underline "TEST RUN INCOMPLETE"),
                  yellow
                    ( chunk <| case List.length skipped of
                        1 -> " because 1 test was skipped"
                        n -> " because " ++ Text.fromInt n ++ " tests were skipped"
                    ),
                  "\n\n",
                  black (chunk <| "Passed:    " ++ Text.fromInt amountPassed),
                  "\n",
                  black (chunk <| "Skipped:   " ++ Text.fromInt amountSkipped),
                  "\n"
                ]
              ]
    Internal.TestsFailed passed skipped failed -> do
      let amountPassed = List.length passed
      let amountFailed = List.length failed
      let amountSkipped = List.length skipped
      let failures = List.map (map Tuple.second) failed
      srcLocs <- Prelude.traverse Test.Reporter.Internal.readSrcLoc failures
      let failuresSrcs = List.map renderFailureInFile srcLocs
      Prelude.pure
        <| List.concat
          [ List.concat
              <| List.map2
                ( \srcLines test ->
                    prettyPath red test
                      ++ srcLines
                      ++ [testFailure test, "\n\n"]
                )
                failuresSrcs
                failures,
            [ red (Text.Colour.underline "TEST RUN FAILED"),
              "\n\n",
              black (chunk <| "Passed:    " ++ Text.fromInt amountPassed),
              "\n"
            ],
            if amountSkipped == 0
              then []
              else
                [ black (chunk <| "Skipped:   " ++ Text.fromInt amountSkipped),
                  "\n"
                ],
            [black (chunk <| "Failed:    " ++ Text.fromInt amountFailed), "\n"]
          ]
    Internal.NoTestsInSuite ->
      Prelude.pure
        [ yellow (Text.Colour.underline "TEST RUN INCOMPLETE"),
          yellow " because the test suite is empty.",
          "\n"
        ]

renderFailureInFile :: Maybe (Stack.SrcLoc, BS.ByteString) -> List Text.Colour.Chunk
renderFailureInFile maybeSrcLoc =
  case maybeSrcLoc of
    Just (loc, src) -> Test.Reporter.Internal.renderSrcLoc loc src
    Nothing -> []

prettyPath :: (Text.Colour.Chunk -> Text.Colour.Chunk) -> Internal.SingleTest a -> List Text.Colour.Chunk
prettyPath style test =
  List.concat
    [ case Internal.loc test of
        Nothing -> []
        Just loc ->
          [ grey
              <| chunk
                ( "↓ "
                    ++ Text.fromList (Stack.srcLocFile loc)
                    ++ ":"
                    ++ Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine loc))
                    ++ "\n"
                )
          ],
      [ grey
          ( chunk
              <| Prelude.foldMap
                (\text -> "↓ " ++ text ++ "\n")
                (Internal.describes test)
          ),
        style (chunk ("✗ " ++ Internal.name test)),
        "\n"
      ]
    ]

testFailure :: Internal.SingleTest Internal.Failure -> Text.Colour.Chunk
testFailure test =
  chunk
    <| case Internal.body test of
      Internal.FailedAssertion msg _ -> msg
      Internal.ThrewException exception ->
        "Test threw an exception\n"
          ++ Text.fromList (Exception.displayException exception)
      Internal.TookTooLong -> "Test timed out"
      Internal.TestRunnerMessedUp msg ->
        "Test runner encountered an unexpected error:\n"
          ++ msg
          ++ "\n"
          ++ "This is a bug.\n\n"
          ++ "If you have some time to report the bug it would be much appreciated!\n"
          ++ "You can do so here: https://github.com/NoRedInk/haskell-libraries/issues"
