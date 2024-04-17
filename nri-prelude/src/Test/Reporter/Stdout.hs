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
import qualified Maybe
import NriPrelude
import qualified Numeric
import qualified Platform.Internal
import qualified System.IO
import qualified Test.Internal as Internal
import Test.Reporter.Internal (black, green, grey, red, yellow)
import qualified Test.Reporter.Internal
import qualified Text
import Text.Colour (chunk)
import qualified Text.Colour
import qualified Text.Colour.Capabilities.FromEnv
import qualified Prelude

report :: System.IO.Handle -> Internal.SuiteResult -> Prelude.IO ()
report handle results = do
  terminalCapabilities <- Text.Colour.Capabilities.FromEnv.getTerminalCapabilitiesFromHandle handle
  reportChunks <- renderReport results
  Text.Colour.hPutChunksUtf8With terminalCapabilities handle reportChunks
  System.IO.hFlush handle

renderReport :: Internal.SuiteResult -> Prelude.IO (List (Text.Colour.Chunk))
renderReport results =
  let elapsed = formatElapsedDuration results
   in case results of
        Internal.AllPassed passed ->
          let amountPassed = List.length passed
           in Prelude.pure
                [ green (Text.Colour.underline "TEST RUN PASSED"),
                  "\n\n",
                  black <| chunk <| "Duration:  " ++ elapsed,
                  "\n",
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
                      black <| chunk <| "Duration:  " ++ elapsed,
                      "\n",
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
                      black <| chunk <| "Duration:  " ++ elapsed,
                      "\n",
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
          let failures = List.map (map (\(Internal.FailedSpan _ failure) -> failure)) failed
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
                  black <| chunk <| "Duration:  " ++ elapsed,
                  "\n",
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
  let loc = Internal.loc test
   in List.concat
        [ [ grey
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

formatElapsedDuration :: Internal.SuiteResult -> Text
formatElapsedDuration result =
  result |> resultSpans |> elapsedMilliseconds |> formatElapsedMilliseconds

elapsedMilliseconds :: List Platform.Internal.TracingSpan -> Float
elapsedMilliseconds spans =
  let startTime =
        spans
          |> List.map Platform.Internal.started
          |> List.minimum
          |> Maybe.withDefault 0
      finishTime =
        spans
          |> List.map Platform.Internal.finished
          |> List.maximum
          |> Maybe.withDefault 0
   in finishTime - startTime |> Prelude.fromIntegral |> (/ 1000)

resultSpans :: Internal.SuiteResult -> List Platform.Internal.TracingSpan
resultSpans result =
  case result of
    Internal.AllPassed passed ->
      List.map Internal.body passed
    Internal.OnlysPassed passed _skipped ->
      List.map Internal.body passed
    Internal.PassedWithSkipped passed _skipped ->
      List.map Internal.body passed
    Internal.TestsFailed passed _skipped failed -> do
      List.map Internal.body passed
        ++ (failed |> List.map Internal.body |> List.map (\(Internal.FailedSpan span _) -> span))
    Internal.NoTestsInSuite ->
      []

-- and maybe add something like
-- for rendering the elapsed time into a human readable format not only to ms
formatElapsedMilliseconds :: Float -> Text
formatElapsedMilliseconds ms =
  if ms < 1000
    then Text.fromInt (round ms) ++ " ms"
    else Text.fromList (Numeric.showFFloat (Just 2) (ms / 1000) " s")
