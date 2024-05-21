module Spec.Redis.Script (tests) where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Either (Either (..))
import qualified Expect
import Redis.Script
import qualified Test

tests :: Test.Test
tests =
  Test.describe
    "Redis.Script"
    [ Test.describe "parser" parserTests
    ]

parserTests :: List Test.Test
parserTests =
  [ Test.test "1 word" <| \_ ->
      (Attoparsec.parseOnly parser "Jabuticaba")
        |> Expect.equal (Right [ScriptText "Jabuticaba"]),
    Test.test "3 words" <| \_ ->
      (Attoparsec.parseOnly parser "Picolé de Jabuticaba")
        |> Expect.equal (Right [ScriptText "Picolé de Jabuticaba"]),
    Test.test "1 value" <| \_ ->
      (Attoparsec.parseOnly parser "${value}")
        |> Expect.equal (Right [ScriptVariable "value"]),
    Test.test "function application" <| \_ ->
      (Attoparsec.parseOnly parser "${func arg1 arg2}")
        |> Expect.equal (Right [ScriptVariable "func arg1 arg2"]),
    Test.test "text and variables" <| \_ ->
      (Attoparsec.parseOnly parser "some text ${value} some more text ${ anotherValue }")
        |> Expect.equal
          ( Right
              [ ScriptText "some text ",
                ScriptVariable "value",
                ScriptText " some more text ",
                ScriptVariable "anotherValue"
              ]
          ),
    Test.only <| Test.test "ERROR: nested ${}" <| \_ -> do
      (Attoparsec.parseOnly parser "asdasd ${ ${ value } }")
        |> Expect.equal (Left "Expected at least one > No '$', '{' or '}' allowed in interpolated expression. Note: I'm a simple parser and I don't support records inside ${}.: Failed reading: takeWhile1"),
    Test.test "ERROR: misplaced ${ inside ${}" <| \_ -> do
      (Attoparsec.parseOnly parser "${ v$alue }")
        |> Expect.equal (Left "Expected at least one > Expected '}' after: ${v > '}': Failed reading: satisfy"),
    Test.test "ERROR: misplaced ${ inside ${}" <| \_ -> do
      (Attoparsec.parseOnly parser "${ v{alue }")
        |> Expect.equal (Left "Expected at least one > Expected '}' after: ${v > '}': Failed reading: satisfy")
  ]
