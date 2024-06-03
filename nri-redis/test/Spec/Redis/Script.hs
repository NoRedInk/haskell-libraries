{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Redis.Script (tests) where

import qualified Data.Bifunctor
import Data.Either (Either (..))
import qualified Expect
import Redis.Script
import qualified Test
import qualified Text.Megaparsec as P

tests :: Test.Test
tests =
  Test.describe
    "Redis.Script"
    [ Test.describe "parser" parserTests,
      Test.describe "th tests" thTests
    ]

parserTests :: List Test.Test
parserTests =
  [ Test.test "1 word" <| \_ ->
      P.runParser parser "" "Jabuticaba"
        |> Expect.equal (Right [ScriptText "Jabuticaba"]),
    Test.test "3 words" <| \_ ->
      P.runParser parser "" "Picolé de Jabuticaba"
        |> Expect.equal (Right [ScriptText "Picolé de Jabuticaba"]),
    Test.test "1 value" <| \_ ->
      P.runParser parser "" "${value}"
        |> Expect.equal (Right [ScriptVariable "value"]),
    Test.test "function application" <| \_ ->
      P.runParser parser "" "${func arg1 arg2}"
        |> Expect.equal (Right [ScriptVariable "func arg1 arg2"]),
    Test.test "text and variables" <| \_ ->
      P.runParser parser "" "some text ${value} some more text ${ anotherValue }"
        |> Expect.equal
          ( Right
              [ ScriptText "some text ",
                ScriptVariable "value",
                ScriptText " some more text ",
                ScriptVariable "anotherValue"
              ]
          ),
    Test.test "ERROR: empty" <| \_ -> do
      P.runParser parser "" ""
        |> Data.Bifunctor.first P.errorBundlePretty
        |> Expect.equal
          ( Left
              "1:1:\n\
              \  |\n\
              \1 | <empty line>\n\
              \  | ^\n\
              \unexpected end of input\n\
              \expecting \"${\" or some plain text\n\
              \"
          ),
    Test.test "ERROR: empty variable" <| \_ -> do
      P.runParser parser "" "${}"
        |> Data.Bifunctor.first P.errorBundlePretty
        |> Expect.equal
          ( Left
              "1:3:\n\
              \  |\n\
              \1 | ${}\n\
              \  |   ^\n\
              \unexpected '}'\n\
              \expecting anything but '$', '{' or '}' (no records, sorry) or white space\n\
              \"
          ),
    Test.test "ERROR: nested ${}" <| \_ -> do
      P.runParser parser "" "asdasd ${ ${ value } }"
        |> Data.Bifunctor.first P.errorBundlePretty
        |> Expect.equal
          ( Left
              "1:11:\n\
              \  |\n\
              \1 | asdasd ${ ${ value } }\n\
              \  |           ^\n\
              \unexpected '$'\n\
              \expecting anything but '$', '{' or '}' (no records, sorry) or white space\n\
              \"
          ),
    Test.test "ERROR: misplaced ${ inside ${}" <| \_ -> do
      P.runParser parser "" "${ v$alue }"
        |> Data.Bifunctor.first P.errorBundlePretty
        |> Expect.equal
          ( Left
              "1:5:\n\
              \  |\n\
              \1 | ${ v$alue }\n\
              \  |     ^\n\
              \unexpected '$'\n\
              \expecting '}' or anything but '$', '{' or '}' (no records, sorry)\n\
              \"
          ),
    Test.test "ERROR: misplaced { inside ${}" <| \_ -> do
      P.runParser parser "" "${ v{alue }"
        |> Data.Bifunctor.first P.errorBundlePretty
        |> Expect.equal
          ( Left
              "1:5:\n\
              \  |\n\
              \1 | ${ v{alue }\n\
              \  |     ^\n\
              \unexpected '{'\n\
              \expecting '}' or anything but '$', '{' or '}' (no records, sorry)\n\
              \"
          )
  ]

thTests :: List Test.Test
thTests =
  [ Test.test "just text" <| \_ ->
      [script|some text|]
        |> Expect.equal
          ( Script
              { luaScript = "some text",
                quasiQuotedString = "some text",
                keys = [],
                arguments = Log.mkSecret []
              }
          ),
    Test.test "one key argument" <| \_ ->
      [script|${Key "hi"}|]
        |> Expect.equal
          ( Script
              { luaScript = "KEYS[1]",
                quasiQuotedString = "${Key \"hi\"}",
                keys = ["hi"],
                arguments = Log.mkSecret []
              }
          ),
    Test.test "one literal argument" <| \_ ->
      [script|${Literal "hi"}|]
        |> Expect.equal
          ( Script
              { luaScript = "ARGV[1]",
                quasiQuotedString = "${Literal \"hi\"}",
                keys = [],
                arguments = Log.mkSecret ["hi"]
              }
          ),
    Test.test "one key one literal argument" <| \_ ->
      [script|${Key "a key"} ${Literal "a literal"}|]
        |> Expect.equal
          ( Script
              { luaScript = "KEYS[1] ARGV[1]",
                quasiQuotedString = "${Key \"a key\"} ${Literal \"a literal\"}",
                keys = ["a key"],
                arguments = Log.mkSecret ["a literal"]
              }
          ),
    Test.test "multiple keys and literals" <| \_ ->
      [script|${Key "key1"} ${Key "key2"} ${Key "key3"} ${Literal "literal1"} ${Literal "literal2"} ${Literal "literal3"}|]
        |> Expect.equal
          ( Script
              { luaScript = "KEYS[1] KEYS[2] KEYS[3] ARGV[1] ARGV[2] ARGV[3]",
                quasiQuotedString = "${Key \"key1\"} ${Key \"key2\"} ${Key \"key3\"} ${Literal \"literal1\"} ${Literal \"literal2\"} ${Literal \"literal3\"}",
                keys = ["key1", "key2", "key3"],
                arguments = Log.mkSecret ["literal1", "literal2", "literal3"]
              }
          ),
    Test.test "fails on type-checking when not given Key or Literal" <| \_ ->
      [script|${False}|]
        |> arguments
        |> Log.unSecret
        |> Expect.equal ["this would have been a type-checking error"]
  ]

-- This instance is picked when none of the instances in src/Redis/Script.hs work..
-- proving in real code we would have a type-checking error.
instance {-# INCOHERENT #-} HasScriptParam Bool where
  getScriptParam _ = Literal "this would have been a type-checking error"
