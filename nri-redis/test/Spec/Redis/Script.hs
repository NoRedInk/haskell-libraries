module Spec.Redis.Script (tests) where

import Data.Either (Either (..))
import qualified Expect
import Redis.Script
import qualified Test
import qualified Text.Megaparsec as P

tests :: Test.Test
tests =
  Test.describe
    "Redis.Script"
    [ Test.describe "parser" parserTests
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
        |> mapLeft P.errorBundlePretty
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
        |> mapLeft P.errorBundlePretty
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
        |> mapLeft P.errorBundlePretty
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
        |> mapLeft P.errorBundlePretty
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
        |> mapLeft P.errorBundlePretty
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

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b
