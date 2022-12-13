module Main
  ( main,
  )
where

import qualified Dict
import qualified Environment
import qualified Expect
import Test (Test, describe, run, test)
import qualified Prelude

main :: Prelude.IO ()
main = run tests

data AB = A | B
  deriving (Show, Eq)

tests :: Test
tests =
  describe
    "Environment"
    [ describe
        "oneOf"
        [ test "should decode to the correct value"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "A"
                      }
                    ( Environment.oneOf
                        Environment.text
                        [ ("A", A),
                          ("B", B)
                        ]
                    )
                )
                (Dict.singleton "TEST" "A")
                |> Expect.equal (Ok A),
          test "should error if the value is not in the enum"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "A"
                      }
                    ( Environment.oneOf
                        Environment.text
                        [ ("A", A),
                          ("B", B)
                        ]
                    )
                )
                (Dict.singleton "TEST" "C")
                |> Expect.equal (Err "Parsing TEST failed: Unknown option: \"C\" ( \"A\", \"B\" )"),
          test "should use the parser to parse into the type of the key"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "0"
                      }
                    ( Environment.oneOf
                        Environment.int
                        [ (0, A),
                          (1, B)
                        ]
                    )
                )
                (Dict.singleton "TEST" "1")
                |> Expect.equal (Ok B),
          test "should fail if the parser fails"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "0"
                      }
                    ( Environment.oneOf
                        Environment.int
                        [ (0, A),
                          (1, B)
                        ]
                    )
                )
                (Dict.singleton "TEST" "A")
                |> Expect.equal (Err "Parsing TEST failed: Could not parse as integer: A"),
          test "should use the default value if the key is not present"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "1"
                      }
                    ( Environment.oneOf
                        Environment.int
                        [ (0, A),
                          (1, B)
                        ]
                    )
                )
                (Dict.empty)
                |> Expect.equal (Ok B)
        ]
    ]
