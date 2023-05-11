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
        "enum"
        [ test "should decode to the correct value"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "A"
                      }
                    ( Environment.enum
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
                    ( Environment.enum
                        [ ("A", A),
                          ("B", B)
                        ]
                    )
                )
                (Dict.singleton "TEST" "C")
                |> Expect.equal (Err "Parsing TEST failed: Unknown option: C ( A, B )"),
          test "should use the default value if the key is not present"
            <| \() ->
              Environment.decodePairs
                ( Environment.variable
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "B"
                      }
                    ( Environment.enum
                        [ ("A", A),
                          ("B", B)
                        ]
                    )
                )
                (Dict.empty)
                |> Expect.equal (Ok B)
        ],
      describe
        "variableWithOptionalPrefix"
        [ test "Should use the prefixed value if available"
            <| \() ->
              Environment.decodePairs
                ( Environment.variableWithOptionalPrefix
                    "PREFIX_"
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "default"
                      }
                    Environment.text
                )
                (Dict.singleton "PREFIX_TEST" "prefixed")
                |> Expect.equal (Ok "prefixed"),
          test "Should use the prefixed value if both prefixed and unprefixed are available"
            <| \() ->
              Environment.decodePairs
                ( Environment.variableWithOptionalPrefix
                    "PREFIX_"
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "default"
                      }
                    Environment.text
                )
                (Dict.fromList [("PREFIX_TEST", "prefixed"), ("TEST", "unprefixed")])
                |> Expect.equal (Ok "prefixed"),
          test "Should use the unprefixed value if only unprefixed is available"
            <| \() ->
              Environment.decodePairs
                ( Environment.variableWithOptionalPrefix
                    "PREFIX_"
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "default"
                      }
                    Environment.text
                )
                (Dict.singleton "TEST" "unprefixed")
                |> Expect.equal (Ok "unprefixed"),
          test "Should use the default value if prefixed nor unprefixed is available"
            <| \() ->
              Environment.decodePairs
                ( Environment.variableWithOptionalPrefix
                    "PREFIX_"
                    Environment.Variable
                      { Environment.name = "TEST",
                        Environment.description = "test",
                        Environment.defaultValue = "default"
                      }
                    Environment.text
                )
                Dict.empty
                |> Expect.equal (Ok "default")
        ]
    ]
