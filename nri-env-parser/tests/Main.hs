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
      let enumDecoder =
            Environment.variable
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
          variantA =
            Environment.variable
              Environment.Variable
                { Environment.name = "VARIANT_A",
                  Environment.description = "variant a",
                  Environment.defaultValue = "variant_a"
                }
              Environment.text
          variantB =
            Environment.variable
              Environment.Variable
                { Environment.name = "VARIANT_B",
                  Environment.description = "variant b",
                  Environment.defaultValue = "variant_b"
                }
              Environment.text
          decoder =
            enumDecoder
              |> andThen
                ( \variant ->
                    case variant of
                      A -> variantA
                      B -> variantB
                )
       in describe
            "andThen"
            [ test "consumes only tracks the initial variable" <| \() ->
                Environment.consumes decoder
                  |> Expect.equal
                    [ Environment.Variable
                        { Environment.name = "TEST",
                          Environment.description = "test",
                          Environment.defaultValue = "B"
                        }
                    ],
              test "Uses the default values if none available" <| \() ->
                Environment.decodePairs decoder Dict.empty
                  |> Expect.equal (Ok "variant_b"),
              test "Uses the default value for the inner decoder" <| \() ->
                Environment.decodePairs decoder (Dict.singleton "TEST" "A")
                  |> Expect.equal (Ok "variant_a"),
              test "Uses the environment for the inner decoder" <| \() ->
                Environment.decodePairs
                  decoder
                  ( Dict.fromList
                      [ ("TEST", "A"),
                        ("VARIANT_A", "foobar")
                      ]
                  )
                  |> Expect.equal (Ok "foobar")
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
