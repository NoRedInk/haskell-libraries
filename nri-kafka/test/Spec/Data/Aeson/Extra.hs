module Spec.Data.Aeson.Extra (tests) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (Segment (..), decodeIntoFlatDict)
import qualified Dict
import qualified Expect
import Test

tests :: Test
tests =
  describe
    "Data.Aeson.Extra"
    [ decodeIntoFlatDictTest
    ]

decodeIntoFlatDictTest :: Test
decodeIntoFlatDictTest =
  describe
    "decodeIntoFlatDict"
    [ test "simple object" <| \() ->
        "{\"foo\": 1}"
          |> decodeIntoFlatDict
          |> Expect.equal (Ok (Dict.fromList [([Key "foo"], Aeson.Number 1)])),
      test "with nested object" <| \() ->
        "{\"foo\": 1, \"bar\": { \"moo\": \"cow\" }}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ([Key "foo"], Aeson.Number 1),
                      ([Key "bar", Key "moo"], Aeson.String "cow")
                    ]
                )
            ),
      test "with more nesting object" <| \() ->
        "{\"foo\": 1, \"bar\": { \"moo\": \"cow\", \"hello\": { \"world\": true }}}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ([Key "foo"], Aeson.Number 1),
                      ([Key "bar", Key "moo"], Aeson.String "cow"),
                      ([Key "bar", Key "hello", Key "world"], Aeson.Bool True)
                    ]
                )
            ),
      test "with nested arrays" <| \() ->
        "{\"foo\": 1, \"bar\": [ 1, 2, 3 ]}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ([Key "foo"], Aeson.Number 1),
                      ([Key "bar", Index 0], Aeson.Number 1),
                      ([Key "bar", Index 1], Aeson.Number 2),
                      ([Key "bar", Index 2], Aeson.Number 3)
                    ]
                )
            ),
      test "with top-level array" <| \() ->
        "[1, 2, 3 ]"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ([Index 0], Aeson.Number 1),
                      ([Index 1], Aeson.Number 2),
                      ([Index 2], Aeson.Number 3)
                    ]
                )
            ),
      test "with top-level value" <| \() ->
        "true"
          |> decodeIntoFlatDict
          |> Expect.equal (Ok (Dict.fromList [([], Aeson.Bool True)]))
    ]
