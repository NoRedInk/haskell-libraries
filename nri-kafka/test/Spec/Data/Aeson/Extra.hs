module Spec.Data.Aeson.Extra (tests) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (Segment (..), decodeIntoFlatDict, pathToText)
import qualified Dict
import qualified Expect
import Test

tests :: Test
tests =
  describe
    "Data.Aeson.Extra"
    [ decodeIntoFlatDictTest,
      pathToTextTest
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
            )
    ]

pathToTextTest :: Test
pathToTextTest =
  describe
    "pathToText"
    [ test "keys get separated" <| \() ->
        pathToText [Key "foo", Key "bar"]
          |> Expect.equal "foo.bar",
      test "indexes have brackets" <| \() ->
        pathToText [Index 0]
          |> Expect.equal "[0]",
      test "indexes within a key" <| \() ->
        pathToText [Key "foo", Index 0, Index 1]
          |> Expect.equal "foo[0][1]",
      test "keys within an index" <| \() ->
        pathToText [Index 0, Key "foo", Key "bar"]
          |> Expect.equal "[0].foo.bar",
      test "keys get escaped" <| \() ->
        pathToText [Key "foo.bar[0]"]
          |> Expect.equal "foo\\.bar\\[0\\]"
    ]
