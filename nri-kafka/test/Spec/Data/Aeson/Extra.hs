module Spec.Data.Aeson.Extra (tests) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (decodeIntoFlatDict)
import qualified Dict
import qualified Expect
import Test

tests :: Test
tests =
  describe
    "decodeIntoFlatDict"
    [ test "simple object" <| \() ->
        "{\"foo\": 1}"
          |> decodeIntoFlatDict
          |> Expect.equal (Ok (Dict.fromList [("foo", Aeson.Number 1)])),
      test "with nested object" <| \() ->
        "{\"foo\": 1, \"bar\": { \"moo\": \"cow\" }}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ("foo", Aeson.Number 1),
                      ("bar.moo", Aeson.String "cow")
                    ]
                )
            ),
      test "with more nesting object" <| \() ->
        "{\"foo\": 1, \"bar\": { \"moo\": \"cow\", \"hello\": { \"world\": true }}}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ("foo", Aeson.Number 1),
                      ("bar.moo", Aeson.String "cow"),
                      ("bar.hello.world", Aeson.Bool True)
                    ]
                )
            ),
      test "with nested arrays" <| \() ->
        "{\"foo\": 1, \"bar\": [ 1, 2, 3 ]}"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ("foo", Aeson.Number 1),
                      ("bar[0]", Aeson.Number 1),
                      ("bar[1]", Aeson.Number 2),
                      ("bar[2]", Aeson.Number 3)
                    ]
                )
            ),
      test "with top-level array" <| \() ->
        "[1, 2, 3 ]"
          |> decodeIntoFlatDict
          |> Expect.equal
            ( Ok
                ( Dict.fromList
                    [ ("[0]", Aeson.Number 1),
                      ("[1]", Aeson.Number 2),
                      ("[2]", Aeson.Number 3)
                    ]
                )
            )
    ]
