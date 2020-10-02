module BitwiseSpec (tests) where

import Bitwise
import qualified Expect
import NriPrelude
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Bitwise"
    [ describe
        "and"
        [ test "and with 32 bit integers" <| \() ->
            Bitwise.and 5 3
              |> Expect.equal 1,
          test "and with 0 as first argument" <| \() ->
            Bitwise.and 0 1450
              |> Expect.equal 0,
          test "and with 0 as second argument" <| \() ->
            Bitwise.and 274 0
              |> Expect.equal 0,
          test "and with -1 as first argument" <| \() ->
            Bitwise.and (-1) 2671
              |> Expect.equal 2671,
          test "and with -1 as second argument" <| \() ->
            Bitwise.and 96 (-1)
              |> Expect.equal 96
        ],
      describe
        "or"
        [ test "or with 32 bit integers" <| \() ->
            Bitwise.or 9 14
              |> Expect.equal 15,
          test "or with 0 as first argument" <| \() ->
            Bitwise.or 0 843
              |> Expect.equal 843,
          test "or with 0 as second argument" <| \() ->
            Bitwise.or 19 0
              |> Expect.equal 19,
          test "or with -1 as first argument" <| \() ->
            Bitwise.or (-1) 2360
              |> Expect.equal (-1),
          test "or with -1 as second argument" <| \() ->
            Bitwise.or 3 (-1)
              |> Expect.equal (-1)
        ],
      describe
        "xor"
        [ test "xor with 32 bit integers" <| \() ->
            Bitwise.xor 580 24
              |> Expect.equal 604,
          test "xor with 0 as first argument" <| \() ->
            Bitwise.xor 0 56
              |> Expect.equal 56,
          test "xor with 0 as second argument" <| \() ->
            Bitwise.xor (-268) 0
              |> Expect.equal (-268),
          test "xor with -1 as first argument" <| \() ->
            Bitwise.xor (-1) 24
              |> Expect.equal (-25),
          test "xor with -1 as second argument" <| \() ->
            Bitwise.xor (-25602) (-1)
              |> Expect.equal 25601
        ],
      describe
        "complement"
        [ test "complement a positive" <| \() ->
            Bitwise.complement 8
              |> Expect.equal (-9),
          test "complement a negative" <| \() ->
            Bitwise.complement (-279)
              |> Expect.equal 278
        ],
      describe
        "shiftLeftBy"
        [ test "8 |> shiftLeftBy 1 == 16" <| \() ->
            8 |> Bitwise.shiftLeftBy 1
              |> Expect.equal 16,
          test "8 |> shiftLeftby 2 == 32" <| \() ->
            8 |> Bitwise.shiftLeftBy 2
              |> Expect.equal 32
        ],
      describe
        "shiftRightBy"
        [ test "32 |> shiftRight 1 == 16" <| \() ->
            32 |> Bitwise.shiftRightBy 1
              |> Expect.equal 16,
          test "32 |> shiftRight 2 == 8" <| \() ->
            32 |> Bitwise.shiftRightBy 2
              |> Expect.equal 8,
          test "-32 |> shiftRight 1 == -16" <| \() ->
            -32 |> Bitwise.shiftRightBy 1
              |> Expect.equal (-16)
        ],
      describe
        "shiftRightZfBy"
        [ test "32 |> shiftRightZfBy 1 == 16" <| \() ->
            32 |> Bitwise.shiftRightZfBy 1
              |> Expect.equal 16,
          test "32 |> shiftRightZfBy 2 == 8" <| \() ->
            32 |> Bitwise.shiftRightZfBy 2
              |> Expect.equal 8,
          test "-32 |> shiftRightZfBy 1 == 9223372036854775792" <| \() ->
            -32 |> Bitwise.shiftRightZfBy 1
              |> Expect.equal 9223372036854775792
        ]
    ]
