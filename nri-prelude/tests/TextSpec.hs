module TextSpec (tests) where

import qualified Expect
import NriPrelude
import Test (Test, describe, test)
import Text

tests :: Test
tests =
  describe
    "Text"
    [ describe
        "Simple Stuff"
        [ test "is empty" <| \() ->
            Expect.equal True (Text.isEmpty ""),
          test "is not empty" <| \() ->
            Expect.equal True (not (Text.isEmpty "the world")),
          test "length" <| \() ->
            Expect.equal 11 (Text.length "innumerable"),
          test "endsWith" <| \() ->
            Expect.equal True <| Text.endsWith "ship" "spaceship",
          test "reverse" <| \() ->
            Expect.equal "desserts" (Text.reverse "stressed"),
          test "repeat" <| \() ->
            Expect.equal "hahaha" (Text.repeat 3 "ha"),
          test "indexes" <| \() ->
            Expect.equal [0, 2] (Text.indexes "a" "aha"),
          test "empty indexes" <| \() ->
            Expect.equal [] (Text.indexes "" "aha")
        ],
      describe
        "Combining"
        [ test "uncons non-empty" <| \() ->
            Expect.equal (Just ('a', "bc")) (Text.uncons "abc"),
          test "uncons empty" <| \() ->
            Expect.equal Nothing (Text.uncons ""),
          test "append 1" <| \() ->
            Expect.equal "butterfly" (Text.append "butter" "fly"),
          test "append 2" <| \() ->
            Expect.equal "butter" (Text.append "butter" ""),
          test "append 3" <| \() ->
            Expect.equal "butter" (Text.append "" "butter"),
          test "concat" <| \() ->
            Expect.equal "nevertheless" (Text.concat ["never", "the", "less"]),
          test "split commas" <| \() ->
            Expect.equal ["cat", "dog", "cow"] (Text.split "," "cat,dog,cow"),
          test "split slashes" <| \() ->
            Expect.equal ["home", "steve", "Desktop", ""] (Text.split "/" "home/steve/Desktop/"),
          test "join spaces" <| \() ->
            Expect.equal "cat dog cow" (Text.join " " ["cat", "dog", "cow"]),
          test "join slashes" <| \() ->
            Expect.equal "home/steve/Desktop" (Text.join "/" ["home", "steve", "Desktop"]),
          test "slice 1" <| \() ->
            Expect.equal "c" (Text.slice 2 3 "abcd"),
          test "slice 2" <| \() ->
            Expect.equal "abc" (Text.slice 0 3 "abcd"),
          test "slice 3" <| \() ->
            Expect.equal "abc" (Text.slice 0 (-1) "abcd"),
          test "slice 4" <| \() ->
            Expect.equal "cd" (Text.slice (-2) 4 "abcd"),
          test "slice 5" <| \() ->
            Expect.equal "ab" (Text.slice (-6) 2 "abcd"),
          test "slice 6" <| \() ->
            Expect.equal "cd" (Text.slice 2 6 "abcd")
        ],
      describe
        "toInt"
        [ goodInt "1234" 1234,
          goodInt "+1234" 1234,
          goodInt "-1234" (-1234),
          badInt "1.34",
          badInt "1e31",
          badInt "123a",
          goodInt "0123" 123,
          goodInt "0x001A" 26,
          goodInt "0x001a" 26,
          goodInt "0xBEEF" 48879,
          badInt "0x12.0",
          badInt "0x12an"
        ],
      describe
        "toFloat"
        [ goodFloat "123" 123,
          goodFloat "3.14" 3.14,
          goodFloat "+3.14" 3.14,
          goodFloat "-3.14" (-3.14),
          goodFloat "0.12" 0.12,
          goodFloat ".12" 0.12,
          goodFloat "1e-42" 1e-42,
          goodFloat "6.022e23" 6.022e23,
          goodFloat "6.022E23" 6.022e23,
          goodFloat "6.022e+23" 6.022e23,
          badFloat "6.022e",
          badFloat "6.022n",
          badFloat "6.022.31"
        ],
      describe
        "UTF-16 Encoding"
        [ test "reverse 1" <| \() ->
            Expect.equal "𝌆c𝌆b𝌆a𝌆" (Text.reverse "𝌆a𝌆b𝌆c𝌆"),
          test "reverse 2" <| \() ->
            Expect.equal "nàm" (Text.reverse "màn"),
          test "reverse 3" <| \() ->
            Expect.equal "😣ba" (Text.reverse "ab😣"),
          test "filter" <| \() ->
            Expect.equal "mànabc" (Text.filter (/= '😣') "màn😣abc"),
          test "toList" <| \() ->
            Expect.equal ['𝌆', 'a', '𝌆', 'b', '𝌆'] (Text.toList "𝌆a𝌆b𝌆"),
          test "uncons" <| \() ->
            Expect.equal (Just ('😃', "bc")) (Text.uncons "😃bc"),
          test "map 1" <| \() ->
            Expect.equal "aaa" (Text.map (\_ -> 'a') "😃😃😃"),
          test "map 2" <| \() ->
            Expect.equal "😃😃😃" (Text.map (\_ -> '😃') "aaa"),
          test "foldl" <| \() ->
            Expect.equal (3 :: Int) (Text.foldl (\_ c -> c + 1) 0 "😃😃😃"),
          test "foldr" <| \() ->
            Expect.equal (3 :: Int) (Text.foldr (\_ c -> c + 1) 0 "😃😃😃"),
          test "all" <| \() ->
            Expect.equal True (Text.all (== '😃') "😃😃😃"),
          test "any" <| \() ->
            Expect.equal True (Text.any (== '😃') "abc😃123")
        ]
    ]

-- NUMBER HELPERS
goodInt :: Text -> Int -> Test
goodInt str int =
  test (str ++ " is good") <| \() ->
    Expect.equal (Text.toInt str) (Just int)

badInt :: Text -> Test
badInt str =
  test (str ++ " is bad") <| \() ->
    Expect.equal (Text.toInt str) Nothing

goodFloat :: Text -> Float -> Test
goodFloat str float =
  test (str ++ " is good") <| \() ->
    Expect.equal (Text.toFloat str) (Just float)

badFloat :: Text -> Test
badFloat str =
  test (str ++ " is bad") <| \() ->
    Expect.equal (Text.toFloat str) Nothing
