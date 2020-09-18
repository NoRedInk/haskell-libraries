module DictSpec (tests) where

import Basics
import qualified Dict
import qualified Expect
import qualified List
import Maybe (Maybe (Just, Nothing))
import Test (Test, describe, test)
import Text (Text)

tests :: Test
tests =
  let buildTests =
        describe
          "build Tests"
          [ test "empty" <| \() -> Expect.equal (Dict.fromList []) (Dict.empty :: Dict.Dict () ()),
            test "singleton" <| \() -> Expect.equal (Dict.fromList [(k, v)]) (Dict.singleton k v),
            test "insert" <| \() -> Expect.equal (Dict.fromList [(k, v)]) (Dict.insert k v Dict.empty),
            test "insert replace" <| \() -> Expect.equal (Dict.fromList [(k, vv)]) (Dict.insert k vv (Dict.singleton k v)),
            test "update" <| \() -> Expect.equal (Dict.fromList [(k, vv)]) (Dict.update k (\_v -> Just vv) (Dict.singleton k v)),
            test "update Nothing" <| \() -> Expect.equal Dict.empty (Dict.update k (always Nothing) (Dict.singleton k v)),
            test "remove" <| \() -> Expect.equal Dict.empty (Dict.remove k (Dict.singleton k v)),
            test "remove not found" <| \() -> Expect.equal (Dict.singleton k v) (Dict.remove kk (Dict.singleton k v))
          ]
      queryTests =
        describe
          "query Tests"
          [ test "member 1" <| \() -> Expect.equal True (Dict.member tom animals),
            test "member 2" <| \() -> Expect.equal False (Dict.member spike animals),
            test "get 1" <| \() -> Expect.equal (Just cat) (Dict.get tom animals),
            test "get 2" <| \() -> Expect.equal Nothing (Dict.get spike animals),
            test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty),
            test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
          ]
      combineTests =
        describe
          "combine Tests"
          [ test "union" <| \() -> Expect.equal animals (Dict.union (Dict.singleton jerry mouse) (Dict.singleton "Tom" "cat")),
            test "union collison" <| \() -> Expect.equal (Dict.singleton tom cat) (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse")),
            test "intersect" <| \() -> Expect.equal (Dict.singleton tom cat) (Dict.intersect animals (Dict.singleton "Tom" "cat")),
            test "diff" <| \() -> Expect.equal (Dict.singleton jerry mouse) (Dict.diff animals (Dict.singleton tom cat))
          ]
      transformTests =
        describe
          "transform Tests"
          [ test "filter" <| \() -> Expect.equal (Dict.singleton tom cat) (Dict.filter (\key _v -> key == "Tom") animals),
            test "partition" <| \() -> Expect.equal (Dict.singleton tom cat, Dict.singleton "Jerry" "mouse") (Dict.partition (\key _v -> key == "Tom") animals)
          ]
      mergeTests =
        let insertBoth key leftVal rightVal =
              Dict.insert key (leftVal ++ rightVal)
            s1 =
              Dict.empty |> Dict.insert u1 [1]
            s2 =
              Dict.empty |> Dict.insert u2 [2]
            s23 =
              Dict.empty |> Dict.insert u2 [3]
            b1 =
              List.map (\i -> (i, [i])) (List.range 1 10) |> Dict.fromList
            b2 =
              List.map (\i -> (i, [i])) (List.range 5 15) |> Dict.fromList
            bExpected =
              [(1, [1]), (2, [2]), (3, [3]), (4, [4]), (5, [5, 5]), (6, [6, 6]), (7, [7, 7]), (8, [8, 8]), (9, [9, 9]), (10, [10, 10]), (11, [11]), (12, [12]), (13, [13]), (14, [14]), (15, [15])]
         in describe
              "merge Tests"
              [ test "merge empties" <| \() ->
                  Expect.equal
                    (Dict.empty :: Dict.Dict Text [Int])
                    (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty),
                test "merge singletons in order" <| \() ->
                  Expect.equal
                    [(u1, [1 :: Int]), (u2, [2])]
                    (Dict.toList (Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty)),
                test "merge singletons out of order" <| \() ->
                  Expect.equal
                    [(u1, [1]), (u2, [2])]
                    (Dict.toList (Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty)),
                test "merge with duplicate key" <| \() ->
                  Expect.equal
                    [(u2, [2, 3])]
                    (Dict.toList (Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty)),
                test "partially overlapping" <| \() ->
                  Expect.equal
                    bExpected
                    (Dict.toList (Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty))
              ]
   in describe
        "Dict Tests"
        [ buildTests,
          queryTests,
          combineTests,
          transformTests,
          mergeTests
        ]

-- Most of the names below exist because Haskell can't figure out the string
-- type to use, so we must annotate the type. They're here rather than in the
-- tests above because it gets noisy with all the annotations in-line. The
-- original Elm tests did not have this problem.
--
k :: Text
k = "k"

kk :: Text
kk = "kk"

v :: Text
v = "v"

vv :: Text
vv = "vv"

animals :: Dict.Dict Text Text
animals =
  Dict.fromList [("Tom", "cat"), ("Jerry", "mouse")]

tom :: Text
tom = "Tom"

spike :: Text
spike = "Spike"

jerry :: Text
jerry = "Jerry"

cat :: Text
cat = "cat"

mouse :: Text
mouse = "mouse"

u1 :: Text
u1 = "u1"

u2 :: Text
u2 = "u2"
