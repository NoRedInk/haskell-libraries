module ArraySpec (tests) where

import Array
import NriPrelude hiding (map)
import qualified Expect
import qualified Fuzz
import qualified List
import Test (Test, describe, fuzz, fuzz2, test)

tests :: Test
tests =
  describe
    "Array tests"
    [ initTests,
      isEmptyTests,
      lengthTests,
      getSetTests,
      conversionTests,
      transformTests,
      sliceTests
    ]

limit :: Int
limit = 10000

size :: Fuzz.Fuzzer Int
size = Fuzz.intRange 1 limit

toLimit :: Int -> Fuzz.Fuzzer Int
toLimit x = Fuzz.intRange x limit

initTests :: Test
initTests =
  describe
    "Initialization"
    [ fuzz size "initialize" <| \size_ ->
        initialize size_ identity
          |> toList
          |> Expect.equal (List.range 0 (size_ - 1)),
      fuzz size "push" <| \size_ ->
        size_ - 1
          |> List.range 0
          |> List.foldl push empty
          |> Expect.equal (initialize size_ identity),
      test "initialize non-identity" <| \() ->
        initialize 4 (\n -> n * n)
          |> toList
          |> Expect.equal
            [0, 1, 4, 9],
      test "initialize empty" <| \() ->
        initialize 0 identity
          |> toList
          |> Expect.equal [],
      test "initialize negative" <| \() ->
        initialize (-2) identity
          |> toList
          |> Expect.equal []
    ]

isEmptyTests :: Test
isEmptyTests =
  describe
    "isEmpty"
    [ test "all empty arrays are equal" <| \() ->
        empty
          |> Expect.equal
            (fromList [] :: Array ()),
      test "empty array" <| \() ->
        isEmpty empty
          |> Expect.equal True,
      test "empty converted array" <| \() ->
        isEmpty (fromList [])
          |> Expect.equal True,
      test "non-empty array" <| \() ->
        isEmpty (fromList [1 :: Int])
          |> Expect.equal False
    ]

lengthTests :: Test
lengthTests =
  describe
    "Length"
    [ test "empty array" <| \() ->
        Expect.equal (length empty) 0,
      fuzz size "non-empty array" <| \size_ ->
        initialize size_ identity
          |> length
          |> Expect.equal size_,
      fuzz size "push" <| \size_ ->
        initialize size_ identity
          |> push size_
          |> length
          |> Expect.equal (size_ + 1),
      fuzz size "append" <| \size_ ->
        initialize (size_ // 2) identity
          |> append (initialize size_ identity)
          |> length
          |> Expect.equal (size_ + (size_ // 2)),
      fuzz size "set does not increase" <| \size_ ->
        initialize size_ identity
          |> set (size_ // 2) 1
          |> length
          |> Expect.equal size_,
      fuzz (toLimit 100) "big slice" <| \size_ ->
        initialize size_ identity
          |> slice 35 (-35)
          |> length
          |> Expect.equal (size_ - 70),
      fuzz2
        (Fuzz.intRange (-32) (-1))
        (toLimit 100)
        "small slice end"
        <| \n size_ ->
          initialize size_ identity
            |> slice 0 n
            |> length
            |> Expect.equal (size_ + n)
    ]

getSetTests :: Test
getSetTests =
  describe
    "Get and set"
    [ fuzz2 size size "can retrieve element" <| \x y ->
        let n = min x y
            size_ = max x y
         in get n (initialize (size_ + 1) identity)
              |> Expect.equal (Just n),
      fuzz2
        (Fuzz.intRange 1 50)
        (toLimit 100)
        "out of bounds retrieval returns nothing"
        <| \n size_ ->
          let arr = initialize size_ identity
           in Expect.all
                [ \() -> Expect.equal (get (negate n) arr) Nothing,
                  \() -> Expect.equal (get (size_ + n) arr) Nothing
                ]
                (),
      fuzz2 size size "set replaces value" <| \x y ->
        let n = min x y
            size_ = max x y
         in initialize (size_ + 1) identity
              |> set n 5
              |> get n
              |> Expect.equal (Just 5),
      fuzz2
        (Fuzz.intRange 1 50)
        size
        "set out of bounds returns original array"
        <| \n size_ ->
          let arr = initialize size_ identity
           in set (negate n) 5 arr
                |> set (size_ + n) 5
                |> Expect.equal arr,
      test "Retrieval works from tail" <| \() ->
        initialize 1035 identity
          |> set 1030 5
          |> get 1030
          |> Expect.equal (Just 5)
    ]

conversionTests :: Test
conversionTests =
  describe
    "Conversion"
    [ fuzz size "back and forth" <| \size_ ->
        let ls = List.range 0 (size_ - 1)
         in fromList ls
              |> toList
              |> Expect.equal ls,
      fuzz size "indexed" <| \size_ ->
        initialize size_ (1 +)
          |> toIndexedList
          |> Expect.equal
            ( toList
                ( initialize
                    size_
                    (\idx -> (idx, idx + 1))
                )
            )
    ]

transformTests :: Test
transformTests =
  describe
    "Transform"
    [ fuzz size "foldl" <| \size_ ->
        initialize size_ identity
          |> foldl (:) []
          |> Expect.equal
            ( List.reverse (List.range 0 (size_ - 1))
            ),
      fuzz size "foldr" <| \size_ ->
        initialize size_ identity
          |> foldr (:) []
          |> Expect.equal (List.range 0 (size_ - 1)),
      fuzz size "filter" <| \size_ ->
        initialize size_ identity
          |> filter (\a -> modBy 2 a == 0)
          |> toList
          |> Expect.equal
            ( List.filter
                (\a -> modBy 2 a == 0)
                (List.range 0 (size_ - 1))
            ),
      fuzz size "map" <| \size_ ->
        initialize size_ identity
          |> map (1 +)
          |> Expect.equal (initialize size_ (1 +)),
      fuzz size "indexedMap" <| \size_ ->
        repeat size_ 5
          |> indexedMap (*)
          |> Expect.equal (initialize size_ (5 *)),
      fuzz size "push appends one element" <| \size_ ->
        initialize size_ identity
          |> push size_
          |> Expect.equal (initialize (size_ + 1) identity),
      fuzz (Fuzz.intRange 1 1050) "append" <| \size_ ->
        initialize size_ (size_ +)
          |> append (initialize size_ identity)
          |> Expect.equal (initialize (size_ * 2) identity),
      fuzz2 size (Fuzz.intRange 1 32) "small appends" <| \s1 s2 ->
        initialize s2 (s1 +)
          |> append (initialize s1 identity)
          |> Expect.equal (initialize (s1 + s2) identity)
    ]

sliceTests :: Test
sliceTests =
  let smallSample = fromList (List.range 1 8)
   in describe
        "Slice"
        [ fuzz2 (Fuzz.intRange (-50) (-1)) (toLimit 100) "both" <| \n size_ ->
            initialize size_ identity
              |> slice (abs n) n
              |> Expect.equal
                ( initialize
                    (size_ + n + n)
                    (\idx -> idx - n)
                ),
          fuzz2 (Fuzz.intRange (-50) (-1)) (toLimit 100) "left" <| \n size_ ->
            let arr = initialize size_ identity
             in slice (abs n) (length arr) arr
                  |> Expect.equal
                    ( initialize (size_ + n) (\idx -> idx - n)
                    ),
          fuzz2 (Fuzz.intRange (-50) (-1)) (toLimit 100) "right" <| \n size_ ->
            initialize size_ identity
              |> slice 0 n
              |> Expect.equal (initialize (size_ + n) identity),
          fuzz size "slicing all but the last item" <| \size_ ->
            initialize size_ identity
              |> slice (-1) size_
              |> toList
              |> Expect.equal [size_ - 1],
          test "both small" <| \() ->
            slice 2 5 smallSample
              |> toList
              |> Expect.equal (List.range 3 5),
          test "start small" <| \() ->
            slice 2 (length smallSample) smallSample
              |> toList
              |> Expect.equal (List.range 3 8),
          test "negative" <| \() ->
            slice (-5) (-2) smallSample
              |> toList
              |> Expect.equal (List.range 4 6),
          test "impossible" <| \() ->
            slice (-1) (-2) smallSample
              |> toList
              |> Expect.equal [],
          test "crash" <| \() ->
            repeat (33 * 32) 1
              |> slice 0 1
              |> Expect.equal (repeat 1 1 :: Array Int)
        ]
