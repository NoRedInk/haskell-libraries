-- |  A library to create @Expectation@s, which describe a claim to be tested.
--
-- = Quick Reference
--
-- - 'equal' @(arg2 == arg1)@
-- - 'notEqual' @(arg2 /= arg1)@
-- - 'lessThan' @(arg2 < arg1)@
-- - 'atMost' @(arg2 <= arg1)@
-- - 'greaterThan' @(arg2 > arg1)@
-- - 'atLeast' @(arg2 >= arg1)@
-- - 'true' @(arg == True)@
-- - 'false' @(arg == False)@
module Expect
  ( -- * Basic Expectations
    Expectation,
    equal,
    notEqual,
    all,
    concat,
    equalToContentsOf,

    -- * Numeric Comparisons
    lessThan,
    atMost,
    greaterThan,
    atLeast,

    -- * Booleans
    true,
    false,

    -- * Collections
    ok,
    err,

    -- * Customizing
    pass,
    fail,
    onFail,
    fromResult,

    -- * Testing tasks
    succeeds,
    fails,
    andCheck,

    -- * Fancy Expectations
    fromIO,
    Internal.Expectation',
  )
where

import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug
import qualified List
import NriPrelude
import qualified Platform.Internal
import qualified Pretty.Diff as Diff
import qualified System.Console.Terminal.Size as Terminal
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Task
import Test.Internal (Expectation)
import qualified Test.Internal as Internal
import qualified Text.Show.Pretty
import Prelude (IO, show)
import qualified Prelude

-- | Always passes.
--
-- > import Json.Decode exposing (decodeString, int)
-- > import Test exposing (test)
-- > import Expect
-- >
-- >
-- > test "Json.Decode.int can decode the number 42." <|
-- >     \_ ->
-- >         case decodeString int "42" of
-- >             Ok _ ->
-- >                 Expect.pass
-- >
-- >             Err err ->
-- >                 Expect.fail err
pass :: Expectation
pass = Internal.Expectation (Task.succeed ())

-- | Fails with the given message.
--
-- > import Json.Decode exposing (decodeString, int)
-- > import Test exposing (test)
-- > import Expect
-- >
-- >
-- > test "Json.Decode.int can decode the number 42." <|
-- >     \_ ->
-- >         case decodeString int "42" of
-- >             Ok _ ->
-- >                 Expect.pass
-- >
-- >             Err err ->
-- >                 Expect.fail err
fail :: Text -> Expectation
fail msg =
  msg
    |> Internal.FailedAssertion
    |> Task.fail
    |> Internal.Expectation

-- | If the given expectation fails, replace its failure message with a custom one.
--
-- > "something"
-- >     |> Expect.equal "something else"
-- >     |> Expect.onFail "thought those two strings would be the same"
onFail :: Text -> Expectation -> Expectation
onFail msg (Internal.Expectation task) =
  task
    |> Task.onError
      ( \_ ->
          Task.fail (Internal.FailedAssertion msg)
      )
    |> Internal.Expectation

-- | Passes if the arguments are equal.
--
-- > Expect.equal 0 (List.length [])
-- >
-- > -- Passes because (0 == 0) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because the expected value didn't split the space in "Betty Botter"
-- > Text.split " " "Betty Botter bought some butter"
-- >     |> Expect.equal [ "Betty Botter", "bought", "some", "butter" ]
-- >
-- > {-
-- >
-- > [ "Betty", "Botter", "bought", "some", "butter" ]
-- > ╷
-- > │ Expect.equal
-- > ╵
-- > [ "Betty Botter", "bought", "some", "butter" ]
-- >
-- > -}
equal :: (Show a, Eq a) => a -> a -> Expectation
equal = assert (==) "Expect.equal"

-- | Passes if the arguments are not equal.
--
-- > -- Passes because (11 /= 100) is True
-- > 90 + 10
-- >     |> Expect.notEqual 11
-- >
-- >
-- > -- Fails because (100 /= 100) is False
-- > 90 + 10
-- >     |> Expect.notEqual 100
-- >
-- > {-
-- >
-- > 100
-- > ╷
-- > │ Expect.notEqual
-- > ╵
-- > 100
-- >
-- > -}
notEqual :: (Show a, Eq a) => a -> a -> Expectation
notEqual = assert (/=) "Expect.notEqual"

-- | Passes if the second argument is less than the first.
--
-- > Expect.lessThan 1 (List.length [])
-- >
-- > -- Passes because (0 < 1) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (0 < -1) is False
-- > List.length []
-- >     |> Expect.lessThan -1
-- >
-- >
-- > {-
-- >
-- > 0
-- > ╷
-- > │ Expect.lessThan
-- > ╵
-- > -1
-- >
-- > -}
lessThan :: (Show a, Ord a) => a -> a -> Expectation
lessThan = assert (>) "Expect.lessThan"

-- | Passes if the second argument is less than or equal to the first.
--
-- > Expect.atMost 1 (List.length [])
-- >
-- > -- Passes because (0 <= 1) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (0 <= -3) is False
-- > List.length []
-- >     |> Expect.atMost -3
-- >
-- > {-
-- >
-- > 0
-- > ╷
-- > │ Expect.atMost
-- > ╵
-- > -3
-- >
-- > -}
atMost :: (Show a, Ord a) => a -> a -> Expectation
atMost = assert (>=) "Expect.atMost"

-- | Passes if the second argument is greater than the first.
--
-- > Expect.greaterThan -2 List.length []
-- >
-- > -- Passes because (0 > -2) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (0 > 1) is False
-- > List.length []
-- >     |> Expect.greaterThan 1
-- >
-- > {-
-- >
-- > 0
-- > ╷
-- > │ Expect.greaterThan
-- > ╵
-- > 1
-- >
-- > -}
greaterThan :: (Show a, Ord a) => a -> a -> Expectation
greaterThan = assert (<) "Expect.greaterThan"

-- | Passes if the second argument is greater than or equal to the first.
--
-- > Expect.atLeast -2 (List.length [])
-- >
-- > -- Passes because (0 >= -2) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (0 >= 3) is False
-- > List.length []
-- >     |> Expect.atLeast 3
-- >
-- > {-
-- >
-- > 0
-- > ╷
-- > │ Expect.atLeast
-- > ╵
-- > 3
-- >
-- > -}
atLeast :: (Show a, Ord a) => a -> a -> Expectation
atLeast = assert (<=) "Expect.atLeast"

-- | Passes if the argument is 'True', and otherwise fails with the given message.
--
-- > Expect.true "Expected the list to be empty." (List.isEmpty [])
-- >
-- > -- Passes because (List.isEmpty []) is True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because List.isEmpty returns False, but we expect True.
-- > List.isEmpty [ 42 ]
-- >     |> Expect.true "Expected the list to be empty."
-- >
-- > {-
-- >
-- > Expected the list to be empty.
-- >
-- > -}
true :: Bool -> Expectation
true x = assert (&&) "Expect.true" x True

-- | Passes if the argument is 'False', and otherwise fails with the given message.
--
-- > Expect.false "Expected the list not to be empty." (List.isEmpty [ 42 ])
-- >
-- > -- Passes because (List.isEmpty [ 42 ]) is False
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (List.isEmpty []) is True
-- > List.isEmpty []
-- >     |> Expect.false "Expected the list not to be empty."
-- >
-- > {-
-- >
-- > Expected the list not to be empty.
-- >
-- > -}
false :: Bool -> Expectation
false x = assert xor "Expect.false" x True

-- | Passes if each of the given functions passes when applied to the subject.
--
-- Passing an empty list is assumed to be a mistake, so Expect.all [] will always return a failed expectation no matter what else it is passed.
--
-- > Expect.all
-- >     [ Expect.greaterThan -2
-- >     , Expect.lessThan 5
-- >     ]
-- >     (List.length [])
-- > -- Passes because (0 > -2) is True and (0 < 5) is also True
--
-- Failures resemble code written in pipeline style, so you can tell which argument is which:
--
-- > -- Fails because (0 < -10) is False
-- > List.length []
-- >     |> Expect.all
-- >         [ Expect.greaterThan -2
-- >         , Expect.lessThan -10
-- >         , Expect.equal 0
-- >         ]
-- > {-
-- > 0
-- > ╷
-- > │ Expect.lessThan
-- > ╵
-- > -10
-- > -}
all :: List (subject -> Expectation) -> subject -> Expectation
all expectations subject =
  List.foldl
    ( \expectation acc ->
        Internal.append
          acc
          (expectation subject)
    )
    pass
    expectations

-- | Combine multiple expectations into one. The resulting expectation is a
-- failure if any of the original expectations are a failure.
concat :: List Expectation -> Expectation
concat expectations =
  List.foldl
    ( \expectation acc ->
        Internal.append
          acc
          expectation
    )
    pass
    expectations

-- | Passes if the Result is an Ok rather than Err. This is useful for tests where you expect not to see an error, but you don't care what the actual result is.
--
-- (Tip: If your function returns a Maybe instead, consider Expect.notEqual Nothing.)
--
-- > -- Passes
-- > String.toInt "not an int"
-- >     |> Expect.err
--
-- Test failures will be printed with the unexpected Ok value contrasting with any Err.
--
-- > -- Fails
-- > String.toInt "20"
-- >     |> Expect.err
-- >
-- > {-
-- >
-- > Ok 20
-- > ╷
-- > │ Expect.err
-- > ╵
-- > Err _
-- >
-- > -}
ok :: Show b => Result b a -> Expectation
ok res =
  case res of
    Ok _ -> pass
    Err message -> fail ("I expected a Ok but got Err (" ++ Debug.toString message ++ ")")

-- | Passes if the Result is an Err rather than Ok. This is useful for tests where you expect to get an error but you don't care what the actual error is.
--
-- (Tip: If your function returns a Maybe instead, consider Expect.equal Nothing.)
--
-- > -- Passes
-- > String.toInt "not an int"
-- >     |> Expect.err
--
-- Test failures will be printed with the unexpected Ok value contrasting with any Err.
--
-- > -- Fails
-- > String.toInt "20"
-- >     |> Expect.err
-- >
-- > {-
-- >
-- > Ok 20
-- > ╷
-- > │ Expect.err
-- > ╵
-- > Err _
-- >
-- > -}
err :: Show a => Result b a -> Expectation
err res =
  case res of
    Ok value -> fail ("I expected a Err but got Ok (" ++ Debug.toString value ++ ")")
    Err _ -> pass

-- | Check if a string is equal to the contents of a file.
--
-- > Debug.toString complicatedObject
-- >     |> Expect.equalToContentsOf "golden-results/complicated-object.txt"
--
-- If the file does not exist it will be created and the test will pass.
-- Subsequent runs will check the test output matches the now existing file.
--
-- This can be useful when checking big strings, like for example JSON
-- encodings. When a test fails we can throw away the file, rerun the test, and
-- use @git diff golden-results/complicated-object.txt@ to check whether the
-- changes are acceptable.
equalToContentsOf :: Text -> Text -> Expectation
equalToContentsOf filepath' actual = do
  let filepath = Data.Text.unpack filepath'
  exists <-
    fromIO <| do
      Directory.createDirectoryIfMissing True (FilePath.takeDirectory filepath)
      Directory.doesFileExist filepath
  if exists
    then do
      expected <- fromIO (Data.Text.IO.readFile filepath)
      assert
        (==)
        "Expect.equalToContentsOf"
        (UnescapedShow expected)
        (UnescapedShow actual)
    else do
      fromIO (Data.Text.IO.writeFile filepath actual)
      pass

-- By default we will compare values with each other after they have been
-- passed to @show@. Unfortunately @show@ for the @Text@ type escapes special
-- characters, so a string like this:
--
--    Hi there,
--    newline!
--
-- Is rendered in test output as this:
--
--    \"Hi there,\nnewline!\"
--
-- And then test output looks all garbled.
--
-- This newtype wrapper for @Text@ makes the show instance render it without
-- escaping any character, resulting in cleaner test output!
newtype UnescapedShow = UnescapedShow Text deriving (Eq)

instance Show UnescapedShow where
  show (UnescapedShow text) = Data.Text.unpack text

assert :: Show a => (a -> a -> Bool) -> Text -> a -> a -> Expectation
assert pred funcName actual expected =
  if pred actual expected
    then pass
    else do
      window <- fromIO Terminal.size
      let terminalWidth = case window of
            Just Terminal.Window {Terminal.width} -> width - 4 -- indentation
            Nothing -> 80
      let expectedText = Data.Text.pack (Text.Show.Pretty.ppShow expected)
      let actualText = Data.Text.pack (Text.Show.Pretty.ppShow actual)
      let numLines text = List.length (Data.Text.lines text)
      Diff.pretty
        Diff.Config
          { Diff.separatorText = Just funcName,
            Diff.wrapping = Diff.Wrap terminalWidth,
            Diff.multilineContext =
              if numLines expectedText < 6 && numLines actualText < 6
                then Diff.FullContext
                else Diff.Surrounding 2 "..."
          }
        expectedText
        actualText
        |> fail

fromIO :: Prelude.IO a -> Internal.Expectation' a
fromIO io =
  Platform.Internal.Task (\_ -> map Ok io)
    |> Internal.Expectation

-- | Used for making matchers
-- expectOneItem :: Expectation' [a] -> Expectation' a
-- expectOneItem t = do
--   xs <- t
--   case xs of
--     [x] -> Ok x
--     _ -> Err ("Expected one item, but got " ++ Debug.toString (List.length xs) ++ ".")
--   |> Expect.fromResult
fromResult :: Show b => Result b a -> Internal.Expectation' a
fromResult (Ok a) = Prelude.pure a
fromResult (Err msg) =
  msg
    |> Debug.toString
    |> Internal.FailedAssertion
    |> Task.fail
    |> Internal.Expectation

-- | Check a task returns an expected value, than pass that value on.
--
-- > test "Greetings are friendly" <| \_ -> do
-- >     getGreeting
-- >         |> andCheck (Expect.equal "Hi!")
andCheck :: Show err => (a -> Expectation) -> Task err a -> Internal.Expectation' a
andCheck expectation task = do
  x <- succeeds task
  expectation x
  Prelude.pure x

-- | Check a task succeeds.
--
-- > test "solve rubicskube" <| \_ -> do
-- >     solveRubicsKube
-- >         |> succeeds
succeeds :: Show err => Task err a -> Internal.Expectation' a
succeeds task =
  Task.mapError
    ( \message ->
        Internal.FailedAssertion (Debug.toString message)
    )
    task
    |> Internal.Expectation

-- | Check a task fails.
--
-- > test "chemistry experiment" <| \_ -> do
-- >     mixRedAndGreenLiquids
-- >         |> fails
fails :: Show a => Task err a -> Internal.Expectation' err
fails task =
  task
    |> Task.map (\succ -> Err ("Expected failure but succeeded with " ++ Debug.toString succ))
    |> Task.onError (\err' -> Task.succeed (Ok err'))
    |> Task.andThen (Internal.unExpectation << fromResult)
    |> Internal.Expectation
