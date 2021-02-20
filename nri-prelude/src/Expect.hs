{-# LANGUAGE RankNTypes #-}

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
    around,
  )
where

import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug
import qualified GHC.Stack as Stack
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
pass :: Stack.HasCallStack => Expectation
pass = Stack.withFrozenCallStack Internal.pass "Expect.pass" ()

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
fail :: Stack.HasCallStack => Text -> Expectation
fail msg =
  Stack.withFrozenCallStack Internal.failAssertion "Expect.fail" msg

-- | If the given expectation fails, replace its failure message with a custom one.
--
-- > "something"
-- >     |> Expect.equal "something else"
-- >     |> Expect.onFail "thought those two strings would be the same"
onFail :: Stack.HasCallStack => Text -> Expectation -> Expectation
onFail msg (Internal.Expectation task) =
  task
    |> Task.onError
      ( \_ ->
          Stack.withFrozenCallStack Internal.failAssertion "Expect.onFail" msg
            |> Internal.unExpectation
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
equal :: (Stack.HasCallStack, Show a, Eq a) => a -> a -> Expectation
equal = Stack.withFrozenCallStack assert (==) "Expect.equal"

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
notEqual :: (Stack.HasCallStack, Show a, Eq a) => a -> a -> Expectation
notEqual = Stack.withFrozenCallStack assert (/=) "Expect.notEqual"

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
lessThan :: (Stack.HasCallStack, Show a, Ord a) => a -> a -> Expectation
lessThan = Stack.withFrozenCallStack assert (>) "Expect.lessThan"

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
atMost :: (Stack.HasCallStack, Show a, Ord a) => a -> a -> Expectation
atMost = Stack.withFrozenCallStack assert (>=) "Expect.atMost"

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
greaterThan :: (Stack.HasCallStack, Show a, Ord a) => a -> a -> Expectation
greaterThan = Stack.withFrozenCallStack assert (<) "Expect.greaterThan"

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
atLeast :: (Stack.HasCallStack, Show a, Ord a) => a -> a -> Expectation
atLeast = Stack.withFrozenCallStack assert (<=) "Expect.atLeast"

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
true :: Stack.HasCallStack => Bool -> Expectation
true x = Stack.withFrozenCallStack assert (&&) "Expect.true" x True

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
false :: Stack.HasCallStack => Bool -> Expectation
false x = Stack.withFrozenCallStack assert xor "Expect.false" x True

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
all :: Stack.HasCallStack => List (subject -> Expectation) -> subject -> Expectation
all expectations subject =
  List.foldl
    ( \expectation acc ->
        Internal.append
          acc
          (Stack.withFrozenCallStack expectation subject)
    )
    pass
    expectations

-- | Combine multiple expectations into one. The resulting expectation is a
-- failure if any of the original expectations are a failure.
concat :: Stack.HasCallStack => List Expectation -> Expectation
concat expectations =
  List.foldl
    ( \expectation acc ->
        Internal.append
          acc
          (Stack.withFrozenCallStack expectation)
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
ok :: (Stack.HasCallStack, Show b) => Result b a -> Expectation
ok res =
  case res of
    Ok _ ->
      Stack.withFrozenCallStack
        Internal.pass
        "Expect.ok"
        ()
    Err message ->
      Stack.withFrozenCallStack
        Internal.failAssertion
        "Expect.ok"
        ("I expected a Ok but got Err (" ++ Debug.toString message ++ ")")

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
err :: (Stack.HasCallStack, Show a) => Result b a -> Expectation
err res =
  case res of
    Ok value ->
      Stack.withFrozenCallStack
        Internal.failAssertion
        "Expect.err"
        ("I expected a Err but got Ok (" ++ Debug.toString value ++ ")")
    Err _ ->
      Stack.withFrozenCallStack
        Internal.pass
        "Expect.err"
        ()

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
      Stack.withFrozenCallStack
        assert
        (==)
        "Expect.equalToContentsOf"
        (UnescapedShow expected)
        (UnescapedShow actual)
    else do
      fromIO (Data.Text.IO.writeFile filepath actual)
      Stack.withFrozenCallStack
        Internal.pass
        "Expect.equalToContentsOf"
        ()

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

assert :: (Stack.HasCallStack, Show a) => (a -> a -> Bool) -> Text -> a -> a -> Expectation
assert pred funcName actual expected =
  if pred actual expected
    then Stack.withFrozenCallStack Internal.pass funcName ()
    else do
      window <- fromIO Terminal.size
      let terminalWidth = case window of
            Just Terminal.Window {Terminal.width} -> width - 4 -- indentation
            Nothing -> 80
      let expectedText = Data.Text.pack (Text.Show.Pretty.ppShow expected)
      let actualText = Data.Text.pack (Text.Show.Pretty.ppShow actual)
      let numLines text = List.length (Data.Text.lines text)
      Stack.withFrozenCallStack Internal.failAssertion funcName
        <| Diff.pretty
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
fromResult :: (Stack.HasCallStack, Show b) => Result b a -> Internal.Expectation' a
fromResult (Ok a) =
  Stack.withFrozenCallStack
    Internal.pass
    "Expect.fromResult"
    a
fromResult (Err msg) =
  Stack.withFrozenCallStack
    Internal.failAssertion
    "Expect.fromResult"
    (Debug.toString msg)

-- | Check a task returns an expected value.
--
-- > test "Greetings are friendly" <| \_ -> do
-- >     getGreeting
-- >         |> andCheck (Expect.equal "Hi!")
andCheck :: (Stack.HasCallStack, Show err) => (a -> Expectation) -> Task err a -> Internal.Expectation
andCheck expectation task = do
  x <- succeeds task
  Stack.withFrozenCallStack expectation x

-- | Check a task succeeds.
--
-- > test "solve rubicskube" <| \_ -> do
-- >     solveRubicsKube
-- >         |> succeeds
succeeds :: (Stack.HasCallStack, Show err) => Task err a -> Internal.Expectation' a
succeeds task =
  task
    |> Task.onError
      ( \message ->
          Stack.withFrozenCallStack
            Internal.failAssertion
            "Expect.succeeds"
            (Debug.toString message)
            |> Internal.unExpectation
      )
    |> Task.andThen
      ( \a ->
          Stack.withFrozenCallStack
            Internal.pass
            "Expect.succeeds"
            a
            |> Internal.unExpectation
      )
    |> Internal.Expectation

-- | Check a task fails.
--
-- > test "chemistry experiment" <| \_ -> do
-- >     mixRedAndGreenLiquids
-- >         |> fails
fails :: (Stack.HasCallStack, Show a) => Task err a -> Internal.Expectation' err
fails task =
  task
    |> Task.map (\succ -> Err ("Expected failure but succeeded with " ++ Debug.toString succ))
    |> Task.onError (\err' -> Task.succeed (Ok err'))
    |> Task.andThen
      ( \res ->
          Internal.unExpectation
            <| case res of
              Ok a ->
                Stack.withFrozenCallStack
                  Internal.pass
                  "Expect.fails"
                  a
              Err msg ->
                Stack.withFrozenCallStack
                  Internal.failAssertion
                  "Expect.fails"
                  (Debug.toString msg)
      )
    |> Internal.Expectation

-- | This can be used to create custom test functions that contain some setup
-- and teardown logic, for example to make tests run in a database transaction
-- that gets rolled back afterwards.
--
--     dbTest ::
--       Stack.HasCallStack =>
--       Text ->
--       (Db.Connection -> Expect.Expectation) ->
--       Test.Test
--     dbTest description body =
--       Stack.withFrozenCallStack Test.test description <| \_ -> do
--         Expect.around
--           ( \task' -> do
--               conn <- Db.getConnection
--               Platform.finally
--                 (task' conn)
--                 (Db.rollback conn)
--           )
--           body
around ::
  (forall e a. (arg -> Task e a) -> Task e a) ->
  (arg -> Expectation) ->
  Expectation
around runTask runExpectation =
  Internal.Expectation
    ( runTask
        ( \arg ->
            runExpectation arg
              |> Internal.unExpectation
        )
    )
