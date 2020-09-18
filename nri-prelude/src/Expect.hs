module Expect
  ( Expectation,
    equal,
    notEqual,
    lessThan,
    atMost,
    greaterThan,
    atLeast,
    true,
    false,
    all,
    concat,
    pass,
    fail,
    onFail,
    ok,
    err,
    withIO,
    just,
    nothing,
    equalToContentsOf,
  )
where

import NriPrelude
import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug
import qualified Internal.Expectation
import qualified Internal.TestResult
import qualified List
import List (List)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import Prelude (Eq, IO, Ord, Show, show)

type Expectation =
  Internal.Expectation.Expectation Internal.TestResult.TestResult

withIO :: (a -> Expectation) -> IO a -> Expectation
withIO fn io =
  Internal.Expectation.fromIO io |> andThen fn

pass :: Expectation
pass = Internal.Expectation.pass

fail :: Text -> Expectation
fail = Internal.Expectation.fail

onFail :: Text -> Expectation -> Expectation
onFail = Internal.Expectation.onFail

equal :: (Show a, Eq a) => a -> a -> Expectation
equal = Internal.Expectation.build (==) "Expect.equal"

notEqual :: (Show a, Eq a) => a -> a -> Expectation
notEqual = Internal.Expectation.build (/=) "Expect.notEqual"

lessThan :: (Show a, Ord a) => a -> a -> Expectation
lessThan = Internal.Expectation.build (>) "Expect.lessThan"

atMost :: (Show a, Ord a) => a -> a -> Expectation
atMost = Internal.Expectation.build (>=) "Expect.atMost"

greaterThan :: (Show a, Ord a) => a -> a -> Expectation
greaterThan = Internal.Expectation.build (<) "Expect.greaterThan"

atLeast :: (Show a, Ord a) => a -> a -> Expectation
atLeast = Internal.Expectation.build (<=) "Expect.atLeast"

true :: Bool -> Expectation
true x = Internal.Expectation.build (&&) "Expect.true" x True

false :: Bool -> Expectation
false x = Internal.Expectation.build xor "Expect.false" x True

all :: List (subject -> Expectation) -> subject -> Expectation
all expectations subject =
  List.foldl
    ( \expectation acc ->
        Internal.Expectation.join
          acc
          (expectation subject)
    )
    Internal.Expectation.pass
    expectations

concat :: List Expectation -> Expectation
concat expectations =
  List.foldl
    ( \expectation acc ->
        Internal.Expectation.join
          acc
          expectation
    )
    Internal.Expectation.pass
    expectations

ok :: Show b => (a -> Expectation) -> Result b a -> Expectation
ok f res =
  case res of
    Ok value -> f value
    Err message -> fail ("I expected a Ok but got Err (" ++ Debug.toString message ++ ")")

err :: Show a => (b -> Expectation) -> Result b a -> Expectation
err f res =
  case res of
    Ok value -> fail ("I expected a Err but got Ok (" ++ Debug.toString value ++ ")")
    Err message -> f message

just :: (a -> Expectation) -> Maybe a -> Expectation
just f res =
  case res of
    Just value -> f value
    Nothing -> fail "I expected a Just but got Nothing"

nothing :: Show a => Maybe a -> Expectation
nothing res =
  case res of
    Just value -> fail ("I expected a Nothing but got Just (" ++ Debug.toString value ++ ")")
    Nothing -> pass

equalToContentsOf :: Text -> Text -> Expectation
equalToContentsOf filepath' actual = do
  let filepath = Data.Text.unpack filepath'
  exists <- Internal.Expectation.fromIO <| do
    Directory.createDirectoryIfMissing True (FilePath.takeDirectory filepath)
    Directory.doesFileExist filepath
  if exists
    then do
      expected <- Internal.Expectation.fromIO (Data.Text.IO.readFile filepath)
      Internal.Expectation.build
        (==)
        "Expect.equalToContentsOf"
        (UnescapedShow expected)
        (UnescapedShow actual)
    else do
      Internal.Expectation.fromIO (Data.Text.IO.writeFile filepath actual)
      Internal.Expectation.pass

-- By default we will compare values with each other after they have been
-- passed to `show`. Unfortunately `show` for the `Text` type escapes special
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
-- This newtype wrapper for `Text` makes the show instance render it without
-- escaping any character, resulting in cleaner test output!
newtype UnescapedShow = UnescapedShow Text deriving (Eq)

instance Show UnescapedShow where
  show (UnescapedShow text) = Data.Text.unpack text
