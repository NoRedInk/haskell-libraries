module DebugSpec (tests) where

import Control.Exception.Safe (SomeException)
import Control.Exception.Safe as Exception
import qualified Debug
import qualified Expect
import List (head)
import NriPrelude
import Test (Test, describe, test)
import qualified Text
import Prelude (Either (Left, Right), Show)

tests :: Test
tests =
  describe
    "Debug Tests"
    [ describe "toString" toStringTests,
      describe "log" logTests,
      describe "todo" todoTests
    ]

toStringTests :: List Test
toStringTests =
  [ test "returns the show form of an empty String"
      <| \() -> Expect.equal "\"\"" (Debug.toString ("" :: Text)),
    test "returns the show form of an Int"
      <| \() -> Expect.equal "0" (Debug.toString (0 :: Int))
  ]

logTests :: List Test
logTests =
  [ test "returns passed value"
      <| \() -> Expect.equal 3.14 (3.14 :: Float)
  ]

todoTests :: List Test
todoTests =
  [ test "that an exception is raised with the given message"
      <| \() ->
        Expect.withIO
          ( \result -> case result of
              Left (exception :: SomeException) -> Expect.equal (Just "Not yet!") (firstLine exception)
              Right _home -> Expect.fail "No exception raised"
          )
          (Exception.try (Debug.todo ("Not yet!" :: Text)))
  ]

-- | Extracts the first line of a given text string if it exists. Otherwise returns Nothing.
firstLine :: Show a => a -> Maybe Text
firstLine =
  Debug.toString >> Text.lines >> head
