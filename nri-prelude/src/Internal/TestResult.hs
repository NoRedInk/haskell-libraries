module Internal.TestResult
  ( TestResult (Passed, Failed, Skipped),
    TestFailure (TestFailure),
    concat,
    join,
    passed,
    failed,
    skipped,
    onFail,
    fromPredicate,
    throwFailingTest,
  )
where

import NriPrelude
import Control.Exception.Safe (Exception, throwIO)
import Data.Dynamic (Typeable)
import qualified Data.Text
import List (List)
import qualified List
import qualified Pretty.Diff as Diff
import qualified System.Console.Terminal.Size as Terminal
import qualified Text
import qualified Text.Show.Pretty
import Prelude (IO, Monoid, Semigroup ((<>)), Show, pure, show)

data TestResult
  = Passed
  | Skipped
  | Failed TestFailure

passed :: TestResult
passed = Passed

failed :: Text -> TestResult
failed = Failed << TestFailure

skipped :: TestResult
skipped = Skipped

onFail :: Text -> TestResult -> TestResult
onFail message result =
  case result of
    Passed -> result
    Skipped -> result
    Failed _ -> failed message

concat :: List TestResult -> TestResult
concat xs =
  case xs of
    [] -> Passed
    x : rest -> join x (concat rest)

join :: TestResult -> TestResult -> TestResult
join a b =
  case (a, b) of
    (Passed, Passed) -> Passed
    (Passed, Failed x) -> Failed x
    (Passed, Skipped) -> Skipped
    (Failed x, Failed y) -> Failed (x <> y)
    (Failed x, Passed) -> Failed x
    (Failed x, Skipped) -> Failed x
    (Skipped, Skipped) -> Skipped
    (Skipped, Passed) -> Skipped
    (Skipped, Failed x) -> Failed x

fromPredicate :: Show a => (a -> a -> Bool) -> Text -> a -> a -> IO TestResult
fromPredicate pred funcName actual expected =
  if pred actual expected
    then pure Passed
    else do
      window <- Terminal.size
      let terminalWidth = case window of
            Just Terminal.Window {Terminal.width} -> width - 4 -- indentation
            Nothing -> 80
      Diff.pretty
        Diff.Config
          { Diff.separatorText = Just funcName,
            Diff.wrapping = Diff.Wrap terminalWidth
          }
        (PrettyShow expected)
        (PrettyShow actual)
        |> failed
        |> pure

newtype PrettyShow a = PrettyShow a

instance Show a => Show (PrettyShow a) where
  show (PrettyShow x) = Text.Show.Pretty.ppShow x

newtype TestFailure = TestFailure Text
  deriving (Typeable, Semigroup, Monoid)

instance Exception TestFailure

instance Show TestFailure where
  show (TestFailure err) = Data.Text.unpack (indent err)

throwFailingTest :: TestResult -> IO ()
throwFailingTest result =
  case result of
    Passed -> pure ()
    Skipped -> pure ()
    Failed err -> throwIO err

indent :: Text -> Text
indent = Data.Text.lines >> List.map ("    " <>) >> Text.join "\n"
