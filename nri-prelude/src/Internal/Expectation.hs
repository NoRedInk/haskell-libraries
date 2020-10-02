module Internal.Expectation
  ( Expectation,
    pass,
    fail,
    join,
    build,
    toResult,
    fromResult,
    fromIO,
    onFail,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Internal.TestResult as TestResult
import Internal.TestResult (TestResult)
import NriPrelude
import Prelude (Applicative, Functor, IO, Monad, Show, pure)

newtype Expectation a = Expectation (IO a)
  deriving (Functor, Applicative, Monad)

fromIO :: IO a -> Expectation a
fromIO = Expectation << liftIO

join :: Expectation TestResult -> Expectation TestResult -> Expectation TestResult
join (Expectation x) (Expectation y) = Expectation (map2 TestResult.join x y)

pass :: Expectation TestResult
pass = Expectation (pure TestResult.passed)

fail :: Text -> Expectation TestResult
fail = Expectation << pure << TestResult.failed

onFail :: Text -> Expectation TestResult -> Expectation TestResult
onFail message =
  fmap (TestResult.onFail message)

build :: Show a => (a -> a -> Bool) -> Text -> a -> a -> Expectation TestResult
build pred funcName x y =
  TestResult.fromPredicate pred funcName x y
    |> Expectation

toResult :: Expectation TestResult -> IO TestResult
toResult (Expectation r) = r

fromResult :: TestResult.TestFailure -> Expectation TestResult
fromResult r = Expectation (pure (TestResult.Failed r))
