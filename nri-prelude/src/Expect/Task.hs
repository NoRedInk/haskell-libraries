module Expect.Task
  ( check,
    andCheck,
    succeeds,
    fail,
    TestFailure,
  )
where

import NriPrelude
import qualified Debug
import qualified Expect
import qualified Internal.Expectation
import qualified Internal.TestResult
import Internal.TestResult (TestFailure)
import qualified Platform
import qualified Platform.DoAnything as DoAnything
import qualified Task

andCheck :: (a -> Expect.Expectation) -> Task TestFailure a -> Task TestFailure a
andCheck expectation t = do
  x <- t
  expectation x
    |> Internal.Expectation.toResult
    |> map Ok
    |> Platform.doAnything DoAnything.Handler
    |> Task.andThen
      ( \res -> case res of
          Internal.TestResult.Passed -> Task.succeed ()
          Internal.TestResult.Skipped -> Task.succeed ()
          Internal.TestResult.Failed message -> Task.fail message
      )
  Task.succeed x

check :: Expect.Expectation -> Task TestFailure ()
check expectation =
  Task.succeed ()
    |> andCheck (\() -> expectation)

succeeds :: Show err => Task err a -> Task TestFailure a
succeeds =
  Task.mapError
    ( \message ->
        Internal.TestResult.TestFailure (Debug.toString message)
    )

fail :: Text -> Task TestFailure a
fail =
  Task.fail << Internal.TestResult.TestFailure << Debug.toString
