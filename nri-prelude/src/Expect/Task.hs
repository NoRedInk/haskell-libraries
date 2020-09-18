-- | A library to create @Expecation@s for @Task@s.
module Expect.Task
  ( check,
    andCheck,
    succeeds,
    fails,
    TestFailure,
  )
where

import qualified Debug
import qualified Expect
import qualified Internal.Expectation
import qualified Internal.TestResult
import Internal.TestResult (TestFailure)
import NriPrelude
import qualified Platform
import qualified Platform.DoAnything as DoAnything
import qualified Task

-- | Check a task returns an expected value, than pass that value on.
--
-- > task "Greetings are friendly" <| do
-- >     getGreeting
-- >         |> andCheck (Expect.equal "Hi!")
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

-- | Check an expectation in the middle of a @do@ block.
--
-- > task "Laundry gets done" <| do
-- >     weightInKgs <- clothesInWasher
-- >     check (weightInKgs |> Expect.atMost 8)
-- >     soapInWasher
-- >     startMachine
check :: Expect.Expectation -> Task TestFailure ()
check expectation =
  Task.succeed ()
    |> andCheck (\() -> expectation)

-- | Check a task succeeds.
--
-- > task "solve rubicskube" <| do
-- >     solveRubicsKube
-- >         |> succeeds
succeeds :: Show err => Task err a -> Task TestFailure a
succeeds =
  Task.mapError
    ( \message ->
        Internal.TestResult.TestFailure (Debug.toString message)
    )

-- | Check a task fails.
--
-- > task "chemistry experiment" <| do
-- >     mixRedAndGreenLiquids
-- >         |> fails
fails :: Text -> Task TestFailure a
fails =
  Task.fail << Internal.TestResult.TestFailure << Debug.toString
