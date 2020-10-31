-- | A library to create @Expecation@s for @Task@s.
module Expect.Task
  ( check,
    andCheck,
    succeeds,
    fails,
    Failure,
  )
where

import qualified Debug
import qualified Expect
import NriPrelude
import qualified Task
import qualified Test.Internal as Internal

-- | Error generated when a test expectation is not met.
type Failure = Internal.Failure

-- | Check a task returns an expected value, than pass that value on.
--
-- > task "Greetings are friendly" <| do
-- >     getGreeting
-- >         |> andCheck (Expect.equal "Hi!")
andCheck :: (a -> Expect.Expectation) -> Task Failure a -> Task Failure a
andCheck expectation task = do
  x <- task
  res <-
    expectation x
      |> Internal.unExpectation
      |> Task.mapError never
  case res of
    Internal.Succeeded -> task
    Internal.Failed failure -> Task.fail failure

-- | Check an expectation in the middle of a @do@ block.
--
-- > task "Laundry gets done" <| do
-- >     weightInKgs <- clothesInWasher
-- >     check (weightInKgs |> Expect.atMost 8)
-- >     soapInWasher
-- >     startMachine
check :: Expect.Expectation -> Task Failure ()
check expectation =
  Task.succeed ()
    |> andCheck (\() -> expectation)

-- | Check a task succeeds.
--
-- > task "solve rubicskube" <| do
-- >     solveRubicsKube
-- >         |> succeeds
succeeds :: Show err => Task err a -> Task Failure a
succeeds task =
  Task.mapError
    ( \message ->
        Internal.FailedAssertion (Debug.toString message)
    )
    task

-- | Check a task fails.
--
-- > task "chemistry experiment" <| do
-- >     mixRedAndGreenLiquids
-- >         |> fails
fails :: Text -> Task Failure a
fails msg =
  msg
    |> Internal.FailedAssertion
    |> Task.fail
