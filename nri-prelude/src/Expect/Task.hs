-- | A library to create @Expecation@s for @Task@s.
module Expect.Task
  ( check,
    andCheck,
    succeeds,
    fails,
    fromResult,
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
    Internal.Succeeded -> Task.succeed x
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
fails :: Show a => Task err a -> Task Failure err
fails task =
  task
    |> Task.map (\succ -> Err ("Expected failure but succeeded with " ++ Debug.toString succ))
    |> Task.onError (\err -> Task.succeed (Ok err))
    |> Task.andThen fromResult

failWith :: Show b => b -> Task Failure a
failWith msg =
  msg
    |> Debug.toString
    |> Internal.FailedAssertion
    |> Task.fail

succeedWith :: a -> Task Failure a
succeedWith payload =
  Task.succeed payload
    |> Task.mapError (\_ -> ())
    |> succeeds

-- | Used for making matchers
-- expectOneItem :: Task Expect.Task.Failure [a] -> Task Expect.Task.Failure a
-- expectOneItem t = do
--   xs <- t
--   case xs of
--     [x] -> Ok x
--     _ -> Err ("Expected one item, but got " ++ Debug.toString (List.length xs) ++ ".")
--   |> Expect.Task.fromResult
fromResult :: Show b => Result b a -> Task Failure a
fromResult (Ok a) = succeedWith a
fromResult (Err msg) = failWith msg
