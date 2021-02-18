{-# LANGUAGE ExtendedDefaultRules #-}

-- | A library to create @Expecation@s for @Task@s.
module Expect.Task
  ( andCheck,
    succeeds,
    fails,
    fromResult,
  )
where

import qualified Debug
import qualified Expect
import NriPrelude
import qualified Task
import qualified Test.Internal as Internal
import qualified Prelude

-- | Check a task returns an expected value, than pass that value on.
--
-- > task "Greetings are friendly" <| do
-- >     getGreeting
-- >         |> andCheck (Expect.equal "Hi!")
andCheck :: Show err => (a -> Expect.Expectation) -> Task err a -> Internal.Expectation' a
andCheck expectation task = do
  x <- succeeds task
  expectation x
  Prelude.pure x

-- | Check a task succeeds.
--
-- > task "solve rubicskube" <| do
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
-- > task "chemistry experiment" <| do
-- >     mixRedAndGreenLiquids
-- >         |> fails
fails :: Show a => Task err a -> Internal.Expectation' err
fails task =
  task
    |> Task.map (\succ -> Err ("Expected failure but succeeded with " ++ Debug.toString succ))
    |> Task.onError (\err -> Task.succeed (Ok err))
    |> Task.andThen (Internal.unExpectation << fromResult)
    |> Internal.Expectation

failWith :: Show b => b -> Internal.Expectation' a
failWith msg =
  msg
    |> Debug.toString
    |> Internal.FailedAssertion
    |> Task.fail
    |> Internal.Expectation

succeedWith :: a -> Internal.Expectation' a
succeedWith payload =
  Task.succeed payload
    |> Task.mapError (\_ -> ())
    |> succeeds

-- | Used for making matchers
-- expectOneItem :: Expectation' [a] -> Expectation' a
-- expectOneItem t = do
--   xs <- t
--   case xs of
--     [x] -> Ok x
--     _ -> Err ("Expected one item, but got " ++ Debug.toString (List.length xs) ++ ".")
--   |> Expect.Task.fromResult
fromResult :: Show b => Result b a -> Internal.Expectation' a
fromResult (Ok a) = succeedWith a
fromResult (Err msg) = failWith msg
