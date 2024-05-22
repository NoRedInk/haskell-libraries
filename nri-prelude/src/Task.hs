-- GHC wants us to remove `Err never` branches from case statements, because it
-- knows we'll never end up in those branches. We like them though, because
-- missing such a branch in a case statement looks like a problem and so is
-- distracting.
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- | Tasks make it easy to describe asynchronous operations that may fail, like
-- HTTP requests or writing to a database.
module Task
  ( -- * Tasks
    Task,
    Task',
    perform,
    attempt,

    -- * Chains
    andThen,
    succeed,
    fail,
    sequence,

    -- * Maps
    map,
    map2,
    map3,
    map4,
    map5,
    map6,

    -- * Errors
    onError,
    mapError,

    -- * Special (custom helpers not found in Elm)
    timeout,
    concurrently,
    parallel,
    background,
  )
where

import Basics
import qualified Control.Concurrent.Async as Async
import qualified Internal.Shortcut as Shortcut
import List (List)
import Maybe (Maybe (..))
import Platform.Internal (Task', Task)
import qualified Platform.Internal as Internal
import Result (Result (..))
import qualified System.Timeout
import Prelude (IO)
import qualified Prelude

-- BASICS

-- | Just having a @Task@ does not mean it is done. We must @perform@ the task:
--
-- > import qualified Task
-- > import qualified Platform
-- >
-- > main :: IO
-- > main =
-- >   Task.perform Platform.silentHandler Time.now
perform :: env -> Task' env Never a -> IO a
perform output task =
  let onResult result =
        case result of
          -- If you remove this branch, consider also removing the
          -- -fno-warn-overlapping-patterns warning above.
          Err err -> never err
          Ok x -> x
   in attempt output task
        |> Shortcut.map onResult

-- | This is very similar to perform except it can handle failures!
attempt :: env -> Task' env x a -> IO (Result x a)
attempt output task =
  let onResult result =
        Prelude.pure result
   in Internal._run task output |> Shortcut.andThen onResult

-- | A task that succeeds immediately when run. It is usually used with
-- @andThen@. You can use it like @map@ if you want:
--
-- > import qualified Time
-- >
-- > timeInMillis : Task x Int
-- > timeInMillis =
-- >   Time.now
-- >     |> andThen (\t -> succeed (Time.posixToMillis t))
succeed :: a -> Task' env x a
succeed a =
  Internal.Task <| \_ -> Prelude.pure (Ok a)

-- | A task that fails immediately when run. Like with @succeed@, this can be
-- used with @andThen@ to check on the outcome of another task.
--
-- > type Error = NotFound
-- >
-- > notFound : Task Error a
-- > notFound =
-- >   fail NotFound
fail :: x -> Task' env x a
fail x =
  Internal.Task <| \_ -> Prelude.pure (Err x)

-- MAPS

-- | Transform a task. Maybe you want to figure out what time it will be in one
-- hour:
--
-- > import Task exposing (Task)
-- > import qualified Time
-- >
-- > timeInOneHour : Task x Time.Posix
-- > timeInOneHour =
-- >   Task.map addAnHour Time.now
-- >
-- > addAnHour : Time.Posix -> Time.Posix
-- > addAnHour time =
-- >   Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)
map :: (a -> b) -> Task' env x a -> Task' env x b
map =
  Shortcut.map

-- | Put the results of two tasks together. For example, if we wanted to know
-- the current month, we could ask:
--
--  >  import qualified Task exposing (Task)
--  >  import qualified Time
--  >
--  >  getMonth : Task x Int
--  >  getMonth =
--  >    Task.map2 Time.toMonth Time.here Time.now
--
-- __Note:__ Say we were doing HTTP requests instead. @map2@ does each task in
-- order, so it would try the first request and only continue after it succeeds.
-- If it fails, the whole thing fails!
map2 :: (a -> b -> result) -> Task' env x a -> Task' env x b -> Task' env x result
map2 =
  Shortcut.map2

map3 :: (a -> b -> c -> result) -> Task' env x a -> Task' env x b -> Task' env x c -> Task' env x result
map3 =
  Shortcut.map3

map4 :: (a -> b -> c -> d -> result) -> Task' env x a -> Task' env x b -> Task' env x c -> Task' env x d -> Task' env x result
map4 =
  Shortcut.map4

map5 :: (a -> b -> c -> d -> e -> result) -> Task' env x a -> Task' env x b -> Task' env x c -> Task' env x d -> Task' env x e -> Task' env x result
map5 =
  Shortcut.map5

map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task' env x a -> Task' env x b -> Task' env x c -> Task' env x d -> Task' env x e -> Task' env x f -> Task' env x result
map6 =
  Shortcut.map6

-- | Chain together a task and a callback. The first task will run, and if it is
-- successful, you give the result to the callback resulting in another task. This
-- task then gets run. We could use this to make a task that resolves an hour from
-- now:
--
-- > import qualified Time
-- > import qualified Process
-- >
-- > timeInOneHour : Task' env x Time.Posix
-- > timeInOneHour =
-- >   Process.sleep (60 * 60 * 1000)
-- >     |> andThen (\_ -> Time.now)
--
-- First the process sleeps for an hour __and then__ it tells us what time it is.
andThen :: (a -> Task' env x b) -> Task' env x a -> Task' env x b
andThen =
  Shortcut.andThen

-- | Start with a list of tasks, and turn them into a single task that returns a
-- list. The tasks will be run in order one-by-one and if any task fails the whole
-- sequence fails.
--
-- > sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
sequence :: List (Task' env x a) -> Task' env x (List a)
sequence = Prelude.sequence

-- | Start with a list of tasks, and turn them into a single task that returns a
-- list. The tasks will be run in parallel and if any task fails the whole
-- parallel call fails.
--
-- > parallel [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
parallel :: List (Task' env x a) -> Task' env x (List a)
parallel tasks =
  Internal.Task
    ( \handler ->
        Async.forConcurrently tasks (\task -> Internal._run task handler)
          |> Shortcut.map Prelude.sequence
    )

-- | Given two tasks, turn them into a single task that returns a tuple.
-- The tasks will be run concurrently and if either task fails the combined
-- task also fails.
--
-- > concurrently (succeed 1) (succeed "Expecto Patronum!") == succeed (1, "Expecto Patronum!")
concurrently :: Task' env x a -> Task' env x b -> Task' env x (a, b)
concurrently taskA taskB =
  Internal.Task
    ( \handler -> do
        (resultA, resultB) <- Async.concurrently (Internal._run taskA handler) (Internal._run taskB handler)
        Prelude.pure <| Shortcut.map2 (,) resultA resultB
    )

-- | Given a task, execute the task in a greenthread.
background :: Task' env Never a -> Task' env x ()
background task =
  Internal.Task
    ( \handler -> do
        _ <- Async.async (Internal._run task handler)
        Prelude.pure <| Ok ()
    )

-- | Recover from a failure in a task. If the given task fails, we use the
-- callback to recover.
--
-- > fail "file not found"
-- >   |> onError (\msg -> succeed 42)
-- >   -- succeed 42
-- >
-- > succeed 9
-- >   |> onError (\msg -> succeed 42)
-- >   -- succeed 9
onError :: (x -> Task' env y a) -> Task' env x a -> Task' env y a
onError func task =
  Internal.Task <| \key ->
    let onResult result =
          case result of
            Ok ok -> Prelude.pure (Ok ok)
            Err err -> Internal._run (func err) key
     in Internal._run task key
          |> Shortcut.andThen onResult

-- | Transform the error value. This can be useful if you need a bunch of error
-- types to match up.
--
-- > type Error
-- >   = Http Http.Error
-- >   | WebGL WebGL.Error
-- >
-- > getResources : Task Error Resource
-- > getResources =
-- >   sequence
-- >     [ mapError Http serverTask
-- >     , mapError WebGL textureTask
-- >     ]
mapError :: (x -> y) -> Task' env x a -> Task' env y a
mapError func task =
  task |> onError (fail << func)

-- | Run a task. If it doesn't complete within the given number of milliseconds
-- then fail it with the provided error.
--
-- > Process.sleep 2000
-- >   |> timeout 1000 "overslept!"
timeout :: Float -> err -> Task' env err a -> Task' env err a
timeout duration err task =
  Internal.Task
    ( \handler -> do
        maybeResult <-
          System.Timeout.timeout
            (Prelude.round (1000 * duration))
            (Internal._run task handler)
        case maybeResult of
          Just result -> Prelude.pure result
          Nothing -> Prelude.pure (Err err)
    )
