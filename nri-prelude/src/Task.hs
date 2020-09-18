module Task
  ( -- * Tasks
    -- Tasks make it easy to describe asynchronous operations that may fail, like
    -- HTTP requests or writing to a database.
    Task,
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
    parallel,
  )
where

import Basics
import qualified Control.Concurrent.Async as Async
import qualified Internal.Shortcut as Shortcut
import List (List)
import qualified List
import Maybe (Maybe (..))
import qualified Platform.Internal as Internal
import Result (Result (..))
import qualified System.Timeout
import Prelude (IO)
import qualified Prelude

-- | A task is a _description_ of what you need to do. Like a todo
-- list. Or like a grocery list. Or like GitHub issues. So saying "the task is
-- to tell me the current POSIX time" does not complete the task! You need
-- [`perform`](#perform) tasks or [`attempt`](#attempt) tasks.
type Task x a =
  Internal.Task x a

-- BASICS

-- | Just having a `Task` does not mean it is done. We must `perform` the task:
--
--  >  import Cherry.Task
--  >
--  >  main :: IO
--  >  main =
--  >    Task.perform Log.none Time.now
perform :: Internal.Handler -> Task Never a -> IO a
perform output task =
  let onResult result =
        case result of
          Err err -> never err
          Ok x -> x
   in attempt output task
        |> Shortcut.map onResult

-- | Just having a `Task` does not mean it is done. We must `attempt` the task:
--
--  >  import Cherry.Task
--  >
--  >  main :: IO
--  >  main =
--  >    Task.attempt Log.none Time.now
attempt :: Internal.Handler -> Task x a -> IO (Result x a)
attempt output task =
  let onResult result =
        Prelude.pure result
   in Internal._run task output |> Shortcut.andThen onResult

-- | A task that succeeds immediately when run. It is usually used with
-- [`andThen`](#andThen). You can use it like `map` if you want:
--
--  >  import Time
--  >
--  >  timeInMillis : Task x Int
--  >  timeInMillis =
--  >    Time.now
--  >      |> andThen (\t -> succeed (Time.posixToMillis t))
succeed :: a -> Task x a
succeed a =
  Internal.Task <| \_ -> Prelude.pure (Ok a)

-- | A task that fails immediately when run. Like with `succeed`, this can be
-- used with `andThen` to check on the outcome of another task.
--
--  >  type Error = NotFound
--  >
--  >  notFound : Task Error a
--  >  notFound =
--  >    fail NotFound
fail :: x -> Task x a
fail x =
  Internal.Task <| \_ -> Prelude.pure (Err x)

-- MAPS

-- | Transform a task. Maybe you want to use [`elm/time`][time] to figure
-- out what time it will be in one hour:
--
--  >  import Task exposing (Task)
--  >  import Time -- elm install elm/time
--  >
--  >  timeInOneHour : Task x Time.Posix
--  >  timeInOneHour =
--  >    Task.map addAnHour Time.now
--  >
--  >  addAnHour : Time.Posix -> Time.Posix
--  >  addAnHour time =
--  >    Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)
--
-- [time]: /packages/elm/time/latest/
map :: (a -> b) -> Task x a -> Task x b
map =
  Shortcut.map

-- | Put the results of two tasks together. For example, if we wanted to know
-- the current month, we could use [`elm/time`][time] to ask:
--
--  >  import Task exposing (Task)
--  >  import Time -- elm install elm/time
--  >
--  >  getMonth : Task x Int
--  >  getMonth =
--  >    Task.map2 Time.toMonth Time.here Time.now
--
-- **Note:** Say we were doing HTTP requests instead. `map2` does each task in
-- order, so it would try the first request and only continue after it succeeds.
-- If it fails, the whole thing fails!
--
-- [time]: /packages/elm/time/latest/
map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 =
  Shortcut.map2

-- |
map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 =
  Shortcut.map3

-- |
map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 =
  Shortcut.map4

-- |
map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 =
  Shortcut.map5

-- |
map6 :: (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 =
  Shortcut.map6

-- | Chain together a task and a callback. The first task will run, and if it is
-- successful, you give the result to the callback resulting in another task. This
-- task then gets run. We could use this to make a task that resolves an hour from
-- now:
--
--  >  import Time -- elm install elm/time
--  >  import Process
--  >
--  >  timeInOneHour : Task x Time.Posix
--  >  timeInOneHour =
--  >    Process.sleep (60 * 60 * 1000)
--  >      |> andThen (\_ -> Time.now)
--
-- First the process sleeps for an hour **and then** it tells us what time it is.
andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen =
  Shortcut.andThen

-- | Start with a list of tasks, and turn them into a single task that returns a
-- list. The tasks will be run in order one-by-one and if any task fails the whole
-- sequence fails.
--
--  >  sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
sequence :: List (Task x a) -> Task x (List a)
sequence tasks =
  List.foldr (Shortcut.map2 (:)) (succeed []) tasks

-- | Start with a list of tasks, and turn them into a single task that returns a
-- list. The tasks will be run in parallel and if any task fails the whole
-- parallel call fails.
--
--  >  parallel [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]
parallel :: List (Task x a) -> Task x (List a)
parallel tasks =
  Internal.Task
    ( \handler ->
        Async.forConcurrently tasks (\task -> Internal._run task handler)
          |> Shortcut.map Prelude.sequence
    )

-- | Recover from a failure in a task. If the given task fails, we use the
-- callback to recover.
--
--  >  fail "file not found"
--  >    |> onError (\msg -> succeed 42)
--  >    -- succeed 42
--  >
--  >  succeed 9
--  >    |> onError (\msg -> succeed 42)
--  >    -- succeed 9
onError :: (x -> Task y a) -> Task x a -> Task y a
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
--  >  type Error
--  >    = Http Http.Error
--  >    | WebGL WebGL.Error
--  >
--  >  getResources : Task Error Resource
--  >  getResources =
--  >    sequence
--  >      [ mapError Http serverTask
--  >      , mapError WebGL textureTask
--  >      ]
mapError :: (x -> y) -> Task x a -> Task y a
mapError func task =
  task |> onError (fail << func)

-- | Run a task. If it doesn't complete within the given number of milliseconds
-- then fail it with the provided error.
--
--    Process.sleep 2000
--      |> timeout 1000 "overslept!"
timeout :: Float -> err -> Task err a -> Task err a
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
