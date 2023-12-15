module Process
  ( Id,
    spawn,
    sleep,
    kill,
  )
where

import Basics
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Internal.Shortcut
import qualified Platform.Internal as Internal
import qualified Result
import Task (Task)
import qualified Prelude

-- | A light-weight process that runs concurrently. You can use @spawn@ to get
-- a bunch of different tasks running in different processes. The Elm Haskell
-- will interleave their progress. So if a task is taking too long, we will
-- pause it at an @andThen@ and switch over to other stuff.
--
-- __Note:__ We make a distinction between concurrency which means interleaving
-- different sequences and parallelism which means running different sequences
-- at the exact same time. For example, a time-sharing system is definitely
-- concurrent, but not necessarily parallel.
newtype Id = Id (Async.Async ())

-- | Run a task in its own light-weight process. In the following example,
-- @task1@ and @task2@ will be interleaved. If @task1@ makes a long HTTP
-- request or is just taking a long time, we can hop over to @task2@ and do
-- some work there.
--
-- > spawn task1
-- >     |> Task.andThen (\_ -> spawn task2)
--
-- __Note:__ This creates a relatively restricted kind of Process because it
-- cannot receive any messages. More flexibility for user-defined processes
-- will come in a later release!
spawn :: Task x a -> Task y Id
spawn (Internal.Task f) =
  Internal.Task
    ( \handler ->
        f handler
          |> Async.async
          |> map (Result.Ok << Id << map (\_ -> ()))
    )

-- | Block progress on the current process for the given number of
-- milliseconds. The JavaScript equivalent of this is @setTimeout@ which lets
-- you delay work until later.
sleep :: Float -> Task x ()
sleep delay = Internal.Task (\_ -> map Result.Ok (Control.Concurrent.threadDelay (Prelude.floor (delay * 1000))))

-- | Sometimes you @spawn@ a process, but later decide it would be a waste to
-- have it keep running and doing stuff. The @kill@ function will force a
-- process to bail on whatever task it is running. So if there is an HTTP
-- request in flight, it will also abort the request.
kill :: Id -> Task x ()
kill (Id async) = Internal.Task (\_ -> map Result.Ok (Async.cancel async))
