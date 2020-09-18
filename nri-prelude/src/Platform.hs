-- This module is for running applications we build using this library, and for
-- integrating external Haskell libraries into our code. You normally shouldn't
-- need to use this module, unless you're building a library or creating a
-- wrapper for an existing Haskell library.
module Platform
  ( -- * Turning a `IO` type into a `Task`.
    DoAnythingHandler,
    doAnythingHandler,
    doAnything,

    -- * Working with the log handler
    LogHandler,
    logHandler,
    requestId,
    silentHandler,

    -- * Creating custom tracingSpans in libraries
    Internal.tracingSpan,
    Internal.tracingSpanIO,
    Internal.rootTracingSpanIO,
    Internal.setTracingSpanDetails,
    Internal.setTracingSpanDetailsIO,
    Internal.markTracingSpanFailed,
    Internal.markTracingSpanFailedIO,

    -- * Interpreting tracingSpans for reporting to monitoring platforms
    Internal.TracingSpan (..),
    Internal.Succeeded (..),
    Internal.TracingSpanDetails (..),
    Internal.SomeTracingSpanDetails,
    Internal.Renderer (Renderer),
    Internal.renderTracingSpanDetails,
    Internal.MonotonicTime,
    Internal.inMicroseconds,

    -- * Ensuring cleanup logic gets ran in case of exceptions.
    bracketWithError,
    finally,

    -- * Exception throwing, in rare cases we need it.
    unsafeThrowException,
  )
where

import Basics
import NriPrelude
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.Catch as Catch
import qualified Data.Text
import qualified GHC.Stack as Stack
import qualified Platform.DoAnything as DoAnything
import qualified Platform.Internal as Internal
import qualified Task
import Prelude (IO, pure)

-- |
-- A value of this type allows you to turn an `IO` type into a `Task` using the
-- `doAnything` function.
--
-- The intended use for this is creating other handlers for running specific
-- types of effects. Suppose you're creating a library for making queries to
-- a database. You might create a `Handler` type for it like this:
--
--     data Handler = Handler
--        { doAnything :: DoAnythingHandler
--        , host :: Text
--        , port :: Text
--        }
--
-- You create this handler in the root of your application and then pass it to
-- wherever you need to perform database requests. Using the `DoAnythingHandler`
-- available to it your library can perform the query, then wrap the resulting
-- `IO` up in a `Task`.
type DoAnythingHandler = DoAnything.Handler

-- |
-- Get a key that allows you to run arbitrary IO in a `Task`. This key you can
-- then pass to `doAnything`. See the documentation for `DoAnythingHandler`.
doAnythingHandler :: IO DoAnything.Handler
doAnythingHandler = pure DoAnything.Handler

-- |
-- Allow running arbitrary IO in `Task`, but only if you have a license for it.
doAnything :: DoAnything.Handler -> IO (Result e a) -> Task e a
doAnything _ io = Internal.Task (\_ -> io)

-- |
-- `bracket` allows us to acquire a resource (the first argument), use it (the
-- third argument), and release it afterward (the second argument). Critically,
-- the `release` phase always runs, even if the use phase fails with an error.
--
-- `bracket` is defined in the `exceptions` package for all types
-- implementing the `MonadMask` type class. We could acquire it for `Task` by
-- deriving `MonadMask` for it, but this would require us to implement super
-- classes `MonadThrow` and `MonadCatch` for `Task` as well.
--
-- We don't want to implement `MonadThrow` for `Task` because it would allow us
-- to throw exceptions directly in the `IO` monad hidden in `Task`. These types
-- of exceptions disappear from the types: `IO` does not have a type parameter
-- indicating possible errors. We want to ensure our own errors end up in the
-- error argument of the `Task` type, so we don't implement `MonadThrow`.
--
-- The implementation below is mostly taken from the implementation of
-- `generalBracket` for `ExceptT e m a` in the `Control.Monad.Catch` module.
bracketWithError ::
  Task e a ->
  (Internal.Succeeded -> a -> Task e c) ->
  (a -> Task e b) ->
  Task e b
bracketWithError (Internal.Task acquire) release use =
  Internal.Task <| \log -> do
    (eb, ec) <-
      Exception.generalBracket
        (acquire log)
        ( \eresource exitCase ->
            case eresource of
              Err err -> pure (Err err) -- nothing to release, acquire didn't succeed
              Ok resource ->
                case exitCase of
                  Catch.ExitCaseSuccess (Ok _) -> Internal._run (release Internal.Succeeded resource) log
                  _ -> Internal._run (release Internal.Failed resource) log
        )
        ( \result ->
            case result of
              Err err -> pure (Err err)
              Ok x -> Internal._run (use x) log
        )
    pure <| do
      -- The order in which we perform those two 'Either' effects determines
      -- which error will win if they are both 'Left's. We want the error from
      -- 'release' to win.
      _ <- ec
      eb

-- | Ensure some cleanup logic always run, regardless of whether the task it
-- runs after failed with an exception.
--
--     finally
--       doSomeWork
--       (Log.info "Finished doing work." [])
finally :: Task e a -> Task e b -> Task e a
finally run cleanup =
  bracketWithError
    (Task.succeed ())
    (\_ _ -> cleanup)
    (\_ -> run)

-- |
-- Our `Task` type secretly passed a value of this type throughout our
-- application. Anywhere in our application we can add context to the log
-- handler. For example we might wrap our database queries in a tracingSpan
-- called "query" and add some bits of context, such as the SQL operation the
-- query is performing. These bits of metadata will then be used as much as
-- possible in logging messages, tracing, and error reporting.
type LogHandler = Internal.Handler

-- |
-- Access the log handler in a task.
logHandler :: Task e LogHandler
logHandler = Internal.Task (pure << Ok)

-- |
-- Get the ID of the current request.
requestId :: Task e Text
requestId = map Internal.requestId logHandler

silentHandler :: IO LogHandler
silentHandler = Internal.mkHandler "" (Internal.Clock (pure 0)) (\_ -> pure ()) ""

-- |
-- Throw a runtime exception that cannot be caught. This function, like
-- `Debug.todo`, breaks type level guarantees and should be avoided. Where
-- possible use a type like `Result` or `Task` that explicitly handlers errors.
--
-- Some external libraries and API depend on sometimes require us to throw
-- errors. When that is the case prefer this function over different ways to
-- throw an exception in `Control.Exception`, because it results in better logs
-- for those who'll need to investigate these problems.
unsafeThrowException ::
  Stack.HasCallStack =>
  Text ->
  Task e a
unsafeThrowException title =
  Internal.Task
    <| \_ ->
      Exception.throwString (Data.Text.unpack title)
