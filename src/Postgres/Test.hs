{-# OPTIONS_GHC -fno-cse #-}

module Postgres.Test (task) where

import qualified Control.Concurrent.MVar as MVar
import qualified Environment
import qualified Expect.Task
import qualified GHC.Stack as Stack
import qualified Platform
import qualified Postgres
import qualified Postgres.Settings as Settings
import qualified System.IO.Unsafe
import qualified Test
import qualified Prelude

-- | A variant of `Test.task` that is passed a Postgres connection, for doing tests
-- that require access to Postgres. The test body is run within a transaction that
-- gets rolled back after the test completes.
--
-- Usage:
--
--     Postgres.Test.task "My Postgres test" <| \Postgres -> do
--        -- test stuff!
task ::
  Stack.HasCallStack =>
  Text ->
  (Postgres.Connection -> Task Expect.Task.Failure a) ->
  Test.Test
task description body =
  Stack.withFrozenCallStack Test.task description <| do
    conn <- getTestConnection
    Postgres.inTestTransaction conn body

-- Obtain a Postgres connection for use in tests.
getTestConnection :: Task e Postgres.Connection
getTestConnection =
  -- The MVar exists to allow this function to be called by multiple tests
  -- running in parallel, and only the first test calling it will create a
  -- connection pool. The other tests will block until the pool is created, then
  -- share it.
  --
  -- This works because `MVar.modifyMVar` ensures the function we pass it is
  -- not called concurrently.
  MVar.modifyMVar
    testConnectionVar
    ( \maybeConn -> do
        conn <-
          case maybeConn of
            Just conn -> Prelude.pure conn
            Nothing -> do
              settings <- Environment.decode Settings.decoder
              Postgres.connectionIO settings
        Prelude.pure (Just conn, conn)
    )
    |> map Ok
    |> Platform.doAnything testDoAnything

-- | Create a 'global' variable containing the connection we want to use in
-- tests.
--
-- It's not truly global, only functions in this module can access it (because
-- we do not expose it). But it is a bit global in the sense that they'll be
-- able to access this variable without needing to be passed a reference to it
-- from outside.
--
-- The `NOINLINE` is instruction to Haskell not to try be efficient and inline
-- this function in where it's called. If Haskell did that it would result
-- in a new `MVar` being created every time we use `testConnectionVar`, instead
-- of a single `MVar` being shared between all these calls.
{-# NOINLINE testConnectionVar #-}
testConnectionVar :: MVar.MVar (Maybe Postgres.Connection)
testConnectionVar = System.IO.Unsafe.unsafePerformIO (MVar.newMVar Nothing)

-- | Creates a unpacked `DoAnythingHandler`, allowing us to use it without
-- to turn `IO` into `Task` types without needing to pass it in as an argument,
-- in the context of this test helper.
{-# NOINLINE testDoAnything #-}
testDoAnything :: Platform.DoAnythingHandler
testDoAnything = System.IO.Unsafe.unsafePerformIO Platform.doAnythingHandler
