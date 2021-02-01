{-# OPTIONS_GHC -fno-cse #-}

module MySQL.Test (test, inTestTransaction) where

import qualified Control.Concurrent.MVar as MVar
import qualified Environment
import qualified Expect.Task
import qualified GHC.Stack as Stack
import MySQL.Internal
import qualified MySQL.Settings as Settings
import NriPrelude
import qualified Platform
import qualified System.IO.Unsafe
import qualified Test
import qualified Prelude

-- | A variant of `Test.task` that is passed a MySQL connection, for doing tests
-- that require access to MySQL. The test body is ran without a transaction that
-- gets rolled back after the test completes.
--
-- Usage:
--
--     MySQL.Test.test "My MySQL test" <| \mysql -> do
--        -- test stuff!
test ::
  Stack.HasCallStack =>
  Text ->
  (Connection -> Task Expect.Task.Failure a) ->
  Test.Test
test description body =
  Stack.withFrozenCallStack Test.task description <| do
    conn <- getTestConnection
    inTestTransaction conn body

-- Obtain a MySQL connection for use in tests.
getTestConnection :: Task e Connection
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
              acquire
                settings
                  { Settings.mysqlPool =
                      Settings.defaultPoolSettings
                        { Settings.mysqlPoolSize = Settings.MysqlPoolSize 1
                        },
                    Settings.mysqlConnection =
                      Settings.defaultConnectionSettings
                        { Settings.database = Settings.Database "noredink_test",
                          Settings.connection =
                            Settings.ConnectTcp
                              (Settings.Host "127.0.0.1")
                              (Settings.Port 3306)
                        }
                  }
        Prelude.pure (Just conn, conn)
    )
    |> map Ok
    |> Platform.doAnything testDoAnything

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction conn' func =
  withTransaction conn' <| \conn ->
    Platform.bracketWithError
      (do rollbackAll conn; begin conn)
      (\_ () -> rollbackAll conn)
      (\() -> func conn)

-- | Rollback all active 'begin's.
rollbackAll :: Connection -> Task e ()
rollbackAll conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> Prelude.pure ()
      Just _ -> executeCommand_ conn (queryFromText "ROLLBACK")

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
testConnectionVar :: MVar.MVar (Maybe Connection)
testConnectionVar = System.IO.Unsafe.unsafePerformIO (MVar.newMVar Nothing)

-- | Creates a unpacked `DoAnythingHandler`, allowing us to use it without
-- to turn `IO` into `Task` types without needing to pass it in as an argument,
-- in the context of this test helper.
{-# NOINLINE testDoAnything #-}
testDoAnything :: Platform.DoAnythingHandler
testDoAnything = System.IO.Unsafe.unsafePerformIO Platform.doAnythingHandler
