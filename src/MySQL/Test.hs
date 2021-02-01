{-# OPTIONS_GHC -fno-cse #-}

module MySQL.Test (test) where

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

test ::
  Stack.HasCallStack =>
  Text ->
  (Connection -> Task Expect.Task.Failure a) ->
  Test.Test
test description body =
  Stack.withFrozenCallStack Test.task description <| do
    conn <- getTestConnection
    inTestTransaction conn body

getTestConnection :: Task e Connection
getTestConnection =
  MVar.modifyMVar
    testConnectionVar
    ( \maybeConn -> do
        conn <-
          case maybeConn of
            Just conn -> Prelude.pure conn
            Nothing -> do
              settings <- Environment.decode Settings.decoder
              acquire settings
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

{-# NOINLINE testConnectionVar #-}
testConnectionVar :: MVar.MVar (Maybe Connection)
testConnectionVar = System.IO.Unsafe.unsafePerformIO (MVar.newMVar Nothing)

{-# NOINLINE testDoAnything #-}
testDoAnything :: Platform.DoAnythingHandler
testDoAnything = System.IO.Unsafe.unsafePerformIO Platform.doAnythingHandler
