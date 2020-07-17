{-# LANGUAGE QuasiQuotes #-}

module Internal.GenericDb
  ( Connection (..),
    SingleOrPool (..),
    runTaskWithConnection,
    Transaction (Transaction, begin, commit, rollback, rollbackAll),
    transaction,
    inTestTransaction,
    handleError,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import qualified Data.Pool
import Data.String (IsString)
import qualified Data.Text
import Database.PostgreSQL.Typed (PGConnection)
import qualified Health
import qualified Internal.Query as Query
import qualified Internal.Time as Time
import qualified Oops
import qualified Platform
import qualified Result
import qualified System.Exit
import qualified System.Timeout
import qualified Task
import qualified Tuple
import Prelude (Either (Left, Right), IO, fromIntegral, pure)

data Connection
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool PGConnection,
        logContext :: Platform.QueryConnectionInfo,
        timeout :: Time.Interval
      }

-- | A database connection type.
--   Defining our own type makes it easier to change it in the future, without
--   having to fix compilation errors all over the codebase.
data SingleOrPool c
  = -- | By default a connection pool is passed around. It will:
    --   - Create new connections in the pool up to a certain limit.
    --   - Remove connections from the pool after a query in a connection errored.
    Pool (Data.Pool.Pool c)
  | -- | A single connection is only used in the context of a transaction, where
    --   we need to insure several SQL statements happen on the same connection.
    Single c

--
-- CONNECTION HELPERS
--

runTaskWithConnection :: Connection -> (PGConnection -> IO (Result Query.Error a)) -> Task Query.Error a
runTaskWithConnection conn action =
  let withTimeout :: IO (Result Query.Error a) -> IO (Result Query.Error a)
      withTimeout io = do
        maybeResult <- System.Timeout.timeout (fromIntegral (Time.microseconds (timeout conn))) io
        case maybeResult of
          Just result -> pure result
          Nothing -> pure (Err timeoutError)
      --
      timeoutError :: Query.Error
      timeoutError =
        Query.Timeout Query.ClientTimeout (timeout conn)
   in --
      withConnection conn <| \dbConnection ->
        action dbConnection
          |> (if Time.microseconds (timeout conn) > 0 then withTimeout else identity)
          |> Platform.doAnything (doAnything conn)

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection -> (PGConnection -> Task e a) -> Task e a
withConnection conn@Connection {singleOrPool} func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        doIO conn
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool conn -> (conn, Data.Pool.LocalPool conn) -> ExitCase x -> Task y ()
      release pool (c, localPool) exitCase =
        doIO conn
          <| case exitCase of
            ExitCaseSuccess _ ->
              Data.Pool.putResource localPool c
            ExitCaseException _ ->
              Data.Pool.destroyResource pool localPool c
            ExitCaseAbort ->
              Data.Pool.destroyResource pool localPool c
   in --
      case singleOrPool of
        (Single c) ->
          func c
        --
        (Pool pool) ->
          Platform.generalBracket (acquire pool) (release pool) (Tuple.first >> func)
            |> map Tuple.first

--
-- READINESS
--

handleError :: Text -> Exception.IOException -> IO a
handleError connectionString err = do
  _ <-
    Oops.putNiceError
      [Oops.help|# Could not connect to Database
                |
                |We couldn't connect to the database.
                |You might see this error when you try to start the content creation app or during compilation.
                |
                |Are you sure your database is running?
                |Bring it up by running `aide setup-postgres`.
                |We're trying to connect with the credentials stored in `.env`, perhaps you can try to connect manually.
                |
                |If credentials recently changed, regenerating configuration files might also work.
                |The command for that is:
                |
                |```
                |$ ./Shakefile.hs .env
                |```
                |
                |]
      [ Oops.extra "Exception" err,
        Oops.extra "Attempted to connect to" connectionString
      ]
  Exception.displayException err
    |> System.Exit.die

--
-- TRANSACTIONS
--

data Transaction
  = Transaction
      { commit :: PGConnection -> IO (),
        begin :: PGConnection -> IO (),
        rollback :: PGConnection -> IO (),
        rollbackAll :: PGConnection -> IO ()
      }

-- |
-- Perform a database transaction.
transaction :: forall e a. Transaction -> Connection -> (Connection -> Task e a) -> Task e a
transaction Transaction {commit, begin, rollback} conn func =
  let start :: PGConnection -> Task x PGConnection
      start c =
        doIO conn <| do
          begin c
          pure c
      --
      end :: PGConnection -> ExitCase b -> Task x ()
      end c exitCase =
        doIO conn
          <| case exitCase of
            ExitCaseSuccess _ -> commit c
            ExitCaseException _ -> rollback c
            ExitCaseAbort -> rollback c
      --
      setSingle :: PGConnection -> Connection
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single c}
   in withConnection conn <| \c ->
        Platform.generalBracket (start c) end (setSingle >> func)
          |> map Tuple.first

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: forall x a. Transaction -> Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction transaction_@Transaction {begin} conn func =
  let start :: PGConnection -> Task x PGConnection
      start c = do
        rollbackAllSafe transaction_ conn c
        doIO conn <| begin c
        pure c
      --
      end :: PGConnection -> ExitCase b -> Task x ()
      end c _ =
        rollbackAllSafe transaction_ conn c
      --
      setSingle :: PGConnection -> Connection
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single c}
   in --
      withConnection conn <| \c ->
        Platform.generalBracket (start c) end (setSingle >> func)
          |> map Tuple.first

rollbackAllSafe :: Transaction -> Connection -> PGConnection -> Task x ()
rollbackAllSafe Transaction {begin, rollbackAll} conn c =
  doIO conn <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    begin c
    rollbackAll c

-- HELPER

doIO :: Connection -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)
