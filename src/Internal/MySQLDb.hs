{-# LANGUAGE QuasiQuotes #-}

module Internal.MySQLDb
  ( Connection (..),
    SingleOrPool (..),
    runTaskWithConnection,
    Transaction (Transaction, begin, commit, rollback, rollbackAll),
    transaction,
    inTestTransaction,
    readiness,
    handleError,
    toQueryError,
    eitherToResult,
    TransactionCount (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import Data.IORef
import qualified Data.Pool
import Data.String (IsString)
import qualified Data.Text
import Data.Word (Word)
import qualified Database.Persist.MySQL as MySQL
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

newtype TransactionCount
  = TransactionCount (IORef Word)

data Connection
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool MySQL.SqlBackend,
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
    Single TransactionCount c

--
-- CONNECTION HELPERS
--

runTaskWithConnection :: Connection -> (MySQL.SqlBackend -> IO (Result Query.Error a)) -> Task Query.Error a
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
withConnection :: Connection -> (MySQL.SqlBackend -> Task e a) -> Task e a
withConnection conn@Connection {singleOrPool} func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        doIO conn
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool MySQL.SqlBackend -> (MySQL.SqlBackend, Data.Pool.LocalPool MySQL.SqlBackend) -> ExitCase x -> Task y ()
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
        (Single _ c) ->
          func c
        --
        (Pool pool) ->
          Platform.generalBracket (acquire pool) (release pool) (Tuple.first >> func)
            |> map Tuple.first

--
-- READINESS
--

-- |
-- Check that we are ready to be take traffic.
readiness :: IsString s => (MySQL.SqlBackend -> s -> IO ()) -> Platform.LogHandler -> Connection -> IO Health.Status
readiness runQuery log' conn =
  let query c =
        runQuery c "SELECT 1"
          |> Exception.tryAny
          |> map (Result.mapError toQueryError << eitherToResult)
   in runTaskWithConnection conn query
        |> Task.mapError (Data.Text.pack << Exception.displayException)
        |> Task.attempt log'
        |> map Health.fromResult

toQueryError :: Exception.Exception e => e -> Query.Error
toQueryError err =
  Exception.displayException err
    |> Data.Text.pack
    |> Query.Other

eitherToResult :: Either e a -> Result e a
eitherToResult either =
  case either of
    Left err -> Err err
    Right x -> Ok x

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
      { commit :: (TransactionCount, MySQL.SqlBackend) -> IO (),
        begin :: (TransactionCount, MySQL.SqlBackend) -> IO (),
        rollback :: (TransactionCount, MySQL.SqlBackend) -> IO (),
        rollbackAll :: (TransactionCount, MySQL.SqlBackend) -> IO ()
      }

-- |
-- Perform a database transaction.
transaction :: Transaction -> Connection -> (Connection -> Task e a) -> Task e a
transaction Transaction {commit, begin, rollback} conn func =
  let start :: TransactionCount -> MySQL.SqlBackend -> Task x MySQL.SqlBackend
      start tc c =
        doIO conn <| do
          begin (tc, c)
          pure c
      --
      end :: TransactionCount -> MySQL.SqlBackend -> ExitCase b -> Task x ()
      end tc c exitCase =
        doIO conn
          <| case exitCase of
            ExitCaseSuccess _ -> commit (tc, c)
            ExitCaseException _ -> rollback (tc, c)
            ExitCaseAbort -> rollback (tc, c)
      --
      setSingle :: TransactionCount -> MySQL.SqlBackend -> Connection
      setSingle tc c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single tc c}
   in withConnection conn <| \c -> do
        tc <- map TransactionCount (doIO conn (newIORef 0))
        Platform.generalBracket (start tc c) (end tc) (setSingle tc >> func)
          |> map Tuple.first

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: Transaction -> Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction transaction_@Transaction {begin} conn func =
  let start :: TransactionCount -> MySQL.SqlBackend -> Task x MySQL.SqlBackend
      start tc c = do
        rollbackAllSafe tc transaction_ conn c
        doIO conn <| begin (tc, c)
        pure c
      --
      end :: TransactionCount -> MySQL.SqlBackend -> ExitCase b -> Task x ()
      end tc c _ =
        rollbackAllSafe tc transaction_ conn c
      --
      setSingle :: TransactionCount -> MySQL.SqlBackend -> Connection
      setSingle tc c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single tc c}
   in --
      withConnection conn <| \c -> do
        tc <- map TransactionCount (doIO conn (newIORef 0))
        Platform.generalBracket (start tc c) (end tc) (setSingle tc >> func)
          |> map Tuple.first

rollbackAllSafe :: TransactionCount -> Transaction -> Connection -> MySQL.SqlBackend -> Task x ()
rollbackAllSafe tc Transaction {begin, rollbackAll} conn c =
  doIO conn <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    begin (tc, c)
    rollbackAll (tc, c)

-- HELPER

doIO :: Connection -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)
