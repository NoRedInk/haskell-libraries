{-# LANGUAGE QuasiQuotes #-}

module Internal.GenericDb
  ( Connection (..),
    SingleOrPool (..),
    runTaskWithConnection,
    Transaction (Transaction, begin, commit, rollback, rollbackAll),
    TransactionCount (..),
    transaction,
    inTestTransaction,
    readiness,
    handleError,
    toQueryError,
    eitherToResult,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.Catch
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import qualified Data.Pool
import Data.String (IsString)
import qualified Data.Text
import Data.Word (Word)
import qualified Health
import qualified Internal.Query as Query
import qualified Oops
import qualified Platform
import qualified Result
import qualified System.Exit
import qualified System.Timeout
import qualified Task
import qualified Tuple
import Prelude (Either (Left, Right), IO, fromIntegral, pure)

data Connection c
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool c,
        transactionCount :: TransactionCount,
        logContext :: Platform.QueryConnectionInfo,
        timeoutMicroSeconds :: Int
      }

data TransactionCount
  = TransactionCount (IORef Word)

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

runTaskWithConnection :: Connection conn -> (conn -> IO (Result Query.Error a)) -> Task Query.Error a
runTaskWithConnection conn action =
  let microseconds = timeoutMicroSeconds conn
      --
      withTimeout :: IO (Result Query.Error a) -> IO (Result Query.Error a)
      withTimeout io = do
        maybeResult <- System.Timeout.timeout (fromIntegral microseconds) io
        case maybeResult of
          Just result -> pure result
          Nothing -> pure (Err timeoutError)
      --
      timeoutError :: Query.Error
      timeoutError =
        Query.TimeoutAfterSeconds Query.ClientTimeout (fromIntegral microseconds / 10e6)
   in --
      withConnection conn <| \dbConnection ->
        action dbConnection
          |> (if microseconds > 0 then withTimeout else identity)
          |> Platform.doAnything (doAnything conn)

--

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection conn -> (conn -> Task e a) -> Task e a
withConnection Connection {doAnything, singleOrPool} func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        perform
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool conn -> (conn, Data.Pool.LocalPool conn) -> ExitCase x -> Task y ()
      release pool (c, localPool) exitCase =
        perform
          <| case exitCase of
            ExitCaseSuccess _ ->
              Data.Pool.putResource localPool c
            ExitCaseException _ ->
              Data.Pool.destroyResource pool localPool c
            ExitCaseAbort ->
              Data.Pool.destroyResource pool localPool c
      --
      perform :: IO a -> Task x a
      perform =
        map Ok >> Platform.doAnything doAnything
   in case singleOrPool of
        (Single c) ->
          func c
        (Pool pool) ->
          Platform.generalBracket (acquire pool) (release pool) (Tuple.first >> func)
            |> map Tuple.first

-- | A version of `withConnection` that doesn't use `Data.Pool.withResource`.
--   This has the advantage it doesn't put a `MonadBaseControl IO m` constraint
--   on the return type, which we require for the `inTestTransaction` to be
--   useful (reason: `PropertyT` does not implement a `MonadBaseControl IO a`
--   instance). The trade-off is that this function isn't quite as safe, and has
--   a small chance to leek database connections. For tests that seems okay.
withConnectionUnsafe ::
  (MonadIO m, Exception.MonadCatch m) => Connection conn -> (conn -> m a) -> m a
withConnectionUnsafe Connection {singleOrPool} f =
  case singleOrPool of
    (Pool pool) -> do
      (c, localPool) <- liftIO <| Data.Pool.takeResource pool
      x <-
        Data.Pool.destroyResource pool localPool c
          |> liftIO
          |> Control.Monad.Catch.onException (f c)
      liftIO (Data.Pool.putResource localPool c)
      pure x
    (Single c) -> f c

-- |
-- Check that we are ready to be take traffic.
readiness :: IsString s => (conn -> s -> IO ()) -> Platform.LogHandler -> Connection conn -> IO Health.Status
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

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction ::
  forall conn m a.
  (MonadIO m, Exception.MonadCatch m) =>
  Transaction conn ->
  Connection conn ->
  (Connection conn -> m a) ->
  m a
inTestTransaction t@Transaction {begin} conn f =
  withConnectionUnsafe conn <| \c -> do
    rollbackAllSafe t conn c
    liftIO <| begin (transactionCount conn) c
    x <-
      f (conn {singleOrPool = Single c})
        `Control.Monad.Catch.onException` rollbackAllSafe t conn c
    rollbackAllSafe t conn c
    pure x

data Transaction conn
  = Transaction
      { commit :: TransactionCount -> conn -> IO (),
        begin :: TransactionCount -> conn -> IO (),
        rollback :: TransactionCount -> conn -> IO (),
        rollbackAll :: TransactionCount -> conn -> IO ()
      }

-- |
-- Perform a database transaction.
transaction :: forall conn e a. Transaction conn -> Connection conn -> (Connection conn -> Task e a) -> Task e a
transaction Transaction {commit, begin, rollback} conn f =
  withConnection conn <| \c ->
    map Tuple.first
      <| Platform.generalBracket
        (start c)
        end
        (f << (\c_ -> conn {singleOrPool = Single c_}))
  where
    start :: conn -> Task e conn
    start c =
      Platform.doAnything (doAnything conn) <| map Ok <| do
        begin (transactionCount conn) c
        pure c
    end :: conn -> ExitCase a -> Task e ()
    end c =
      Platform.doAnything (doAnything conn) << map Ok
        << ( \exitCase ->
               case exitCase of
                 ExitCaseSuccess _ -> commit (transactionCount conn) c
                 ExitCaseException _ -> rollback (transactionCount conn) c
                 ExitCaseAbort -> rollback (transactionCount conn) c
           )

rollbackAllSafe :: forall conn m. (MonadIO m) => Transaction conn -> Connection conn -> conn -> m ()
rollbackAllSafe Transaction {begin, rollbackAll} conn c =
  liftIO <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    begin (transactionCount conn) c
    rollbackAll (transactionCount conn) c
