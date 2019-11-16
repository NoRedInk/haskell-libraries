{-# LANGUAGE QuasiQuotes #-}

module Internal.GenericDb
  ( Connection (logContext),
    PoolConfig
      ( PoolConfig,
        connect,
        disconnect,
        stripes,
        maxIdleTime,
        size,
        toConnectionString,
        toConnectionLogContext
      ),
    connection,
    runTaskWithConnection,
    Transaction (Transaction, begin, commit, rollback, rollbackAll),
    transaction,
    inTestTransaction,
    readiness,
  )
where

import qualified Control.Exception
import Control.Exception.Safe (MonadCatch)
import qualified Control.Exception.Safe
import qualified Control.Monad.Catch
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import qualified Data.Acquire
import qualified Data.Int
import qualified Data.Pool
import Data.Time.Clock (NominalDiffTime)
import qualified Health
import Nri.Prelude
import qualified Oops
import qualified Platform
import qualified Task
import qualified Tuple

data Connection c
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool c,
        logContext :: Platform.QueryConnectionInfo
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

data PoolConfig db conn
  = PoolConfig
      { connect :: db -> IO conn,
        disconnect :: conn -> IO (),
        stripes :: Data.Int.Int,
        maxIdleTime :: NominalDiffTime,
        size :: Data.Int.Int,
        toConnectionString :: db -> Text,
        toConnectionLogContext :: db -> Platform.QueryConnectionInfo
      }

connection :: db -> PoolConfig db conn -> Data.Acquire.Acquire (Connection conn)
connection
  database
  PoolConfig
    { connect,
      disconnect,
      stripes,
      maxIdleTime,
      size,
      toConnectionString,
      toConnectionLogContext
    } = Data.Acquire.mkAcquire acquire release
    where
      acquire = do
        doAnything <- Platform.doAnythingHandler
        pool <-
          map Pool
            <| Data.Pool.createPool
              (connect database `catch` handleError (toConnectionString database))
              disconnect
              stripes
              maxIdleTime
              size
        pure (Connection doAnything pool (toConnectionLogContext database))
      release Connection {singleOrPool} =
        case singleOrPool of
          Pool pool -> Data.Pool.destroyAllResources pool
          Single single -> disconnect single

runTaskWithConnection :: Connection t -> (t -> IO a) -> Task e a
runTaskWithConnection conn f =
  withConnection
    conn
    ( \c ->
        Platform.doAnything (doAnything conn) (map Ok (f c))
    )

--

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection conn -> (conn -> Task e a) -> Task e a
withConnection Connection {doAnything, singleOrPool} f =
  case singleOrPool of
    (Single c) -> f c
    (Pool pool) -> map Tuple.first <| Platform.generalBracket acquire release (f << Tuple.first)
      where
        acquire =
          Data.Pool.takeResource pool
            |> map Ok
            |> Platform.doAnything doAnything
        release (c, localPool) =
          Platform.doAnything doAnything << map Ok
            << ( \exitCase ->
                   case exitCase of
                     ExitCaseSuccess _ -> Data.Pool.putResource localPool c
                     ExitCaseException _ ->
                       Data.Pool.destroyResource pool localPool c
                     ExitCaseAbort -> Data.Pool.destroyResource pool localPool c
               )

-- | A version of `withConnection` that doesn't use `Data.Pool.withResource`.
--   This has the advantage it doesn't put a `MonadBaseControl IO m` constraint
--   on the return type, which we require for the `inTestTransaction` to be
--   useful (reason: `PropertyT` does not implement a `MonadBaseControl IO a`
--   instance). The trade-off is that this function isn't quite as safe, and has
--   a small chance to leek database connections. For tests that seems okay.
withConnectionUnsafe ::
  (MonadIO m, MonadCatch m) => Connection conn -> (conn -> m a) -> m a
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
readiness runQuery log' conn = do
  response <-
    flip runQuery "SELECT 1"
      |> runTaskWithConnection conn
      |> Task.perform identity
      |> Platform.runCmd log'
      |> Control.Exception.Safe.tryAny
  pure <| Health.fromResult <| case response of
    Left err -> Err (toS (displayException err))
    Right x -> Ok x

handleError :: Text -> IOException -> IO a
handleError connectionString err = do
  _ <-
    Oops.putNiceError
      [Oops.help|# Could not connect to Database
                |
                |We couldn't connect to the database.
                |You might see this error when you try to start the content creation app or during compilation.
                |
                |Are you sure your database is running?
                |Bring it up by running `aide build setup-postgres`.
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
  Control.Exception.displayException err
    |> toS
    |> die

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction ::
  forall conn m a.
  (MonadIO m, MonadCatch m) =>
  Transaction conn ->
  Connection conn ->
  (Connection conn -> m a) ->
  m a
inTestTransaction t@Transaction {begin} conn f =
  withConnectionUnsafe conn <| \c -> do
    rollbackAllSafe t c
    liftIO <| begin c
    x <-
      f (conn {singleOrPool = Single c})
        `Control.Monad.Catch.onException` rollbackAllSafe t c
    rollbackAllSafe t c
    pure x

data Transaction conn
  = Transaction
      { commit :: conn -> IO (),
        begin :: conn -> IO (),
        rollback :: conn -> IO (),
        rollbackAll :: conn -> IO ()
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
        begin c
        pure c
    end :: conn -> ExitCase a -> Task e ()
    end c =
      Platform.doAnything (doAnything conn) << map Ok
        << ( \exitCase ->
               case exitCase of
                 ExitCaseSuccess _ -> commit c
                 ExitCaseException _ -> rollback c
                 ExitCaseAbort -> rollback c
           )

rollbackAllSafe ::
  forall conn m.
  (MonadIO m) =>
  Transaction conn ->
  conn ->
  m ()
rollbackAllSafe Transaction {begin, rollbackAll} c =
  liftIO <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    begin c
    rollbackAll c
