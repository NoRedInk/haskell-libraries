{-# LANGUAGE QuasiQuotes #-}

module Internal.GenericDb
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
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import qualified Data.Pool
import Data.String (IsString)
import qualified Data.Text
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

data Connection internal conn
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool conn,
        toInternalConnection :: conn -> internal,
        logContext :: Platform.QueryConnectionInfo,
        timeoutMicroSeconds :: Int
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

runTaskWithConnection :: Connection internal conn -> (internal -> IO (Result Query.Error a)) -> Task Query.Error a
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
        action (toInternalConnection conn dbConnection)
          |> (if microseconds > 0 then withTimeout else identity)
          |> Platform.doAnything (doAnything conn)

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection internal conn -> (conn -> Task e a) -> Task e a
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
withConnectionUnsafe :: forall internal conn x a. Connection internal conn -> (conn -> Task x a) -> Task x a
withConnectionUnsafe conn@Connection {singleOrPool} func =
  case singleOrPool of
    (Pool pool) -> do
      (c, localPool) <- doIO conn (Data.Pool.takeResource pool)
      func c
        |> Task.andThen (\a -> do
            doIO conn (Data.Pool.putResource localPool c)
            Task.succeed a
           )
        |> Task.onError (\x -> do
            doIO conn (Data.Pool.destroyResource pool localPool c)
            Task.fail x
          )
    --
    (Single c) ->
      func c

--
-- READINESS
--

-- |
-- Check that we are ready to be take traffic.
readiness :: IsString s => (internal -> s -> IO ()) -> Platform.LogHandler -> Connection internal conn -> IO Health.Status
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

data Transaction internal
  = Transaction
      { commit :: internal -> IO (),
        begin :: internal -> IO (),
        rollback :: internal -> IO (),
        rollbackAll :: internal -> IO ()
      }

-- |
-- Perform a database transaction.
transaction :: forall internal conn e a. Transaction internal -> Connection internal conn -> (Connection internal conn -> Task e a) -> Task e a
transaction Transaction {commit, begin, rollback} conn func =
  let start :: conn -> Task x conn
      start c =
        perform <| do
          begin (toInternalConnection conn c)
          pure c
      --
      end :: conn -> ExitCase b -> Task x ()
      end c exitCase =
        perform
          <| case exitCase of
            ExitCaseSuccess _ -> commit (toInternalConnection conn c)
            ExitCaseException _ -> rollback (toInternalConnection conn c)
            ExitCaseAbort -> rollback (toInternalConnection conn c)
      --
      perform :: IO c -> Task x c
      perform =
        map Ok >> Platform.doAnything (doAnything conn)
      --
      setSingle :: conn -> Connection internal conn
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single c}
   in withConnection conn <| \c ->
        Platform.generalBracket (start c) end (setSingle >> func)
          |> map Tuple.first

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: forall internal conn x a. Transaction internal -> Connection internal conn -> (Connection internal conn -> Task x a) -> Task x a
inTestTransaction transaction_@Transaction {begin} conn func =
  withConnectionUnsafe conn <| \c -> do
    rollbackAllSafe transaction_ conn c
    doIO conn <| begin (toInternalConnection conn c)
    let singleConn = conn {singleOrPool = Single c}
    -- All queries in a transactions must run on the same thread.
    x <- func singleConn
    rollbackAllSafe transaction_ conn c
    pure x

rollbackAllSafe :: Transaction internal -> Connection internal conn -> conn -> Task x ()
rollbackAllSafe Transaction {begin, rollbackAll} conn c =
  doIO conn <| do
    -- Because calling `rollbackAllTransactions` when no transactions are
    -- running will result in a warning message in the log (even if tests
    -- pass), let's start by beginning a transaction, so that we alwas have
    -- at least one to kill.
    begin (toInternalConnection conn c)
    rollbackAll (toInternalConnection conn c)


-- HELPER

doIO :: Connection internal conn -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)
