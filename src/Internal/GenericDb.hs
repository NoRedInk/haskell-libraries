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
import qualified Internal.Time as Time
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

runTaskWithConnection :: Connection internal conn -> (internal -> IO (Result Query.Error a)) -> Task Query.Error a
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
        action (toInternalConnection conn dbConnection)
          |> (if Time.microseconds (timeout conn) > 0 then withTimeout else identity)
          |> Platform.doAnything (doAnything conn)

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection internal conn -> (conn -> Task e a) -> Task e a
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
        doIO conn <| do
          begin (toInternalConnection conn c)
          pure c
      --
      end :: conn -> ExitCase b -> Task x ()
      end c exitCase =
        doIO conn
          <| case exitCase of
            ExitCaseSuccess _ -> commit (toInternalConnection conn c)
            ExitCaseException _ -> rollback (toInternalConnection conn c)
            ExitCaseAbort -> rollback (toInternalConnection conn c)
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
  let start :: conn -> Task x conn
      start c = do
        rollbackAllSafe transaction_ conn c
        doIO conn <| begin (toInternalConnection conn c)
        pure c
      --
      end :: conn -> ExitCase b -> Task x ()
      end c _ =
        rollbackAllSafe transaction_ conn c
      --
      setSingle :: conn -> Connection internal conn
      setSingle c =
        -- All queries in a transactions must run on the same thread.
        conn {singleOrPool = Single c}
   in --
      withConnection conn <| \c ->
        Platform.generalBracket (start c) end (setSingle >> func)
          |> map Tuple.first

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
