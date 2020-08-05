{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries, but for
-- MySQL. They return the correct amount of results in a Servant handler, or throw
-- a Rollbarred error.
module MySQL
  ( -- Connection
    Connection,
    connection,
    readiness, -- Creating a connection handler.
    -- Settings
    Settings.Settings,
    Settings.decoder,
    -- Querying
    Query,
    Query.Error (..),
    sql,
    doQuery,
    -- Handling transactions
    transaction,
    inTestTransaction,
    -- Helpers for uncommon queries
    unsafeBulkifyInserts,
    BulkifiedInsert (..),
    onConflictUpdate,
    onDuplicateDoNothing,
    sqlYearly,
    lastInsertedPrimaryKey,
    replace,
    MySQLParam,
    Query.MySQLColumn,
  )
where

import Cherry.Prelude hiding (e)
import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Lens.Regex.Text as R
import Control.Monad.Catch (ExitCase (ExitCaseAbort, ExitCaseException, ExitCaseSuccess))
import qualified Data.Acquire
import qualified Data.ByteString.Lazy
import qualified Data.Hashable as Hashable
import qualified Data.IORef as IORef
import qualified Data.Int
import qualified Data.Pool
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.MySQL.Base as Base
import qualified Database.MySQL.Connection
import qualified Database.MySQL.Protocol.Packet
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Dict
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Health
import Internal.CaselessRegex (caselessRegex)
import qualified Internal.Time as Time
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified List
import qualified Log
import MySQL.FromRow (CountColumns, FromRow (fromRow))
import MySQL.MySQLParam (MySQLParam)
import qualified MySQL.Query as Query
import MySQL.Query (Query (..))
import qualified MySQL.Settings as Settings
import qualified Platform
import qualified System.IO.Streams.List
import qualified Task
import qualified Text
import qualified Tuple
import Prelude (IO, error, fromIntegral, pure, show)
import qualified Prelude

newtype TransactionCount
  = TransactionCount Int
  deriving (Eq)

data Connection
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
        singleOrPool :: SingleOrPool Base.MySQLConn,
        logContext :: Platform.QueryConnectionInfo,
        timeout :: Time.Interval,
        -- | In this IORef we keep a list of statements we've prepared with
        -- MySQL. These are SQL commands with some placeholders that MySQL has
        -- parsed and is ready to run, if we only provide the placeholders. This
        -- is a faster way of running similar queries multiple times than
        -- sending MySQL full query strings every time, which it then has to
        -- parse first.
        --
        -- If we prepare a query MySQL makes it available for the current
        -- connection only, which is why we're not sharing this hash between
        -- database connections.
        --
        -- Database connections can live for a long time. We have to be careful
        -- with the memory they use because they won't get cleaned up after
        -- every request. That's why we're using a hash of the query as the key
        -- in the dictionary below instead of the query itself.
        preparedQueries :: IORef.IORef (Dict.Dict PreparedQueryHash Base.StmtID)
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

newtype PreparedQueryHash = PreparedQueryHash Prelude.Int
  deriving (Eq, Ord)

connection :: Settings.Settings -> Data.Acquire.Acquire Connection
connection settings =
  Data.Acquire.mkAcquire acquire release
  where
    acquire = do
      doAnything <- Platform.doAnythingHandler
      pool <-
        map Pool
          <| Data.Pool.createPool
            (Base.connect database)
            Base.close
            stripes
            maxIdleTime
            size
      preparedQueries <- IORef.newIORef Dict.empty
      pure
        ( Connection
            doAnything
            pool
            (toConnectionLogContext settings)
            (Settings.mysqlQueryTimeoutSeconds settings)
            preparedQueries
        )
    release Connection {singleOrPool} =
      case singleOrPool of
        Pool pool -> Data.Pool.destroyAllResources pool
        Single _ _ -> pure ()
    stripes = Settings.unMysqlPoolStripes (Settings.mysqlPoolStripes (Settings.mysqlPool settings)) |> fromIntegral
    maxIdleTime = Settings.unMysqlPoolMaxIdleTime (Settings.mysqlPoolMaxIdleTime (Settings.mysqlPool settings))
    size = Settings.unMysqlPoolSize (Settings.mysqlPoolSize (Settings.mysqlPool settings)) |> fromIntegral
    database = toConnectInfo settings

--
-- CONNECTION HELPERS
--

toConnectionLogContext :: Settings.Settings -> Platform.QueryConnectionInfo
toConnectionLogContext settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Settings.unDatabase (Settings.database connectionSettings)
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Platform.UnixSocket
            Platform.MySQL
            (Data.Text.pack (Settings.unSocket socket))
            database
        Settings.ConnectTcp host port ->
          Platform.TcpSocket
            Platform.MySQL
            (Settings.unHost host)
            (Data.Text.pack (show (Settings.unPort port)))
            database

toConnectInfo :: Settings.Settings -> Base.ConnectInfo
toConnectInfo settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Data.Text.Encoding.encodeUtf8 (Settings.unDatabase (Settings.database connectionSettings))
      user = Data.Text.Encoding.encodeUtf8 (Settings.unUser (Settings.user connectionSettings))
      password = Data.Text.Encoding.encodeUtf8 (Log.unSecret (Settings.unPassword (Settings.password connectionSettings)))
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Base.defaultConnectInfo
            { Base.ciHost = Settings.unSocket socket,
              Base.ciUser = user,
              Base.ciPassword = password,
              Base.ciDatabase = database,
              Base.ciCharset = Database.MySQL.Connection.utf8mb4_unicode_ci
            }
        Settings.ConnectTcp host port ->
          Base.defaultConnectInfo
            { Base.ciHost = Data.Text.unpack (Settings.unHost host),
              Base.ciUser = user,
              Base.ciPassword = password,
              Base.ciDatabase = database,
              Base.ciPort = fromIntegral (Settings.unPort port),
              Base.ciCharset = Database.MySQL.Connection.utf8mb4_unicode_ci
            }

--
-- READINESS
--

-- |
-- Check that we are ready to be take traffic.
readiness :: Platform.LogHandler -> Connection -> Health.Check
readiness log conn =
  executeQuery conn (queryFromText "SELECT 1")
    |> Task.map (\(_ :: [Int]) -> ())
    |> Task.mapError (Data.Text.pack << Exception.displayException)
    |> Task.attempt log
    |> map Health.fromResult
    |> Health.mkCheck "mysql"

queryFromText :: Text -> Query a
queryFromText text = Query
  { preparedStatement = text,
    params = Log.mkSecret [],
    prepareQuery = Query.DontPrepare,
    quasiQuotedString = text,
    sqlOperation = "",
    queriedRelation = ""
  }

--
-- EXECUTE QUERIES
--
class MySqlQueryable query result | result -> query where
  executeSql :: Connection -> Query query -> Task Query.Error result

instance FromRow (CountColumns row) row => MySqlQueryable row [row] where
  executeSql c query = executeQuery c query

instance MySqlQueryable () () where
  executeSql c query = executeCommand c query

doQuery ::
  (HasCallStack, MySqlQueryable row result) =>
  Connection ->
  Query.Query row ->
  (Result Query.Error result -> Task e a) ->
  Task e a
doQuery conn query handleResponse =
  let --
      runQuery = do
        withFrozenCallStack Log.info "Running MySQL query" []
        result <- executeSql conn query
        -- If not currently inside a transaction and original query succeeded, then commit
        case transactionCount conn of
          Nothing -> executeCommand conn (queryFromText "COMMIT")
          _ -> pure ()
        pure result
      infoForContext :: Platform.QueryInfo
      infoForContext = Platform.QueryInfo
        { Platform.queryText = Log.mkSecret (Query.preparedStatement query),
          Platform.queryTemplate = Query.quasiQuotedString query,
          Platform.queryConn = logContext conn,
          Platform.queryOperation = Query.sqlOperation query,
          Platform.queryCollection = Query.queriedRelation query
        }
   in runQuery
        -- Handle the response before wrapping the operation in a context. This way,
        -- if the response handling logic creates errors, those errors can inherit
        -- context values like the query string.
        |> Task.map Ok
        |> Task.onError (Task.succeed << Err)
        |> Task.andThen handleResponse
        |> Log.withContext "mysql-query" [Platform.queryContext infoForContext]

executeQuery :: forall row. FromRow (CountColumns row) row => Connection -> Query row -> Task Query.Error [row]
executeQuery conn query =
  withConnection conn <| \backend ->
    let params = Query.params query |> Log.unSecret
        toRows stream = do
          rows <- System.IO.Streams.List.toList stream
          rows
            |> map (fromRow (Proxy :: Proxy (CountColumns row)))
            |> pure
     in withTimeout conn
          <| Platform.doAnything (doAnything conn)
          <| handleMySqlException
          <| case prepareQuery query of
            Query.Prepare -> do
              stmtId <- getPreparedQueryStmtID conn backend query
              (_, stream) <- Base.queryStmt backend stmtId params
              toRows stream
            Query.DontPrepare -> do
              (_, stream) <- Base.query backend (toBaseQuery query) params
              toRows stream

executeCommand :: Connection -> Query () -> Task Query.Error ()
executeCommand conn query =
  withConnection conn <| \backend ->
    let params = Query.params query |> Log.unSecret
     in withTimeout conn
          <| Platform.doAnything (doAnything conn)
          <| handleMySqlException
          <| case Query.prepareQuery query of
            Query.Prepare -> do
              stmtId <- getPreparedQueryStmtID conn backend query
              _ <- Base.executeStmt backend stmtId params
              Prelude.pure ()
            Query.DontPrepare -> do
              _ <- Base.execute backend (toBaseQuery query) params
              Prelude.pure ()

toBaseQuery :: Query row -> Base.Query
toBaseQuery query =
  Query.preparedStatement query
    |> Data.Text.Encoding.encodeUtf8
    |> Data.ByteString.Lazy.fromStrict
    |> Base.Query

-- | Get the prepared statement id for a particular query. If such an ID exists
-- MySQL already knows this query and has it parsed in memory. We just have to
-- provide it with the values for the placeholder fields in the query, and
-- MySQL can run it.
getPreparedQueryStmtID :: Connection -> Base.MySQLConn -> Query a -> IO Base.StmtID
getPreparedQueryStmtID conn backend query = do
  let baseQuery = toBaseQuery query
  let queryHash = PreparedQueryHash (Hashable.hash (Base.fromQuery baseQuery))
  alreadyPrepped <- IORef.readIORef (preparedQueries conn)
  case Dict.get queryHash alreadyPrepped of
    -- We've already prepared this statement before and stored the id.
    Just stmtId -> pure stmtId
    -- We've not prepared this statement before, so we do so now.
    Nothing -> do
      stmtId <- Base.prepareStmt backend baseQuery
      IORef.atomicModifyIORef'
        (preparedQueries conn)
        (\dict -> (Dict.insert queryHash stmtId dict, ()))
      pure stmtId

handleMySqlException :: IO result -> IO (Result Query.Error result)
handleMySqlException io =
  Exception.catches
    (map Ok io)
    [ Exception.Handler
        ( \(Base.ERRException err) ->
            let errCode = Database.MySQL.Protocol.Packet.errCode err
                errState = Database.MySQL.Protocol.Packet.errState err
                errMsg = Database.MySQL.Protocol.Packet.errMsg err
             in Query.Other
                  ("MySQL query failed with error code " ++ Text.fromInt (fromIntegral errCode))
                  [ Log.context "error state" (Data.Text.Encoding.decodeUtf8 errState),
                    Log.context "error message" (Data.Text.Encoding.decodeUtf8 errMsg)
                  ]
                  |> Err
                  |> pure
        ),
      Exception.Handler
        ( \Base.NetworkException ->
            Query.Other "MySQL query failed with a network exception" []
              |> Err
              |> pure
        ),
      Exception.Handler
        ( \(err :: Exception.SomeException) ->
            Exception.displayException err
              |> Data.Text.pack
              -- We add the full error in the context array rather than the
              -- message string, to help errors being grouped correctly in a
              -- bug tracker. Errors might contain unique bits of data like
              -- generated id's or timestamps which when included in the main
              -- error message would result in each error being grouped by
              -- itself.
              |> (\err' -> Query.Other "MySQL query failed with unexpected error" [Log.context "error" err'])
              |> Err
              |> pure
        )
    ]

withTimeout :: Connection -> Task Query.Error a -> Task Query.Error a
withTimeout conn task =
  if Time.microseconds (timeout conn) > 0
    then
      Task.timeout
        (Time.milliseconds (timeout conn))
        (Query.Timeout Query.ClientTimeout (timeout conn))
        task
    else task

--
-- TRANSACTIONS
--

-- |
-- Perform a database transaction.
transaction :: Connection -> (Connection -> Task e a) -> Task e a
transaction conn' func =
  withTransaction conn' <| \conn ->
    Platform.generalBracket
      (begin conn)
      ( \() exitCase ->
          case exitCase of
            ExitCaseSuccess _ -> commit conn
            ExitCaseException _ -> rollback conn
            ExitCaseAbort -> rollback conn
      )
      (\() -> func conn)
      |> map Tuple.first

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction conn' func =
  withTransaction conn' <| \conn ->
    Platform.generalBracket
      (do rollbackAll conn; begin conn)
      (\() _ -> rollbackAll conn)
      (\() -> func conn)
      |> map Tuple.first

transactionCount :: Connection -> Maybe TransactionCount
transactionCount conn =
  case singleOrPool conn of
    Single tc _ -> Just tc
    Pool _ -> Nothing

-- | Begin a new transaction. If there is already a transaction in progress (created with 'begin' or 'pgTransaction') instead creates a savepoint.
begin :: Connection -> Task e ()
begin conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> pure ()
      Just (TransactionCount 0) -> executeCommand conn (queryFromText "BEGIN")
      Just (TransactionCount current) -> executeCommand conn (queryFromText ("SAVEPOINT pgt" ++ Text.fromInt current))

-- | Rollback to the most recent 'begin'.
rollback :: Connection -> Task e ()
rollback conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> pure ()
      Just (TransactionCount 0) -> executeCommand conn (queryFromText "ROLLBACK")
      Just (TransactionCount current) ->
        executeCommand conn (queryFromText ("ROLLBACK TO SAVEPOINT pgt" ++ Text.fromInt current))

-- | Commit the most recent 'begin'.
commit :: Connection -> Task e ()
commit conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> pure ()
      Just (TransactionCount 0) -> executeCommand conn (queryFromText "COMMIT")
      Just (TransactionCount current) -> executeCommand conn (queryFromText ("RELEASE SAVEPOINT pgt" ++ Text.fromInt current))

-- | Rollback all active 'begin's.
rollbackAll :: Connection -> Task e ()
rollbackAll conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> pure ()
      Just _ -> executeCommand conn (queryFromText "ROLLBACK")

throwRuntimeError :: Task Query.Error a -> Task e a
throwRuntimeError task = do
  logHandler <- Platform.logHandler
  Task.onError
    ( \err ->
        Platform.unsafeThrowException
          logHandler
          (Data.Text.pack (Exception.displayException err))
          ( Log.TriageInfo
              Log.UserBlocked
              "Low-level MySQL operations are failing. Either we are not able to talk to the database or there is a bug in the `MySQL` module of the `database` package. Verify the database connection is fine by checking the service's health endpoint. If it is please report this as a bug the owner of the `database` library."
          )
          []
    )
    task

withTransaction :: Connection -> (Connection -> Task e a) -> Task e a
withTransaction conn func =
  case singleOrPool conn of
    Single (TransactionCount tc) c ->
      func conn {singleOrPool = Single (TransactionCount (tc + 1)) c}
    Pool _ ->
      withConnection conn <| \c ->
        func conn {singleOrPool = Single (TransactionCount 0) c}

-- | by default, queries pull a connection from the connection pool.
--   For SQL transactions, we want all queries within the transaction to run
--   on the same connection. withConnection lets transaction bundle
--   queries on the same connection.
withConnection :: Connection -> (Base.MySQLConn -> Task e a) -> Task e a
withConnection conn func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        Log.withContext "acquiring MySQL connection from pool" []
          <| doIO conn
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool Base.MySQLConn -> (Base.MySQLConn, Data.Pool.LocalPool Base.MySQLConn) -> ExitCase x -> Task y ()
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
      case singleOrPool conn of
        (Single _ c) ->
          func c
        --
        (Pool pool) ->
          Platform.generalBracket (acquire pool) (release pool) (Tuple.first >> func)
            |> map Tuple.first

--
-- TYPE CLASSES
--
--

-- | Combine a number of insert queries that all insert a single row into the
-- same table into a single query.
--
-- This helper is not type-safe. It projects you from accidentally combining
-- queries that are selects, because it only work for queries that return `()`.
-- But if you try to combine queries that insert into different tables, or
-- insert different columns, then you're going to see some weird runtime errors.
--
-- For queries against Postgres there's a better ways to do this, see this
-- how-to:
-- https://github.com/NoRedInk/NoRedInk/blob/master/docs/how-tos/insert-multiple-rows-postgresql-typed.md
--
-- For MySQL there might be other approaches we could take, which might offer
-- more compile-time guarantees.
data BulkifiedInsert a
  = BulkifiedInsert a
  | UnableToBulkify Text
  | EmptyInsert
  deriving (Prelude.Functor, Show, Eq)

unsafeBulkifyInserts :: [Query ()] -> BulkifiedInsert (Query ())
unsafeBulkifyInserts [] = EmptyInsert
unsafeBulkifyInserts (first : rest) =
  first
    { preparedStatement =
        Data.Text.intercalate
          ","
          (preparedStatement first : map (Text.dropLeft splitAt << preparedStatement) rest),
      params =
        Prelude.traverse params (first : rest)
          |> map List.concat
    }
    |> BulkifiedInsert
  where
    splitAt =
      preparedStatement first
        |> Data.Text.toLower
        |> Data.Text.breakOn "values"
        |> Tuple.first
        |> Text.length
        |> (+) 6

-- | Appends a query with `ON DUPLICATE KEY UPDATE` to allow updating in case
-- the key isn't unique.
onConflictUpdate :: [Text] -> Query () -> Query ()
onConflictUpdate columns q@Query {preparedStatement, sqlOperation} =
  let onDuplicateKeyUPDATE = "ON DUPLICATE KEY UPDATE"
   in q
        { preparedStatement =
            Text.join
              " "
              [ preparedStatement,
                onDuplicateKeyUPDATE,
                columns
                  |> List.map (\column -> column ++ " = VALUES(" ++ column ++ ")")
                  |> Text.join ","
              ],
          sqlOperation = sqlOperation ++ " " ++ onDuplicateKeyUPDATE
        }

-- | Use for insert queries that are allowed to fail. In Postgres we would use
-- an `ON CONFLICT DO NOTHING` clause for this, but MySQL doesn't support it.
-- `MySQL` recommends using `INSERT IGNORE` syntax, but that Postgres does not
-- support. This helper hacks the `IGNORE` clause into a query after we run
-- Postgres compile-time checks.
onDuplicateDoNothing :: Query () -> Query ()
onDuplicateDoNothing query =
  query
    { preparedStatement =
        Lens.over
          ([caselessRegex|^\s*INSERT\s+INTO|] << R.match)
          (\_ -> "INSERT IGNORE INTO")
          (preparedStatement query)
    }

-- | Special quasi quoter for accessing yearly tables like `mastery_2019`. Use
-- it like this:
--
--     MySQL.doQuery
--       handler
--       (
--         [sqlYearly|!
--           SELECT * FROM mastery_[[YEAR]]
--           LIMIT 1
--         |]
--         2019
--       )
--
-- How this works: whereas the `sql` quasiquoter generates code of a type
-- `Query row`, `sqlYearly` generates code of a type `Int -> Query row`: That's
-- a function that takes a year and substitutes it in the place of the
-- `[[YEAR]]` placeholder.
sqlYearly :: QuasiQuoter
sqlYearly =
  QuasiQuoter
    { quoteExp = qqSQLYearly,
      quoteType = Prelude.fail "sql not supported in types",
      quotePat = Prelude.fail "sql not supported in patterns",
      quoteDec = Prelude.fail "sql not supported in declarations"
    }

qqSQLYearly :: Prelude.String -> ExpQ
qqSQLYearly query =
  let queryFor :: Int -> Prelude.String
      queryFor year =
        Data.Text.pack query
          |> Data.Text.replace "[[YEAR]]" (Text.fromInt year)
          |> Data.Text.unpack
   in [e|
        ( \(year :: Int) ->
            case year of
              2015 -> $(quoteExp sql (queryFor 2015))
              2016 -> $(quoteExp sql (queryFor 2016))
              2017 -> $(quoteExp sql (queryFor 2017))
              2018 -> $(quoteExp sql (queryFor 2018))
              2019 -> $(quoteExp sql (queryFor 2019))
              2020 -> $(quoteExp sql (queryFor 2020))
              2021 -> $(quoteExp sql (queryFor 2021))
              2022 -> $(quoteExp sql (queryFor 2022))
              2023 -> $(quoteExp sql (queryFor 2023))
              2024 -> $(quoteExp sql (queryFor 2024))
              2025 -> $(quoteExp sql (queryFor 2025))
              2026 -> $(quoteExp sql (queryFor 2026))
              2027 -> $(quoteExp sql (queryFor 2027))
              2028 -> $(quoteExp sql (queryFor 2028))
              2029 -> $(quoteExp sql (queryFor 2029))
              _ -> Prelude.error ("Unsupported school year: " ++ Prelude.show year)
        )
        |]

-- |
-- Get the primary key of the last row inserted by the MySQL connection we're
-- currently in. This uses MySQL's `LAST_INSERT_ID()` function and has all the
-- same caveats and limitations. In particular:
--
-- - This gets the last inserted by the current connection. This means we can
--   only use it within a MySQL transaction created using this library. Such a
--   transaction holds on to a reserved connection, preventing other threads
--   from using the connection to make requests in between our `INSERT` query
--   and our use of this function.
-- - If the last insert inserted multiple rows this function will return the
--   primary key of the first of this batch of rows that was inserted.
--
-- In Postgres use a `RETURNING` statement to get inserted id's for inserted
-- rows:
--
--    insertedIds <-
--      Postgres.doQuery
--        [Postgres.sql|!
--          INSERT INTO peanut_butters (brand, chunkiness)
--          VALUES ('Original', 'granite')
--          RETURNING id
--        |]
--        expectQuerySuccess
--
-- For more information: https://dev.mysql.com/doc/refman/8.0/en/getting-unique-id.html
lastInsertedPrimaryKey :: Connection -> Task Query.Error (Maybe Int)
lastInsertedPrimaryKey c =
  let query = queryFromText "SELECT LAST_INSERT_ID()"
   in doQuery
        c
        query
        ( \res -> case res of
            Ok [] -> Task.succeed Nothing
            Ok (x : _) -> Task.succeed x
            Err err -> Task.fail err
        )

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = quoteExp Query.sql,
      quoteType = Prelude.fail "sql not supported in types",
      quotePat = Prelude.fail "sql not supported in patterns",
      quoteDec = Prelude.fail "sql not supported in declarations"
    }

instance PGTypes.PGColumn "boolean" Data.Int.Int16 where
  pgDecode tid tv =
    PGTypes.pgDecode tid tv |> boolToSmallInt

instance PGTypes.PGParameter "real" Float where
  pgEncode tid tv =
    let (i :: Prelude.Float) = Prelude.realToFrac tv
     in PGTypes.pgEncode tid i

boolToSmallInt :: Bool -> Data.Int.Int16
boolToSmallInt b =
  if b
    then 1
    else 0

doIO :: Connection -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)

-- |
-- Hack to allow us to do things like replace "-quoted idenitifier with `-quoted
-- identifiers.
--
-- If we turn on ANSI quotes on the MySQL client MySQL would accept "-quotes and
-- this hack would no longer be necessary. At the moment the Haskell quiz engine
-- code is using the Rails MySQL client, so we cannot enable it for queries from
-- Haskell alone, and are scared to enable it for all queries. This might change
-- in the future.
replace :: Text -> Text -> Query row -> Query row
replace original replacement query =
  query {preparedStatement = Text.replace original replacement (preparedStatement query)}
