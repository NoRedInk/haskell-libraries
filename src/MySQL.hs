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
  ( -- * Connection
    Connection,
    connection,
    readiness,

    -- * Settings
    Settings.Settings,
    Settings.decoder,

    -- * Querying
    sql,
    doQuery,
    Query.Query,
    Error.Error (..),
    Query.Info (..),
    Query.ConnectionInfo (..),
    -- These type classes are for serializing and deserializing data from the
    -- database.
    --
    -- The intent for the PGColumn instance that `postgresql-typed` gives us
    -- and the `MySQLColumn` instance that we model after it is that it
    -- describes safe conversions of database types into Haskell types. It's
    -- intentionally not a decoder with a failure mode.
    --
    -- If we try to shoehorn decoding operations into it we have to resort to
    -- using functions like `Debug.todo ""` in places where decoding fails.
    -- That's not a great pattern to embrace in our code base.
    --
    -- So to prevent ourselves to go down this road we don't expose
    -- `mysqlDecode`, preventing us from defining custom `PGColumn` instances.
    -- We can derive them on newtypes, which is fine and safe. If we want to
    -- read data from the database and transform it into other data in ways
    -- that can fail we can still do so, but not as part of MySQL parsing
    -- logic.
    MySQL.MySQLColumn.MySQLColumn,
    MySQL.MySQLParameter.MySQLParameter,

    -- * Handling transactions
    transaction,
    inTestTransaction,

    -- * Helpers for uncommon queries
    unsafeBulkifyInserts,
    onConflictUpdate,
    onDuplicateDoNothing,
    sqlYearly,
    replace,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Lens.Regex.Text as R
import qualified Data.Acquire
import qualified Data.ByteString.Lazy
import qualified Data.Int
import qualified Data.Pool
import qualified Data.Proxy as Proxy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.MySQL.Base as Base
import qualified Database.MySQL.Connection
import qualified Database.MySQL.Protocol.Packet
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Debug
import qualified GHC.Stack as Stack
import qualified Health
import qualified Internal.CaselessRegex as CaselessRegex
import qualified Internal.Error as Error
import qualified Internal.Time as Time
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import qualified List
import qualified Log
import qualified MySQL.FromRow as FromRow
import qualified MySQL.MySQLColumn
import qualified MySQL.MySQLParameter
import qualified MySQL.Query as Query
import qualified MySQL.Settings as Settings
import qualified Platform
import qualified System.IO.Streams as Streams
import qualified Task
import qualified Text
import qualified Tuple
import qualified Prelude

newtype TransactionCount
  = TransactionCount Int
  deriving (Eq)

data Connection
  = Connection
      { doAnything :: Platform.DoAnythingHandler,
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
        singleOrPool :: SingleOrPool Base.MySQLConn,
        logContext :: Query.ConnectionInfo,
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
      Prelude.pure
        ( Connection
            doAnything
            pool
            (toConnectionLogContext settings)
            (Settings.mysqlQueryTimeoutSeconds settings)
        )
    release Connection {singleOrPool} =
      case singleOrPool of
        Pool pool -> Data.Pool.destroyAllResources pool
        Single _ c -> Base.close c
    stripes =
      Settings.mysqlPool settings
        |> Settings.mysqlPoolStripes
        |> Settings.unMysqlPoolStripes
        |> Prelude.fromIntegral
    maxIdleTime =
      Settings.mysqlPool settings
        |> Settings.mysqlPoolMaxIdleTime
        |> Settings.unMysqlPoolMaxIdleTime
    size =
      Settings.mysqlPool settings
        |> Settings.mysqlPoolSize
        |> Settings.unMysqlPoolSize
        |> Prelude.fromIntegral
    database = toConnectInfo settings

--
-- CONNECTION HELPERS
--

toConnectionLogContext :: Settings.Settings -> Query.ConnectionInfo
toConnectionLogContext settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Settings.unDatabase (Settings.database connectionSettings)
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Query.UnixSocket
            (Data.Text.pack (Settings.unSocket socket))
            database
        Settings.ConnectTcp host port ->
          Query.TcpSocket
            (Settings.unHost host)
            (Text.fromInt (Settings.unPort port))
            database

toConnectInfo :: Settings.Settings -> Base.ConnectInfo
toConnectInfo settings =
  let connectionSettings = Settings.mysqlConnection settings
      database = Data.Text.Encoding.encodeUtf8 (Settings.unDatabase (Settings.database connectionSettings))
      user = Data.Text.Encoding.encodeUtf8 (Settings.unUser (Settings.user connectionSettings))
      password = Data.Text.Encoding.encodeUtf8 (Log.unSecret (Settings.unPassword (Settings.password connectionSettings)))
   in case Settings.connection connectionSettings of
        Settings.ConnectSocket socket ->
          Base.defaultConnectInfoMB4
            { Base.ciHost = Settings.unSocket socket,
              Base.ciUser = user,
              Base.ciPassword = password,
              Base.ciDatabase = database,
              Base.ciCharset = Database.MySQL.Connection.utf8mb4_unicode_ci
            }
        Settings.ConnectTcp host port ->
          Base.defaultConnectInfoMB4
            { Base.ciHost = Data.Text.unpack (Settings.unHost host),
              Base.ciUser = user,
              Base.ciPassword = password,
              Base.ciDatabase = database,
              Base.ciPort = Prelude.fromIntegral (Settings.unPort port),
              Base.ciCharset = Database.MySQL.Connection.utf8mb4_unicode_ci
            }

--
-- READINESS
--

-- |
-- Check that we are ready to be take traffic.
readiness :: Stack.HasCallStack => Connection -> Health.Check
readiness conn = Health.mkCheck "mysql" <| do
  log <- Platform.silentHandler
  Stack.withFrozenCallStack executeQuery conn (queryFromText "SELECT 1")
    |> Task.map (\(_ :: [Int]) -> ())
    |> Task.mapError (Data.Text.pack << Exception.displayException)
    |> Task.attempt log
    |> map Health.fromResult

queryFromText :: Text -> Query.Query a
queryFromText text =
  Query.Query
    { Query.preparedStatement = text,
      Query.params = Log.mkSecret [],
      Query.quasiQuotedString = text,
      Query.sqlOperation = text,
      Query.queriedRelation = ""
    }

--
-- EXECUTE QUERIES
--
class MySqlQueryable query result | result -> query where
  executeSql :: Stack.HasCallStack => Connection -> Query.Query query -> Task Error.Error result

instance FromRow.FromRow (FromRow.CountColumns row) row => MySqlQueryable row [row] where
  executeSql c query = Stack.withFrozenCallStack executeQuery c query

instance MySqlQueryable () Int where
  executeSql c query = Stack.withFrozenCallStack executeCommand c query

instance MySqlQueryable () () where
  executeSql c query = Stack.withFrozenCallStack executeCommand_ c query

doQuery ::
  Stack.HasCallStack =>
  (MySqlQueryable row result) =>
  Connection ->
  Query.Query row ->
  (Result Error.Error result -> Task e a) ->
  Task e a
doQuery conn query handleResponse =
  Stack.withFrozenCallStack executeSql conn query
    -- Handle the response before wrapping the operation in a context. This way,
    -- if the response handling logic creates errors, those errors can inherit
    -- context values like the query string.
    |> Task.map Ok
    |> Task.onError (Task.succeed << Err)
    |> Task.andThen handleResponse

executeQuery ::
  forall row.
  Stack.HasCallStack =>
  FromRow.FromRow (FromRow.CountColumns row) row =>
  Connection ->
  Query.Query row ->
  Task Error.Error [row]
executeQuery conn query =
  withConnection conn <| \backend ->
    let params = Query.params query |> Log.unSecret
        toRows stream =
          stream
            |> Streams.map (FromRow.fromRow (Proxy.Proxy :: Proxy.Proxy (FromRow.CountColumns row)))
            |> andThen Streams.toList
        encodedQuery = toBaseQuery query
     in Base.query backend encodedQuery params
          |> andThen (toRows << Tuple.second)
          |> handleMySqlException
          |> Platform.doAnything (doAnything conn)
          |> Stack.withFrozenCallStack traceQuery conn (Just List.length) query
          |> withTimeout conn

executeCommand_ :: Stack.HasCallStack => Connection -> Query.Query () -> Task Error.Error ()
executeCommand_ conn query = do
  _ <- Stack.withFrozenCallStack executeCommand conn query
  Task.succeed ()

executeCommand :: Stack.HasCallStack => Connection -> Query.Query () -> Task Error.Error Int
executeCommand conn query =
  withConnection conn <| \backend ->
    let params = Query.params query |> Log.unSecret
        encodedQuery = toBaseQuery query
     in Base.execute backend encodedQuery params
          |> map (Prelude.fromIntegral << Base.okLastInsertID)
          |> handleMySqlException
          |> Platform.doAnything (doAnything conn)
          |> Stack.withFrozenCallStack traceQuery conn Nothing query
          |> withTimeout conn

traceQuery :: Stack.HasCallStack => Connection -> Maybe (a -> Int) -> Query.Query q -> Task e a -> Task e a
traceQuery conn maybeCountRows query task =
  let infoForContext = Query.mkInfo query (logContext conn)
   in Stack.withFrozenCallStack Platform.tracingSpan "MySQL Query" <| do
        res <- Platform.finally task (Platform.setTracingSpanDetails infoForContext)
        -- If we end up here it means the query succeeded. Overwrite the tracing
        -- details to contain the amount of selected rows. This information can be
        -- useful when debugging slow queries.

        Platform.setTracingSpanDetails
          <| case maybeCountRows of
            Just countRows ->
              infoForContext {Query.infoRowsReturned = countRows res}
            Nothing ->
              infoForContext
        Prelude.pure res

toBaseQuery :: Query.Query row -> Base.Query
toBaseQuery query =
  Query.preparedStatement query
    |> Data.Text.Encoding.encodeUtf8
    |> Data.ByteString.Lazy.fromStrict
    |> Base.Query

handleMySqlException :: Prelude.IO result -> Prelude.IO (Result Error.Error result)
handleMySqlException io =
  Exception.catches
    (map Ok io)
    [ Exception.Handler
        ( \(Base.ERRException err) ->
            let errCode = Database.MySQL.Protocol.Packet.errCode err
                errState = Database.MySQL.Protocol.Packet.errState err
                errMsg = Database.MySQL.Protocol.Packet.errMsg err
             in Error.Other
                  ("MySQL query failed with error code " ++ Text.fromInt (Prelude.fromIntegral errCode))
                  [ Log.context "error state" (Data.Text.Encoding.decodeUtf8 errState),
                    Log.context "error message" (Data.Text.Encoding.decodeUtf8 errMsg)
                  ]
                  |> Err
                  |> Prelude.pure
        ),
      Exception.Handler
        ( \Base.NetworkException ->
            Error.Other "MySQL query failed with a network exception" []
              |> Err
              |> Prelude.pure
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
              |> (\err' -> Error.Other ("MySQL query failed with unexpected error: " ++ Debug.toString err') [])
              |> Err
              |> Prelude.pure
        )
    ]

withTimeout :: Connection -> Task Error.Error a -> Task Error.Error a
withTimeout conn task =
  if Time.microseconds (timeout conn) > 0
    then
      Task.timeout
        (Time.milliseconds (timeout conn))
        (Error.Timeout Error.ClientTimeout (timeout conn))
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
    Platform.bracketWithError
      (begin conn)
      ( \succeeded () ->
          case succeeded of
            Platform.Succeeded -> commit conn
            Platform.Failed -> rollback conn
            Platform.FailedWith _ -> rollback conn
      )
      (\() -> func conn)

-- | Run code in a transaction, then roll that transaction back.
--   Useful in tests that shouldn't leave anything behind in the DB.
inTestTransaction :: Connection -> (Connection -> Task x a) -> Task x a
inTestTransaction conn' func =
  withTransaction conn' <| \conn ->
    Platform.bracketWithError
      (do rollbackAll conn; begin conn)
      (\_ () -> rollbackAll conn)
      (\() -> func conn)

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
      Nothing -> Prelude.pure ()
      Just (TransactionCount 0) -> executeCommand_ conn (queryFromText "BEGIN")
      Just (TransactionCount current) -> executeCommand_ conn (queryFromText ("SAVEPOINT pgt" ++ Text.fromInt current))

-- | Rollback to the most recent 'begin'.
rollback :: Connection -> Task e ()
rollback conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> Prelude.pure ()
      Just (TransactionCount 0) -> executeCommand_ conn (queryFromText "ROLLBACK")
      Just (TransactionCount current) ->
        executeCommand_ conn (queryFromText ("ROLLBACK TO SAVEPOINT pgt" ++ Text.fromInt current))

-- | Commit the most recent 'begin'.
commit :: Connection -> Task e ()
commit conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> Prelude.pure ()
      Just (TransactionCount 0) -> executeCommand_ conn (queryFromText "COMMIT")
      Just (TransactionCount current) -> executeCommand_ conn (queryFromText ("RELEASE SAVEPOINT pgt" ++ Text.fromInt current))

-- | Rollback all active 'begin's.
rollbackAll :: Connection -> Task e ()
rollbackAll conn =
  throwRuntimeError
    <| case transactionCount conn of
      Nothing -> Prelude.pure ()
      Just _ -> executeCommand_ conn (queryFromText "ROLLBACK")

throwRuntimeError :: Task Error.Error a -> Task e a
throwRuntimeError task =
  Task.onError
    ( \err ->
        Exception.displayException err
          |> Data.Text.pack
          |> Platform.unsafeThrowException
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
withConnection :: Stack.HasCallStack => Connection -> (Base.MySQLConn -> Task e a) -> Task e a
withConnection conn func =
  let acquire :: Data.Pool.Pool conn -> Task x (conn, Data.Pool.LocalPool conn)
      acquire pool =
        Stack.withFrozenCallStack Log.withContext "acquiring MySQL connection from pool" []
          <| doIO conn
          <| Data.Pool.takeResource pool
      --
      release :: Data.Pool.Pool conn -> Platform.Succeeded -> (conn, Data.Pool.LocalPool conn) -> Task y ()
      release pool succeeded (c, localPool) =
        doIO conn
          <| case succeeded of
            Platform.Succeeded ->
              Data.Pool.putResource localPool c
            Platform.Failed ->
              Data.Pool.destroyResource pool localPool c
            Platform.FailedWith _ ->
              Data.Pool.destroyResource pool localPool c
   in --
      case singleOrPool conn of
        (Single _ c) ->
          func c
        --
        (Pool pool) ->
          Platform.bracketWithError (acquire pool) (release pool) (Tuple.first >> func)

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
unsafeBulkifyInserts ::
  (Query.Query () -> a) ->
  Query.Query () ->
  [Query.Query ()] ->
  a
unsafeBulkifyInserts runCombined first rest =
  first
    { Query.preparedStatement =
        Data.Text.intercalate
          ","
          ( Query.preparedStatement first
              : map (Text.dropLeft splitAt << Query.preparedStatement) rest
          ),
      Query.params =
        Prelude.traverse Query.params (first : rest)
          |> map List.concat
    }
    |> runCombined
  where
    splitAt =
      Query.preparedStatement first
        |> Data.Text.toLower
        |> Data.Text.breakOn "values"
        |> Tuple.first
        |> Text.length
        |> (+) 6

-- | Appends a query with `ON DUPLICATE KEY UPDATE` to allow updating in case
-- the key isn't unique.
onConflictUpdate :: [Text] -> Query.Query () -> Query.Query ()
onConflictUpdate columns query =
  let onDuplicateKeyUPDATE = "ON DUPLICATE KEY UPDATE"
   in query
        { Query.preparedStatement =
            Text.join
              " "
              [ Query.preparedStatement query,
                onDuplicateKeyUPDATE,
                columns
                  |> List.map (\column -> column ++ " = VALUES(" ++ column ++ ")")
                  |> Text.join ","
              ],
          Query.sqlOperation = Query.sqlOperation query ++ " " ++ onDuplicateKeyUPDATE
        }

-- | Use for insert queries that are allowed to fail. In Postgres we would use
-- an `ON CONFLICT DO NOTHING` clause for this, but MySQL doesn't support it.
-- `MySQL` recommends using `INSERT IGNORE` syntax, but that Postgres does not
-- support. This helper hacks the `IGNORE` clause into a query after we run
-- Postgres compile-time checks.
onDuplicateDoNothing :: Query.Query () -> Query.Query ()
onDuplicateDoNothing query =
  query
    { Query.preparedStatement =
        Lens.over
          ([CaselessRegex.caselessRegex|^\s*INSERT\s+INTO|] << R.match)
          (\_ -> "INSERT IGNORE INTO")
          (Query.preparedStatement query)
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
sqlYearly :: QQ.QuasiQuoter
sqlYearly =
  QQ.QuasiQuoter
    { QQ.quoteExp = qqSQLYearly,
      QQ.quoteType = Prelude.fail "sql not supported in types",
      QQ.quotePat = Prelude.fail "sql not supported in patterns",
      QQ.quoteDec = Prelude.fail "sql not supported in declarations"
    }

qqSQLYearly :: Prelude.String -> TH.ExpQ
qqSQLYearly query =
  let queryFor :: Int -> Prelude.String
      queryFor year =
        Data.Text.pack query
          |> Data.Text.replace "[[YEAR]]" (Text.fromInt year)
          |> Data.Text.unpack
   in [e|
        ( \(year :: Int) ->
            case year of
              2015 -> $(QQ.quoteExp sql (queryFor 2015))
              2016 -> $(QQ.quoteExp sql (queryFor 2016))
              2017 -> $(QQ.quoteExp sql (queryFor 2017))
              2018 -> $(QQ.quoteExp sql (queryFor 2018))
              2019 -> $(QQ.quoteExp sql (queryFor 2019))
              2020 -> $(QQ.quoteExp sql (queryFor 2020))
              2021 -> $(QQ.quoteExp sql (queryFor 2021))
              2022 -> $(QQ.quoteExp sql (queryFor 2022))
              2023 -> $(QQ.quoteExp sql (queryFor 2023))
              2024 -> $(QQ.quoteExp sql (queryFor 2024))
              2025 -> $(QQ.quoteExp sql (queryFor 2025))
              2026 -> $(QQ.quoteExp sql (queryFor 2026))
              2027 -> $(QQ.quoteExp sql (queryFor 2027))
              2028 -> $(QQ.quoteExp sql (queryFor 2028))
              2029 -> $(QQ.quoteExp sql (queryFor 2029))
              _ -> Prelude.error ("Unsupported school year: " ++ Data.Text.unpack (Text.fromInt year))
        )
        |]

sql :: QQ.QuasiQuoter
sql =
  QQ.QuasiQuoter
    { QQ.quoteExp = QQ.quoteExp Query.sql,
      QQ.quoteType = Prelude.fail "sql not supported in types",
      QQ.quotePat = Prelude.fail "sql not supported in patterns",
      QQ.quoteDec = Prelude.fail "sql not supported in declarations"
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

doIO :: Connection -> Prelude.IO a -> Task x a
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
replace :: Text -> Text -> Query.Query row -> Query.Query row
replace original replacement query =
  query
    { Query.preparedStatement =
        Text.replace original replacement (Query.preparedStatement query)
    }
