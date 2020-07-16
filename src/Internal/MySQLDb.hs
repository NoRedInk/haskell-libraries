{-# LANGUAGE QuasiQuotes #-}

module Internal.MySQLDb
  ( Connection (..),
    SingleOrPool (..),
    handleError,
    TransactionCount (..),
    withConnection,
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

-- HELPER

doIO :: Connection -> IO a -> Task x a
doIO conn io =
  Platform.doAnything (doAnything conn) (io |> map Ok)
