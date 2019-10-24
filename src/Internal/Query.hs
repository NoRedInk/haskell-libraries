{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module Internal.Query
  ( sql,
    Query (Query),
    execute,
    expectOne,
    Error (ExpectChange, MoreRowsThanExpected),
    withLogContext,
  )
where

import Control.Monad (fail)
import Data.String (String)
import Database.PostgreSQL.Typed (pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Query (PGQuery, getQueryString)
import Database.PostgreSQL.Typed.Types (unknownPGTypeEnv)
import qualified Environment
import Internal.GenericDb (Connection, logContext, runTaskWithConnection)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
import qualified Log
import Nri.Prelude
import qualified Postgres.Settings

-- | A wrapper around a `postgresql-typed` query. This will ensure all queries
--   we handle in this module will be created using helpers in this module.
--   I.E.: It will be impossible to use the `pgSql` function from the
--   `postgresql-typed` library directly.
newtype Query q
  = Query q
  deriving (Show)

qqSQL :: String -> ExpQ
qqSQL query = do
  let db = map (either panic Postgres.Settings.toPGDatabase) (Environment.decode Postgres.Settings.decoder)
  db' <- runIO db
  void (useTPGDatabase db')
  [e|Query $(quoteExp pgSQL query)|]

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = qqSQL,
      quoteType = fail "sql not supported in types",
      quotePat = fail "sql not supported in patterns",
      quoteDec = fail "sql not supported in declarations"
    }

data Error
  = ExpectChange Text
  | MoreRowsThanExpected Text
  deriving (Show)

expectOne ::
  Text ->
  [a] ->
  Task Error a
expectOne queryString rows =
  case rows of
    [] -> throwError <| ExpectChange queryString
    [x] -> pure x
    _ -> throwError <| MoreRowsThanExpected queryString

execute ::
  (HasCallStack, Show q) =>
  (q -> conn -> IO [a]) ->
  Connection conn ->
  Query q ->
  Task e [a]
execute runQuery conn (Query query) = do
  withFrozenCallStack Log.debug (show query)
  runTaskWithConnection conn (runQuery query)

-- TODO: Figure out if there's a way to get this into `execute` above without
-- causing errors about constraints, etc.
withLogContext :: PGQuery q a => Connection c -> Query q -> Task e b -> Task e b
withLogContext conn (Query query) task =
  Log.withContext "database-query" [Log.context "query" queryInfo] task
  where
    queryInfo = Log.QueryInfo
      { Log.queryText = toS <| getQueryString unknownPGTypeEnv query,
        Log.queryConn = logContext conn
      }
