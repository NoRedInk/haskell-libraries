{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Helpers for running queries.

This module expose some helpers for running postgresql-typed queries. They
return the correct amount of results in a Servant handler, or throw a
Rollbarred error.

-}
module Internal.Query
  ( sql,
    Query,
    execute,
    modifyExactlyOne,
    Error (ExpectChange, MoreRowsThanExpected)
    )
where

import Control.Monad (fail)
import Data.String (String)
import Database.PostgreSQL.Typed (pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import qualified Environment
import Internal.GenericDb (Connection, runTaskWithConnection)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType)
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

-- | Modify exactly one row or fail with a 500.
--
--   @
--     modifyExactlyOne c
--       [pgSQL|
--         INSERT INTO my_table (name)
--           VALUES ($1)
--         RETURNING id, name
--       |]
--   @
modifyExactlyOne
  :: (HasCallStack, Show q)
  => (q -> conn -> IO [a])
  -> Connection conn
  -> Query q
  -> Task Error a
modifyExactlyOne runQuery c query = do
  row <- withFrozenCallStack execute runQuery c query
  case row of
    [] -> throwError <| ExpectChange (show query)
    [x] -> pure x
    _ -> throwError <| MoreRowsThanExpected (show query)

execute
  :: (HasCallStack, Show q)
  => (q -> conn -> IO [a])
  -> Connection conn
  -> Query q
  -> Task e [a]
execute runQuery conn (Query query) = do
  withFrozenCallStack Log.debug (show query)
  runTaskWithConnection conn
    (runQuery query)
