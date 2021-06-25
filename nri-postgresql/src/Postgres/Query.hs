{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module Postgres.Query
  ( sql,
    Query (..),
    Error (..),
    TimeoutOrigin (..),
    format,
    details,
  )
where

import Control.Monad (void)
import Data.String (String)
import qualified Data.Text.Encoding
import Database.PostgreSQL.Typed (PGConnection, pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Query (getQueryString, pgQuery)
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Environment
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
import qualified List
import qualified Log
import qualified Log.SqlQuery as SqlQuery
import Postgres.Error (Error (..), TimeoutOrigin (..))
import qualified Postgres.QueryParser as Parser
import qualified Postgres.Settings
import qualified Text
import Prelude (IO)
import qualified Prelude

-- | A Postgres query. Create one of these using the `sql` quasiquoter.
data Query row = Query
  { -- | Run a query against Postgres
    runQuery :: PGConnection -> IO [row],
    -- | The raw SQL string
    sqlString :: Text,
    -- | The query string as extracted from an `sql` quasi quote.
    quasiQuotedString :: Text,
    -- | SELECT / INSERT / UPDATE / INSERT ON DUPLICATE KEY UPDATE ...
    sqlOperation :: Text,
    -- | The main table/view/.. queried.
    queriedRelation :: Text
  }

qqSQL :: String -> ExpQ
qqSQL query = do
  let db =
        Environment.decode Postgres.Settings.decoder
          |> map Postgres.Settings.toPGDatabase
  db' <- runIO db
  void (useTPGDatabase db')
  let meta = Parser.parse (Text.fromList query)
  let op = Text.toList (Parser.sqlOperation meta)
  let rel = Text.toList (Parser.queriedRelation meta)
  [e|
    let q = $(quoteExp pgSQL query)
     in Query
          { runQuery = \c -> pgQuery c q,
            sqlString = Data.Text.Encoding.decodeUtf8 (getQueryString PGTypes.unknownPGTypeEnv q),
            quasiQuotedString = Text.fromList query,
            sqlOperation = op,
            queriedRelation = rel
          }
    |]

-- | Quasi-quoter that allows you to write plain SQL in your code. The query is
-- checked at compile-time using the 'postgresql-typed' library.
--
-- Requires the QuasiQuotes language extension to be enabled.
--
-- > [sql| SELECT name, breed FROM doggos |]
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = qqSQL,
      quoteType = Prelude.error "sql not supported in types",
      quotePat = Prelude.error "sql not supported in patterns",
      quoteDec = Prelude.error "sql not supported in declarations"
    }

format :: Query row -> Text.Text
format query =
  let fixBang query_ =
        case Text.uncons query_ of
          Just ('!', rest) -> "! " ++ Text.trim rest
          Just _ -> query_
          Nothing -> query_
      indent string =
        "    " ++ string
   in quasiQuotedString query
        |> Text.split "\n"
        |> List.map Text.trim
        |> Text.join "\n        "
        |> fixBang
        |> indent

details :: Query row -> SqlQuery.Details -> SqlQuery.Details
details query connectionDetails =
  connectionDetails
    { SqlQuery.query = Just (Log.mkSecret (sqlString query)),
      SqlQuery.queryTemplate = Just (quasiQuotedString query),
      SqlQuery.sqlOperation = Just (sqlOperation query),
      SqlQuery.queriedRelation = Just (queriedRelation query)
    }
