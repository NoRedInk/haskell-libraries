{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module Internal.Query
  ( sql,
    Query (..),
    expectOne,
    Error (ExpectChange, MoreRowsThanExpected),
  )
where

import Control.Monad (fail)
import qualified Data.Int
import Data.String (String)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.MySQL.Simple as MySQL.Simple
import Database.PostgreSQL.Typed (PGConnection, pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Query (getQueryString, pgQuery)
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Environment
import qualified Internal.Query.Parser as Parser
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
import Nri.Prelude
import qualified Postgres.Settings

-- |
-- A wrapper around a `postgresql-typed` query. This type has a number of
-- different purposes.
--
-- 1. By not using the native `postgresql-typed` Query type we ensure all our
--    queries are made through the `database` package, which in turn ensures
--    they're all logged and traced.
-- 2. The `postgresql-typed` query type is parametrized over `q`. It's not
--    immediately clear what this `q` means. Our query type is parametrized over
--    `row`, the type of the rows this query returns. That's conceptually much
--    easier to gok.
-- 3. We attach a bunch of meta data that can be derived from the wrapped
--    `postgresql-typed` query type. Although this information could be
--    calculated on the fly for each query, attaching it to our own `Query`
--    type ensures we only need to calculate the metadata once, at compile time.
data Query row
  = Query
      { -- | Run a query against Postgres
        runQuery :: PGConnection -> IO [row],
        -- | The raw SQL string
        sqlString :: Text,
        -- | The query string as extracted from an `sql` quasi quote.
        quasiQuotedString :: Text,
        -- | SELECT / INSERT / UPDATE / ...
        sqlOperation :: Text,
        -- | The main table/view/.. queried.
        queriedRelation :: Text
      }

qqSQL :: String -> ExpQ
qqSQL query = do
  let db = map (either panic Postgres.Settings.toPGDatabase) (Environment.decode Postgres.Settings.decoder)
  db' <- runIO db
  void (useTPGDatabase db')
  let meta = Parser.parse (toS query)
  let op = Data.Text.unpack (Parser.sqlOperation meta)
  let rel = Data.Text.unpack (Parser.queriedRelation meta)
  [e|
    let q = $(quoteExp pgSQL query)
     in Query
          { runQuery = \c -> pgQuery c q,
            sqlString = Data.Text.Encoding.decodeUtf8 (getQueryString PGTypes.unknownPGTypeEnv q),
            quasiQuotedString = query,
            sqlOperation = op,
            queriedRelation = rel
          }
    |]

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

-- |
-- The default `Int` type we use in our Haskell code is an `Int64`. This
-- corresponds to a `bigint` in SQL. Most of our MySQL tables use regular
-- `integers` though, which are 32 bits.
--
-- In our Postgres databases we default to using `bigint` for columns, but in
-- our legacy MySQL database we have missed that boat. We'd still like to be
-- able to write our Haskell default Ints to/from MySQL without ceremony, so
-- we add these instances to make it possible.
instance PGTypes.PGColumn "integer" Int where
  pgDecode tid tv =
    let (i :: Data.Int.Int32) = PGTypes.pgDecode tid tv
     in fromIntegral i

instance PGTypes.PGParameter "integer" Int where
  pgEncode tid tv =
    let (i :: Data.Int.Int32) = fromIntegral tv
     in PGTypes.pgEncode tid i

instance PGTypes.PGColumn t a => PGTypes.PGColumn t (MySQL.Simple.Only a) where
  pgDecode tid tv =
    PGTypes.pgDecode tid tv
      |> MySQL.Simple.Only
