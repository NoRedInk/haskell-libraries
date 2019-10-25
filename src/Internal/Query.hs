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
    Query (Query),
    expectOne,
    Error (ExpectChange, MoreRowsThanExpected),
  )
where

import Control.Monad (fail)
import qualified Data.Int
import Data.String (String)
import qualified Database.MySQL.Simple as MySQL.Simple
import Database.PostgreSQL.Typed (pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Environment
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
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
