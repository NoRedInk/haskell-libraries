{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module MySQL.Query
  ( sql,
    Query (..),
    Error (..),
    TimeoutOrigin (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad (fail, void)
import qualified Data.Int
import Data.String (String)
import qualified Data.Text
import qualified Database.MySQL.Base as Base
import Database.PostgreSQL.Typed (pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import qualified Database.PostgreSQL.Typed.SQLToken as SQLToken
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Environment
import qualified Internal.QueryParser as Parser
import qualified Internal.Time as Time
import Language.Haskell.TH (ExpQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
import qualified List
import qualified Log
import MySQL.Internal (inToAny)
import qualified Platform
import qualified Postgres.Settings
import qualified Text
import qualified Tuple
import Prelude (Show (show), fromIntegral)
import qualified Prelude

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
      { -- | The query as a prepared statement
        preparedStatement :: Text,
        -- | The parameters that fill the placeholders in this query
        params :: Log.Secret [Base.MySQLValue],
        -- | The query string as extracted from an `sql` quasi quote.
        quasiQuotedString :: Text,
        -- | SELECT / INSERT / UPDATE / INSERT ON DUPLICATE KEY UPDATE ...
        sqlOperation :: Text,
        -- | The main table/view/.. queried.
        queriedRelation :: Text
      }
  deriving (Show)

data Error
  = Timeout TimeoutOrigin Time.Interval
  | UniqueViolation Text
  | Other Text [Platform.Context]

instance Show Error where
  show (Timeout _ interval) = "Query timed out after " ++ Data.Text.unpack (Text.fromFloat (Time.seconds interval)) ++ " seconds"
  show (UniqueViolation err) = "Query violated uniqueness constraint: " ++ Data.Text.unpack err
  show (Other msg _) = "Query failed with unexpected error: " ++ Data.Text.unpack msg

instance Exception.Exception Error

data TimeoutOrigin = ClientTimeout | ServerTimeout
  deriving (Show)

qqSQL :: String -> ExpQ
qqSQL query = do
  let db =
        Environment.decode Postgres.Settings.decoder
          |> map Postgres.Settings.toPGDatabase
  db' <- runIO db
  void (useTPGDatabase db')
  let meta = Parser.parse (Data.Text.pack query)
  let op = Data.Text.unpack (Parser.sqlOperation meta)
  let rel = Data.Text.unpack (Parser.queriedRelation meta)
  let sqlTokens = SQLToken.sqlTokens query
  let preparedStatement' =
        toPreparedQuery sqlTokens
          |> Data.Text.pack
          |> Text.replace "monolith." ""
          |> Data.Text.unpack
  -- We run the postgresql-typed quasi quoter for it's type-checking logic, but
  -- we're uninterested in the results it produces. At runtime we're taking our
  -- queries straight to MySQL.
  _ <- quoteExp pgSQL (Data.Text.unpack (inToAny (Data.Text.pack query)))
  [e|
    Query
      { preparedStatement = preparedStatement',
        params = $(preparedQueryParamsE sqlTokens),
        quasiQuotedString =
          query
            |> Data.Text.pack
            |> inToAny,
        sqlOperation = op,
        queriedRelation = rel
      }
    |]

-- Take a quasiquoted query like this:
--
--     SELECT first_name FROM monolith.users WHERE id = ${userId} AND username = ${username}
--
-- And turn it into a prepared query like this:
--
--     SELECT first_name FROM monolith.users WHERE id = $1 AND username = $2
--
toPreparedQuery :: [SQLToken.SQLToken] -> String
toPreparedQuery tokens =
  tokens
    |> List.foldl
      ( \token (n, res) ->
          case token of
            SQLToken.SQLExpr _ -> (n + 1, SQLToken.SQLParam n : res)
            SQLToken.SQLParam _ -> (n, token : res)
            SQLToken.SQLToken _ -> (n, token : res)
            SQLToken.SQLQMark _ -> (n, token : res)
      )
      (1, [])
    |> Tuple.second
    |> (\tokens' -> Prelude.showList (List.reverse tokens') "")

-- A Given quasi quoted query string might look like this:
--
--     SELECT first_name FROM monolith.users WHERE id = ${userId} AND username = ${username}
--
-- This function will return generated Haskell code containing a list of encoded
-- MySQL column values, one for each placeholder in the query. For example, the
-- generated code for the query above will look like this:
--
--     [
--         mysqlEncode userId,
--         mysqlEncode username
--     ]
--
preparedQueryParamsE :: [SQLToken.SQLToken] -> TH.ExpQ
preparedQueryParamsE sqlTokens = do
  names <- Prelude.traverse TH.lookupValueName (placeholderNames sqlTokens)
  names
    |> List.filterMap identity
    |> map (TH.AppE (TH.VarE 'mysqlEncode) << TH.VarE)
    |> TH.ListE
    |> TH.AppE (TH.VarE 'Log.mkSecret)
    |> Prelude.pure

-- | A type class describing how to encode values for MySQL. The `MySQLValue`
-- type is defined by our MySQL driver library (`mysql-haskell`).
--
-- This is the counterpart of the PGColumn typeclass in `postgresql-typed` for
-- Postgres values.
class MySQLColumn a where
  mysqlEncode :: a -> Base.MySQLValue

instance MySQLColumn Int where
  mysqlEncode = Base.MySQLInt64

instance MySQLColumn Text where
  mysqlEncode = Base.MySQLText

-- A Given quasi quoted query string might look like this:
--
--     SELECT first_name FROM monolith.users WHERE id = ${userId} AND username = ${username}
--
-- This function will return a list of the placeholder names. In the example
-- above that would be:
--
--     ["userId", "username"]
--
placeholderNames :: [SQLToken.SQLToken] -> [String]
placeholderNames tokens =
  tokens
    |> List.filterMap
      ( \token ->
          case token of
            SQLToken.SQLExpr name -> Just name
            SQLToken.SQLParam _ -> Nothing
            SQLToken.SQLToken _ -> Nothing
            SQLToken.SQLQMark _ -> Nothing
      )

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = qqSQL,
      quoteType = fail "sql not supported in types",
      quotePat = fail "sql not supported in patterns",
      quoteDec = fail "sql not supported in declarations"
    }

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

-- |
-- Several monolith tables use smaller int sizes to represent
-- enumerables; this type class allows us to extract them safely
-- and without too much ceremony
instance PGTypes.PGColumn "smallint" Int where
  pgDecode tid tv =
    let (i :: Data.Int.Int16) = PGTypes.pgDecode tid tv
     in fromIntegral i

instance PGTypes.PGParameter "smallint" Int where
  pgEncode tid tv =
    let (i :: Data.Int.Int16) = fromIntegral tv
     in PGTypes.pgEncode tid i
