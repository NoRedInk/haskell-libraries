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
    PreparedMySQLQuery (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad (fail, void)
import qualified Data.Int
import Data.String (String)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.MySQL.Base as MySQL
import qualified Database.Persist.MySQL as MySQL
import Database.PostgreSQL.Typed (PGConnection, pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Query (getQueryString, pgQuery)
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
import MySQL.Internal (inToAny)
import qualified Platform
import qualified Postgres.Settings
import qualified Text
import qualified Tuple
import Prelude (IO, Show (show), fromIntegral)
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
      { -- | Run a query against Postgres
        runQuery :: PGConnection -> IO [row],
        -- | The raw SQL string
        sqlString :: Text,
        -- | The query as a prepared statement
        preparedMySQLQuery :: PreparedMySQLQuery,
        -- | The query string as extracted from an `sql` quasi quote.
        quasiQuotedString :: Text,
        -- | SELECT / INSERT / UPDATE / INSERT ON DUPLICATE KEY UPDATE ...
        sqlOperation :: Text,
        -- | The main table/view/.. queried.
        queriedRelation :: Text
      }

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
  [e|
    let q = $(quoteExp pgSQL (Data.Text.unpack (inToAny (Data.Text.pack query))))
     in Query
          { runQuery = \c -> pgQuery c q,
            sqlString = Data.Text.Encoding.decodeUtf8 (getQueryString PGTypes.unknownPGTypeEnv q),
            preparedMySQLQuery = $(preparedQueryE query),
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

data PreparedMySQLQuery
  = PreparedMySQLQuery
      { preparedStatement :: String,
        params :: [MySQL.MySQLValue]
      }
  deriving (Show)

preparedQueryE :: String -> TH.ExpQ
preparedQueryE query = do
  let sqlTokens = SQLToken.sqlTokens query
  let preparedStatement' = toPreparedQuery sqlTokens
  -- The query string passed in will contain a number of placeholder variables,
  -- for example: `${id}` or `${name}`. Below is some template haskell for
  -- generating code for a list of encoded MySQL values. That code will look
  -- something like this:
  --
  --     [
  --         toMySQLValue id,
  --         toMySQLValue name
  --     ]
  names <- Prelude.traverse TH.lookupValueName (placeholderNames sqlTokens)
  let params' =
        names
          |> List.filterMap identity
          |> map (TH.AppE (TH.VarE 'toMySQLValue) << TH.VarE)
          |> TH.ListE
          |> Prelude.pure
  [e|PreparedMySQLQuery preparedStatement' $(params')|]

-- | A type class describing how to encode values for MySQL. The `MySQLValue`
-- type is defined by our MySQL driver library (`mysql-haskell`).
class ToMySQLValue a where
  toMySQLValue :: a -> MySQL.MySQLValue

instance ToMySQLValue Int where
  toMySQLValue = MySQL.MySQLInt64

instance ToMySQLValue Text where
  toMySQLValue = MySQL.MySQLText

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

instance PGTypes.PGColumn t a => PGTypes.PGColumn t (MySQL.Single a) where
  pgDecode tid tv =
    PGTypes.pgDecode tid tv
      |> MySQL.Single

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
