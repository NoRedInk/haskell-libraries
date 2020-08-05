{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries. They
-- return the correct amount of results in a Servant handler, or throw a
-- Rollbarred error.
module MySQL.Query
  ( sql,
    Query (..),
    PrepareQuery (..),
    Error (..),
    TimeoutOrigin (..),
    MySQLColumn (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import Control.Monad (fail, void)
import qualified Data.Int
import Data.Proxy (Proxy (Proxy))
import Data.String (String)
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Word
import qualified Database.MySQL.Base as Base
import Database.PostgreSQL.Typed (pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import qualified Database.PostgreSQL.Typed.SQLToken as SQLToken
import qualified Environment
import Internal.Error (Error (..), TimeoutOrigin (..))
import Internal.Instances ()
import qualified Internal.QueryParser as Parser
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH (ExpQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax (runIO)
import qualified List
import qualified Log
import MySQL.Internal (inToAny)
import qualified Postgres.Settings
import qualified Text
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
        -- | Whether to prepare this query or not. Prepared queries have
        -- better performance, but not all queries can be prepared and for some
        -- that have dynamic parts it would be inefficient.
        prepareQuery :: PrepareQuery,
        -- | The query string as extracted from an `sql` quasi quote.
        quasiQuotedString :: Text,
        -- | SELECT / INSERT / UPDATE / INSERT ON DUPLICATE KEY UPDATE ...
        sqlOperation :: Text,
        -- | The main table/view/.. queried.
        queriedRelation :: Text
      }
  deriving (Eq, Show)

data PrepareQuery = Prepare | DontPrepare deriving (Eq, Show)

qqSQL :: String -> ExpQ
qqSQL queryWithPgTypedFlags = do
  let db =
        Environment.decode Postgres.Settings.decoder
          |> map Postgres.Settings.toPGDatabase
  db' <- runIO db
  void (useTPGDatabase db')
  -- We run the postgresql-typed quasi quoter for it's type-checking logic, but
  -- we're uninterested in the results it produces. At runtime we're taking our
  -- queries straight to MySQL. Consider the line below like a validation
  -- function running against the query string at compile time.
  _ <- quoteExp pgSQL (Data.Text.unpack (inToAny (Data.Text.pack queryWithPgTypedFlags)))
  -- Drop the special flags the `pgSQL` quasiquoter from `postgresql-typed` suppots.
  let query =
        queryWithPgTypedFlags
          |> Prelude.dropWhile (\char -> char == '!' || char == '$' || char == '?')
          |> Data.Text.pack
          |> Text.replace "monolith." ""
          |> Data.Text.unpack
  let meta = Parser.parse (Data.Text.pack query)
  let op = Data.Text.unpack (Parser.sqlOperation meta)
  let rel = Data.Text.unpack (Parser.queriedRelation meta)
  [e|
    let tokens = $(tokenize query)
     in Query
          { preparedStatement = generatePreparedStatement tokens,
            params = collectQueryParams tokens,
            prepareQuery = shouldPrepare tokens,
            quasiQuotedString = queryWithPgTypedFlags,
            sqlOperation = op,
            queriedRelation = Data.Text.pack rel
          }
    |]

-- Our own token type, not to be confused with the `SQLToken` type provided by
-- the `postgresql-typed` library. When convert queries into a list of these
-- tokens because it's a format from which we can generate both a template
-- query string and the list of placeholder values to fill it with.
data SqlToken
  = -- | Just a regular bit of SQL string.
    SqlToken String
  | -- | A dynamic part of the SQL string, where we want to insert one or more
    -- values. Typically the list of values will contain only a single element,
    -- but in a `WHERE x IN (${listTime})` clause we will pass in a list of
    -- values.
    SqlParams [Log.Secret Base.MySQLValue]

-- | Take a query string and parse it, turning it into a list of SQL tokens.
-- We do this at compile time, which is why the return type is not `[SqlToken]`
-- but rather `TH.ExpQ` (which means 'generated haskell code').
tokenize :: String -> TH.ExpQ
tokenize query =
  SQLToken.sqlTokens query
    |> Prelude.traverse parseToken
    |> map TH.ListE

-- | Take a single `SQLToken` provided by `postgresql-typed`'s parsing logic
-- and construct one of our own `SqlToken`s. We do this at compile time, which
-- is why the return type is not `SqlToken` but rather `TH.ExpQ` (which means
-- 'generated haskell code').
parseToken :: SQLToken.SQLToken -> TH.ExpQ
parseToken token =
  case token of
    SQLToken.SQLExpr expr ->
      case parseExp expr of
        Prelude.Left err -> fail ("Could not parse: " ++ err)
        Prelude.Right x ->
          [e|
            ensureList $(Prelude.pure x)
              |> map (Log.mkSecret << mysqlEncode)
              |> SqlParams
            |]
    SQLToken.SQLToken _ -> tokenE (Prelude.show token)
    SQLToken.SQLParam _ -> tokenE (Prelude.show token)
    SQLToken.SQLQMark _ -> tokenE (Prelude.show token)

-- | Generate a prepared statement. Params in the query will be replaced with
-- `$1` placeholders. For example, the following quasiquoted query:
--
--     [MySQL.sql|SELECT names FROM users WHERE id IN (${[1, 2, 3]})|]
--
--  Will result in the following prepared statement:
--
--     "SELECT names FROM users WHERE id in ($1,$2,$3)"
generatePreparedStatement :: [SqlToken] -> Text
generatePreparedStatement tokens =
  tokens
    |> Prelude.foldMap
      ( \token ->
          case token of
            SqlToken str -> Builder.fromString str
            SqlParams params ->
              List.repeat (List.length params) "?"
                |> List.intersperse ","
                |> Prelude.mconcat
      )
    |> Builder.toLazyText
    |> Data.Text.Lazy.toStrict

collectQueryParams :: [SqlToken] -> Log.Secret [Base.MySQLValue]
collectQueryParams tokens =
  tokens
    |> Prelude.traverse
      ( \token ->
          case token of
            SqlToken _ -> Log.mkSecret []
            SqlParams params -> Prelude.sequenceA params
      )
    |> map List.concat

shouldPrepare :: [SqlToken] -> PrepareQuery
shouldPrepare tokens =
  if List.all
    ( \token ->
        case token of
          SqlToken _ -> True
          SqlParams params -> List.length params <= 3
    )
    tokens
    then Prepare
    else DontPrepare

tokenE :: String -> TH.ExpQ
tokenE str = [e|SqlToken str|]

newtype HaskellParseError = HaskellParseError String
  deriving (Show)

instance Exception.Exception HaskellParseError

-- | `ensureList` is a magical function that takes an argument of any type. If
-- the argument is a list it is returned unchanged. If the argument is not a
-- list then it's wrapped in one. The bottom line is it always returns a list.
--
--     ensureList [1,2,3]  --> [1,2,3]
--     ensureList "Hi!"    --> ["Hi!"]
ensureList :: forall a b. EnsureList (IsList a) a b => a -> [b]
ensureList = ensureList' (Proxy :: Proxy (IsList a))

type family IsList a :: Bool where
  IsList [a] = 'True
  IsList a = 'False

class EnsureList (t :: Bool) a b | t b -> a where
  ensureList' :: Proxy t -> a -> [b]

instance EnsureList 'True [a] a where
  ensureList' _ xs = xs

instance EnsureList 'False a a where
  ensureList' _ x = [x]

-- | A type class describing how to encode values for MySQL. The `MySQLValue`
-- type is defined by our MySQL driver library (`mysql-haskell`).
--
-- This is the counterpart of the PGColumn typeclass in `postgresql-typed` for
-- Postgres values.
class MySQLColumn a where
  mysqlEncode :: a -> Base.MySQLValue

instance MySQLColumn Data.Int.Int8 where
  mysqlEncode = Base.MySQLInt8

instance MySQLColumn Data.Word.Word16 where
  mysqlEncode = Base.MySQLInt16U

instance MySQLColumn Data.Int.Int16 where
  mysqlEncode = Base.MySQLInt16

instance MySQLColumn Data.Word.Word32 where
  mysqlEncode = Base.MySQLInt32U

instance MySQLColumn Data.Int.Int32 where
  mysqlEncode = Base.MySQLInt32

instance MySQLColumn Data.Word.Word64 where
  mysqlEncode = Base.MySQLInt64U

instance MySQLColumn Int where
  mysqlEncode = Base.MySQLInt64

instance MySQLColumn Prelude.Float where
  mysqlEncode = Base.MySQLFloat

instance MySQLColumn Float where
  mysqlEncode = Base.MySQLDouble

instance MySQLColumn Clock.UTCTime where
  mysqlEncode = Base.MySQLDateTime << LocalTime.utcToLocalTime LocalTime.utc

-- DODO
-- instance MySQLColumn Local where
--   mysqlEncode = Base.MySQLDateTime

-- instance MySQLColumn _ where
--   mysqlEncode = Base.MySQLTimeStamp

-- instance MySQLColumn _ where
--   mysqlEncode = Base.MySQLDate

-- instance MySQLColumn _ where
--   mysqlEncode = Base.MySQLTime

instance MySQLColumn Text where
  mysqlEncode = Base.MySQLText

instance MySQLColumn a => MySQLColumn (Maybe a) where
  mysqlEncode Nothing = Base.MySQLNull
  mysqlEncode (Just a) = mysqlEncode a

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = qqSQL,
      quoteType = fail "sql not supported in types",
      quotePat = fail "sql not supported in patterns",
      quoteDec = fail "sql not supported in declarations"
    }
