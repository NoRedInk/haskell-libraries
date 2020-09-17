{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module expose some helpers for running mysql typed.
module MySQL.Query
  ( sql,
    Query (..),
    Info (..),
    ConnectionInfo (..),
    mkInfo,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Proxy as Proxy
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Database.MySQL.Base as Base
import qualified Database.PostgreSQL.Typed as PGTyped
import Database.PostgreSQL.Typed.Array ()
import qualified Database.PostgreSQL.Typed.SQLToken as SQLToken
import qualified Environment
import Internal.Instances ()
import qualified Internal.QueryParser as Parser
import qualified Language.Haskell.Meta.Parse as Parse
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import qualified List
import qualified Log
import qualified MySQL.Internal as Internal
import qualified MySQL.MySQLParameter as MySQLParameter
import qualified Platform
import qualified Postgres.Settings
import qualified Text
import qualified Prelude

-- |
-- A type representing a MySQL query.
--
-- This gets generated at compile time by using the `sql` quasi quoter. Creating
-- one of these requires parsing an SQL string and performing some interpolation
-- operations in it. As much of this as we can we do at compile time, to ensure
-- we have less work to do when we run a query.
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
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON (Query row)

qqSQL :: Prelude.String -> TH.ExpQ
qqSQL queryWithPgTypedFlags = do
  let db =
        Environment.decode Postgres.Settings.decoder
          |> map Postgres.Settings.toPGDatabase
  db' <- TH.runIO db
  _ <- PGTyped.useTPGDatabase db'
  -- We run the postgresql-typed quasi quoter for it's type-checking logic, but
  -- we're uninterested in the results it produces. At runtime we're taking our
  -- queries straight to MySQL. Consider the line below like a validation
  -- function running against the query string at compile time.
  _ <-
    Data.Text.pack queryWithPgTypedFlags
      |> Internal.inToAny
      |> Data.Text.unpack
      |> QQ.quoteExp PGTyped.pgSQL
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
    SqlToken Prelude.String
  | -- | A dynamic part of the SQL string, where we want to insert one or more
    -- values. Typically the list of values will contain only a single element,
    -- but in a `WHERE x IN (${listTime})` clause we will pass in a list of
    -- values.
    SqlParams [Log.Secret Base.MySQLValue]

-- | Take a query string and parse it, turning it into a list of SQL tokens.
-- We do this at compile time, which is why the return type is not `[SqlToken]`
-- but rather `TH.ExpQ` (which means 'generated haskell code').
tokenize :: Prelude.String -> TH.ExpQ
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
      case Parse.parseExp expr of
        Prelude.Left err -> Prelude.fail ("Could not parse: " ++ err)
        Prelude.Right x ->
          [e|
            ensureList $(Prelude.pure x)
              |> map (Log.mkSecret << MySQLParameter.mysqlEncode)
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
            SqlParams [] -> "\"THIS_IS_NEVER_TRUE_ELSE_COMPLAIN_TO_PUFFERFISH\""
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

tokenE :: Prelude.String -> TH.ExpQ
tokenE str = [e|SqlToken str|]

newtype HaskellParseError = HaskellParseError Prelude.String
  deriving (Show)

instance Exception.Exception HaskellParseError

-- | `ensureList` is a magical function that takes an argument of any type. If
-- the argument is a list it is returned unchanged. If the argument is not a
-- list then it's wrapped in one. The bottom line is it always returns a list.
--
--     ensureList [1,2,3]  --> [1,2,3]
--     ensureList "Hi!"    --> ["Hi!"]
ensureList :: forall a b. EnsureList (IsList a) a b => a -> [b]
ensureList = ensureList' (Proxy.Proxy :: Proxy.Proxy (IsList a))

type family IsList a :: Bool where
  IsList [a] = 'True
  IsList a = 'False

class EnsureList (t :: Bool) a b | t b -> a where
  ensureList' :: Proxy.Proxy t -> a -> [b]

instance EnsureList 'True [a] a where
  ensureList' _ xs = xs

instance EnsureList 'False a a where
  ensureList' _ x = [x]

sql :: QQ.QuasiQuoter
sql =
  QQ.QuasiQuoter
    { QQ.quoteExp = qqSQL,
      QQ.quoteType = Prelude.fail "sql not supported in types",
      QQ.quotePat = Prelude.fail "sql not supported in patterns",
      QQ.quoteDec = Prelude.fail "sql not supported in declarations"
    }

--
-- TracingSpanDetails
--

data Info
  = Info
      { -- | The full query we're executing (prepared statement).
        infoQuery :: Text,
        -- | The quasi-quoted string of the query we're executing.
        infoQueryTemplate :: Text,
        -- | Our best guess of the SQL operation we're performing (SELECT /
        -- DELETE / ...).
        infoSqlOperation :: Text,
        -- | Our best guess of the relation we're querying.
        infoQueriedRelation :: Text,
        -- | Connection information of the database we're sending the query to.
        infoConnection :: ConnectionInfo
      }
  deriving (Generic)

instance Aeson.ToJSON Info where

  toJSON = Aeson.genericToJSON infoEncodingOptions

  toEncoding = Aeson.genericToEncoding infoEncodingOptions

infoEncodingOptions :: Aeson.Options
infoEncodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 ' ' << List.drop 4
    }

instance Platform.TracingSpanDetails Info

mkInfo :: Query row -> ConnectionInfo -> Info
mkInfo query conn =
  Info
    { infoQuery = preparedStatement query,
      infoQueryTemplate = quasiQuotedString query,
      infoSqlOperation = sqlOperation query,
      infoQueriedRelation = queriedRelation query,
      infoConnection = conn
    }

data ConnectionInfo
  = TcpSocket Host Port DatabaseName
  | UnixSocket SocketPath DatabaseName
  deriving (Generic)

instance Aeson.ToJSON ConnectionInfo where

  toJSON = Aeson.toJSON << connectionToText

  toEncoding = Aeson.toEncoding << connectionToText

type Host = Text

type Port = Text

type SocketPath = Text

type DatabaseName = Text

connectionToText :: ConnectionInfo -> Text
connectionToText (TcpSocket host port db) = host ++ ":" ++ port ++ "/" ++ db
connectionToText (UnixSocket path db) = path ++ ":" ++ db
