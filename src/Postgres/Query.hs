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
    Info (..),
    ConnectionInfo (..),
    mkInfo,
  )
where

import Cherry.Prelude
import Control.Monad (fail, void)
import qualified Data.Aeson as Aeson
import Data.String (String)
import qualified Data.Text
import qualified Data.Text.Encoding
import Database.PostgreSQL.Typed (PGConnection, pgSQL, useTPGDatabase)
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Query (getQueryString, pgQuery)
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import qualified Environment
import Internal.Error (Error (..), TimeoutOrigin (..))
import Internal.Instances ()
import qualified Internal.QueryParser as Parser
import Language.Haskell.TH (ExpQ)
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
import Prelude (IO)

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
  let meta = Parser.parse (Data.Text.pack query)
  let op = Data.Text.unpack (Parser.sqlOperation meta)
  let rel = Data.Text.unpack (Parser.queriedRelation meta)
  [e|
    let q = $(quoteExp pgSQL (Data.Text.unpack (inToAny (Data.Text.pack query))))
     in Query
          { runQuery = \c -> pgQuery c q,
            sqlString = Data.Text.Encoding.decodeUtf8 (getQueryString PGTypes.unknownPGTypeEnv q),
            quasiQuotedString =
              query
                |> Data.Text.pack
                |> inToAny,
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

--
-- TracingSpanDetails
--

data Info
  = Info
      { -- | The full query we're sending to a database. Wrapped in a Secret
        -- because some queries might contain sensitive information, and we
        -- don't know which ones.
        infoQuery :: Log.Secret Text,
        -- | The query template (QuasiQuote) that was used to build the template.
        -- This we don't need to wrap in a `Secret` and can be safely logged.
        infoQueryTemplate :: Text,
        -- | Our best guess of the relation we're querying.
        infoQueriedRelation :: Text,
        -- | Our best guess of the SQL operation we're performing (SELECT /
        -- DELETE / ...).
        infoSqlOperation :: Text,
        -- | Connection information of the database we're sending the query to.
        infoConnection :: ConnectionInfo
      }
  deriving (Generic)

mkInfo :: Query row -> ConnectionInfo -> Info
mkInfo query conn =
  Info
    { infoQuery = Log.mkSecret (sqlString query),
      infoQueryTemplate = quasiQuotedString query,
      infoSqlOperation = sqlOperation query,
      infoQueriedRelation = queriedRelation query,
      infoConnection = conn
    }

instance Aeson.ToJSON Info where
  toJSON = Aeson.genericToJSON infoEncodingOptions

  toEncoding = Aeson.genericToEncoding infoEncodingOptions

infoEncodingOptions :: Aeson.Options
infoEncodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 ' ' << List.drop 4
    }

instance Platform.TracingSpanDetails Info

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
