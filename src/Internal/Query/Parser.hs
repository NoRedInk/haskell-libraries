-- |
-- Parse some high-level information out of a query, for use in tracing.
-- We try to find the query method (SELECT / INSERT / ...) and queried table
-- in the root SQL query. We assume the root query to be the first query not
-- in a sub query. We assume everything between parens `( ... )` to be a
-- sub query.
module Internal.Query.Parser
  ( parse,
    QueryMeta (..),
  )
where

import Control.Applicative
import Data.Attoparsec.Text (Parser, anyChar, asciiCI, char, inClass, manyTill, skipSpace, space, takeWhile)
import qualified Data.Attoparsec.Text as Attoparsec
import Nri.Prelude

parse :: Text -> Maybe QueryMeta
parse query =
  case Attoparsec.parseOnly parser query of
    Left _ -> Nothing
    Right result -> Just result

data QueryMeta
  = QueryMeta
      { queriedTable :: Text,
        queryOperation :: Text
      }
  deriving (Eq, Show)

parser :: Parser QueryMeta
parser =
  keepLooking
    <| asum
      [ delete,
        insert,
        select,
        truncate',
        update
      ]

keepLooking :: Parser a -> Parser a
keepLooking p = do
  skipSpace
  asum
    [ -- 1. If we encounter sub queries (bounded in parens), skip them first.
      do
        void <| some skipSubExpression
        keepLooking p,
      -- 2. Try to run the target parser.
      p,
      -- 3. Failing all else, move forward a word and try again.
      do
        void <| manyTill anyChar (space <|> char '(')
        keepLooking p
    ]

skipSubExpression :: Parser ()
skipSubExpression = do
  void <| char '('
  void <| keepLooking (char ')')

delete :: Parser QueryMeta
delete = do
  void <| asciiCI "DELETE"
  skipSpace
  void <| asciiCI "FROM"
  skipSpace
  queriedTable <- tableName
  pure QueryMeta {queriedTable, queryOperation = "DELETE"}

insert :: Parser QueryMeta
insert = do
  void <| asciiCI "INSERT"
  skipSpace
  void <| asciiCI "INTO"
  skipSpace
  queriedTable <- tableName
  pure QueryMeta {queriedTable, queryOperation = "INSERT"}

select :: Parser QueryMeta
select = do
  void <| asciiCI "SELECT"
  keepLooking <| do
    void <| asciiCI "FROM"
    keepLooking <| do
      queriedTable <- tableName
      pure QueryMeta {queriedTable, queryOperation = "SELECT"}

tableName :: Parser Text
tableName =
  takeWhile (inClass "a-zA-Z0-9._")

truncate' :: Parser QueryMeta
truncate' = do
  void <| asciiCI "UPDATE"
  skipSpace
  (asciiCI "ONLY" |> void) <|> pure ()
  skipSpace
  queriedTable <- tableName
  pure QueryMeta {queriedTable, queryOperation = "UPDATE"}

update :: Parser QueryMeta
update = do
  void <| asciiCI "TRUNCATE"
  skipSpace
  (asciiCI "TABLE" |> void) <|> pure ()
  (asciiCI "ONLY" |> void) <|> pure ()
  skipSpace
  queriedTable <- tableName
  pure QueryMeta {queriedTable, queryOperation = "TRUNCATE"}
