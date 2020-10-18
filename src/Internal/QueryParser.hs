-- |
-- Parse some high-level information out of a query, for use in tracing.
-- We try to find the query method (SELECT / INSERT / ...) and queried table
-- in the root SQL query. We assume the root query to be the first query not
-- in a sub query. We assume everything between parens `( ... )` to be a
-- sub query.
module Internal.QueryParser
  ( parse,
    QueryMeta (..),
  )
where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.Text (Parser, anyChar, asciiCI, char, inClass, manyTill, skipSpace, space, takeWhile)
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Foldable (asum)
import qualified List
import qualified Maybe
import NriPrelude
import qualified Text
import Prelude (Either (Left, Right))

parse :: Text -> QueryMeta
parse query =
  case Attoparsec.parseOnly parser query of
    Left _ ->
      QueryMeta
        { queriedRelation =
            Text.lines query
              |> List.head
              |> Maybe.withDefault "",
          sqlOperation = "UNKNOWN"
        }
    Right result -> result

data QueryMeta
  = QueryMeta
      { queriedRelation :: Text,
        sqlOperation :: Text
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
  queriedRelation <- tableName
  pure QueryMeta {queriedRelation, sqlOperation = "DELETE"}

insert :: Parser QueryMeta
insert = do
  void <| asciiCI "INSERT"
  skipSpace
  void <| asciiCI "INTO"
  skipSpace
  queriedRelation <- tableName
  pure QueryMeta {queriedRelation, sqlOperation = "INSERT"}

select :: Parser QueryMeta
select = do
  void <| asciiCI "SELECT"
  keepLooking <| do
    void <| asciiCI "FROM"
    keepLooking <| do
      queriedRelation <- tableName
      pure QueryMeta {queriedRelation, sqlOperation = "SELECT"}

tableName :: Parser Text
tableName =
  takeWhile (inClass "a-zA-Z0-9._")

truncate' :: Parser QueryMeta
truncate' = do
  void <| asciiCI "UPDATE"
  skipSpace
  (asciiCI "ONLY" |> void) <|> pure ()
  skipSpace
  queriedRelation <- tableName
  pure QueryMeta {queriedRelation, sqlOperation = "UPDATE"}

update :: Parser QueryMeta
update = do
  void <| asciiCI "TRUNCATE"
  skipSpace
  (asciiCI "TABLE" |> void) <|> pure ()
  (asciiCI "ONLY" |> void) <|> pure ()
  skipSpace
  queriedRelation <- tableName
  pure QueryMeta {queriedRelation, sqlOperation = "TRUNCATE"}
