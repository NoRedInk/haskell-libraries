module Test.CliParser where

import Control.Applicative (optional, some, (*>), (<*), (<|>))
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Functor (void)
import qualified List
import NriPrelude
import qualified Test.Internal as Internal
import qualified Text
import qualified Prelude

parseArgs :: List Prelude.String -> Prelude.Either Prelude.String Internal.Request
parseArgs args =
  Prelude.traverse parse args
    |> map
      ( \lists -> case List.concat lists of
          [] -> Internal.All
          subsetOfTests -> Internal.Some subsetOfTests
      )

parse :: Prelude.String -> Prelude.Either Prelude.String (List Internal.SubsetOfTests)
parse input =
  input
    |> Text.fromList
    |> Attoparsec.parseOnly (argParser <* endParser)

endParser :: Parser ()
endParser = Attoparsec.endOfInput <|> void (Attoparsec.char ',') <|> unexpectedInput

argParser :: Parser (List Internal.SubsetOfTests)
argParser = do
  _ <- Attoparsec.string "--files" <?> "expected argument: --files"
  _ <- Attoparsec.skip (\c -> c == '=') <|> Attoparsec.skipSpace
  Attoparsec.sepBy1' (fileParser <|> unexpectedInput) (Attoparsec.char ',') <?> "must inform at least one file"

fileParser :: Parser Internal.SubsetOfTests
fileParser = do
  requestedPath <- some (Attoparsec.satisfy (\c -> c /= ':' && c /= ','))
  lineOfCode <- optional (Attoparsec.char ':' *> Attoparsec.decimal)
  Prelude.pure Internal.SubsetOfTests {Internal.requestedPath, Internal.lineOfCode}

unexpectedInput :: Parser a
unexpectedInput = do
  rest <- some Attoparsec.anyChar
  Prelude.fail ("expected format: --files=bla.hs or --files bla.hs: \"" ++ rest ++ "\"")
