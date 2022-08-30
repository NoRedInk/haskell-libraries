module Test.CliParser where

import Control.Applicative (some, (<*), (<|>))
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char
import Data.Functor (void)
import qualified List
import NriPrelude
import qualified Test.Internal as Internal
import qualified Text
import qualified Prelude

parseArgs :: [Prelude.String] -> Prelude.Either Text Internal.Request
parseArgs args =
  case Prelude.traverse parse args of
    Prelude.Left err -> Prelude.Left (Text.fromList err)
    Prelude.Right lists ->
      Prelude.Right <| case List.concat lists of
        [] -> Internal.All
        subsetOfTests -> Internal.Some subsetOfTests

parse :: Prelude.String -> Prelude.Either Prelude.String [Internal.SubsetOfTests]
parse input =
  input
    |> Text.fromList
    |> Attoparsec.parseOnly (argParser <* Attoparsec.endOfInput)

argParser :: Parser [Internal.SubsetOfTests]
argParser = do
  _ <- Attoparsec.string "--files" <|> Prelude.fail "expected argument: --files"
  _ <-
    void (Attoparsec.string "=")
      <|> void (some (Attoparsec.satisfy Data.Char.isSpace))
      <|> Prelude.fail "expected format: --files=bla.hs or --files bla.hs"
  Attoparsec.sepBy1' fileParser (Attoparsec.char ',') <?> "must inform at least one file"

fileParser :: Parser Internal.SubsetOfTests
fileParser = do
  fileName <- (Attoparsec.takeWhile1 <| \word -> word /= ':' && word /= ',') <?> "filename"
  let filepath = Text.toList fileName
  maybeNextChar <- Attoparsec.peekChar
  case maybeNextChar of
    Nothing -> Prelude.pure <| Internal.SubsetOfTests filepath Nothing
    Just ',' -> Prelude.pure <| Internal.SubsetOfTests filepath Nothing
    Just ':' -> do
      _ <- Attoparsec.char ':'
      maybeLoc <- (Attoparsec.takeWhile1 <| \word -> word /= ',') <|> Prelude.fail "missing line number"
      case Text.toInt maybeLoc of
        Nothing -> Prelude.fail <| "invalid line number: " ++ (Text.toList maybeLoc)
        Just loc -> Prelude.pure <| Internal.SubsetOfTests filepath (Just loc)
    Just c -> Prelude.fail <| c : ": invalid character" -- Can't ever happen
