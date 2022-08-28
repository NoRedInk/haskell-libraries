module Test.CliParser where

import Control.Applicative ((<*), (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Bifunctor
import qualified List
import NriPrelude
import qualified Test.Internal as Internal
import qualified Text
import qualified Prelude

parseArgs :: [Prelude.String] -> Prelude.Either Text Internal.Request
parseArgs args =
  let parsedArgs :: Prelude.Either Text [Internal.SubsetOfTests]
      parsedArgs =
        args
          |> map Text.fromList
          |> Prelude.traverse parse
          |> Data.Bifunctor.second List.concat
   in parsedArgs
        |> Data.Bifunctor.second
          ( \list ->
              case list of
                [] -> Internal.All
                subsetOfTests -> Internal.Some subsetOfTests
          )

parse :: Text -> Prelude.Either Text [Internal.SubsetOfTests]
parse input =
  input
    |> Attoparsec.parseOnly (argParser <* Attoparsec.endOfInput)
    |> Data.Bifunctor.first Text.fromList

argParser :: Parser [Internal.SubsetOfTests]
argParser = do
  _ <- Attoparsec.string "--files"
  _ <- Attoparsec.string "=" <|> Attoparsec.string " "
  Attoparsec.sepBy1' fileParser (Attoparsec.char ',')

fileParser :: Parser Internal.SubsetOfTests
fileParser = do
  fileName <- Attoparsec.takeWhile1 <| \word -> word /= ':' && word /= ','
  let filepath = Text.toList fileName
  maybeNextChar <- Attoparsec.peekChar
  case maybeNextChar of
    Nothing -> Prelude.pure <| Internal.SubsetOfTests filepath Nothing
    Just ',' -> Prelude.pure <| Internal.SubsetOfTests filepath Nothing
    Just ':' -> do
      _ <- Attoparsec.char ':'
      loc <- Attoparsec.decimal
      Prelude.pure <| Internal.SubsetOfTests filepath (Just loc)
    Just c -> Prelude.fail <| c : ": invalid character"
