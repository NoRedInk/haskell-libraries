module Test.CliParser where

import Control.Applicative (optional, some, (*>), (<*), (<|>))
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Functor (void)
import NriPrelude
import qualified Result
import qualified Test.Internal as Internal
import qualified Text
import qualified Prelude

parseArgs :: List Prelude.String -> Result Prelude.String Internal.Request
parseArgs args =
  case args of
    [] -> Ok Internal.All
    ["--files"] -> Err "must inform at least one file: not enough input"
    ["--files", files] ->
      parse files
        |> Result.andThen
          ( \lists -> case lists of
              [] -> Err "must inform at least one file: not enough input"
              subsetOfTests -> Ok (Internal.Some subsetOfTests)
          )
    _ : rest -> parseArgs rest

parse :: Prelude.String -> Result Prelude.String (List Internal.SubsetOfTests)
parse input =
  input
    |> Text.fromList
    |> Attoparsec.parseOnly (argParser <* endParser)
    |> Prelude.either Err Ok

endParser :: Parser ()
endParser = Attoparsec.endOfInput <|> void (Attoparsec.char ',') <|> unexpectedInput

argParser :: Parser (List Internal.SubsetOfTests)
argParser = Attoparsec.sepBy1' (fileParser <|> unexpectedInput) (Attoparsec.char ',') <?> "must inform at least one file"

fileParser :: Parser Internal.SubsetOfTests
fileParser = do
  requestedPath <- some (Attoparsec.satisfy (\c -> c /= ':' && c /= ','))
  lineOfCode <- optional (Attoparsec.char ':' *> Attoparsec.decimal)
  Prelude.pure Internal.SubsetOfTests {Internal.requestedPath, Internal.lineOfCode}

unexpectedInput :: Parser a
unexpectedInput = do
  rest <- some Attoparsec.anyChar
  Prelude.fail ("expected format: --files=bla.hs or --files bla.hs: \"" ++ rest ++ "\"")
