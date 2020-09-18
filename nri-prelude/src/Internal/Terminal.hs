module Internal.Terminal (write, read, red, blue, green, magenta, yellow, cyan, lightBlue, black, white, reset, underline, italic, newline, indent, message, header, blank, paragraphs) where

import Basics
import qualified Data.Text
import List (List)
import qualified Text
import qualified Prelude as P

-- |
write :: Text.Text -> P.IO ()
write string =
  P.putStr (Data.Text.unpack string)

-- |
read :: P.IO Text.Text
read =
  P.getLine
    |> P.fmap Data.Text.pack

-- CHARACTERS

red :: Text.Text
red =
  "\x1b[31m"

blue :: Text.Text
blue =
  "\x1b[34m"

magenta :: Text.Text
magenta =
  "\x1b[35m"

green :: Text.Text
green =
  "\x1b[32m"

yellow :: Text.Text
yellow =
  "\x1b[33m"

cyan :: Text.Text
cyan =
  "\x1b[36m"

lightBlue :: Text.Text
lightBlue =
  "\x1b[94m"

black :: Text.Text
black =
  "\x1b[30m"

white :: Text.Text
white =
  "\x1b[37m"

reset :: Text.Text
reset =
  "\x1b[0m"

newline :: Text.Text
newline =
  "\n"

blank :: Text.Text
blank =
  newline ++ newline

underline :: Text.Text
underline =
  "\x1b[4m"

italic :: Text.Text
italic =
  "\x1b[3m"

indent :: Int -> Text.Text
indent number =
  Text.repeat number " "

-- MESSAGE

message :: Text.Text -> Text.Text -> Text.Text -> List Text.Text -> P.IO ()
message color title location content = do
  write (color ++ header title location ++ reset)
  write blank
  write (paragraphs content)
  write blank

header :: Text.Text -> Text.Text -> Text.Text
header title location =
  let number = 75 - Text.length title - Text.length location
      dashes = Text.repeat number "-"
   in "-- " ++ Text.toUpper title ++ " " ++ dashes ++ " " ++ location ++ " "

-- PARAGRAPHS

paragraphs :: List Text.Text -> Text.Text
paragraphs =
  Text.join blank
