module Test.Console.Color
  ( Style,
    styled,

    -- * colors
    blue,
    grey,
    magenta,
    red,
    yellow,
  )
where

import qualified Data.Text
import List (List)
import NriPrelude
import qualified System.Console.ANSI as Console
import Text (Text)

type Style = List Console.SGR

code :: Style -> Text
code = Data.Text.pack << Console.setSGRCode

styled :: Style -> Text -> Text
styled col t = code col ++ t ++ code [reset]

reset :: Console.SGR
reset = Console.Reset

red :: Console.SGR
red = Console.SetColor Console.Foreground Console.Dull Console.Red

magenta :: Console.SGR
magenta = Console.SetColor Console.Foreground Console.Dull Console.Magenta

blue :: Console.SGR
blue = Console.SetColor Console.Foreground Console.Dull Console.Blue

yellow :: Console.SGR
yellow = Console.SetColor Console.Foreground Console.Dull Console.Yellow

grey :: Console.SGR
grey = Console.SetColor Console.Foreground Console.Dull Console.White
