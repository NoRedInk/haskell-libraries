module Test.Reporter.Internal where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified System.Console.ANSI as ANSI
import qualified System.Directory
import System.FilePath ((</>))
import qualified Test.Internal as Internal
import qualified Prelude

extraLinesOnFailure :: Int
extraLinesOnFailure = 2

readSrcLoc :: Internal.SingleTest Internal.Failure -> Prelude.IO (Maybe (Stack.SrcLoc, BS.ByteString))
readSrcLoc test =
  case Internal.body test of
    Internal.FailedAssertion _ (Just loc) -> do
      cwd <- System.Directory.getCurrentDirectory
      let path = cwd </> Stack.srcLocFile loc
      exists <- System.Directory.doesFileExist path
      if exists
        then do
          contents <- BS.readFile path
          Prelude.pure (Just (loc, contents))
        else Prelude.pure Nothing
    _ -> Prelude.pure Nothing

renderSrcLoc ::
  ([ANSI.SGR] -> Builder.Builder -> Builder.Builder) ->
  Stack.SrcLoc ->
  BS.ByteString ->
  Builder.Builder
renderSrcLoc styled loc contents = do
  let startLine = Prelude.fromIntegral (Stack.srcLocStartLine loc)
  let lines =
        contents
          |> BS.split 10 -- splitting newlines
          |> List.drop (startLine - extraLinesOnFailure - 1)
          |> List.take (extraLinesOnFailure * 2 + 1)
          |> List.indexedMap
            ( \i l ->
                Builder.intDec
                  ( Prelude.fromIntegral
                      <| startLine + i - extraLinesOnFailure
                  )
                  ++ ": "
                  ++ Builder.byteString l
            )
  case lines of
    [] -> ""
    lines' ->
      "\n"
        ++ "Expectation failed at "
        ++ Builder.stringUtf8 (Stack.srcLocFile loc)
        ++ ":"
        ++ Builder.intDec (Stack.srcLocStartLine loc)
        ++ "\n"
        ++ Prelude.foldMap
          ( \(nr, line) ->
              if nr == extraLinesOnFailure
                then styled [red] ("✗ " ++ line) ++ "\n"
                else "  " ++ styled [dullGrey] line ++ "\n"
          )
          (List.indexedMap (,) lines')
        ++ "\n"

sgr :: [ANSI.SGR] -> Builder.Builder
sgr = Builder.stringUtf8 << ANSI.setSGRCode

red :: ANSI.SGR
red = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red

yellow :: ANSI.SGR
yellow = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow

green :: ANSI.SGR
green = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green

grey :: ANSI.SGR
grey = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black

dullGrey :: ANSI.SGR
dullGrey = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black

black :: ANSI.SGR
black = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White

underlined :: ANSI.SGR
underlined = ANSI.SetUnderlining ANSI.SingleUnderline
