module Test.Reporter.Internal where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified GHC.Stack as Stack
import qualified List
import NriPrelude
import qualified System.Directory
import System.FilePath ((</>))
import qualified Test.Internal as Internal
import qualified Text
import Text.Colour (chunk)
import qualified Text.Colour
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

renderSrcLoc :: Stack.SrcLoc -> BS.ByteString -> List Text.Colour.Chunk
renderSrcLoc loc contents = do
  let startLine = Prelude.fromIntegral (Stack.srcLocStartLine loc)
  let lines =
        contents
          |> BS.split 10 -- splitting newlines
          |> List.drop (startLine - extraLinesOnFailure - 1)
          |> List.take (extraLinesOnFailure * 2 + 1)
          |> List.indexedMap
            ( \i l ->
                Text.fromInt (startLine + i - extraLinesOnFailure)
                  ++ ": "
                  ++ TE.decodeUtf8 l
            )
  case lines of
    [] -> []
    lines' ->
      List.concat
        [ [ "\n",
            "Expectation failed at ",
            chunk (Text.fromList (Stack.srcLocFile loc)),
            ":",
            chunk (Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine loc))),
            "\n"
          ],
          List.indexedMap
            ( \nr line ->
                if nr == extraLinesOnFailure
                  then red (chunk ("✗ " ++ line ++ "\n"))
                  else dullGrey (chunk ("  " ++ line ++ "\n"))
            )
            lines',
          ["\n"]
        ]

red :: Text.Colour.Chunk -> Text.Colour.Chunk
red = Text.Colour.fore Text.Colour.red

yellow :: Text.Colour.Chunk -> Text.Colour.Chunk
yellow = Text.Colour.fore Text.Colour.yellow

green :: Text.Colour.Chunk -> Text.Colour.Chunk
green = Text.Colour.fore Text.Colour.green

grey :: Text.Colour.Chunk -> Text.Colour.Chunk
grey = Text.Colour.fore Text.Colour.brightBlack

dullGrey :: Text.Colour.Chunk -> Text.Colour.Chunk
dullGrey = Text.Colour.fore Text.Colour.black

black :: Text.Colour.Chunk -> Text.Colour.Chunk
black = Text.Colour.fore Text.Colour.white
