module Main (main) where

import qualified Control.Concurrent
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.IORef as IORef
import qualified GHC.IO.Encoding
import NriPrelude
import qualified System.Directory
import System.FilePath ((</>))
import qualified System.IO
import qualified Prelude

main :: Prelude.IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  partOfLine <- IORef.newIORef Prelude.mempty
  tmpDir <- System.Directory.getTemporaryDirectory
  let logFile = tmpDir </> "nri-prelude-logs"
  System.IO.appendFile logFile "" -- touch file to ensure it exists
  System.IO.withFile logFile System.IO.ReadMode (tailLines partOfLine Prelude.print)
  Prelude.putStrLn "DONE"

-- Tail a file handle, calling a callback function every time a new line is
-- read. This function will intentionally hang, waiting for additional input to
-- the handle it's reading from.
tailLines ::
  IORef.IORef Builder.Builder ->
  (ByteString.ByteString -> Prelude.IO ()) ->
  System.IO.Handle ->
  Prelude.IO ()
tailLines partOfLine withLine handle = do
  chunk <- ByteString.hGetSome handle 10
  case ByteString.split 10 {- \n -} chunk of
    [] -> do
      Control.Concurrent.threadDelay 100000 {- 100 ms -}
      tailLines partOfLine withLine handle
    [""] -> do
      Control.Concurrent.threadDelay 100000 {- 100 ms -}
      tailLines partOfLine withLine handle
    [segment] -> do
      IORef.modifyIORef' partOfLine (\acc -> acc ++ Builder.byteString segment)
      tailLines partOfLine withLine handle
    endOfOldLine : rest -> do
      let startOfNewLine = Builder.byteString (Prelude.last rest)
      startOfOldLine <-
        IORef.atomicModifyIORef'
          partOfLine
          (\acc -> (startOfNewLine, acc))
      let firstFullLine =
            startOfOldLine ++ Builder.byteString endOfOldLine
              |> Builder.toLazyByteString
              |> ByteString.Lazy.toStrict
      let fullLines =
            firstFullLine
              : Prelude.init rest
      _ <- Prelude.traverse withLine fullLines
      tailLines partOfLine withLine handle
