module Main (main) where

import qualified Brick
import qualified Brick.BChan
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.IORef as IORef
import qualified GHC.IO.Encoding
import qualified Graphics.Vty as Vty
import qualified List
import NriPrelude
import qualified System.Directory
import System.FilePath ((</>))
import qualified System.IO
import qualified Text
import qualified Prelude

data Model
  = Model
      { loglines :: [ByteString.ByteString]
      }

data Msg
  = AddLogline ByteString.ByteString

update :: Model -> Msg -> Brick.EventM () (Brick.Next Model)
update model msg =
  case msg of
    AddLogline line ->
      model {loglines = line : loglines model}
        |> Brick.continue

view :: Model -> [Brick.Widget ()]
view model = [Brick.txt ("Logs: " ++ Text.fromInt (List.length (loglines model)))]

-- Brick App boilerplate

main :: Prelude.IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  partOfLine <- IORef.newIORef Prelude.mempty
  tmpDir <- System.Directory.getTemporaryDirectory
  let logFile = tmpDir </> "nri-prelude-logs"
  System.IO.appendFile logFile "" -- touch file to ensure it exists
  eventChan <- Brick.BChan.newBChan 10
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  let pushLine = AddLogline >> Brick.BChan.writeBChan eventChan
  Async.race_
    (System.IO.withFile logFile System.IO.ReadMode (tailLines partOfLine pushLine))
    (Brick.customMain initialVty buildVty (Just eventChan) app (Model []))

app :: Brick.App Model Msg ()
app =
  Brick.App
    { Brick.appDraw = view,
      Brick.appChooseCursor = \_ -> List.head,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = Prelude.pure,
      Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
    }

handleEvent :: Model -> Brick.BrickEvent () Msg -> Brick.EventM () (Brick.Next Model)
handleEvent model event =
  case event of
    (Brick.VtyEvent vtyEvent) ->
      case vtyEvent of
        Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt model
        Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt model
        Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] -> Brick.halt model
        _ -> Brick.continue model
    (Brick.MouseDown _ _ _ _) -> Brick.continue model
    (Brick.MouseUp _ _ _) -> Brick.continue model
    (Brick.AppEvent msg) -> update model msg

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
