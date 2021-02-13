module Main (main) where

import qualified Brick
import qualified Brick.BChan
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.IORef as IORef
import qualified GHC.IO.Encoding
import qualified Graphics.Vty as Vty
import qualified List
import qualified Maybe
import NriPrelude
import qualified Platform
import qualified System.Directory
import System.FilePath ((</>))
import qualified System.IO
import qualified Text
import qualified Zipper
import qualified Prelude

data Model
  = Model
      { loglines :: Maybe (Zipper.Zipper Platform.TracingSpan)
      }

data Msg
  = AddLogline ByteString.ByteString
  | DownOne
  | UpOne

init :: Model
init = Model {loglines = Nothing}

update :: Model -> Msg -> Brick.EventM () (Brick.Next Model)
update model msg =
  case msg of
    AddLogline line ->
      case Aeson.decodeStrict' line of
        Nothing ->
          Brick.continue model
        Just span ->
          model
            { loglines = case loglines model of
                Nothing -> Just (Zipper.singleton span)
                Just zipper -> Just (Zipper.prepend [span] zipper)
            }
            |> Brick.continue
    DownOne ->
      model
        { loglines = Maybe.map Zipper.next (loglines model)
        }
        |> Brick.continue
    UpOne ->
      model
        { loglines = Maybe.map Zipper.prev (loglines model)
        }
        |> Brick.continue

view :: Model -> [Brick.Widget ()]
view model =
  [ Brick.vBox
      [ viewContents model,
        viewKey model
      ]
  ]

viewKey :: Model -> Brick.Widget ()
viewKey model =
  let exit = "q: exit"
      updown = "↑↓: select log"
      shortcuts =
        case loglines model of
          Nothing -> [exit]
          Just _ -> [exit, updown]
   in Brick.vBox
        [ Brick.padTop Brick.Max Border.hBorder,
          shortcuts
            |> Text.join "   "
            |> Brick.txt
            |> Center.hCenter
        ]

viewContents :: Model -> Brick.Widget ()
viewContents model =
  case loglines model of
    Nothing ->
      Brick.txt "Waiting for logs...\n\nGo run some tests!"
        |> Center.hCenter
    Just logs ->
      logs
        |> Zipper.indexedMap
          ( \i span ->
              Brick.txt (Platform.name span)
                |> Center.hCenter
                |> if i == 0
                  then Brick.withAttr "selected"
                  else identity
          )
        |> Zipper.toList
        |> Brick.vBox

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
  let pushMsg = Brick.BChan.writeBChan eventChan
  Async.race_
    ( System.IO.withFile
        logFile
        System.IO.ReadMode
        (tailLines partOfLine (AddLogline >> pushMsg))
    )
    (Brick.customMain initialVty buildVty (Just eventChan) (app pushMsg) init)

app :: (Msg -> Prelude.IO ()) -> Brick.App Model Msg ()
app pushMsg =
  Brick.App
    { Brick.appDraw = view,
      Brick.appChooseCursor = \_ -> List.head,
      Brick.appHandleEvent = handleEvent pushMsg,
      Brick.appStartEvent = Prelude.pure,
      Brick.appAttrMap = \_ -> attrMap
    }

attrMap :: Brick.AttrMap
attrMap =
  Brick.attrMap
    Vty.defAttr
    [ ("selected", Vty.withStyle Vty.defAttr Vty.reverseVideo)
    ]

handleEvent ::
  (Msg -> Prelude.IO ()) ->
  Model ->
  Brick.BrickEvent () Msg ->
  Brick.EventM () (Brick.Next Model)
handleEvent pushMsg model event =
  case event of
    (Brick.VtyEvent vtyEvent) ->
      case vtyEvent of
        -- Quiting
        Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt model
        Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt model
        Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] -> Brick.halt model
        -- Navigation
        Vty.EvKey Vty.KDown [] -> do
          liftIO (pushMsg DownOne)
          Brick.continue model
        Vty.EvKey (Vty.KChar 'j') [] -> do
          liftIO (pushMsg DownOne)
          Brick.continue model
        Vty.EvKey Vty.KUp [] -> do
          liftIO (pushMsg UpOne)
          Brick.continue model
        Vty.EvKey (Vty.KChar 'k') [] -> do
          liftIO (pushMsg UpOne)
          Brick.continue model
        -- Fallback
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
  chunk <- ByteString.hGetSome handle 10000
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
