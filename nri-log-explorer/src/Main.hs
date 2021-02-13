{-# LANGUAGE NumericUnderscores #-}

module Main
  ( main,
  )
where

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
import qualified Data.Time as Time
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
import qualified Tuple
import qualified Zipper
import qualified Prelude

data Model
  = Model
      { currentTime :: Time.UTCTime,
        loglines :: Maybe (Zipper.Zipper (Time.UTCTime, Platform.TracingSpan)),
        selectedRootSpan :: Maybe (Zipper.Zipper Span)
      }

data Span
  = Span
      { nesting :: Int,
        original :: Platform.TracingSpan
      }

data Msg
  = AddLogline ByteString.ByteString
  | DownOne
  | UpOne
  | ShowDetails
  | Exit
  | SetCurrentTime Time.UTCTime

init :: Time.UTCTime -> Model
init now =
  Model
    { currentTime = now,
      loglines = Nothing,
      selectedRootSpan = Nothing
    }

update :: Model -> Msg -> Brick.EventM () (Brick.Next Model)
update model msg =
  case msg of
    SetCurrentTime time ->
      model {currentTime = time}
        |> Brick.continue
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
      ( \page ->
          case page of
            NoLogsFound -> NoLogsFound
            SpanList time spans -> SpanList time (Zipper.next spans)
            SpanDetails root spans -> SpanDetails root (Zipper.next spans)
      )
        |> withPage model
        |> Brick.continue
    UpOne ->
      ( \page ->
          case page of
            NoLogsFound -> NoLogsFound
            SpanList time spans -> SpanList time (Zipper.prev spans)
            SpanDetails root spans -> SpanDetails root (Zipper.prev spans)
      )
        |> withPage model
        |> Brick.continue
    ShowDetails ->
      model
        { selectedRootSpan =
            Maybe.map
              (Zipper.current >> Tuple.second >> toFlatList)
              (loglines model)
        }
        |> Brick.continue
    Exit ->
      model
        { selectedRootSpan = Nothing
        }
        |> Brick.continue

toFlatList :: Platform.TracingSpan -> Zipper.Zipper Span
toFlatList span =
  case Zipper.fromList (toFlatListHelper 0 span) of
    Just zipper -> zipper
    Nothing ->
      Zipper.singleton
        Span
          { nesting = 0,
            original = span
          }

toFlatListHelper :: Int -> Platform.TracingSpan -> [Span]
toFlatListHelper nesting span =
  Span
    { nesting = nesting,
      original = span
    }
    : List.concatMap (toFlatListHelper (nesting + 1)) (Platform.children span)

view :: Model -> [Brick.Widget ()]
view model =
  let page = toPage model
   in [ Brick.vBox
          [ viewContents page,
            viewKey page
          ]
      ]

data Page
  = NoLogsFound
  | SpanList Time.UTCTime (Zipper.Zipper (Time.UTCTime, Platform.TracingSpan))
  | SpanDetails Platform.TracingSpan (Zipper.Zipper Span)

toPage :: Model -> Page
toPage model =
  case (selectedRootSpan model, loglines model) of
    (Just selected, Just spans) ->
      SpanDetails
        (Tuple.second (Zipper.current spans))
        selected
    (Nothing, Just spans) -> SpanList (currentTime model) spans
    (_, Nothing) -> NoLogsFound

withPage :: Model -> (Page -> Page) -> Model
withPage model fn =
  case fn (toPage model) of
    NoLogsFound -> model {selectedRootSpan = Nothing, loglines = Nothing}
    SpanList _ zipper -> model {selectedRootSpan = Nothing, loglines = Just zipper}
    SpanDetails _ zipper -> model {selectedRootSpan = Just zipper}

viewKey :: Page -> Brick.Widget ()
viewKey page =
  let exit = "q: exit"
      updown = "↑↓: select"
      select = "enter: show details"
      unselect = "backspace: unselect"
      shortcuts =
        case page of
          NoLogsFound -> [exit]
          SpanList _ _ -> [exit, updown, select]
          SpanDetails _ _ -> [exit, unselect]
   in Brick.vBox
        [ Brick.padTop Brick.Max Border.hBorder,
          shortcuts
            |> Text.join "   "
            |> Brick.txt
            |> Center.hCenter
        ]

viewContents :: Page -> Brick.Widget ()
viewContents page =
  case page of
    NoLogsFound ->
      Brick.txt "Waiting for logs...\n\nGo run some tests!"
        |> Center.hCenter
    SpanList now logs ->
      logs
        |> Zipper.indexedMap
          ( \i (time, span) ->
              Brick.hBox
                [ Brick.txt (howFarBack time now)
                    |> Brick.padLeft Brick.Max
                    |> Brick.hLimit 20,
                  Brick.txt "   ",
                  Brick.txt (Platform.name span)
                    |> Brick.padRight Brick.Max
                ]
                |> Center.hCenter
                |> if i == 0
                  then Brick.withAttr "selected"
                  else identity
          )
        |> Zipper.toList
        |> Brick.vBox
        |> Brick.padLeftRight 1
    SpanDetails rootSpan spans ->
      Brick.vBox
        [ Brick.txt (Platform.name rootSpan)
            |> Center.hCenter,
          Border.hBorder,
          spans
            |> Zipper.indexedMap
              ( \i span ->
                  Brick.hBox
                    [ Brick.txt (Platform.name (original span))
                        |> Brick.padLeft (Brick.Pad (Prelude.fromIntegral (2 * (nesting span))))
                    ]
                    |> if i == 0
                      then Brick.withAttr "selected"
                      else identity
              )
            |> Zipper.toList
            |> Brick.vBox
            |> Brick.padLeftRight 1
        ]

howFarBack :: Time.UTCTime -> Time.UTCTime -> Text
howFarBack date1 date2
  | diff < 60 = "seconds ago"
  | diff < 60 * 60 = Text.fromInt (diff `Prelude.div` 60) ++ " minutes ago"
  | diff < 60 * 60 * 24 = Text.fromInt (diff `Prelude.div` (60 * 60)) ++ " hours ago"
  | Prelude.otherwise = Text.fromInt (diff `Prelude.div` (60 * 60 * 24)) ++ " days ago"
  where
    diff =
      Time.diffUTCTime date1 date2
        |> Prelude.round
        |> abs

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
  now <- Time.getCurrentTime
  Async.race_
    ( Async.race_
        ( System.IO.withFile
            logFile
            System.IO.ReadMode
            (tailLines partOfLine (AddLogline >> pushMsg))
        )
        (updateTime (SetCurrentTime >> pushMsg))
    )
    ( Brick.customMain
        initialVty
        buildVty
        (Just eventChan)
        (app pushMsg)
        (init now)
    )

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
        Vty.EvKey (Vty.KChar 'q') [] -> do Brick.halt model
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
        Vty.EvKey Vty.KEnter [] -> do
          liftIO (pushMsg ShowDetails)
          Brick.continue model
        Vty.EvKey (Vty.KChar 'l') [] -> do
          liftIO (pushMsg ShowDetails)
          Brick.continue model
        Vty.EvKey Vty.KBS [] -> do
          liftIO (pushMsg Exit)
          Brick.continue model
        Vty.EvKey (Vty.KChar 'h') [] -> do
          liftIO (pushMsg Exit)
          Brick.continue model
        -- Fallback
        _ -> Brick.continue model
    (Brick.MouseDown _ _ _ _) -> Brick.continue model
    (Brick.MouseUp _ _ _) -> Brick.continue model
    (Brick.AppEvent msg) -> update model msg

updateTime :: (Time.UTCTime -> Prelude.IO ()) -> Prelude.IO ()
updateTime withTime = do
  time <- Time.getCurrentTime
  withTime time
  Control.Concurrent.threadDelay 10_000_000 {- 10 s -}
  updateTime withTime

-- Tail a file handle, calling a callback function every time a new line is
-- read. This function will intentionally hang, waiting for additional input to
-- the handle it's reading from.
tailLines ::
  IORef.IORef Builder.Builder ->
  (ByteString.ByteString -> Prelude.IO ()) ->
  System.IO.Handle ->
  Prelude.IO ()
tailLines partOfLine withLine handle = do
  chunk <- ByteString.hGetSome handle 10_000 {- 10 kb -}
  case ByteString.split 10 {- \n -} chunk of
    [] -> do
      Control.Concurrent.threadDelay 100_000 {- 100 ms -}
      tailLines partOfLine withLine handle
    [""] -> do
      Control.Concurrent.threadDelay 100_000 {- 100 ms -}
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
