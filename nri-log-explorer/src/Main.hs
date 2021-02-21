{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main,
  )
where

import qualified Brick
import qualified Brick.BChan
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.List as ListWidget
import qualified Control.Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified GHC.IO.Encoding
import qualified GHC.Stack as Stack
import qualified Graphics.Vty as Vty
import qualified List
import NriPrelude
import qualified Paths_nri_log_explorer as Paths
import qualified Platform
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.IO
import qualified System.Process
import qualified Text
import qualified Prelude

data Model = Model
  { -- Used to calculate "2 minutes ago" type info for root spans.
    currentTime :: Time.UTCTime,
    -- A tool like pbcopy or xclip for copying to clipboard.
    clipboardCommand :: Maybe Text,
    -- The actual data displayed.
    loglines :: ListWidget.List Name Logline,
    -- If we're in the detail view for a root span, this will contain a copy of
    -- the data of that particular span in a format more suitable for this view.
    selectedRootSpan :: Maybe (ListWidget.List Name Span),
    -- Loading in initial data happens piecemeal, starting with the oldest data
    -- first, so visually filling the view from the bottom up. Until the user
    -- interacts we're going to keep the focus on the last-loaded element, but
    -- once the user starts controlling the app themselves we want to keep our
    -- hands off the controls.
    userDidSomething :: Bool
  }

-- One log entry on the main page. The Platform.TracingSpan contains the data we
-- parsed (it in turn contains nested child spans, and so on).
data Logline = Logline
  { logTime :: Time.UTCTime,
    logSpan :: Platform.TracingSpan
  }

-- A single span in the detail view of a root span. The root span has nesting 0,
-- its children nesting 1, and so on.
data Span = Span
  { nesting :: Int,
    original :: Platform.TracingSpan
  }

data Msg
  = AddLogline ByteString.ByteString
  | DownBy Int
  | UpBy Int
  | ShowDetails
  | Exit
  | SetCurrentTime Time.UTCTime
  | CopyDetails

-- Brick's view elements have a Widget type, which is sort of the equivalent of
-- the Html type in an Elm application. Unlike Elm those widgets can have their
-- own state not stored in the main Model type above. Widgets that have state
-- like that need a unique name which brick uses as a key for storage.
data Name
  = RootSpanList
  | RootSpanBreakdown Prelude.Int
  deriving (Eq, Ord, Show)

-- An alternative data type containing part of the same data as above, in a
-- format more convenient for some update and view functions.
data Page
  = NoLogsFound
  | SpanList Time.UTCTime (ListWidget.List Name Logline)
  | SpanDetails (Maybe Text) Logline (ListWidget.List Name Span)

toPage :: Model -> Page
toPage model =
  case (selectedRootSpan model, ListWidget.listSelectedElement (loglines model)) of
    (_, Nothing) -> NoLogsFound
    (Just selected, Just (_, elem)) ->
      SpanDetails
        (clipboardCommand model)
        elem
        selected
    (Nothing, Just _) -> SpanList (currentTime model) (loglines model)

withPage :: Model -> (Page -> Brick.EventM Name Page) -> Brick.EventM Name Model
withPage model fn =
  map
    ( \newPage ->
        case newPage of
          NoLogsFound -> model {selectedRootSpan = Nothing}
          SpanList _ zipper -> model {selectedRootSpan = Nothing, loglines = zipper}
          SpanDetails _ _ zipper -> model {selectedRootSpan = Just zipper}
    )
    (fn (toPage model))

init :: Maybe Text -> Time.UTCTime -> Model
init clipboardCommand now =
  Model
    { currentTime = now,
      clipboardCommand = clipboardCommand,
      loglines = ListWidget.list RootSpanList Prelude.mempty 1,
      selectedRootSpan = Nothing,
      userDidSomething = False
    }

update :: Model -> Msg -> Brick.EventM Name (Brick.Next Model)
update model msg =
  case msg of
    SetCurrentTime time ->
      model {currentTime = time}
        |> Brick.continue
    AddLogline line ->
      case Aeson.decodeStrict' line of
        Nothing ->
          -- If a line cannot be parsed we ignore it for now.
          Brick.continue model
        Just (date, span) -> do
          let logline = Logline date span
              newModel =
                model
                  { loglines =
                      ListWidget.listInsert
                        0
                        logline
                        (loglines model)
                  }
          -- If the user hasn't interacted yet keep the focus on the top span,
          -- so we don't start the user off at the bottom of the page (spans are
          -- read in oldest-first).
          if userDidSomething model
            then Brick.continue newModel
            else
              moveZipper (ListWidget.listMoveTo 0) newModel
                |> andThen Brick.continue
    DownBy n ->
      moveZipper (repeat n ListWidget.listMoveDown) model
        |> andThen continueAfterUserInteraction
    UpBy n ->
      moveZipper (repeat n ListWidget.listMoveUp) model
        |> andThen continueAfterUserInteraction
    ShowDetails ->
      withPage
        model
        ( \page ->
            case page of
              NoLogsFound -> Prelude.pure page
              SpanDetails _ _ _ -> Prelude.pure page
              SpanList _ spans ->
                case ListWidget.listSelectedElement spans of
                  Nothing -> Prelude.pure page
                  Just (currentIndex, currentSpan) ->
                    SpanDetails
                      (clipboardCommand model)
                      currentSpan
                      (currentSpan |> logSpan |> toFlatList currentIndex)
                      |> Prelude.pure
        )
        |> andThen continueAfterUserInteraction
    Exit ->
      model
        { selectedRootSpan = Nothing
        }
        |> continueAfterUserInteraction
    CopyDetails -> do
      case toPage model of
        NoLogsFound -> Prelude.pure ()
        SpanList _ _ -> Prelude.pure ()
        SpanDetails Nothing _ _ -> Prelude.pure ()
        SpanDetails (Just cmd) _ spans ->
          case ListWidget.listSelectedElement spans of
            Nothing -> Prelude.pure ()
            Just (_, currentSpan) ->
              original currentSpan
                |> spanToClipboard cmd
                |> liftIO
      continueAfterUserInteraction model

-- The root span and detail span pages can each come to maintain a lot of rows.
-- To keep performance acceptable we make use of Brick's view caching
-- functionality. That works well because the data we read is immutable.
--
-- The view for a row only changes when we move focus to it or away from it (at
-- this point emphasis-styling gets applied), so whenever we move the focused
-- element of a zipper we need to invalidate the cache for two entries. This
-- function does so correctly.
--
-- The first argument is supposed to be a focus-changing zipper funtion, such as
-- `Zipper.next` or `Zipper.first`.
moveZipper ::
  (forall a. ListWidget.List Name a -> ListWidget.List Name a) ->
  Model ->
  Brick.EventM Name Model
moveZipper move model =
  withPage model <| \page ->
    case page of
      NoLogsFound -> Prelude.pure NoLogsFound
      SpanList time spans ->
        Prelude.pure (SpanList time (move spans))
      SpanDetails cmd root spans ->
        Prelude.pure (SpanDetails cmd root (move spans))

repeat :: Int -> (a -> a) -> a -> a
repeat n f x =
  if n <= 0
    then x
    else f x |> repeat (n - 1) f

toFlatList :: Prelude.Int -> Platform.TracingSpan -> ListWidget.List Name Span
toFlatList id span =
  ListWidget.list
    (RootSpanBreakdown id)
    (toFlatListHelper 0 span)
    1

toFlatListHelper :: Int -> Platform.TracingSpan -> Vector.Vector Span
toFlatListHelper nesting span =
  Vector.cons
    Span
      { nesting = nesting,
        original = span
      }
    ( Platform.children span
        |> Vector.fromList
        |> Vector.reverse
        |> Prelude.foldMap (toFlatListHelper (nesting + 1))
    )

continueAfterUserInteraction :: Model -> Brick.EventM Name (Brick.Next Model)
continueAfterUserInteraction model =
  Brick.continue model {userDidSomething = True}

-- View functions

view :: Model -> [Brick.Widget Name]
view model =
  let page = toPage model
   in [ Brick.vBox
          [ viewContents page,
            viewKey page
          ]
      ]

viewKey :: Page -> Brick.Widget Name
viewKey page =
  let exit = "q: exit"
      updown = "↑↓: select"
      select = "enter: details"
      unselect = "backspace: back"
      copy = "y: copy details"
      shortcuts =
        case page of
          NoLogsFound -> [exit]
          SpanList _ _ -> [exit, updown, select]
          SpanDetails clipboardCommand _ _ ->
            [exit, updown, unselect]
              ++ ( case clipboardCommand of
                     Nothing -> []
                     Just _ -> [copy]
                 )
   in shortcuts
        |> Text.join "   "
        |> Brick.txt
        |> Center.hCenter

viewContents :: Page -> Brick.Widget Name
viewContents page =
  case page of
    NoLogsFound ->
      Brick.txt "Waiting for logs...\n\nGo run some tests!"
        |> Center.hCenter
        |> Brick.padBottom Brick.Max
    SpanList now logs ->
      logs
        |> ListWidget.renderList
          ( \hasFocus Logline {logSpan, logTime} ->
              Brick.hBox
                [ Brick.txt (howFarBack logTime now)
                    |> Brick.padRight Brick.Max
                    |> Brick.hLimit 15,
                  Brick.txt "   ",
                  Brick.txt (spanSummary logSpan)
                    |> Brick.padRight Brick.Max
                ]
                |> Center.hCenter
                |> if hasFocus
                  then Brick.withAttr "selected"
                  else identity
          )
          True
        |> Brick.padLeftRight 1
    SpanDetails _ logline spans ->
      Brick.vBox
        [ Brick.txt (spanSummary (logSpan logline))
            |> Center.hCenter,
          Border.hBorder,
          Brick.hBox
            [ viewSpanList spans
                |> Brick.hLimitPercent 50,
              ( case ListWidget.listSelectedElement spans of
                  Nothing -> Brick.emptyWidget
                  Just (_, currentSpan) ->
                    viewSpanDetails currentSpan
              )
                |> Brick.padRight (Brick.Pad 1)
                |> Brick.padRight Brick.Max
            ]
        ]

viewSpanList :: ListWidget.List Name Span -> Brick.Widget Name
viewSpanList spans =
  spans
    |> ListWidget.renderList
      ( \hasFocus span ->
          Brick.hBox
            [ Brick.txt (spanSummary (original span))
                |> Brick.padLeft (Brick.Pad (Prelude.fromIntegral (2 * (nesting span))))
                |> Brick.padRight Brick.Max
            ]
            |> if hasFocus
              then Brick.withAttr "selected"
              else identity
      )
      True
    |> Brick.padLeftRight 1

viewSpanDetails :: Span -> Brick.Widget Name
viewSpanDetails Span {original} =
  Brick.vBox
    [ detailEntry "name" (Platform.name original),
      case Platform.summary original of
        Nothing -> Brick.emptyWidget
        Just summary -> detailEntry "summary" summary,
      detailEntry
        "duration"
        ( ( Platform.finished original - Platform.started original
              |> Platform.inMicroseconds
              |> Prelude.fromIntegral
              |> (\n -> n `Prelude.div` 1000)
              |> Text.fromInt
          )
            ++ " ms"
        ),
      case Platform.frame original of
        Nothing -> Brick.emptyWidget
        Just (_, srcLoc) ->
          detailEntry
            "source"
            ( Data.Text.pack (Stack.srcLocFile srcLoc)
                ++ ":"
                ++ Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine srcLoc))
            ),
      case Platform.succeeded original of
        Platform.Succeeded -> detailEntry "result" "succeeded"
        Platform.Failed -> detailEntry "result" "failed"
        Platform.FailedWith exception ->
          detailEntry
            "failed with"
            ( Exception.displayException exception
                |> Data.Text.pack
            ),
      case map Aeson.toJSON (Platform.details original) of
        Nothing -> Brick.emptyWidget
        Just Aeson.Null -> Brick.emptyWidget
        Just (Aeson.String str) -> detailEntry "details" str
        Just (Aeson.Number number) ->
          Data.Text.pack (Prelude.show number)
            |> detailEntry "details"
        Just (Aeson.Bool bool) ->
          Data.Text.pack (Prelude.show bool)
            |> detailEntry "details"
        Just (Aeson.Array array) ->
          detailEntry
            "details"
            ( Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder array
                |> Data.Text.Lazy.Builder.toLazyText
                |> Data.Text.Lazy.toStrict
            )
        Just (Aeson.Object object) ->
          HashMap.toList object
            |> List.map
              ( \(name, val) ->
                  detailEntry
                    name
                    ( case Aeson.toJSON val of
                        Aeson.Null -> "Null"
                        Aeson.String str -> str
                        Aeson.Number number ->
                          Data.Text.pack (Prelude.show number)
                        Aeson.Bool bool ->
                          Data.Text.pack (Prelude.show bool)
                        other ->
                          Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder other
                            |> Data.Text.Lazy.Builder.toLazyText
                            |> Data.Text.Lazy.toStrict
                    )
              )
            |> Brick.vBox
    ]

detailEntry :: Text -> Text -> Brick.Widget Name
detailEntry label val =
  Brick.hBox
    [ Brick.txt (label ++ ": ")
        |> Brick.padLeft Brick.Max
        |> Brick.hLimit 15,
      Brick.txtWrap val
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

spanSummary :: Platform.TracingSpan -> Text
spanSummary span =
  Text.join
    ""
    [ case Platform.succeeded span of
        Platform.Succeeded -> "  "
        Platform.Failed -> "✖ "
        Platform.FailedWith _ -> "✖ ",
      Platform.name span,
      case Platform.summary span of
        Nothing -> ""
        Just summary -> ": " ++ summary
    ]

-- Brick App boilerplate

main :: Prelude.IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    [] -> run
    ["--help"] ->
      [ "Usage:",
        "  log-explorer",
        "",
        "  --help         show this help message",
        "  --version      show application version",
        "  --clear        clear old log entries before starting",
        "",
        "log-explorer is a tool for exploring traces produced by the nri-prelude set of libraries."
      ]
        |> Prelude.unlines
        |> Prelude.putStrLn
    ["--version"] ->
      let version =
            Version.versionBranch Paths.version
              |> map Prelude.show
              |> Data.List.intercalate "."
       in Prelude.putStrLn ("log-explorer " ++ version)
    ["--clear"] -> do
      System.Directory.removeFile logFile
      run
    _ -> System.Exit.die "log-explorer was called with unknown arguments"

logFile :: Prelude.String
logFile = "/tmp/nri-prelude-logs"

run :: Prelude.IO ()
run = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  partOfLine <- IORef.newIORef Prelude.mempty
  System.IO.appendFile logFile "" -- touch file to ensure it exists
  eventChan <- Brick.BChan.newBChan 10
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  let pushMsg = Brick.BChan.writeBChan eventChan
  let pushMsgNonBlocking = Brick.BChan.writeBChanNonBlocking eventChan >> map (\_ -> ())
  now <- Time.getCurrentTime
  clipboardCommand <- chooseCommand copyCommands
  Async.race_
    ( Async.race_
        ( System.IO.withFile
            logFile
            System.IO.ReadMode
            (tailLines partOfLine (AddLogline >> pushMsg))
        )
        (updateTime (SetCurrentTime >> pushMsgNonBlocking))
    )
    ( Brick.customMain
        initialVty
        buildVty
        (Just eventChan)
        (app pushMsgNonBlocking)
        (init clipboardCommand now)
    )

app :: (Msg -> Prelude.IO ()) -> Brick.App Model Msg Name
app pushMsg =
  Brick.App
    { Brick.appDraw = view,
      Brick.appChooseCursor = \_ -> List.head,
      Brick.appHandleEvent = handleEvent pushMsg,
      Brick.appStartEvent = Prelude.pure,
      Brick.appAttrMap = \_ -> attrMap
    }

-- This is like a CSS stylesheet, mapping 'attribute names' (i.e. classes) to
-- styles.
attrMap :: Brick.AttrMap
attrMap =
  Brick.attrMap
    Vty.defAttr
    [ ("selected", Vty.withStyle Vty.defAttr Vty.reverseVideo)
    ]

handleEvent ::
  (Msg -> Prelude.IO ()) ->
  Model ->
  Brick.BrickEvent Name Msg ->
  Brick.EventM Name (Brick.Next Model)
handleEvent pushMsg model event =
  case event of
    (Brick.VtyEvent vtyEvent) ->
      case vtyEvent of
        -- Quiting
        Vty.EvKey (Vty.KChar 'q') [] -> do Brick.halt model
        Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt model
        -- Navigation
        Vty.EvKey Vty.KDown [] -> do
          liftIO (pushMsg (DownBy 1))
          Brick.continue model
        Vty.EvKey (Vty.KChar 'j') [] -> do
          liftIO (pushMsg (DownBy 1))
          Brick.continue model
        Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] -> do
          liftIO (pushMsg (DownBy 10))
          Brick.continue model
        Vty.EvKey Vty.KUp [] -> do
          liftIO (pushMsg (UpBy 1))
          Brick.continue model
        Vty.EvKey (Vty.KChar 'k') [] -> do
          liftIO (pushMsg (UpBy 1))
          Brick.continue model
        Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] -> do
          liftIO (pushMsg (UpBy 10))
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
        -- Clipboard
        Vty.EvKey (Vty.KChar 'y') [] -> do
          liftIO (pushMsg CopyDetails)
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
  -- We're using the relatively primitive operation `hGetSome` because it's one
  -- of the few that doesn't automatically close the file handle if the
  -- end-of-file is reached (this would be bad because we want to tail the file,
  -- wait for additional content to appear). The downside of this is that we
  -- need to split on newlines ourselves.
  chunk <- ByteString.hGetSome handle 10_000 {- 10 kb -}

  -- Splitting a bytestring on the newline symbol is safe, because UTF8
  -- guarantees the byte-encodings of ASCII characters (which include \n) never
  -- appear anywhere in multi-byte character encoders.
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
            firstFullLine :
            Prelude.init rest
      _ <- Prelude.traverse withLine fullLines
      tailLines partOfLine withLine handle

-- Clipboard management

chooseCommand :: [Text] -> Prelude.IO (Maybe Text)
chooseCommand [] = Prelude.pure Nothing
chooseCommand (first : rest) = do
  firstAvailable <-
    Data.Text.takeWhile (/= ' ') first
      |> isAppAvailable
  if firstAvailable
    then Prelude.pure (Just first)
    else chooseCommand rest

-- These are all commands that read stdin into the clipboard.
copyCommands :: [Text]
copyCommands =
  [ "wl-copy",
    "pbcopy",
    "xclip -selection c",
    "xsel -b -i"
  ]

isAppAvailable :: Text -> Prelude.IO Bool
isAppAvailable cmd = do
  System.Directory.findExecutable (Data.Text.unpack cmd)
    |> map (/= Nothing)

spanToClipboard :: Text -> Platform.TracingSpan -> Prelude.IO ()
spanToClipboard cmdAndArgs span =
  case Text.split " " cmdAndArgs of
    [] -> Prelude.pure ()
    cmd : args ->
      span {Platform.children = []}
        |> Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder
        |> Data.Text.Lazy.Builder.toLazyText
        |> Data.Text.Lazy.unpack
        |> System.Process.readProcessWithExitCode
          (Data.Text.unpack cmd)
          (List.map Data.Text.unpack args)
        |> liftIO
        |> map (\_ -> ())
