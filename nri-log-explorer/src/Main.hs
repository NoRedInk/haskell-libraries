{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import qualified Brick
import qualified Brick.BChan
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Edit as Edit
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
import qualified Data.Text.Zipper as TZ
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified GHC.IO.Encoding
import qualified GHC.Stack as Stack
import qualified Graphics.Vty as Vty
import Lens.Micro ((^.))
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
import qualified Text.Fuzzy as Fuzzy
import qualified Prelude

data Model = Model
  { -- Used to calculate "2 minutes ago" type info for root spans.
    currentTime :: Time.UTCTime,
    -- A tool like pbcopy or xclip for copying to clipboard.
    clipboardCommand :: Maybe Text,
    allRootSpans :: List RootSpan,
    -- The actual data displayed.
    rootSpans :: ListWidget.List Name RootSpan,
    -- If we're in the detail view for a root span, this will contain a copy of
    -- the data of that particular span in a format more suitable for this view.
    selectedRootSpan :: Maybe (ListWidget.List Name Span),
    -- Loading in initial data happens piecemeal, starting with the oldest data
    -- first, so visually filling the view from the bottom up. Until the user
    -- interacts we're going to keep the focus on the last-loaded element, but
    -- once the user starts controlling the app themselves we want to keep our
    -- hands off the controls.
    userDidSomething :: Bool,
    filter :: Filter
  }

data Filter
  = NoFilter
  | HasFilter Text (List Text)
  | Filtering (Maybe (Text, List Text)) (Edit.Editor Text Name)

-- One log entry on the main page. The Platform.TracingSpan contains the data we
-- parsed (it in turn contains nested child spans, and so on).
data RootSpan = RootSpan
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
  = AddRootSpan ByteString.ByteString
  | ShowDetails
  | Exit
  | SetCurrentTime Time.UTCTime
  | CopyDetails
  | ShowFilter
  | ClearFilter
  | StopFiltering (Maybe (Text, List Text))
  | ApplyFilter (Edit.Editor Text Name)
  | HandleFiltering (Maybe (Text, List Text)) (Edit.Editor Text Name)

-- Brick's view elements have a Widget type, which is sort of the equivalent of
-- the Html type in an Elm application. Unlike Elm those widgets can have their
-- own state not stored in the main Model type above. Widgets that have state
-- like that need a unique name which brick uses as a key for storage.
data Name
  = RootSpanList
  | SpanBreakdownList Prelude.Int
  | FilterField
  deriving (Eq, Ord, Show)

-- An alternative data type containing part of the same data as above, in a
-- format more convenient for some update and view functions.
data Page
  = NoDataPage Filter
  | RootSpanPage Time.UTCTime Filter (ListWidget.List Name RootSpan)
  | SpanBreakdownPage (Maybe Text) RootSpan (ListWidget.List Name Span)

toPage :: Model -> Page
toPage model =
  case (selectedRootSpan model, ListWidget.listSelectedElement (rootSpans model)) of
    (_, Nothing) -> NoDataPage (filter model)
    (Just selected, Just (_, rootSpan)) ->
      SpanBreakdownPage
        (clipboardCommand model)
        rootSpan
        selected
    (Nothing, Just _) -> RootSpanPage (currentTime model) (filter model) (rootSpans model)

withPage :: Model -> (Page -> Brick.EventM Name Page) -> Brick.EventM Name Model
withPage model fn =
  map
    ( \newPage ->
        case newPage of
          NoDataPage filter -> model {selectedRootSpan = Nothing, filter}
          RootSpanPage _ _ rootSpans ->
            model {selectedRootSpan = Nothing, rootSpans = rootSpans}
          SpanBreakdownPage _ _ spans ->
            model {selectedRootSpan = Just spans}
    )
    (fn (toPage model))

init :: Maybe Text -> Time.UTCTime -> Model
init clipboardCommand now =
  Model
    { currentTime = now,
      clipboardCommand = clipboardCommand,
      allRootSpans = [],
      rootSpans = ListWidget.list RootSpanList Prelude.mempty 1,
      selectedRootSpan = Nothing,
      userDidSomething = False,
      filter = NoFilter
    }

update :: Model -> Msg -> Brick.EventM Name (Brick.Next Model)
update model msg =
  case msg of
    SetCurrentTime time ->
      model {currentTime = time}
        |> Brick.continue
    AddRootSpan line ->
      case Aeson.decodeStrict' line of
        Nothing ->
          -- If a line cannot be parsed we ignore it for now.
          Brick.continue model
        Just (date, span) -> do
          let rootSpan = RootSpan date span
              newModel =
                model
                  { allRootSpans = rootSpan : allRootSpans model,
                    rootSpans = ListWidget.listInsert 0 rootSpan (rootSpans model)
                  }
          -- If the user hasn't interacted yet keep the focus on the top span,
          -- so we don't start the user off at the bottom of the page (spans are
          -- read in oldest-first).
          if userDidSomething model
            then Brick.continue newModel
            else
              scroll (Prelude.pure << ListWidget.listMoveTo 0) newModel
                |> andThen Brick.continue
    ShowDetails ->
      withPage
        model
        ( \page ->
            case page of
              NoDataPage _ -> Prelude.pure page
              SpanBreakdownPage _ _ _ -> Prelude.pure page
              RootSpanPage _ _ spans ->
                case ListWidget.listSelectedElement spans of
                  Nothing -> Prelude.pure page
                  Just (currentIndex, currentSpan) ->
                    SpanBreakdownPage
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
        NoDataPage _ -> Prelude.pure ()
        RootSpanPage _ _ _ -> Prelude.pure ()
        SpanBreakdownPage Nothing _ _ -> Prelude.pure ()
        SpanBreakdownPage (Just cmd) _ spans ->
          case ListWidget.listSelectedElement spans of
            Nothing -> Prelude.pure ()
            Just (_, currentSpan) ->
              original currentSpan
                |> spanToClipboard cmd
                |> liftIO
      continueAfterUserInteraction model
    ShowFilter ->
      let editor = Edit.editorText FilterField (Just 1) ""
       in continueAfterUserInteraction
            model
              { filter =
                  case filter model of
                    NoFilter -> Filtering Nothing editor
                    Filtering previous _ -> Filtering previous editor
                    HasFilter first rest ->
                      Filtering (Just (first, rest))
                        <| Edit.applyEdit (\_ -> TZ.gotoEOL <| TZ.textZipper [Text.join " " (first : rest)] (Just 1)) editor
              }
    ClearFilter ->
      continueAfterUserInteraction
        <| case filter model of
          HasFilter _ _ ->
            model
              { filter = NoFilter,
                rootSpans = ListWidget.list RootSpanList (Vector.fromList (allRootSpans model)) 1
              }
          _ -> model
    StopFiltering maybePrevious ->
      continueAfterUserInteraction
        <| case maybePrevious of
          Just (first, rest) -> model {filter = HasFilter first rest, rootSpans = filterRootSpans first rest model}
          Nothing -> model {filter = NoFilter, rootSpans = ListWidget.list RootSpanList (Vector.fromList (allRootSpans model)) 1}
    ApplyFilter filterEditor ->
      continueAfterUserInteraction
        <| case getFiltersFromEditor filterEditor of
          [] ->
            model
              { filter = NoFilter,
                rootSpans = ListWidget.list RootSpanList (Vector.fromList (allRootSpans model)) 1
              }
          first : rest -> model {filter = HasFilter first rest}
    HandleFiltering previous filterEditor ->
      continueAfterUserInteraction
        model
          { filter = Filtering previous filterEditor,
            rootSpans = case getFiltersFromEditor filterEditor of
              [] ->
                ListWidget.list RootSpanList (Vector.fromList (allRootSpans model)) 1
              first : rest -> filterRootSpans first rest model
          }

filterRootSpans :: Text -> List Text -> Model -> ListWidget.List Name RootSpan
filterRootSpans first rest model =
  ListWidget.list
    RootSpanList
    ( List.filter
        (\RootSpan {logSpan} -> List.all (\filter -> fuzzyMatch filter (filterSummary logSpan)) (first : rest))
        (allRootSpans model)
        |> Vector.fromList
    )
    1
  where
    fuzzyMatch x y = Fuzzy.match x y "" "" identity False /= Nothing

getFiltersFromEditor :: Edit.Editor Text Name -> List Text
getFiltersFromEditor editor =
  Edit.getEditContents editor
    |> Prelude.mconcat
    |> Text.trim
    |> Text.split " "
    |> List.filter (not << Text.isEmpty)

scroll ::
  (forall a. ListWidget.List Name a -> Brick.EventM Name (ListWidget.List Name a)) ->
  Model ->
  Brick.EventM Name Model
scroll move model =
  withPage model <| \page -> do
    case page of
      NoDataPage filter -> Prelude.pure (NoDataPage filter)
      RootSpanPage time filter rootSpans ->
        move rootSpans
          |> map (RootSpanPage time filter)
      SpanBreakdownPage cmd root spans ->
        move spans
          |> map (SpanBreakdownPage cmd root)

toFlatList :: Prelude.Int -> Platform.TracingSpan -> ListWidget.List Name Span
toFlatList id span =
  ListWidget.list
    (SpanBreakdownList id)
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
          [ viewFilter page,
            viewContents page,
            viewKey page
          ]
      ]

viewFilter :: Page -> Brick.Widget Name
viewFilter page =
  case page of
    SpanBreakdownPage _ _ _ -> Brick.txt ""
    NoDataPage filter -> viewFilter' filter
    RootSpanPage _ filter _ -> viewFilter' filter
  where
    viewFilter' filter =
      case filter of
        NoFilter -> Brick.txt ""
        HasFilter first rest ->
          Brick.vBox
            [ Brick.hBox
                ( Brick.txt "Filter: " :
                  List.intersperse
                    (Brick.txt " ")
                    (List.map (Brick.modifyDefAttr modReverse << Brick.txt) (first : rest))
                ),
              Border.hBorder
            ]
        Filtering _ filterEditor ->
          Brick.vBox
            [ viewFiltering filterEditor,
              Border.hBorder
            ]

viewFiltering :: Edit.Editor Text Name -> Brick.Widget Name
viewFiltering editor =
  Brick.hBox
    [ Brick.txt "Filter: ",
      Edit.renderEditor contentWithCursor True editor
    ]
  where
    contentWithCursor t =
      let (_, cursorPos) = TZ.cursorPosition (editor ^. Edit.editContentsL)
          (before, after) = Data.Text.splitAt cursorPos (Prelude.mconcat t)
       in Brick.hBox
            <| Brick.txt before :
          case Data.Text.uncons after of
            Just (x, rest) -> [Brick.modifyDefAttr modReverse <| Brick.txt <| Data.Text.singleton x, Brick.txt rest]
            Nothing -> [Brick.modifyDefAttr modReverse <| Brick.txt " ", Brick.txt after]

modReverse :: Vty.Attr -> Vty.Attr
modReverse attr = Vty.withStyle attr Vty.reverseVideo

viewKey :: Page -> Brick.Widget Name
viewKey page =
  let exit = "q: exit"
      updown = "↑↓: select"
      select = "enter: details"
      unselect = "backspace: back"
      copy = "y: copy details"
      adjustFilter = "/: adjust filter"
      clearFilter = "x: clear filter"
      stopFiltering = "esc: stop filtering"
      shortcuts =
        case page of
          NoDataPage filter ->
            case filter of
              NoFilter -> [exit]
              Filtering _ _ -> [exit, stopFiltering]
              _ -> [exit, adjustFilter, clearFilter]
          RootSpanPage _ filter _ ->
            case filter of
              NoFilter -> [exit, updown, select, "/: filter"]
              HasFilter _ _ -> [exit, updown, select, adjustFilter, clearFilter]
              Filtering (Just _) _ -> [stopFiltering, "enter: apply filter"]
              Filtering Nothing _ -> [stopFiltering, "enter: apply filter"]
          SpanBreakdownPage clipboardCommand _ _ ->
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
    NoDataPage NoFilter ->
      Brick.txt "Waiting for logs...\n\nGo run some tests!"
        |> Center.hCenter
        |> Brick.padBottom Brick.Max
    NoDataPage _ ->
      Brick.txt "Waiting for logs or adjust filter...\n\nGo run some tests!"
        |> Center.hCenter
        |> Brick.padBottom Brick.Max
    RootSpanPage now _ logs ->
      logs
        |> ListWidget.renderList
          ( \hasFocus RootSpan {logSpan, logTime} ->
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
    SpanBreakdownPage _ rootSpan spans ->
      Brick.vBox
        [ Brick.txt (spanSummary (logSpan rootSpan))
            |> Center.hCenter,
          Border.hBorder,
          Brick.hBox
            [ viewSpanBreakdown spans
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

viewSpanBreakdown :: ListWidget.List Name Span -> Brick.Widget Name
viewSpanBreakdown spans =
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
    [ viewDetail "name" (Platform.name original),
      case Platform.summary original of
        Nothing -> Brick.emptyWidget
        Just summary -> viewDetail "summary" summary,
      viewDetail
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
          viewDetail
            "source"
            ( Data.Text.pack (Stack.srcLocFile srcLoc)
                ++ ":"
                ++ Text.fromInt (Prelude.fromIntegral (Stack.srcLocStartLine srcLoc))
            ),
      case Platform.succeeded original of
        Platform.Succeeded -> viewDetail "result" "succeeded"
        Platform.Failed -> viewDetail "result" "failed"
        Platform.FailedWith exception ->
          viewDetail
            "failed with"
            ( Exception.displayException exception
                |> Data.Text.pack
            ),
      case map Aeson.toJSON (Platform.details original) of
        Nothing -> Brick.emptyWidget
        Just Aeson.Null -> Brick.emptyWidget
        Just (Aeson.String str) -> viewDetail "details" str
        Just (Aeson.Number number) ->
          Data.Text.pack (Prelude.show number)
            |> viewDetail "details"
        Just (Aeson.Bool bool) ->
          Data.Text.pack (Prelude.show bool)
            |> viewDetail "details"
        Just (Aeson.Array array) ->
          viewDetail
            "details"
            ( Data.Aeson.Encode.Pretty.encodePrettyToTextBuilder array
                |> Data.Text.Lazy.Builder.toLazyText
                |> Data.Text.Lazy.toStrict
            )
        Just (Aeson.Object object) ->
          HashMap.toList object
            |> List.map
              ( \(name, val) ->
                  viewDetail
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

viewDetail :: Text -> Text -> Brick.Widget Name
viewDetail label val =
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

filterSummary :: Platform.TracingSpan -> Text
filterSummary span =
  Text.join
    ""
    [ case Platform.succeeded span of
        Platform.Succeeded -> "succeeded "
        Platform.Failed -> "failed "
        Platform.FailedWith _ -> "failed ",
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
            (tailLines partOfLine (AddRootSpan >> pushMsg))
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
      case filter model of
        Filtering previous filterEditor ->
          case vtyEvent of
            Vty.EvKey Vty.KEsc [] -> do
              liftIO (pushMsg (StopFiltering previous))
              Brick.continue model
            Vty.EvKey Vty.KEnter [] -> do
              liftIO (pushMsg (ApplyFilter filterEditor))
              Brick.continue model
            _ -> do
              newEditor <- Edit.handleEditorEvent vtyEvent filterEditor
              liftIO (pushMsg (HandleFiltering previous newEditor))
              Brick.continue model
        _ -> case vtyEvent of
          -- Quiting
          Vty.EvKey (Vty.KChar 'q') [] -> do Brick.halt model
          Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt model
          -- Navigation
          Vty.EvKey Vty.KEnter [] -> do
            liftIO (pushMsg ShowDetails)
            Brick.continue model
          Vty.EvKey (Vty.KChar 'l') [] -> do
            liftIO (pushMsg ShowDetails)
            Brick.continue model
          Vty.EvKey Vty.KBS [] -> do
            liftIO (pushMsg Exit)
            Brick.continue model
          Vty.EvKey Vty.KEsc [] -> do
            liftIO (pushMsg Exit)
            Brick.continue model
          Vty.EvKey (Vty.KChar 'h') [] -> do
            liftIO (pushMsg Exit)
            Brick.continue model
          -- Clipboard
          Vty.EvKey (Vty.KChar 'y') [] -> do
            liftIO (pushMsg CopyDetails)
            Brick.continue model
          Vty.EvKey (Vty.KChar '/') [] -> do
            liftIO (pushMsg ShowFilter)
            Brick.continue model
          Vty.EvKey (Vty.KChar 'x') [] -> do
            liftIO (pushMsg ClearFilter)
            Brick.continue model
          -- Fallback
          _ ->
            scroll
              (ListWidget.handleListEventVi ListWidget.handleListEvent vtyEvent)
              model
              |> andThen Brick.continue
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
