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
import qualified Zipper
import qualified Prelude

data Model = Model
  { -- Used to calculate "2 minutes ago" type info for root spans.
    currentTime :: Time.UTCTime,
    -- A tool like pbcopy or xclip for copying to clipboard.
    clipboardCommand :: Maybe Text,
    -- The actual data displayed. In a zipper because one is always selected.
    loglines :: Maybe (Zipper.Zipper Logline),
    -- If we're in the detail view for a root span, this will contain a copy of
    -- the data of that particular span in a format more suitable for this view.
    selectedRootSpan :: Maybe (Zipper.Zipper Span),
    -- Loading in initial data happens piecemeal, starting with the oldest data
    -- first, so visually filling the view from the bottom up. Until the user
    -- interacts we're going to keep the focus on the last-loaded element, but
    -- once the user starts controlling the app themselves we want to keep our
    -- hands off the controls.
    userDidSomething :: Bool,
    -- Incrementing number to give all the root spans a unique identifier.
    lastId :: Id
  }

-- One log entry on the main page. The Platform.TracingSpan contains the data we
-- parsed (it in turn contains nested child spans, and so on).
data Logline = Logline
  { logId :: Id,
    logTime :: Time.UTCTime,
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

newtype Id = Id Int deriving (Prelude.Num, Eq, Ord, Show)

-- Brick's view elements have a Widget type, which is sort of the equivalent of
-- the Html type in an Elm application. Unlike Elm those widgets can have their
-- own state not stored in the main Model type above. Widgets that have state
-- like that need a unique name which brick uses as a key for storage.
data Name
  = RootSpanListViewport
  | RootSpanLine Id
  | SpanDetailsListViewport Id
  | SpanLine Id Int
  deriving (Eq, Ord, Show)

-- An alternative data type containing part of the same data as above, in a
-- format more convenient for some update and view functions.
data Page
  = NoLogsFound
  | SpanList Time.UTCTime (Zipper.Zipper Logline)
  | SpanDetails (Maybe Text) Logline (Zipper.Zipper Span)

toPage :: Model -> Page
toPage model =
  case (selectedRootSpan model, loglines model) of
    (Just selected, Just spans) ->
      SpanDetails
        (clipboardCommand model)
        (Zipper.current spans)
        selected
    (Nothing, Just spans) -> SpanList (currentTime model) spans
    (_, Nothing) -> NoLogsFound

withPage :: Model -> (Page -> Brick.EventM Name Page) -> Brick.EventM Name Model
withPage model fn =
  map
    ( \newPage ->
        case newPage of
          NoLogsFound -> model {selectedRootSpan = Nothing, loglines = Nothing}
          SpanList _ zipper -> model {selectedRootSpan = Nothing, loglines = Just zipper}
          SpanDetails _ _ zipper -> model {selectedRootSpan = Just zipper}
    )
    (fn (toPage model))

init :: Maybe Text -> Time.UTCTime -> Model
init clipboardCommand now =
  Model
    { currentTime = now,
      clipboardCommand = clipboardCommand,
      loglines = Nothing,
      selectedRootSpan = Nothing,
      userDidSomething = False,
      lastId = 0
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
          let newId = lastId model + 1
              logline = Logline newId date span
              newModel =
                model
                  { loglines =
                      case loglines model of
                        Nothing -> Just (Zipper.singleton logline)
                        Just zipper ->
                          -- Each line we read represents a newer log entry then
                          -- the ones before it, so we insert it at the top.
                          Just (Zipper.prepend [logline] zipper),
                    lastId = newId
                  }
          -- If the user hasn't interacted yet keep the focus on the top span,
          -- so we don't start the user off at the bottom of the page (spans are
          -- read in oldest-first).
          if userDidSomething model
            then Brick.continue newModel
            else
              moveZipper Zipper.first newModel
                |> andThen Brick.continue
    DownBy n ->
      moveZipper (repeat n Zipper.next) model
        |> andThen continueAfterUserInteraction
    UpBy n ->
      moveZipper (repeat n Zipper.prev) model
        |> andThen continueAfterUserInteraction
    ShowDetails ->
      withPage
        model
        ( \page ->
            case page of
              NoLogsFound -> Prelude.pure page
              SpanDetails _ _ _ -> Prelude.pure page
              SpanList _ spans ->
                SpanDetails
                  (clipboardCommand model)
                  (Zipper.current spans)
                  (Zipper.current spans |> logSpan |> toFlatList)
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
          original (Zipper.current spans)
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
  (forall a. Zipper.Zipper a -> Zipper.Zipper a) ->
  Model ->
  Brick.EventM Name Model
moveZipper move model = do
  withPage model <| \page ->
    case page of
      NoLogsFound -> Prelude.pure NoLogsFound
      SpanList time spans -> do
        let newSpans = move spans
        Brick.invalidateCacheEntry (RootSpanLine (logId (Zipper.current spans)))
        Brick.invalidateCacheEntry (RootSpanLine (logId (Zipper.current newSpans)))
        Prelude.pure (SpanList time newSpans)
      SpanDetails cmd root spans -> do
        let newSpans = move spans
        Brick.invalidateCacheEntry (SpanLine (logId root) (Zipper.currentIndex spans))
        Brick.invalidateCacheEntry (SpanLine (logId root) (Zipper.currentIndex newSpans))
        Prelude.pure (SpanDetails cmd root newSpans)

repeat :: Int -> (a -> a) -> a -> a
repeat n f x =
  if n <= 0
    then x
    else f x |> repeat (n - 1) f

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
    } :
  List.concatMap (toFlatListHelper (nesting + 1)) (List.reverse (Platform.children span))

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
        |> Zipper.indexedMap
          ( \i Logline {logId, logSpan, logTime} ->
              Brick.hBox
                [ Brick.txt (howFarBack logTime now)
                    |> Brick.padRight Brick.Max
                    |> Brick.hLimit 15,
                  Brick.txt "   ",
                  Brick.txt (spanSummary logSpan)
                    |> Brick.padRight Brick.Max
                ]
                |> Center.hCenter
                |> Brick.cached (RootSpanLine logId)
                |> if i == 0
                  then Brick.withAttr "selected" >> Brick.visible
                  else identity
          )
        |> Zipper.toList
        |> Brick.vBox
        |> Brick.viewport RootSpanListViewport Brick.Vertical
        |> Brick.padLeftRight 1
    SpanDetails _ logline spans ->
      Brick.vBox
        [ Brick.txt (spanSummary (logSpan logline))
            |> Center.hCenter,
          Border.hBorder,
          Brick.hBox
            [ viewSpanList logline spans
                |> Brick.hLimitPercent 50,
              viewSpanDetails (Zipper.current spans)
                |> Brick.padRight (Brick.Pad 1)
                |> Brick.padRight Brick.Max
            ]
        ]

viewSpanList :: Logline -> Zipper.Zipper Span -> Brick.Widget Name
viewSpanList Logline {logId} spans =
  spans
    |> Zipper.indexedMap
      ( \i span ->
          Brick.hBox
            [ Brick.txt (spanSummary (original span))
                |> Brick.padLeft (Brick.Pad (Prelude.fromIntegral (2 * (nesting span))))
                |> Brick.padRight Brick.Max
            ]
            |> Brick.cached (SpanLine logId (i + Zipper.currentIndex spans))
            |> if i == 0
              then Brick.withAttr "selected" >> Brick.visible
              else identity
      )
    |> Zipper.toList
    |> Brick.vBox
    |> Brick.viewport (SpanDetailsListViewport logId) Brick.Vertical
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
