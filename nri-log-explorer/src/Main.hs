{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main,
  )
where

import qualified Brick
import qualified Brick.BChan
import Brick.Types (ViewportType (..))
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Zipper as TZ
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified Filterable
import qualified GHC.IO.Encoding
import qualified GHC.Stack as Stack
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Vty.Attributes
import qualified Graphics.Vty.Attributes.Color as Vty.Color
import Lens.Micro ((^.))
import qualified List
import NriPrelude
import qualified Paths_nri_log_explorer as Paths
import qualified Platform
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.IO
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.ByteString as Streams.ByteString
import qualified System.Process
import qualified Text
import qualified Text.Fuzzy as Fuzzy
import qualified Text.Regex.PCRE.Light as Regex
import qualified Prelude

data Model = Model
  { rootSpanPage :: RootSpanPageData,
    -- If we're in the detail view for a root span, this will contain a copy of
    -- the data of that particular span in a format more suitable for this view.
    spanBreakdownPage :: Maybe SpanBreakdownPageData,
    -- Loading in initial data happens piecemeal, starting with the oldest data
    -- first, so visually filling the view from the bottom up. Until the user
    -- interacts we're going to keep the focus on the last-loaded element, but
    -- once the user starts controlling the app themselves we want to keep our
    -- hands off the controls.
    userDidSomething :: Bool,
    -- A tool like pbcopy or xclip for copying to clipboard.
    clipboardCommand :: Maybe Text
  }

data RootSpanPageData = RootSpanPageData
  { -- Used to calculate "2 minutes ago" type info for root spans.
    currentTime :: Time.UTCTime,
    filter :: Filter,
    rootSpans :: Filterable.ListWidget Name RootSpan,
    failureFilter :: FailureFilter
  }

data SearchMatch = NoMatch | Matches Text
  deriving (Eq)

data SpanBreakdownPageData = SpanBreakdownPageData
  { currentSpan :: RootSpan,
    search :: Search,
    spans :: ListWidget.List Name (SearchMatch, Span),
    focus :: SpanBreakdownFocusedPane
  }

data SpanBreakdownFocusedPane = SpanList | SpanDetails
  deriving (Eq)

data FailureFilter = ShowAll | ShowOnlyFailures

data HistoryLength = FullHistory | MostRecent

data Filter
  = NoFilter
  | HasFilter (Edit.Editor Text Name)
  | EditFilter (Undo (Edit.Editor Text Name))

data Search
  = NoSearch
  | HasSearch (Edit.Editor Text Name)
  | EditSearch (Undo (Edit.Editor Text Name))

data Undo a = Undo {originalValue :: a, currentValue :: a}

initUndo :: a -> Undo a
initUndo x = Undo x x

setCurrent :: a -> Undo a -> Undo a
setCurrent currentValue undo = undo {currentValue}

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
  = AddRootSpan (Time.UTCTime, Platform.TracingSpan)
  | ShowDetails
  | Confirm
  | Cancel
  | SetCurrentTime Time.UTCTime
  | CopyDetails
  | EnterEdit
  | ClearEdit
  | Next
  | Previous
  | EditorEvent Vty.Event
  | ShowHideFailures
  | ToggleSpanBreakdownPageFocus
  | ScrollSpanDetails ScrollingDirection
  | Quit

data ScrollingDirection
  = ScrollUp
  | ScrollDown

-- Brick's view elements have a Widget type, which is sort of the equivalent of
-- the Html type in an Elm application. Unlike Elm those widgets can have their
-- own state not stored in the main Model type above. Widgets that have state
-- like that need a unique name which brick uses as a key for storage.
data Name
  = RootSpanList
  | SpanBreakdownList Prelude.Int
  | FilterField
  | SpanDetail
  deriving (Eq, Ord, Show)

-- An alternative data type containing part of the same data as above, in a
-- format more convenient for some update and view functions.
data Page
  = NoDataPage Filter FailureFilter
  | RootSpanPage RootSpanPageData
  | SpanBreakdownPage SpanBreakdownPageData

toPage :: Model -> Page
toPage model =
  case (spanBreakdownPage model, ListWidget.listSelectedElement <| Filterable.toListWidget (rootSpans (rootSpanPage model))) of
    (_, Nothing) -> NoDataPage (filter (rootSpanPage model)) (failureFilter (rootSpanPage model))
    (Just spanBreakdownPageData, _) -> SpanBreakdownPage spanBreakdownPageData
    (Nothing, _) -> RootSpanPage (rootSpanPage model)

-- Update the page when we don't need side effects
withPage :: Model -> (Page -> Page) -> Brick.EventM Name Model
withPage model fn =
  withPageEvent model (fn >> Prelude.pure)

withPageEvent :: Model -> (Page -> Brick.EventM Name Page) -> Brick.EventM Name Model
withPageEvent model fn =
  map
    ( \case
        NoDataPage filter failureFilter -> model {rootSpanPage = (rootSpanPage model) {filter, failureFilter}}
        RootSpanPage rootSpanPageData ->
          model {rootSpanPage = rootSpanPageData, spanBreakdownPage = Nothing}
        SpanBreakdownPage spanBreakdownPageData ->
          model {spanBreakdownPage = Just spanBreakdownPageData}
    )
    (fn (toPage model))

init :: Maybe Text -> Time.UTCTime -> FailureFilter -> Model
init clipboardCommand currentTime failureFilter =
  Model
    { rootSpanPage =
        RootSpanPageData
          { currentTime,
            rootSpans = Filterable.init RootSpanList,
            filter = NoFilter,
            failureFilter
          },
      spanBreakdownPage = Nothing,
      clipboardCommand,
      userDidSomething = False
    }

update :: Model -> Msg -> Brick.EventM Name (Brick.Next Model)
update model msg =
  case msg of
    SetCurrentTime currentTime ->
      model {rootSpanPage = (rootSpanPage model) {currentTime}}
        |> Brick.continue
    AddRootSpan (date, span) -> do
      let newModel =
            model
              { rootSpanPage =
                  (rootSpanPage model) {rootSpans = Filterable.cons (RootSpan date span) (rootSpans (rootSpanPage model))}
                    |> updateRootSpans
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
              NoDataPage _ _ -> page
              SpanBreakdownPage _ -> page
              RootSpanPage RootSpanPageData {rootSpans} ->
                case ListWidget.listSelectedElement (Filterable.toListWidget rootSpans) of
                  Nothing -> page
                  Just (currentIndex, currentSpan) ->
                    SpanBreakdownPage
                      ( SpanBreakdownPageData
                          { currentSpan,
                            spans = currentSpan |> logSpan |> toFlatList currentIndex,
                            search = NoSearch,
                            focus = SpanList
                          }
                      )
        )
        |> andThen continueAfterUserInteraction
    Confirm ->
      withPage
        model
        ( \page ->
            case page of
              NoDataPage _ _ -> page
              SpanBreakdownPage spanBreakdownPageData ->
                case search spanBreakdownPageData of
                  EditSearch searchEditor ->
                    updateSpanBreakdownSearch (currentValue searchEditor) spanBreakdownPageData
                      |> SpanBreakdownPage
                  _ ->
                    case focus spanBreakdownPageData of
                      SpanList ->
                        -- user is allowed to toggle focus with tab/backtab, but
                        -- pressing enter seems like an intuitive way of
                        -- drilling down into the details.
                        SpanBreakdownPage (spanBreakdownPageData {focus = SpanDetails})
                      _ ->
                        page
              RootSpanPage rootSpanPageData@RootSpanPageData {filter, rootSpans} ->
                case filter of
                  EditFilter filterEditor ->
                    updateFilter (currentValue filterEditor) rootSpanPageData
                      |> updateRootSpans
                      |> RootSpanPage
                  _ ->
                    case ListWidget.listSelectedElement (Filterable.toListWidget rootSpans) of
                      Nothing -> page
                      Just (currentIndex, currentSpan) ->
                        SpanBreakdownPage
                          SpanBreakdownPageData
                            { currentSpan,
                              spans = currentSpan |> logSpan |> toFlatList currentIndex,
                              search = NoSearch,
                              focus = SpanList
                            }
        )
        |> andThen continueAfterUserInteraction
    Cancel ->
      withPage
        model
        ( \page ->
            case page of
              NoDataPage (EditFilter editor) _ ->
                updateFilter (originalValue editor) (rootSpanPage model)
                  |> updateRootSpans
                  |> RootSpanPage
              NoDataPage _ _ -> page
              SpanBreakdownPage spanBreakdownPage ->
                case search spanBreakdownPage of
                  EditSearch editor ->
                    updateSpanBreakdownSearch (originalValue editor) spanBreakdownPage
                      |> SpanBreakdownPage
                  _ -> RootSpanPage (rootSpanPage model)
              RootSpanPage rootSpanPageData ->
                case filter rootSpanPageData of
                  EditFilter editor ->
                    updateFilter (originalValue editor) rootSpanPageData
                      |> updateRootSpans
                      |> RootSpanPage
                  _ -> page
        )
        |> andThen continueAfterUserInteraction
    CopyDetails -> do
      case toPage model of
        NoDataPage _ _ -> Prelude.pure ()
        RootSpanPage _ -> Prelude.pure ()
        SpanBreakdownPage SpanBreakdownPageData {spans} ->
          case clipboardCommand model of
            Nothing -> Prelude.pure ()
            Just cmd ->
              case ListWidget.listSelectedElement spans of
                Nothing -> Prelude.pure ()
                Just (_, (_, currentSpan)) ->
                  original currentSpan
                    |> spanToClipboard cmd
                    |> liftIO
      continueAfterUserInteraction model
    EnterEdit ->
      withPage
        model
        ( \page ->
            let editor = Edit.editorText FilterField (Just 1) ""
             in case page of
                  SpanBreakdownPage spanBreakdownPageData ->
                    SpanBreakdownPage
                      spanBreakdownPageData {search = enterEditSearch (search spanBreakdownPageData) editor}
                  NoDataPage filter' failureFilter' -> NoDataPage (enterEditFilter filter' editor) failureFilter'
                  RootSpanPage rootSpanPageData ->
                    RootSpanPage rootSpanPageData {filter = enterEditFilter (filter rootSpanPageData) editor}
        )
        |> andThen continueAfterUserInteraction
    ClearEdit ->
      continueAfterUserInteraction
        ( case toPage model of
            SpanBreakdownPage spanBreakdownPageData ->
              model {spanBreakdownPage = Just <| resetSpanBreakdownSearch spanBreakdownPageData}
            NoDataPage (EditFilter _) _ ->
              model {rootSpanPage = updateRootSpans (rootSpanPage model) {filter = NoFilter}}
            RootSpanPage rootSpanPageData ->
              model {rootSpanPage = updateRootSpans rootSpanPageData {filter = NoFilter}}
            _ -> model
        )
    EditorEvent vtyEvent ->
      andThen continueAfterUserInteraction
        <| withPageEvent model
        <| \page -> do
          case page of
            NoDataPage (EditFilter editor) _ -> do
              newEditor <- Edit.handleEditorEvent vtyEvent (currentValue editor)
              editRootSpanFilter (setCurrent newEditor editor) (rootSpanPage model)
                |> RootSpanPage
                |> Prelude.pure
            NoDataPage _ _ -> Prelude.pure page
            RootSpanPage rootSpanPageData ->
              case filter rootSpanPageData of
                EditFilter editor -> do
                  newEditor <- Edit.handleEditorEvent vtyEvent (currentValue editor)
                  editRootSpanFilter (setCurrent newEditor editor) rootSpanPageData
                    |> RootSpanPage
                    |> Prelude.pure
                _ -> Prelude.pure page
            SpanBreakdownPage spanBreakdownPageData ->
              case search spanBreakdownPageData of
                EditSearch editor -> do
                  newEditor <- Edit.handleEditorEvent vtyEvent (currentValue editor)
                  spanBreakdownPageData
                    { search = EditSearch (setCurrent newEditor editor),
                      spans =
                        spans spanBreakdownPageData
                          |> Prelude.fmap (Tuple.second >> annotateSearch (Just newEditor))
                    }
                    |> SpanBreakdownPage
                    |> Prelude.pure
                _ -> Prelude.pure page
    Next ->
      withPageEvent
        model
        ( \page ->
            case page of
              NoDataPage _ _ -> Prelude.pure page
              RootSpanPage _ -> Prelude.pure page
              SpanBreakdownPage spanBreakdownPageData ->
                case search spanBreakdownPageData of
                  HasSearch _ -> do
                    case selectNextMatch spanBreakdownPageData of
                      Nothing -> Prelude.pure page
                      Just (index, spanBreakdownPageData') ->
                        spanBreakdownPageData'
                          |> scrollSpanBreakdownPage (Prelude.pure << ListWidget.listMoveTo index)
                          |> map SpanBreakdownPage
                  _ -> Prelude.pure page
        )
        |> andThen continueAfterUserInteraction
    Previous ->
      withPageEvent
        model
        ( \page ->
            case page of
              NoDataPage _ _ -> Prelude.pure page
              RootSpanPage _ -> Prelude.pure page
              SpanBreakdownPage spanBreakdownPageData ->
                case search spanBreakdownPageData of
                  HasSearch _ -> do
                    case selectPreviousMatch spanBreakdownPageData of
                      Nothing -> Prelude.pure page
                      Just (index, spanBreakdownPageData') ->
                        spanBreakdownPageData'
                          |> scrollSpanBreakdownPage (Prelude.pure << ListWidget.listMoveTo index)
                          |> map SpanBreakdownPage
                  _ -> Prelude.pure page
        )
        |> andThen continueAfterUserInteraction
    ShowHideFailures ->
      withPage
        model
        ( \page ->
            case page of
              RootSpanPage rootSpanPageData ->
                rootSpanPageData {failureFilter = toggleFailureFilter (failureFilter rootSpanPageData)}
                  |> updateRootSpans
                  |> RootSpanPage
              SpanBreakdownPage _ -> page -- TODO allow toggling back
              NoDataPage filter failureFilter -> NoDataPage filter (toggleFailureFilter failureFilter)
        )
        |> andThen continueAfterUserInteraction
    ToggleSpanBreakdownPageFocus ->
      withPage
        model
        ( \page ->
            case page of
              NoDataPage _ _ -> page
              RootSpanPage _ -> page
              SpanBreakdownPage spanBreakdownPageData ->
                spanBreakdownPageData
                  |> toggleFocus
                  |> SpanBreakdownPage
        )
        |> andThen continueAfterUserInteraction
    ScrollSpanDetails direction -> do
      case toPage model of
        NoDataPage _ _ -> Brick.continue model
        RootSpanPage _ -> Brick.continue model
        SpanBreakdownPage SpanBreakdownPageData {focus} ->
          case focus of
            SpanList ->
              Brick.continue model
            SpanDetails -> do
              Brick.vScrollBy
                (Brick.viewportScroll SpanDetail)
                ( case direction of
                    ScrollUp -> -1
                    ScrollDown -> 1
                )
              Brick.continue model
    Quit ->
      case toPage model of
        NoDataPage _ _ -> Brick.halt model
        RootSpanPage _ -> Brick.halt model
        SpanBreakdownPage _ -> update model Cancel

toggleFocus :: SpanBreakdownPageData -> SpanBreakdownPageData
toggleFocus spanBreakdownPageData =
  spanBreakdownPageData
    { focus =
        case (focus spanBreakdownPageData) of
          SpanList -> SpanDetails
          SpanDetails -> SpanList
    }

selectNextMatch :: SpanBreakdownPageData -> Maybe (Prelude.Int, SpanBreakdownPageData)
selectNextMatch spanBreakdownPageData = do
  let currentSelectedIndex =
        spans spanBreakdownPageData
          |> ListWidget.listSelectedElement
          |> Maybe.map Tuple.first
  let nextSpans = ListWidget.listFindBy (Tuple.first >> (/=) NoMatch) (spans spanBreakdownPageData)
  case ListWidget.listSelectedElement nextSpans of
    Just (index, _) ->
      if Just index == currentSelectedIndex
        then do
          let nextSpans' =
                nextSpans
                  |> ListWidget.listMoveTo 0
                  |> ListWidget.listFindBy (Tuple.first >> (/=) NoMatch)
          case ListWidget.listSelectedElement nextSpans' of
            Just (index', _) -> Just (index', spanBreakdownPageData {spans = nextSpans'})
            Nothing -> Nothing
        else Just (index, spanBreakdownPageData {spans = nextSpans})
    Nothing -> Nothing

selectPreviousMatch :: SpanBreakdownPageData -> Maybe (Prelude.Int, SpanBreakdownPageData)
selectPreviousMatch spanBreakdownPageData = do
  let currentSelectedIndex =
        spans spanBreakdownPageData
          |> ListWidget.listSelectedElement
          |> Maybe.map Tuple.first
  let previousSpans =
        spans spanBreakdownPageData
          |> ListWidget.listReverse
          |> ListWidget.listFindBy (Tuple.first >> (/=) NoMatch)
          |> ListWidget.listReverse
  case ListWidget.listSelectedElement previousSpans of
    Just (index, _) ->
      if Just index == currentSelectedIndex
        then do
          let previousSpans' =
                previousSpans
                  |> ListWidget.listMoveTo (Vector.length (ListWidget.listElements previousSpans))
                  |> ListWidget.listReverse
                  |> ListWidget.listFindBy (Tuple.first >> (/=) NoMatch)
                  |> ListWidget.listReverse
          case ListWidget.listSelectedElement previousSpans' of
            Just (index', _) -> Just (index', spanBreakdownPageData {spans = previousSpans'})
            Nothing -> Nothing
        else Just (index, spanBreakdownPageData {spans = previousSpans})
    Nothing -> Nothing

enterEditFilter :: Filter -> Edit.Editor Text Name -> Filter
enterEditFilter filter' editor =
  case filter' of
    NoFilter -> EditFilter (initUndo editor)
    EditFilter editor' -> EditFilter editor'
    HasFilter editor' -> EditFilter (initUndo editor')

enterEditSearch :: Search -> Edit.Editor Text Name -> Search
enterEditSearch search' editor =
  case search' of
    NoSearch -> EditSearch (initUndo editor)
    EditSearch editor' -> EditSearch editor'
    HasSearch editor' -> EditSearch (initUndo editor')

toggleFailureFilter :: FailureFilter -> FailureFilter
toggleFailureFilter f =
  case f of
    ShowAll -> ShowOnlyFailures
    ShowOnlyFailures -> ShowAll

updateRootSpans :: RootSpanPageData -> RootSpanPageData
updateRootSpans pageData =
  let matchesFilter :: Edit.Editor Text Name -> Platform.TracingSpan -> Bool
      matchesFilter editor logSpan =
        List.all (\filter -> Fuzzy.match filter (rawSummary logSpan) "" "" identity False /= Nothing) (getEditWords editor)
      failureFilterFunction =
        case failureFilter pageData of
          ShowAll -> \_ -> True
          ShowOnlyFailures -> failedSpan
      filterFunction =
        case filter pageData of
          NoFilter -> \_ -> True
          EditFilter editor -> matchesFilter (currentValue editor)
          HasFilter editor -> matchesFilter editor
   in pageData
        { rootSpans =
            Filterable.filter
              ( \RootSpan {logSpan} ->
                  failureFilterFunction logSpan
                    && filterFunction logSpan
              )
              (rootSpans pageData)
        }

updateFilter :: Edit.Editor Text Name -> RootSpanPageData -> RootSpanPageData
updateFilter editor rootSpanPageData =
  if hasNoFilters editor
    then rootSpanPageData {filter = NoFilter}
    else rootSpanPageData {filter = HasFilter editor}

updateSpanBreakdownSearch :: Edit.Editor Text Name -> SpanBreakdownPageData -> SpanBreakdownPageData
updateSpanBreakdownSearch editor spanBreakdownPageData =
  if getEditContents editor == ""
    then resetSpanBreakdownSearch spanBreakdownPageData
    else
      spanBreakdownPageData
        { search = HasSearch editor,
          spans =
            spans spanBreakdownPageData
              |> Prelude.fmap (Tuple.second >> annotateSearch (Just editor))
        }

resetSpanBreakdownSearch :: SpanBreakdownPageData -> SpanBreakdownPageData
resetSpanBreakdownSearch spanBreakdownPageData =
  spanBreakdownPageData
    { search = NoSearch,
      spans =
        spans spanBreakdownPageData
          |> Prelude.fmap (Tuple.second >> annotateSearch Nothing)
    }

annotateSearch :: Maybe (Edit.Editor Text Name) -> Span -> (SearchMatch, Span)
annotateSearch maybeEditor span =
  case maybeEditor of
    Nothing -> (NoMatch, span)
    Just editor ->
      if getEditContents editor /= ""
        && Text.contains
          (Data.Text.toCaseFold <| getEditContents editor)
          (Data.Text.toCaseFold <| rawSummary (original span))
        then (Matches (getEditContents editor), span)
        else (NoMatch, span)

editRootSpanFilter :: Undo (Edit.Editor Text Name) -> RootSpanPageData -> RootSpanPageData
editRootSpanFilter editor rootSpanPageData =
  updateRootSpans
    rootSpanPageData
      { filter = EditFilter editor
      }

hasNoFilters :: Edit.Editor Text Name -> Bool
hasNoFilters editor = getEditWords editor == []

getEditWords :: Edit.Editor Text Name -> List Text
getEditWords editor =
  getEditContents editor
    |> Text.split " "
    |> List.filter (not << Text.isEmpty)

getEditContents :: Edit.Editor Text Name -> Text
getEditContents editor =
  Edit.getEditContents editor
    |> Prelude.mconcat
    |> Text.trim

scroll ::
  (forall a. ListWidget.List Name a -> Brick.EventM Name (ListWidget.List Name a)) ->
  Model ->
  Brick.EventM Name Model
scroll move model =
  withPageEvent
    model
    ( \page ->
        case page of
          NoDataPage filter failureFilter -> Prelude.pure (NoDataPage filter failureFilter)
          RootSpanPage rootSpanPageData@RootSpanPageData {rootSpans} ->
            move (Filterable.toListWidget rootSpans)
              |> map (Filterable.setListWidget rootSpans)
              |> map (\rootSpans' -> RootSpanPage rootSpanPageData {rootSpans = rootSpans'})
          SpanBreakdownPage spanBreakdownPageData ->
            case focus spanBreakdownPageData of
              SpanList ->
                scrollSpanBreakdownPage move spanBreakdownPageData
                  |> map SpanBreakdownPage
              SpanDetails ->
                Prelude.return page
    )

scrollSpanBreakdownPage ::
  (forall a. ListWidget.List Name a -> Brick.EventM Name (ListWidget.List Name a)) ->
  SpanBreakdownPageData ->
  Brick.EventM Name SpanBreakdownPageData
scrollSpanBreakdownPage move spanBreakdownPageData =
  move (spans spanBreakdownPageData)
    |> map (\spans' -> spanBreakdownPageData {spans = spans'})

toFlatList :: Prelude.Int -> Platform.TracingSpan -> ListWidget.List Name (SearchMatch, Span)
toFlatList id span =
  ListWidget.list
    (SpanBreakdownList id)
    (toFlatListHelper 0 span)
    1
    |> Prelude.fmap (\s -> (NoMatch, s))

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
          [ viewMaybeInfoOnlyFailures page,
            viewMaybeEditor page,
            viewContents page,
            viewKey page (clipboardCommand model)
          ]
          |> Brick.joinBorders
      ]

viewMaybeInfoOnlyFailures :: Page -> Brick.Widget Name
viewMaybeInfoOnlyFailures page =
  let onlyShowingFailures =
        Brick.vBox
          [ Brick.txt "Only Showing Failures"
              |> Brick.withAttr "only-failures",
            Border.hBorder
          ]
   in case page of
        SpanBreakdownPage _ -> Brick.txt ""
        NoDataPage _ ShowAll -> Brick.txt ""
        NoDataPage _ ShowOnlyFailures -> onlyShowingFailures
        RootSpanPage RootSpanPageData {failureFilter} ->
          case failureFilter of
            ShowAll -> Brick.txt ""
            ShowOnlyFailures -> onlyShowingFailures

viewMaybeEditor :: Page -> Brick.Widget Name
viewMaybeEditor page =
  case page of
    SpanBreakdownPage SpanBreakdownPageData {search, spans} -> viewSearch search spans
    NoDataPage filter _ -> viewFilter filter
    RootSpanPage RootSpanPageData {filter} -> viewFilter filter

viewFilter :: Filter -> Brick.Widget Name
viewFilter filter =
  case filter of
    NoFilter -> Brick.txt ""
    HasFilter editor ->
      Brick.vBox
        [ Brick.hBox
            ( Brick.txt "Filter: " :
              ( getEditWords editor
                  |> List.map (Brick.withAttr "underlined" << Brick.txt)
                  |> List.intersperse (Brick.txt " ")
              )
            ),
          Border.hBorder
        ]
    EditFilter filterEditor ->
      Brick.vBox
        [ Brick.hBox
            [ Brick.txt "Filter: ",
              Edit.renderEditor (editorWithCursor (currentValue filterEditor)) True (currentValue filterEditor)
            ],
          Border.hBorder
        ]

viewSearch :: Search -> ListWidget.List Name (SearchMatch, Span) -> Brick.Widget Name
viewSearch search spans =
  let matchingSpans =
        spans
          |> ListWidget.listElements
          |> Vector.imapMaybe
            ( \index (match, _) ->
                if match /= NoMatch
                  then Just index
                  else Nothing
            )
      selectedSpanIndex =
        Vector.findIndex (\index -> ListWidget.listSelected spans == Just index) matchingSpans
      matchCounter =
        [ selectedSpanIndex
            |> Maybe.map (+ 1)
            |> Maybe.withDefault 0
            |> Prelude.fromIntegral
            |> Text.fromInt,
          "of",
          matchingSpans
            |> Vector.length
            |> Prelude.fromIntegral
            |> Text.fromInt,
          "matches"
        ]
          |> Text.join " "
          |> Brick.txt
          |> Brick.padLeft Brick.Max
   in case search of
        NoSearch -> Brick.txt ""
        HasSearch editor ->
          Brick.vBox
            [ Brick.hBox
                [ Brick.txt "Search: ",
                  getEditContents editor
                    |> Brick.txt
                    |> Brick.withAttr "underlined",
                  matchCounter
                ],
              Border.hBorder
            ]
        EditSearch searchEditor ->
          Brick.vBox
            [ Brick.hBox
                [ Brick.txt "Search: ",
                  Edit.renderEditor (editorWithCursor (currentValue searchEditor)) True (currentValue searchEditor),
                  matchCounter
                ],
              Border.hBorder
            ]

editorWithCursor :: Edit.Editor Text Name -> List Text -> Brick.Widget Name
editorWithCursor editor t =
  let (_, cursorPos) = TZ.cursorPosition (editor ^. Edit.editContentsL)
      (before, after) = Data.Text.splitAt cursorPos (Prelude.mconcat t)
   in Brick.hBox
        <| case Data.Text.uncons after of
          Just (x, rest) ->
            [ Brick.txt before,
              Brick.withAttr "selected" <| Brick.txt <| Data.Text.singleton x,
              Brick.txt rest
            ]
          Nothing ->
            [ Brick.txt before,
              Brick.withAttr "selected" <| Brick.txt " ",
              Brick.txt after
            ]

viewKey :: Page -> Maybe Text -> Brick.Widget Name
viewKey page clipboardCommand =
  let exit = "q: exit"
      goBack = "q: root spans"
      updown = "↑↓: select"
      select = "enter: details"
      unselect = "backspace: back"
      copy = "y: copy details"
      adjustFilter = "/: adjust filter"
      clearFilter = "ctrl-u: clear filter"
      stopEditFilter = "esc: stop filtering"
      applyFilter = "enter: apply filter"
      filter' = "/: filter"
      adjustSearch = "/: adjust search"
      previousMatch = "n: previous match"
      nextMatch = "n: next match"
      clearSearch = "ctrl-u: clear search"
      stopEditSearch = "esc: stop searching"
      applySearch = "enter: apply search"
      search' = "/: search"
      showAll = "f: show all"
      showOnlyFailures = "f: only failures"
      shortcuts =
        case page of
          NoDataPage filter failureFilter ->
            let failureFilterText =
                  case failureFilter of
                    ShowAll -> showOnlyFailures
                    ShowOnlyFailures -> showAll
             in case filter of
                  NoFilter -> [exit, failureFilterText]
                  EditFilter _ -> [exit, stopEditFilter, clearFilter, failureFilterText]
                  _ -> [exit, adjustFilter]
          RootSpanPage RootSpanPageData {filter, failureFilter} ->
            let failureFilterText =
                  case failureFilter of
                    ShowAll -> showOnlyFailures
                    ShowOnlyFailures -> showAll
             in case filter of
                  NoFilter -> [exit, updown, select, filter', failureFilterText]
                  HasFilter _ -> [exit, updown, select, adjustFilter, failureFilterText]
                  EditFilter _ -> [stopEditFilter, applyFilter, clearFilter]
          SpanBreakdownPage SpanBreakdownPageData {search, focus} ->
            let focusChange =
                  "tab: toggle focus"
                scrolling =
                  "↑↓: scroll"
                maybeCopy =
                  ( case clipboardCommand of
                      Nothing -> []
                      Just _ -> [copy]
                  )
             in ( case search of
                    NoSearch ->
                      case focus of
                        SpanList ->
                          [goBack, focusChange, updown, unselect, search'] ++ maybeCopy
                        SpanDetails ->
                          [goBack, focusChange, scrolling, unselect, search'] ++ maybeCopy
                    HasSearch _ ->
                      [goBack, updown, unselect, adjustSearch, nextMatch, previousMatch]
                        ++ ( case clipboardCommand of
                               Nothing -> []
                               Just _ -> [copy]
                           )
                    EditSearch _ -> [stopEditSearch, applySearch, clearSearch]
                )
   in Brick.vBox
        [ Border.hBorder,
          shortcuts
            |> Text.join "   "
            |> Brick.txtWrap
        ]

viewContents :: Page -> Brick.Widget Name
viewContents page =
  case page of
    NoDataPage NoFilter _ ->
      Brick.txt "Waiting for logs...\n\nGo run some tests!"
        |> Center.hCenter
        |> Brick.padBottom Brick.Max
    NoDataPage _ _ ->
      Brick.txt "Waiting for logs or adjust filter...\n\nGo run some tests!"
        |> Center.hCenter
        |> Brick.padBottom Brick.Max
    RootSpanPage RootSpanPageData {currentTime, rootSpans} ->
      rootSpans
        |> Filterable.toListWidget
        |> ListWidget.renderList
          ( \hasFocus RootSpan {logSpan, logTime} ->
              Brick.hBox
                [ Brick.txt (howFarBack logTime currentTime)
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
    SpanBreakdownPage SpanBreakdownPageData {currentSpan, spans, focus} ->
      Brick.vBox
        [ Brick.txt (spanSummary (logSpan currentSpan))
            |> Center.hCenter,
          Border.hBorder,
          sideBySide
            focus
            (viewSpanBreakdown focus spans)
            ( case ListWidget.listSelectedElement spans of
                Nothing -> Brick.emptyWidget
                Just (_, (_, currentSpan')) -> viewSpanDetails currentSpan'
            )
        ]

sideBySide :: SpanBreakdownFocusedPane -> Brick.Widget n -> Brick.Widget n -> Brick.Widget n
sideBySide focus leftPane rightPane =
  Brick.hBox
    [ Border.vBorder
        |> attrIf (focus == SpanList) (Just Border.borderAttr) "focused-pane-border",
      leftPane
        |> attrIf (focus == SpanDetails) Nothing "dim-unfocused"
        |> Brick.hLimitPercent 50,
      Border.vBorder
        |> Brick.overrideAttr Border.borderAttr "focused-pane-border",
      rightPane
        |> attrIf (focus == SpanList) Nothing "dim-unfocused"
        |> Brick.padRight (Brick.Pad 1)
        |> Brick.padRight Brick.Max,
      Border.vBorder
        |> attrIf (focus == SpanDetails) (Just Border.borderAttr) "focused-pane-border"
    ]
  where
    attrIf :: Bool -> Maybe Brick.AttrName -> Brick.AttrName -> Brick.Widget n -> Brick.Widget n
    attrIf cond old new =
      if cond
        then case old of
          Just old_ -> Brick.overrideAttr old_ new
          Nothing -> Brick.withAttr new
        else identity

viewSpanBreakdown :: SpanBreakdownFocusedPane -> ListWidget.List Name (SearchMatch, Span) -> Brick.Widget Name
viewSpanBreakdown focusedPane spans =
  spans
    |> ListWidget.renderList
      ( \isSpanFocused (matches, span) ->
          Brick.hBox
            [ ( case matches of
                  Matches matching ->
                    viewSpanBreakdownWithMatch matching span
                  NoMatch ->
                    Brick.txt (spanSummary (original span))
              )
                |> Brick.padLeft (Brick.Pad (Prelude.fromIntegral (2 * nesting span)))
                |> Brick.padRight Brick.Max
            ]
            |> if isSpanFocused
              then
                if focusedPane == SpanList
                  then Brick.withAttr "selected"
                  else Brick.withAttr "underlined"
              else identity
      )
      (focusedPane == SpanList)
    |> Brick.padLeftRight 1

viewSpanBreakdownWithMatch :: Text -> Span -> Brick.Widget n
viewSpanBreakdownWithMatch matching span =
  let eitherRegex =
        Regex.compileM
          ( TE.encodeUtf8
              ("(.*)(" ++ escapeRegex (failToSymbol matching) ++ ")(.*)")
          )
          [Regex.caseless]
   in case eitherRegex of
        Prelude.Left _ -> Brick.txt (spanSummary (original span))
        Prelude.Right regex ->
          case Regex.match regex (TE.encodeUtf8 (spanSummary (original span))) [] of
            Just [_, before, match, after] ->
              Brick.hBox
                [ Brick.txt (TE.decodeUtf8 before),
                  Brick.txt (TE.decodeUtf8 match)
                    |> Brick.withAttr "matched",
                  Brick.txt (TE.decodeUtf8 after)
                ]
            _ ->
              Brick.txt (spanSummary (original span))

viewSpanDetails :: Span -> Brick.Widget Name
viewSpanDetails Span {original} =
  Brick.viewport
    SpanDetail
    Vertical
    ( Brick.vBox
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
    )

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

failSymbol :: Text
failSymbol = "✖ "

failedSpan :: Platform.TracingSpan -> Bool
failedSpan span =
  case Platform.succeeded span of
    Platform.Succeeded -> False
    Platform.Failed -> True
    Platform.FailedWith _ -> True

spanSummary :: Platform.TracingSpan -> Text
spanSummary span =
  Text.join
    ""
    [ if failedSpan span
        then failSymbol
        else "  ",
      Platform.name span,
      case Platform.summary span of
        Nothing -> ""
        Just summary -> ": " ++ summary
    ]

failToSymbol :: Text -> Text
failToSymbol txt =
  if txt == "fail" || txt == "failed"
    then failSymbol
    else txt

rawSummary :: Platform.TracingSpan -> Text
rawSummary span =
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
    [] -> run ShowAll MostRecent
    ["--failures"] -> run ShowOnlyFailures MostRecent
    ["--clear"] -> do
      System.Directory.removeFile logFile
      run ShowAll MostRecent
    ["--help"] ->
      [ "Usage:",
        "  log-explorer",
        "",
        "  --help         show this help message",
        "  --version      show application version",
        "  --all          load all old log entries",
        "  --failures     only show failures",
        "  --clear        clears the log file",
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
    ["--all"] -> do
      run ShowAll FullHistory
    _ -> System.Exit.die "log-explorer was called with unknown arguments"

logFile :: Prelude.String
logFile = "/tmp/nri-prelude-logs"

run :: FailureFilter -> HistoryLength -> Prelude.IO ()
run failureFilter historyLength = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
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
            ( \handle -> do
                case historyLength of
                  FullHistory -> Prelude.pure ()
                  MostRecent -> do
                    size <- System.IO.hFileSize handle
                    System.IO.hSeek handle System.IO.AbsoluteSeek (max 0 (size - 2_000_000))
                stream <-
                  ByteString.hGetSome handle 32752
                    |> andThen
                      ( \chunk -> do
                          -- We need to sleep for a little while when we don't
                          -- receive any data. log-explorer will eat all
                          -- available cpu otherwise. Because it will just read
                          -- all the time.
                          if chunk == ""
                            then Control.Concurrent.threadDelay 100_000 {- 100 ms -}
                            else Prelude.pure ()
                          Prelude.pure (Just chunk)
                      )
                    |> Streams.makeInputStream
                    |> andThen Streams.ByteString.lines
                    |> andThen (Streams.mapMaybe Aeson.decodeStrict')
                    |> andThen Streams.lockingInputStream
                stream
                  |> Streams.mapM_ (AddRootSpan >> pushMsg)
                  |> andThen Streams.skipToEof
            )
        )
        (updateTime (SetCurrentTime >> pushMsgNonBlocking))
    )
    ( Brick.customMain
        initialVty
        buildVty
        (Just eventChan)
        (app pushMsgNonBlocking)
        (init clipboardCommand now failureFilter)
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
    [ ("selected", Vty.withStyle Vty.defAttr Vty.reverseVideo),
      ("underlined", Vty.withStyle Vty.defAttr Vty.underline),
      ( "matched",
        Vty.withForeColor
          ( Vty.withBackColor
              Vty.defAttr
              Vty.Color.brightYellow
          )
          Vty.Color.black
      ),
      ( "only-failures",
        Vty.withForeColor
          ( Vty.withBackColor
              Vty.defAttr
              Vty.Color.red
          )
          Vty.Color.black
      ),
      ( "focused-pane-border",
        Vty.withForeColor
          Vty.defAttr
          ( Vty.Color.rgbColor
              (255 :: Prelude.Integer)
              (204 :: Prelude.Integer)
              (102 :: Prelude.Integer)
          )
      ),
      ("dim-unfocused", Vty.withStyle Vty.defAttr Vty.Attributes.dim),
      ("focused", Vty.withStyle Vty.defAttr Vty.Attributes.standout)
    ]

handleEvent ::
  (Msg -> Prelude.IO ()) ->
  Model ->
  Brick.BrickEvent Name Msg ->
  Brick.EventM Name (Brick.Next Model)
handleEvent pushMsg modelBeforeScrolling event =
  case event of
    (Brick.VtyEvent vtyEvent) -> do
      model <- scroll (ListWidget.handleListEventVi ListWidget.handleListEvent vtyEvent) modelBeforeScrolling

      case (toMode model, vtyEvent) of
        -- Quitting
        (NormalMode, Vty.EvKey (Vty.KChar 'q') []) -> do
          liftIO (pushMsg Quit)
          Brick.continue model
        (_, Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> Brick.halt model
        -- Navigation
        (_, Vty.EvKey Vty.KEnter []) -> do
          liftIO (pushMsg Confirm)
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'l') []) -> do
          liftIO (pushMsg ShowDetails)
          Brick.continue model
        (NormalMode, Vty.EvKey Vty.KBS []) -> do
          liftIO (pushMsg Cancel)
          Brick.continue model
        (_, Vty.EvKey Vty.KEsc []) -> do
          liftIO (pushMsg Cancel)
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'n') []) -> do
          liftIO (pushMsg Next)
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'N') []) -> do
          liftIO (pushMsg Previous)
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'h') []) -> do
          liftIO (pushMsg Cancel)
          Brick.continue model
        -- Clipboard
        (NormalMode, Vty.EvKey (Vty.KChar 'y') []) -> do
          liftIO (pushMsg CopyDetails)
          Brick.continue model
        -- Filtering
        (NormalMode, Vty.EvKey (Vty.KChar '/') []) -> do
          liftIO (pushMsg EnterEdit)
          Brick.continue model
        (EditMode, Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl]) -> do
          liftIO (pushMsg ClearEdit)
          Brick.continue model
        -- Failurefilter
        (NormalMode, Vty.EvKey (Vty.KChar 'f') []) -> do
          liftIO (pushMsg ShowHideFailures)
          Brick.continue model
        -- Fallback
        (EditMode, _) -> do
          liftIO (pushMsg (EditorEvent vtyEvent))
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KUp) []) -> do
          liftIO (pushMsg (ScrollSpanDetails ScrollUp))
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KDown) []) -> do
          liftIO (pushMsg (ScrollSpanDetails ScrollDown))
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'j') []) -> do
          liftIO (pushMsg (ScrollSpanDetails ScrollDown))
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar 'k') []) -> do
          liftIO (pushMsg (ScrollSpanDetails ScrollUp))
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KChar '\t') []) -> do
          liftIO (pushMsg ToggleSpanBreakdownPageFocus)
          Brick.continue model
        (NormalMode, Vty.EvKey (Vty.KBackTab) []) -> do
          liftIO (pushMsg ToggleSpanBreakdownPageFocus)
          Brick.continue model
        _ ->
          Brick.continue model
    Brick.MouseDown {} -> Brick.continue modelBeforeScrolling
    Brick.MouseUp {} -> Brick.continue modelBeforeScrolling
    Brick.AppEvent msg -> update modelBeforeScrolling msg

data Mode = NormalMode | EditMode

toMode :: Model -> Mode
toMode model =
  case (filter (rootSpanPage model), Maybe.map search (spanBreakdownPage model)) of
    (_, Just (EditSearch _)) -> EditMode
    (EditFilter _, _) -> EditMode
    _ -> NormalMode

updateTime :: (Time.UTCTime -> Prelude.IO ()) -> Prelude.IO ()
updateTime withTime = do
  time <- Time.getCurrentTime
  withTime time
  Control.Concurrent.threadDelay 10_000_000 {- 10 s -}
  updateTime withTime

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

-- | Based on https://hackage.haskell.org/package/regex-1.1.0.0/docs/src/Text.RE.ZeInternals.EscapeREString.html#escapeREString
-- Convert a string into a regular expression that will match that
-- string
escapeRegex :: Text -> Text
escapeRegex = Text.fromList << List.foldr esc [] << Text.toList
  where
    esc char acc =
      if isMetaChar char
        then '\\' : char : acc
        else char : acc

-- | returns True iff the charactr is an RE meta character
-- ('[', '*', '{', etc.)
isMetaChar :: Char -> Bool
isMetaChar char =
  case char of
    '^' -> True
    '\\' -> True
    '.' -> True
    '|' -> True
    '*' -> True
    '?' -> True
    '+' -> True
    '(' -> True
    ')' -> True
    '[' -> True
    ']' -> True
    '{' -> True
    '}' -> True
    '$' -> True
    _ -> False
