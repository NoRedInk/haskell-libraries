{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Platform.Internal where

import Basics
import Control.Applicative ((<|>))
import qualified Control.AutoUpdate as AutoUpdate
import qualified Control.Exception.Safe as Exception
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.IORef as IORef
import qualified Data.Text
import qualified Data.Typeable as Typeable
import qualified GHC.Clock as Clock
import GHC.Generics (Generic)
import qualified GHC.Stack as Stack
import qualified GHC.Word
import Internal.Shortcut (andThen, map)
import qualified Internal.Shortcut as Shortcut
import qualified List
import Maybe (Maybe (..))
import Result (Result (Err, Ok))
import qualified System.Mem
import Text (Text)
import qualified Tuple
import Prelude
  ( Applicative (pure, (<*>)),
    Functor,
    IO,
    Monad ((>>=)),
  )
import qualified Prelude

--
-- TASK
--

-- | Here are some common tasks:

-- - @now : Task x Posix@
-- - @query : String -> Task Error ()@
-- - @sleep : Float -> Task x ()@
--
-- In each case we have a Task that will resolve successfully with an a value
-- or unsuccessfully with an x value. So Postgres.query may fail with an Error
-- if the query is invalid. Whereas Time.now never fails so I cannot be more
-- specific than x. No such value will ever exist! Instead it always succeeds
-- with the current POSIX time.
--
-- More generally a task is a /description/ of what you need to do. Like a todo
-- list. Or like a grocery list. Or like GitHub issues. So saying "the task is
-- to tell me the current POSIX time" does not complete the task! You need
-- 'perform' tasks or 'attempt' tasks.
newtype Task x a = Task {_run :: LogHandler -> IO (Result x a)}
  deriving (Functor)

instance Applicative (Task a) where
  pure a =
    Task (\_ -> Prelude.pure (Ok a))

  (<*>) func task =
    Task <| \key ->
      let onResult resultFunc resultTask =
            case (resultFunc, resultTask) of
              (Ok func_, Ok task_) ->
                Ok (func_ task_)
              (Err x, _) ->
                Err x
              (_, Err x) ->
                Err x
       in do
            func_ <- _run func key
            task_ <- _run task key
            pure (onResult func_ task_)

instance Monad (Task a) where
  task >>= func =
    Task <| \key ->
      let onResult result =
            case result of
              Ok ok ->
                _run (func ok) key
              Err err ->
                pure (Err err)
       in _run task key |> andThen onResult

--
-- SPAN
--

-- | A @TracingSpan@ contains debugging information related to a section of the
-- program. TracingSpans can be nested inside other tracingSpans to form a
-- tree, each tracingSpan representing part of the execution of the program.
-- This format is a typical way to store tracing data. Check out this section
-- of the documentation on the open tracing standard for a good introduction on
-- tracing data and tracingSpans:
--
-- https://github.com/opentracing/specification/blob/master/specification.md#the-opentracing-data-model
--
-- From tracingSpans we can derive many other formats of debugging information:
--
-- - Logs are tracingSpans flattened into a series of events ordered by time. For
--   example, consider the following tracingSpans:
--
--       do the laundry   11:00-12:15
--           wash clothes   11:00-12:00
--           hang clothes to dry   12:00-12:15
--
--   we could recover the following logs from this:
--
--       11:00 starting do the laundry
--       11:00 wash clothes
--       12:00 hang clothes to dry
--       12:15 finishing do the laundry
--
-- - Metrics are rolling statistics on tracingSpans. For example, we can
--   increment a counter every time we see a particular tracingSpan pass by.
--
-- So whether we're looking for tracing data, logs, or metrics, tracingSpans
-- got us covered.
data TracingSpan = TracingSpan
  { -- | A description of this tracingSpan. This should not contain any
    -- dynamically generated strings to make grouping tracingSpans easy.
    -- Any contextual info should go into 'details'.
    name :: Text,
    -- | The time this tracingSpan started.
    started :: MonotonicTime,
    -- | The time this tracingSpan finished.
    finished :: MonotonicTime,
    -- | The source code location of this tracingSpan. The first @Text@ is
    -- the name of the function getting called.
    frame :: Maybe (Text, Stack.SrcLoc),
    -- | Unique information for this tracingSpan.
    details :: Maybe SomeTracingSpanDetails,
    -- | A short blurb describing the details of this span, for use in
    -- tooling for inspecting these spans.
    summary :: Maybe Text,
    -- | Whether this tracingSpan succeeded. If any of the children of this
    -- tracingSpan failed, so will this tracingSpan. This will create a
    -- path to the tracingSpan closest to the failure from the root
    -- tracingSpan.
    succeeded :: Succeeded,
    -- | The amount of bytes were allocated on the current thread while this
    -- span was running. This is a proxy for the amount of work done. If
    -- this number is low but the span took a long time to complete this
    -- indicates the thread was blocked for some time, or that work was done
    -- on other threads.
    allocated :: Int,
    -- | Any subtracingSpans nested inside this tracingSpan. These are
    -- ordered in reverse chronological order, so most recent tracingSpan
    -- first, because it's cheaper to append new tracingSpans onto the left
    -- of the list.
    children :: [TracingSpan]
  }
  deriving (Prelude.Show, Generic)

instance Aeson.ToJSON TracingSpan where
  toJSON span =
    Aeson.object
      [ "name" .= name span,
        "started" .= started span,
        "finished" .= finished span,
        "frame" .= map SrcLocForEncoding (frame span),
        "details" .= details span,
        "summary" .= summary span,
        "succeeded" .= succeeded span,
        "allocated" .= allocated span,
        "children" .= children span
      ]
  toEncoding span =
    Aeson.pairs
      ( "name" .= name span
          ++ "started" .= started span
          ++ "finished" .= finished span
          ++ "frame" .= map SrcLocForEncoding (frame span)
          ++ "details" .= details span
          ++ "summary" .= summary span
          ++ "succeeded" .= succeeded span
          ++ "allocated" .= allocated span
          ++ "children" .= children span
      )

instance Aeson.FromJSON TracingSpan where
  parseJSON =
    Aeson.withObject
      "TracingSpan"
      ( \object -> do
          name <- object .: "name"
          started <- object .: "started"
          finished <- object .: "finished"
          frame <- map (map unSrcLocForEncoding) (object .:? "frame")
          details <- object .:? "details"
          summary <- object .:? "summary"
          succeeded <- object .: "succeeded"
          allocated <- object .: "allocated"
          children <- object .: "children"
          Prelude.pure
            TracingSpan
              { name,
                started,
                finished,
                frame,
                details,
                summary,
                succeeded,
                allocated,
                children
              }
      )

newtype SrcLocForEncoding = SrcLocForEncoding {unSrcLocForEncoding :: (Text, Stack.SrcLoc)}

instance Aeson.ToJSON SrcLocForEncoding where
  toJSON (SrcLocForEncoding (name, loc)) =
    Aeson.object
      [ "name" .= name,
        "package" .= Stack.srcLocPackage loc,
        "module" .= Stack.srcLocModule loc,
        "file" .= Stack.srcLocFile loc,
        "startLine" .= Stack.srcLocStartLine loc,
        "startCol" .= Stack.srcLocStartCol loc,
        "endLine" .= Stack.srcLocEndLine loc,
        "endCol" .= Stack.srcLocEndCol loc
      ]
  toEncoding (SrcLocForEncoding (name, loc)) =
    Aeson.pairs
      ( "name" .= name
          ++ "package" .= Stack.srcLocPackage loc
          ++ "module" .= Stack.srcLocModule loc
          ++ "file" .= Stack.srcLocFile loc
          ++ "startLine" .= Stack.srcLocStartLine loc
          ++ "startCol" .= Stack.srcLocStartCol loc
          ++ "endLine" .= Stack.srcLocEndLine loc
          ++ "endCol" .= Stack.srcLocEndCol loc
      )

instance Aeson.FromJSON SrcLocForEncoding where
  parseJSON =
    Aeson.withObject
      "SrcLocForEncoding"
      ( \object -> do
          name <- object .: "name"
          srcLocPackage <- object .: "package"
          srcLocModule <- object .: "module"
          srcLocFile <- object .: "file"
          srcLocStartLine <- object .: "startLine"
          srcLocStartCol <- object .: "startCol"
          srcLocEndLine <- object .: "endLine"
          srcLocEndCol <- object .: "endCol"
          Prelude.pure
            ( SrcLocForEncoding
                ( name,
                  Stack.SrcLoc
                    { Stack.srcLocPackage,
                      Stack.srcLocModule,
                      Stack.srcLocFile,
                      Stack.srcLocStartLine,
                      Stack.srcLocStartCol,
                      Stack.srcLocEndLine,
                      Stack.srcLocEndCol
                    }
                )
            )
      )

-- | A tracing span containing default empty values for all fields. Usually we
-- don't need this because TracingSpans get created for us when we evaluate
-- tasks. This can be useful when testing reporting code to see if it produces
-- the right outputs given a specific tracing span as input.
emptyTracingSpan :: TracingSpan
emptyTracingSpan =
  TracingSpan
    { name = "",
      started = 0,
      finished = 0,
      frame = Nothing,
      details = Nothing,
      summary = Nothing,
      succeeded = Succeeded,
      allocated = 0,
      children = []
    }

-- | The @Succeeded@ type is used to indicate whether or not a particular
-- @TracingSpan@ ran without encountering user-facing problems.
data Succeeded
  = -- | A tracingSpan that didn't fail with an unexpected exception, or was
    -- explicitly marked as failed by the user.
    --
    -- When a tracingSpan returns a failed task we do not count that as @Failed@
    -- here, because a failed task might be part of normal program
    -- operation. We wouldn't want to log those kinds of failures as errors.
    Succeeded
  | -- | A tracingSpan marked as failed by a user, for example by logging with a
    -- high severity to indicate a user is in pain.
    Failed
  | -- | A tracingSpan that failed with an unhandled exception thrown by the
    -- Haskell runtime or a library.
    FailedWith Exception.SomeException
  deriving (Prelude.Show)

instance Aeson.ToJSON Succeeded where
  toJSON Succeeded = Aeson.String "Succeeded"
  toJSON Failed = Aeson.String "Failed"
  toJSON (FailedWith exception) =
    Exception.displayException exception
      |> Data.Text.pack
      |> Aeson.String
  toEncoding Succeeded = Aeson.Encoding.text "Succeeded"
  toEncoding Failed = Aeson.Encoding.text "Failed"
  toEncoding (FailedWith exception) =
    Exception.displayException exception
      |> Aeson.Encoding.string

instance Aeson.FromJSON Succeeded where
  parseJSON =
    Aeson.withText
      "Succeeded"
      ( \text ->
          case text of
            "Succeeded" -> Prelude.pure Succeeded
            "Failed" -> Prelude.pure Failed
            _ ->
              ParsedException text
                |> Exception.toException
                |> FailedWith
                |> Prelude.pure
      )

-- Helper type for when we're decoding a TracingSpan. SomeException doesn't have
-- aeson instances for encoding or decoding. For encoding a SomeException we can
-- make something up, but we can never decode it back into the original
-- exception type. Hence this ParsedException for decoding into instead.
newtype ParsedException = ParsedException Text
  deriving (Aeson.ToJSON)

instance Prelude.Show ParsedException where
  show (ParsedException text) = Data.Text.unpack text

instance Exception.Exception ParsedException

-- | If the first bit of code succeeded and the second failed, the combination
-- of the two has failed as well. The @SemiGroup@ and @Monoid@ type instances
-- for @Succeeded@ allow us to combine @Succeeded@ values in such a fashion.
--
-- The rule expressed here is that the Succeeded value of a combination of
-- computations if the same as the worst thing that happened to any of the
-- individual computations.
instance Prelude.Semigroup Succeeded where
  FailedWith err <> _ = FailedWith err
  _ <> FailedWith err = FailedWith err
  Failed <> _ = Failed
  _ <> Failed = Failed
  _ <> _ = Succeeded

instance Prelude.Monoid Succeeded where
  mempty = Succeeded

--
-- SPAN DETAILS
--

-- | A wrapper around the various types that specify details for different kinds
-- of tracingSpans.
--
-- Depending on what happens within a tracingSpan we want to log different
-- information for debugging. A tracingSpan for a database query might include
-- the SQL of the query, and a tracingSpan for an HTTP request the URL the
-- request is addressed to.
--
-- We could define a single @SomeTracingSpanDetails@ type that can represent all
-- of these different types of details. One way would be to write a union:
--
--     data SomeTracingSpanDetails
--       = Sql SqlDetails
--       | Http HttpDetails
--       | ...
--
-- The disadvantage of this is that nri-prelude will have to know about every
-- possible type of tracingSpan. If a library wanted to log new information it
-- would have to change @nri-prelude@ first to support this. That's a barrier to
-- adding useful logging information we'd prefer not to have.
--
-- Another approach is to have the details field take arbitrary JSON:
--
--     type SomeTracingSpanDetails = Data.Aeson.Value
--
-- This allows any library to log what it wants without requiring any changes in
-- nri-prelude. However, unless we parse that JSON back into the original types
-- (which is wasteful and can fail) we have lost the ability to render specific
-- bits of information in special ways. If we provide Bugsnag with the stack
-- trace of an error it will present it nicely in its UI. NewRelic can treat SQL
-- strings of queries in a special way. But we don't have stack traces or SQL
-- strings to give, just opaque JSON blobs.
--
-- We'd like to both let libraries define custom detail types _and_ be able to
-- read specific fields from those types in loggers that present certain bits of
-- information in nice ways. To do that we allow a bit of type magic here.
-- Analogous to Haskell's @SomeException@ type and @Exception@ type class, we
-- define a @SomeTracingSpanDetails@ type and @TracingSpanDetails@ type class.
--
-- The SomeTracingSpanDetails type can wrap any custom type, as long as it has
-- @TracingSpanDetails@ instance. The @TracingSpanDetails@ instance allows us
-- to recover the original details type if we want to treat it special in a
-- custom logger.
data SomeTracingSpanDetails where
  SomeTracingSpanDetails :: (TracingSpanDetails a) => a -> SomeTracingSpanDetails

instance Aeson.ToJSON SomeTracingSpanDetails where
  toJSON (SomeTracingSpanDetails details) = Aeson.toJSON details

  toEncoding (SomeTracingSpanDetails details) = Aeson.toEncoding details

instance Aeson.FromJSON SomeTracingSpanDetails where
  parseJSON x =
    Aeson.parseJSON x
      |> Prelude.fmap
        (toTracingSpanDetails << ParsedTracingSpandetails)

instance TracingSpanDetails SomeTracingSpanDetails where
  toTracingSpanDetails details = details

  fromTracingSpanDetails = Just

instance Prelude.Show SomeTracingSpanDetails where
  show (SomeTracingSpanDetails details) =
    Aeson.encode details
      |> Prelude.show

-- | A container for tracing span details if we parsed them back from JSON.
-- We don't require users of this library to define FromJSON instances of their
-- own tracing span details because it's not necessary for logging, but to
-- support tooling reading data structures produced by this lib we'd still like
-- to be able to parse tracing spans from JSON. This helper type allows us to do
-- so.
newtype ParsedTracingSpandetails = ParsedTracingSpandetails Aeson.Value
  deriving (Aeson.ToJSON)

instance TracingSpanDetails ParsedTracingSpandetails

-- | Every type we want to use as tracingSpan metadata needs a
-- @TracingSpanDetails@ instance.  The @TracingSpanDetails@ class fulfills
-- these roles:
--
-- - It allows for conversion between the custom details type and the
--   @SomeTracingSpanDetails@ type stored in a @TracingSpan@.
-- - It requires the custom details type to also have a @ToJSON@ instance.
--
-- This gives a logger two options for rendering a @SomeTracingSpanDetails@
-- value into a format understood by a monitoring tool:
--
-- - It can try @fromTracingSpanDetails@ to try to recover one of the custom
--   tracingSpan details types it has implemented custom rendering logic for.
-- - If this particular tracingSpan details type is unknown to this particular
--   logger, it can obtain always obtain a generic JSON blob of the information
--   instead.
class (Typeable.Typeable e, Aeson.ToJSON e) => TracingSpanDetails e where
  toTracingSpanDetails :: e -> SomeTracingSpanDetails
  toTracingSpanDetails = SomeTracingSpanDetails

  fromTracingSpanDetails :: SomeTracingSpanDetails -> Maybe e
  fromTracingSpanDetails (SomeTracingSpanDetails d) = Typeable.cast d

-- | A helper type used for @renderTracingSpanDetails@. Used to wrap rendering
-- functions so they have the same type and can be put in a list together.
data Renderer a where
  Renderer :: TracingSpanDetails s => (s -> a) -> Renderer a

-- | In reporting logic we'd like to case on the different types a
-- 'SomeTracingSpanDetails' can contain and write logic for each one. This
-- helper allows us to do so.
--
-- > newtype ImportantFact = ImportantFact Text
-- > instance ToJSON ImportantFact
-- > instance SpanDetails ImportantFact
-- >
-- > newtype KeyStatistic = KeyStatistic Int
-- > instance ToJSON KeyStatistic
-- > instance SpanDetails KeyStatistic
-- >
-- > toTracingSpanDetails (ImportantFact "Koala's are adorable")
-- >   |> renderTracingSpanDetails
-- >        [ Renderer (\ImportantFact fact -> fact)
-- >        , Renderer (\KeyStatistic stat -> Text.fromInt stat)
-- >        ]
-- >   |> Maybe.withDefault (\details -> show (Data.Aeson.encode details))
--
-- Remember that @SomeTracingSpanDetails@ are always JSON-serializable, so you
-- can use that if you need to render a span of a type you didn't prepare for.
renderTracingSpanDetails :: [Renderer a] -> SomeTracingSpanDetails -> Maybe a
renderTracingSpanDetails rs s =
  case rs of
    [] -> Nothing
    (Renderer r) : rest -> Shortcut.map r (fromTracingSpanDetails s) <|> renderTracingSpanDetails rest s

--
-- HANDLER
--

-- | Our @Task@ type secretly passed a value of this type throughout our
-- application. Anywhere in our application we can add context to the log
-- handler. For example we might wrap our database queries in a tracingSpan
-- called "query" and add some bits of context, such as the SQL operation the
-- query is performing. These bits of metadata will then be used as much as
-- possible in logging messages, tracing, and error reporting.
--
-- Note that we do not report recorded information anywhere (log it to file, or
-- to an observability platform), until we completely finish a request. This
-- gives us the option _not_ to report on a particular request. We might use
-- this to report only on a subset of the succeeding requests, to save us money
-- without loosing important signal. We'll only know whether a request succeeds
-- after it completes though, so we have to hold off on any reporting for a
-- request until it's done.
data LogHandler = LogHandler
  { -- | We're making the assumption that every task we run is ran because
    -- of some sort of request, and that this request has a unique
    -- identifier.  We take this identifier from the incoming request and
    -- pass it on when we call external services. If something goes wrong
    -- we'll be able to collect all information related to a single request
    -- from all the components in our architecture that did work for it.
    requestId :: Text,
    -- | Every tracingSpan gets its own handler. That way if we record
    -- debugging information using a handler we'll know which tracingSpan
    -- the information belongs to. This function creates a new handler for
    -- a child tracingSpan of the current handler.
    startChildTracingSpan :: Stack.HasCallStack => Text -> IO LogHandler,
    -- | There's common fields all tracingSpans have such as a name and
    -- start and finish times. On top of that each tracingSpan can define a
    -- custom type containing useful custom data. This function allows us
    -- to set this custom data for the current tracingSpan. We could design
    -- it so this data is passed in as an extra argument when we create the
    -- tracingSpan, but then we'd miss out on useful details that only
    -- become known as the tracingSpan runs, for example the response code
    -- of an HTTP request.
    setTracingSpanDetailsIO :: forall d. TracingSpanDetails d => d -> IO (),
    -- | Set a summary for the current tracingSpan. This is shown in tools
    -- used to inspect spans as a stand-in for the full tracingSpan details
    -- in places where we only have room to show a little text.
    setTracingSpanSummaryIO :: Text -> IO (),
    -- | Mark the current tracingSpan as failed. Some reporting backends
    -- will use this to decide whether a particular request is worth
    -- reporting on.
    markTracingSpanFailedIO :: IO (),
    -- | Mark the current tracingSpan as finished, which will set the
    -- @finished@ timestamp. What this function does depends on the
    -- tracingSpan. Once we're done collecting data for the root
    -- tracingSpan we'll want to pass the tracingSpan "out", to some code
    -- that will report the debugging data to whatever observability
    -- platform(s) are used. Once we're done collecting data for child
    -- tracingSpans we'll want to add the "completed" child tracingSpan to
    -- its parent.
    finishTracingSpan :: Maybe Exception.SomeException -> IO ()
  }

-- | Helper that creates one of the handler's above. This is intended for
-- internal use in this library only and not for exposing. Outside of this
-- library the @rootTracingSpanIO@ is the more user-friendly way to get hands
-- on a @LogHandler@.
mkHandler ::
  Stack.HasCallStack =>
  Text ->
  Clock ->
  (TracingSpan -> IO ()) ->
  Text ->
  IO LogHandler
mkHandler requestId clock onFinish name' = do
  tracingSpanRef <-
    Stack.withFrozenCallStack startTracingSpan clock name'
      |> andThen IORef.newIORef
  allocationCounterStartVal <- System.Mem.getAllocationCounter
  pure
    LogHandler
      { requestId,
        startChildTracingSpan = mkHandler requestId clock (appendTracingSpanToParent tracingSpanRef),
        setTracingSpanDetailsIO = \details' ->
          updateIORef
            tracingSpanRef
            (\tracingSpan' -> tracingSpan' {details = Just (toTracingSpanDetails details')}),
        setTracingSpanSummaryIO = \text ->
          updateIORef
            tracingSpanRef
            (\tracingSpan' -> tracingSpan' {summary = Just text}),
        markTracingSpanFailedIO =
          updateIORef
            tracingSpanRef
            (\tracingSpan' -> tracingSpan' {succeeded = succeeded tracingSpan' ++ Failed}),
        finishTracingSpan = finalizeTracingSpan clock allocationCounterStartVal tracingSpanRef >> andThen onFinish
      }

-- | Set the details for a tracingSpan created using the @tracingSpan@
-- function. Like @tracingSpan@ this is intended for use in writing libraries
-- that define custom types of effects, such as database queries or http
-- requests.
--
-- It's often a good idea to use this together with @Platform.finally@ or
-- @Platform.bracketWithError@, to ensure we record tracingSpan details even in
-- the event of an exception cutting the execution of our tracingSpan short.
--
--     tracingSpan "holiday" do
--       let bookPick = BookPick "The Stone Sky"
--       Platform.finally
--         (readBook bookPick)
--         (setTracingSpanDetails bookPick)
--
--     newtype BookPick = BookPick Text
--       deriving (Aeson.ToJSON)
--
--     instance TracingSpanDetails BookPick
setTracingSpanDetails :: TracingSpanDetails d => d -> Task e ()
setTracingSpanDetails details =
  Task
    ( \handler ->
        setTracingSpanDetailsIO handler details
          |> map Ok
    )

-- | Set a summary for the tracingSpan created with the @tracingSpan@ function.
-- Like @tracingSpan@ this is intended for use in writing libraries that define
-- custom types of effects, such as database queries or http requests.
--
-- The summary is shown in tools used to inspect spans as a stand-in for the
-- full tracingSpan details in places where we only have room to show a little
-- text.
setTracingSpanSummary :: Text -> Task e ()
setTracingSpanSummary text =
  Task
    ( \handler ->
        setTracingSpanSummaryIO handler text
          |> map Ok
    )

-- | Mark a tracingSpan created with the @tracingSpan@ function as failed. Like
-- @tracingSpan@ this is intended for use in writing libraries that define
-- custom types of effects, such as database queries or http requests.
--
--     tracingSpan "holiday" do
--       Platform.finally
--         (readBook bookPick)
--         (setTracingSpanSummary "The Stone Sky")
markTracingSpanFailed :: Task e ()
markTracingSpanFailed =
  Task (map Ok << markTracingSpanFailedIO)

-- | Create an initial @TracingSpan@ with some initial values.
startTracingSpan :: Stack.HasCallStack => Clock -> Text -> IO TracingSpan
startTracingSpan clock name = do
  started <- monotonicTimeInMsec clock
  pure
    TracingSpan
      { name,
        started,
        finished = started,
        frame =
          -- This records a single stack frame containing the location in source
          -- code that creates this tracingSpan. It wouldn't be that useful if
          -- this single stack frame referenced the line in this source file
          -- where the @startTracingSpan@ function itself gets called, that would
          -- be the same line for every tracingSpan! Instead we'd like the source
          -- location recorded here to be the line outside this library calling
          -- into it. For example: the line in the application doing a database
          -- query, or logging some information.
          --
          -- That's why you see the @Stack.HasCallStack@ constraints and
          -- @Stack.withFrozenCallStack@ calls on this function's callers all the
          -- way to the boundary of the library. Unfortunately, that's what we
          -- need to do to push the stack frame we record out of the library.
          --
          -- We record only a single stack frame because that's all we get
          -- anyway, unless we'd start adding @Stack.HasCallStack@ constraints to
          -- functions in our Haskell applications. But because we record a frame
          -- for each tracingSpan together these frames can create a stack trace
          -- with a couple of different frames.
          --
          -- See the docs of the @GHC.Stack@ module for more information on how
          -- these traces work.
          Stack.callStack
            |> Stack.getCallStack
            |> List.head
            |> Shortcut.map (Tuple.mapFirst Data.Text.pack),
        details = Nothing,
        summary = Nothing,
        succeeded = Succeeded,
        allocated = 0,
        children = []
      }

-- | Some final properties to set on a tracingSpan before calling it done.
finalizeTracingSpan :: Clock -> Int -> IORef.IORef TracingSpan -> Maybe Exception.SomeException -> IO TracingSpan
finalizeTracingSpan clock allocationCounterStartVal tracingSpanRef maybeException = do
  finished <- monotonicTimeInMsec clock
  allocationCounterEndVal <- System.Mem.getAllocationCounter
  tracingSpan' <- IORef.readIORef tracingSpanRef
  pure
    tracingSpan'
      { finished,
        -- Below we implement the rule that if any of the children of a
        -- tracingSpan failed, that tracingSpan itself failed too. The reason
        -- we have this rule is to make it easy to see if a request as a whole
        -- failed (just check the 'succeeded' property of the root tracingSpan
        -- to see if any errors occurred), and to make it easy to trace the
        -- source of a problem from the root tracingSpan upward by following
        -- the failing child tracingSpans.
        succeeded =
          succeeded tracingSpan'
            ++ case maybeException of
              Just exception -> FailedWith exception
              Nothing ->
                map Platform.Internal.succeeded (children tracingSpan')
                  |> Prelude.mconcat,
        -- The allocation counter counts down as it allocations bytest. We
        -- subtract in this order to get a positive number.
        allocated = allocationCounterStartVal - allocationCounterEndVal
      }

appendTracingSpanToParent :: IORef.IORef TracingSpan -> TracingSpan -> IO ()
appendTracingSpanToParent parentRef child =
  updateIORef parentRef <| \parentTracingSpan ->
    -- Note child tracingSpans are consed to the front of the list, so children
    -- are ordered new-to-old.
    parentTracingSpan {children = child : children parentTracingSpan}

updateIORef :: IORef.IORef a -> (a -> a) -> IO ()
updateIORef ref f = IORef.atomicModifyIORef' ref (\x -> (f x, ()))

--
-- SPAN CONSTRUCTION
--

-- | Run a task in a tracingSpan.
--
--     tracingSpan "code dance" <| do
--       waltzPassLeft
--       clockwiseTurn 60
--
-- This will help provide better debugging information if something goes wrong
-- inside the wrapped task.
tracingSpan :: Stack.HasCallStack => Text -> Task e a -> Task e a
tracingSpan name (Task run) =
  Task
    ( \handler ->
        Stack.withFrozenCallStack
          tracingSpanIO
          handler
          name
          run
    )

-- | Like @tracingSpan@, but this one runs in @IO@ instead of @Task@. We
-- sometimes need this in libraries. @Task@ has the concept of a @LogHandler@
-- built in but @IO@ does not, so we'll have to pass it around ourselves.
--
--     tracingSpanIO handler "code dance" <| \childHandler -> do
--       waltzPassLeft childHandler
--       clockwiseTurn childHandler 60
tracingSpanIO :: Stack.HasCallStack => LogHandler -> Text -> (LogHandler -> IO a) -> IO a
tracingSpanIO handler name run =
  Exception.bracketWithError
    (Stack.withFrozenCallStack startChildTracingSpan handler name)
    (Prelude.flip finishTracingSpan)
    run

-- | Special version of @tracingSpanIO@ to call in the root of your application.
-- Instead of taking a parent handler it takes a continuation that will be
-- called with this root tracingSpan after it has run.
--
--     rootTracingSpanIO "request-23" Prelude.print "incoming request" <| \handler ->
--       handleRequest
--       |> Task.perform handler
rootTracingSpanIO :: Stack.HasCallStack => Text -> (TracingSpan -> IO ()) -> Text -> (LogHandler -> IO a) -> IO a
rootTracingSpanIO requestId onFinish name runIO = do
  clock' <- mkClock
  Exception.bracketWithError
    (Stack.withFrozenCallStack mkHandler requestId clock' onFinish name)
    (Prelude.flip finishTracingSpan)
    runIO

--
-- CLOCK
--

-- | A clock we can use to get the current time, to check when tracingSpans are
-- starting or ending. We could call @getCurrentTime@ or somesuch whenever we
-- need the time but we'd be calling this a lot: every time a tracingSpan
-- starts or finishes. The @Clock@ type we pass around here contains cached
-- version of @getCurrentTime@. We can call it as often as we like and it will
-- only get the current time at most once every millisecond.
newtype Clock = Clock {monotonicTimeInMsec :: IO MonotonicTime}

mkClock :: IO Clock
mkClock =
  AutoUpdate.mkAutoUpdate
    AutoUpdate.defaultUpdateSettings
      { AutoUpdate.updateAction =
          Clock.getMonotonicTimeNSec
            |> map (\n -> MonotonicTime (n `Prelude.div` 1000)),
        AutoUpdate.updateFreq = 100 -- Once every 100 microseconds
      }
    |> map Clock

-- |
-- You might expect a timestamp here, but timestamps are unreliable for
-- measuring how long a bit of code runs. For example: events like leap seconds
-- can cause them to move backards. This might result in us measuring the
-- duration of an operation and finding it to be minus 200 milliseconds.
--
-- We use @GHC.Clock.getMonotonicTimeNSec@ to let the OS tell us how much time
-- has passed since an arbitrary but constant moment in the past. That might
-- not seem all that useful, but if we 'sync watches' at one moment by getting
-- the monotonic and "regular" time in the same moment then we'll able to
-- convert any monotonic time to real timestamps. Conversion is not our concern
-- here though, we just store these monotonic times and let code that reporters
-- that use these tracingSpans convert the monotonic times into whatever format
-- they need.
newtype MonotonicTime = MonotonicTime
  { -- | The number of microseconds that have passed since an arbitrary but
    -- constant moment in the past.
    inMicroseconds :: GHC.Word.Word64
  }
  deriving (Prelude.Show, Prelude.Num, Prelude.Eq, Prelude.Ord, Aeson.ToJSON, Aeson.FromJSON)
