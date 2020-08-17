module Observability.Dev (report, logSpanRecursively, Handler, handler, Settings, decoder) where

import Cherry.Prelude
import qualified Conduit
import qualified Control.Exception.Safe as Exception
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Internal as Doc.Internal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal
import qualified Data.Time.Format as Format
import qualified Environment
import qualified GHC.Stack as Stack
import qualified List
import qualified Maybe
import qualified Observability.Timer as Timer
import qualified Platform
import qualified System.IO
import qualified Prelude

report :: Handler -> Platform.Span -> Prelude.IO ()
report handler' span =
  Terminal.hPutDoc
    (handle handler')
    (logSpanRecursively (timer handler') span)

type Doc = Doc.Doc Terminal.AnsiStyle

logSpanRecursively :: Timer.Timer -> Platform.Span -> Doc
logSpanRecursively timer' span =
  let (beforeChildren, afterChildren) = logSingleSpan timer' span
   in vcat
        [ Doc.hang 2 beforeChildren,
          Platform.children span
            |> Prelude.map (logSpanRecursively timer')
            |> vcat,
          case afterChildren of
            Nothing -> Doc.emptyDoc
            Just after -> Doc.hang 2 after
        ]

logSingleSpan :: Timer.Timer -> Platform.Span -> (Doc, Maybe Doc)
logSingleSpan timer' span =
  Platform.details span
    |> andThen (Platform.renderSpanDetails [])
    |> Maybe.withDefault (logNoDetails timer' span)

logNoDetails :: Timer.Timer -> Platform.Span -> (Doc, Maybe Doc)
logNoDetails timer' span =
  if List.isEmpty (Platform.children span)
    then
      ( vsep
          [ hsep
              [ time timer' (Platform.started span),
                name span,
                failed span
              ],
            trace span,
            exception span,
            duration span
          ],
        Nothing
      )
    else
      ( vsep
          [ hsep
              [ time timer' (Platform.started span),
                label "Started:",
                name span
              ],
            trace span
          ],
        vsep
          [ hsep
              [ time timer' (Platform.finished span),
                label "Finished:",
                name span,
                failed span
              ],
            exception span,
            duration span
          ]
          |> Just
      )

label :: Doc -> Doc
label text =
  Doc.annotate (Terminal.colorDull Terminal.White) text

exception :: Platform.Span -> Doc
exception span =
  case Platform.succeeded span of
    Platform.Succeeded -> Doc.emptyDoc
    Platform.Failed -> Doc.emptyDoc
    Platform.FailedWith err ->
      hsep
        [ label "exception:",
          Exception.displayException err
            |> Doc.pretty
            |> Doc.annotate (Terminal.color Terminal.Red)
        ]

trace :: Platform.Span -> Doc
trace span =
  case Platform.frame span of
    Nothing -> Doc.emptyDoc
    Just (_, frame) ->
      hsep
        [ label "source:",
          Doc.pretty (Stack.srcLocFile frame),
          label "line:",
          Doc.pretty (Stack.srcLocStartLine frame),
          label "column:",
          Doc.pretty (Stack.srcLocStartCol frame)
        ]

name :: Platform.Span -> Doc
name = Doc.pretty << Platform.name

time :: Timer.Timer -> Platform.MonotonicTime -> Doc
time timer' time' =
  Timer.toUTC timer' time'
    |> Format.formatTime Format.defaultTimeLocale "%T"
    |> Doc.pretty

duration :: Platform.Span -> Doc
duration span =
  let milliseconds =
        Platform.finished span - Platform.started span
          |> Platform.inMilliseconds
          |> Doc.pretty
   in label "duration: " ++ milliseconds ++ "ms"

failed :: Platform.Span -> Doc
failed span =
  case Platform.succeeded span of
    Platform.Succeeded -> Doc.emptyDoc
    Platform.Failed ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)
    Platform.FailedWith _ ->
      "failed"
        |> Doc.annotate (Terminal.color Terminal.Red)

data Handler
  = Handler
      { timer :: Timer.Timer,
        handle :: System.IO.Handle
      }

handler :: Timer.Timer -> Settings -> Conduit.Acquire Handler
handler timer' Settings =
  Conduit.mkAcquire
    (Prelude.pure (Handler timer' System.IO.stdout))
    (\_ -> Prelude.pure ())

data Settings = Settings

decoder :: Environment.Decoder Settings
decoder = Prelude.pure Settings

--
-- Pretty print helpers for combining lists of docs.
-- These versions of functions of the prettyprinter library remove empty
-- elements from lists to prevent whitespace from being added around empty
-- elements.
--

vsep :: [Doc] -> Doc
vsep = removeEmpties >> Doc.vsep

hsep :: [Doc] -> Doc
hsep = removeEmpties >> Doc.hsep

vcat :: [Doc] -> Doc
vcat = removeEmpties >> Doc.vcat

removeEmpties :: [Doc] -> [Doc]
removeEmpties = List.filter (not << isEmpty)

isEmpty :: Doc -> Bool
isEmpty Doc.Internal.Empty = True
isEmpty _ = False
