module Test.Reporter.Logfile
  ( report,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Time as Time
import qualified Dict
import qualified GHC.Stack as Stack
import qualified List
import qualified Maybe
import NriPrelude
import qualified Platform.Internal as Platform
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO
import qualified Test.Internal as Internal
import qualified Tuple
import qualified Prelude

report ::
  Stack.HasCallStack =>
  Time.UTCTime ->
  System.IO.Handle ->
  Internal.SuiteResult ->
  Prelude.IO ()
report now handle results = do
  projectDir <- map FilePath.takeBaseName Directory.getCurrentDirectory
  let testSpans = spans results
  let maybeFrame =
        Stack.callStack
          |> Stack.getCallStack
          |> List.head
          |> map (Tuple.mapFirst Data.Text.pack)
  let rootSpan =
        Platform.TracingSpan
          { Platform.name = "test run",
            Platform.started =
              List.minimum (List.map Platform.started testSpans)
                |> Maybe.withDefault (Platform.MonotonicTime 0),
            Platform.finished =
              List.maximum (List.map Platform.finished testSpans)
                |> Maybe.withDefault (Platform.MonotonicTime 0),
            Platform.frame = maybeFrame,
            Platform.details = Nothing,
            Platform.summary = Just (Data.Text.pack projectDir),
            Platform.succeeded = case results of
              Internal.AllPassed _ -> Platform.Succeeded
              _ -> Platform.Failed,
            Platform.allocated = 0,
            Platform.children = testSpans
          }
  Aeson.encode (now, rootSpan)
    |> Data.ByteString.Lazy.hPut handle
  Data.ByteString.Lazy.hPut handle "\n"

spans :: Internal.SuiteResult -> [Platform.TracingSpan]
spans results =
  spansAndNamespaces results
    |> groupIntoNamespaces

spansAndNamespaces :: Internal.SuiteResult -> [([Text], Platform.TracingSpan)]
spansAndNamespaces results =
  case results of
    Internal.AllPassed tests -> List.map bodyAndDescribes tests
    Internal.OnlysPassed tests _ -> List.map bodyAndDescribes tests
    Internal.PassedWithSkipped tests _ -> List.map bodyAndDescribes tests
    Internal.TestsFailed passed _ failed ->
      List.map bodyAndDescribes passed
        ++ List.map (Tuple.mapSecond Tuple.first << bodyAndDescribes) failed
    Internal.NoTestsInSuite -> []
  where
    bodyAndDescribes :: Internal.SingleTest body -> ([Text], body)
    bodyAndDescribes test = (Internal.describes test, Internal.body test)

groupIntoNamespaces :: [([Text], Platform.TracingSpan)] -> [Platform.TracingSpan]
groupIntoNamespaces namespacedSpans =
  namespacedSpans
    |> groupBy (List.head << Tuple.first)
    |> Dict.toList
    |> List.concatMap
      ( \(headNamespace, namespacedSpanGroup) ->
          let spans' = List.map Tuple.second namespacedSpanGroup
           in case headNamespace of
                Nothing -> spans'
                Just namespace ->
                  [ Platform.TracingSpan
                      { Platform.name = "describe",
                        Platform.started =
                          List.minimum (List.map Platform.started spans')
                            |> Maybe.withDefault (Platform.MonotonicTime 0),
                        Platform.finished =
                          List.maximum (List.map Platform.finished spans')
                            |> Maybe.withDefault (Platform.MonotonicTime 0),
                        Platform.frame = Nothing,
                        Platform.details = Nothing,
                        Platform.summary = Just namespace,
                        Platform.succeeded =
                          Prelude.mconcat (List.map Platform.succeeded spans'),
                        Platform.allocated = 0,
                        Platform.children =
                          namespacedSpanGroup
                            |> List.filterMap
                              ( \(namespaces, span) ->
                                  case namespaces of
                                    [] -> Nothing
                                    _ : rest -> Just (rest, span)
                              )
                            |> groupIntoNamespaces
                      }
                  ]
      )

groupBy :: Ord b => (a -> b) -> List a -> Dict.Dict b (List a)
groupBy f list =
  List.foldr
    ( \x ->
        Dict.update (f x) <| \val ->
          case val of
            Nothing -> Just [x]
            Just xs -> Just (x : xs)
    )
    Dict.empty
    list
