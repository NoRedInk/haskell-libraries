{-# LANGUAGE GADTs #-}

module Internal.Test
  ( Test (Describe, Test, Skip, Only, Todo, Fuzz, FromTestTree),
    FuzzerFunction (Fuzzer1, Fuzzer2, Fuzzer3),
    FuzzReplay (FuzzReplay),
    run,
    name,
    hasOnly,
    rejectTestTree,
  )
where

import NriPrelude
import qualified Control.Exception.Safe as Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text
import Fuzz (Fuzzer)
import qualified Hedgehog
import qualified Hedgehog.Internal.Property as Hedgehog.Property
import qualified Hedgehog.Internal.Report as Hedgehog.Report
import qualified Hedgehog.Internal.Runner as Hedgehog.Runner
import qualified Hedgehog.Internal.Seed as Seed
import qualified Internal.Expectation
import Internal.Expectation (Expectation)
import qualified Internal.TestResult
import Internal.TestResult (TestResult)
import List (List)
import qualified List
import Test.Tasty (TestTree)
import qualified Text
import Prelude (IO, Monad, Show, pure, show, traverse)

data Test where
  Test :: Text -> (() -> Expectation TestResult) -> Test
  Describe :: Text -> List Test -> Test
  Skip :: Test -> Test
  Only :: Test -> Test
  Todo :: Text -> Test
  Fuzz :: FuzzerFunction -> Text -> Test
  FromTestTree :: Text -> TestTree -> Test

data FuzzerFunction where
  Fuzzer1 ::
    forall a.
    (Show a) =>
    Fuzzer a ->
    (a -> Expectation TestResult) ->
    FuzzerFunction
  Fuzzer2 ::
    forall a b.
    (Show a, Show b) =>
    Fuzzer a ->
    Fuzzer b ->
    (a -> b -> Expectation TestResult) ->
    FuzzerFunction
  Fuzzer3 ::
    forall a b c.
    (Show a, Show b, Show c) =>
    Fuzzer a ->
    Fuzzer b ->
    Fuzzer c ->
    (a -> b -> c -> Expectation TestResult) ->
    FuzzerFunction

--

-- | The replay token to use for replaying a previous test run
newtype FuzzReplay = FuzzReplay (Maybe (Hedgehog.Size, Hedgehog.Seed))

run :: FuzzReplay -> Test -> IO TestResult
run replay test =
  Exception.handle handleException <| case test of
    Describe _ tests ->
      -- NOTE: Tasty actually never runs this, because it builds it's own tree.
      tests
        |> rejectTestTree
        |> traverse (run replay)
        |> fmap Internal.TestResult.concat
    Test _ testToRun -> Internal.Expectation.toResult (testToRun ())
    Skip _ -> pure Internal.TestResult.skipped
    Only test_ -> run replay test_
    Todo _ -> pure <| Internal.TestResult.failed "TODO"
    FromTestTree _ _ ->
      "This should never happen sorry."
        |> Internal.TestResult.failed
        |> pure
    Fuzz gen _ ->
      genForAll gen
        |> andThen (liftIO << Internal.Expectation.toResult)
        |> andThen (liftIO << Internal.TestResult.throwFailingTest)
        |> handleProperty replay
        |> andThen
          ( \reportStatus ->
              case reportStatus of
                Hedgehog.Report.OK -> pure Internal.TestResult.passed
                Hedgehog.Report.GaveUp ->
                  [ "Gave up!",
                    "You can rerun this test with the following command:",
                    "  stack test {package} --test-arguments '--seed \"Size {size} Seed {seed} {seed}\"'",
                    "Search for a line containing the word `recheck` to locate the Size and Seed"
                  ]
                    |> Text.join "\n"
                    |> Internal.TestResult.failed
                    |> pure
                Hedgehog.Report.Failed
                  Hedgehog.Report.FailureReport
                    { Hedgehog.Report.failureMessage,
                      Hedgehog.Report.failureSeed,
                      Hedgehog.Report.failureSize
                    } ->
                    [ Data.Text.pack failureMessage,
                      "You can rerun this test with the following command:",
                      "  stack test {package} --test-arguments '--seed \""
                        ++ Data.Text.pack (show failureSize)
                        ++ " "
                        ++ Data.Text.pack (show failureSeed)
                        ++ "\"'",
                      "Search for a line containing the word `recheck` to locate the Size and Seed"
                    ]
                      |> Text.join "\n"
                      |> Internal.TestResult.failed
                      |> pure
          )

rejectTestTree :: List Test -> List Test
rejectTestTree tests =
  case tests of
    [] -> []
    FromTestTree _ _ : rest -> rejectTestTree rest
    t : rest -> t : rejectTestTree rest

genForAll ::
  Monad m =>
  FuzzerFunction ->
  Hedgehog.PropertyT m (Expectation TestResult)
genForAll fuzzerFunction =
  case fuzzerFunction of
    Fuzzer1 a cb ->
      map cb (Hedgehog.forAll a)
    Fuzzer2 a b cb ->
      map2
        cb
        (Hedgehog.forAll a)
        (Hedgehog.forAll b)
    Fuzzer3 a b c cb ->
      map3
        cb
        (Hedgehog.forAll a)
        (Hedgehog.forAll b)
        (Hedgehog.forAll c)

handleProperty :: FuzzReplay -> Hedgehog.PropertyT IO () -> IO Hedgehog.Report.Result
handleProperty (FuzzReplay replay) prop =
  case replay of
    Nothing -> Hedgehog.property prop |> checkProperty
    Just (size, seed) -> Hedgehog.property prop |> recheck size seed

-- | Check a property using a specific size and seed.
recheck :: MonadIO m => Hedgehog.Size -> Hedgehog.Seed -> Hedgehog.Property -> m Hedgehog.Report.Result
recheck size seed prop0 =
  Hedgehog.withTests 1 prop0
    |> checkHedgehog size seed
    |> liftIO

checkProperty :: MonadIO m => Hedgehog.Property -> m Hedgehog.Report.Result
checkProperty prop = liftIO <| do
  seed <- Seed.random
  checkHedgehog 0 seed prop

checkHedgehog ::
  MonadIO m =>
  Hedgehog.Size ->
  Hedgehog.Seed ->
  Hedgehog.Property ->
  m Hedgehog.Report.Result
checkHedgehog
  size
  seed
  Hedgehog.Property.Property
    { Hedgehog.Property.propertyConfig,
      Hedgehog.Property.propertyTest
    } =
    Hedgehog.Runner.checkReport propertyConfig size seed propertyTest (\_ -> pure ())
      |> map Hedgehog.Report.reportStatus
      |> liftIO

handleException :: Exception.SomeException -> IO TestResult
handleException exception =
  let exceptionMessage =
        exception
          |> Exception.displayException
          |> Data.Text.pack
   in [ "There was an unexpected exception!",
        "",
        "    " ++ exceptionMessage,
        ""
      ]
        |> Data.Text.unlines
        |> Internal.TestResult.failed
        |> pure

name :: Test -> Text
name test =
  case test of
    Test n _ -> n
    Describe n _ -> n
    Skip test_ -> name test_
    Only test_ -> name test_
    Todo n -> n
    FromTestTree n _ -> n
    Fuzz _ n -> n

hasOnly :: Test -> Maybe Test
hasOnly test =
  case test of
    Only t -> Just t
    Test _ _ -> Nothing
    Describe _ tests ->
      tests
        |> rejectTestTree
        |> List.filterMap hasOnly
        |> List.head
    Skip _ -> Nothing
    Todo _ -> Nothing
    FromTestTree _ _ -> Nothing
    Fuzz {} -> Nothing
