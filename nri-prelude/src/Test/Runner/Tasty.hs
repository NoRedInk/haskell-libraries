module Test.Runner.Tasty (main) where

import NriPrelude
import Control.Exception.Safe (throw)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import Data.Typeable (Typeable)
import qualified Internal.Test
import qualified Internal.TestResult as Result
import qualified List
import qualified System.Environment as Env
import qualified Test
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Options as Options
import qualified Test.Tasty.Providers as Providers
import qualified Test.Tasty.Runners.Reporter as Reporter
import qualified Text
import Prelude (IO, pure, show)

main :: Test.Test -> IO ()
main test = do
  -- NOTE: We need to always run AntXML,
  -- because this ingredient actually runs the tests.
  let tastyXmlEnv = "TASTY_XML"
  maybeXml <- Env.lookupEnv tastyXmlEnv
  case maybeXml of
    Just _ -> pure ()
    Nothing -> Env.setEnv tastyXmlEnv "_build/report.xml"
  Tasty.defaultMainWithIngredients [Reporter.ingredient] (setup test)

data TestToRun
  = TestToRun Test.Test
  | Only TestToRun
  deriving (Typeable)

instance Providers.IsTest TestToRun where

  testOptions = pure [Options.Option (Proxy :: Proxy FuzzReplay)]

  run options (Only (TestToRun testToRun)) _progress = do
    result <- runTest options testToRun
    throw (Reporter.TestOnly result)
  run options (Only testToRun) progress = Providers.run options testToRun progress
  run options (TestToRun testToRun) _progress = do
    result <- runTest options testToRun
    case result of
      Reporter.OnlyTestPassed str -> pure (Providers.testPassed str)
      Reporter.OnlyTestFailed str -> pure (Providers.testFailed str)

runTest :: Options.OptionSet -> Test.Test -> IO Reporter.OnlyTestResult
runTest options testToRun = do
  let FuzzReplay replay = Options.lookupOption options
  testResult <- Internal.Test.run replay testToRun
  case testResult of
    Result.Passed -> pure (Reporter.OnlyTestPassed "")
    Result.Skipped -> throw Reporter.TestSkipped
    Result.Failed message ->
      show message
        |> Reporter.OnlyTestFailed
        |> pure

--

-- | The replay token to use for replaying a previous test run
newtype FuzzReplay = FuzzReplay Internal.Test.FuzzReplay
  deriving (Typeable)

instance Options.IsOption FuzzReplay where

  defaultValue = FuzzReplay (Internal.Test.FuzzReplay Nothing)

  parseValue v = map (FuzzReplay << Internal.Test.FuzzReplay << Just) replay
    where
      -- Reads a replay token in the form "{size} {seed}"
      size = List.take 2 (Text.words <| Data.Text.pack v)
      seed = List.drop 2 (Text.words <| Data.Text.pack v)
      replay =
        map2
          (,)
          (Options.safeRead (Data.Text.unpack <| Text.join " " size))
          (Options.safeRead (Data.Text.unpack <| Text.join " " seed))

  optionName = pure "seed"

  optionHelp = pure "Allow running the tests with a predefined seed, rather than a randomly generated seed. This is especially helpful when trying to reproduce a failing fuzz-test."

setup :: Test.Test -> Providers.TestTree
setup tests =
  case Internal.Test.hasOnly tests of
    Just sub ->
      -- only run tests that are wrapped in `only`.
      setup_ True sub
    Nothing ->
      setup_ False tests

setup_ :: Bool -> Test.Test -> Providers.TestTree
setup_ hasOnly test =
  case test of
    Internal.Test.Describe name tests ->
      tests
        |> List.map
          ( \test' ->
              case test' of
                Internal.Test.FromTestTree _ t -> t
                t -> setup_ hasOnly t
          )
        |> Tasty.testGroup (Data.Text.unpack name)
    _ ->
      Providers.singleTest (Data.Text.unpack (Internal.Test.name test))
        <| if hasOnly
          then Only (TestToRun test)
          else TestToRun test
