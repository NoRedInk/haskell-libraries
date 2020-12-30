-- | Module for presenting test results as a Junit XML file.
--
-- Lifted in large part from: https://github.com/stoeffel/tasty-test-reporter
module Test.Reporter.Junit
  ( report,
  )
where

import NriPrelude
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.FilePath as FilePath
import qualified Test.Internal as Internal
import qualified Text.XML.JUnit as JUnit
import qualified Prelude

report :: Internal.Test -> Internal.SuiteResult -> Prelude.IO ()
report suite result = do
  args <- System.Environment.getArgs
  case getPath args of
    Nothing -> Prelude.pure ()
    Just path -> do
      createPathDirIfMissing path
      JUnit.writeXmlReport path (testResults suite result)

testResults :: Internal.Test -> Internal.SuiteResult -> List JUnit.TestSuite
testResults _ _ = []

getPath :: [Prelude.String] -> Maybe FilePath.FilePath
getPath args =
  case args of
    [] -> Nothing
    "--xml" : path : _ -> Just path
    _ : rest -> getPath rest

createPathDirIfMissing :: FilePath.FilePath -> Prelude.IO ()
createPathDirIfMissing path = do
  dirPath <- map FilePath.takeDirectory (Directory.canonicalizePath path)
  Directory.createDirectoryIfMissing True dirPath
