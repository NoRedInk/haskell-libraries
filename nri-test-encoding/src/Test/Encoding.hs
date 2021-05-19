-- | Turns `Examples` into a `Test`
module Test.Encoding (examplesToTest) where

import qualified Examples
import qualified Expect
import NriPrelude
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Test (Test, test)
import qualified Text

-- | Creates tests for some examples
examplesToTest :: Text -> Text -> Examples.Examples -> Test
examplesToTest name fileName examples =
  test name <| \() ->
    Expect.equalToContentsOf
      ( "test" </> "golden-results" </> Text.toList fileName
          |> FilePath.makeValid
          |> Text.fromList
      )
      (Examples.render examples)
