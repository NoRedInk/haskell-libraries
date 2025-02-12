module Main (main) where

import qualified Spec.Kafka.Worker.Integration
import qualified Spec.Kafka.Worker.Partition
import qualified System.Environment
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = do
  -- macos runners seem to be slow and fail on several kafka integration tests
  System.Environment.setEnv "NRI_TEST_TIMEOUT" "240000"
  Test.run tests

tests :: Test.Test
tests =
  Test.describe
    "lib/kafka"
    [ Spec.Kafka.Worker.Integration.tests,
      Spec.Kafka.Worker.Partition.tests
    ]
