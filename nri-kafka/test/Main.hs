module Main (main) where

import qualified Spec.Data.Aeson.Extra
import qualified Spec.Kafka.Worker.Integration
import qualified Spec.Kafka.Worker.Partition
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

tests :: Test.Test
tests =
  Test.describe
    "lib/kafka"
    [ Spec.Kafka.Worker.Integration.tests,
      Spec.Kafka.Worker.Partition.tests,
      Spec.Data.Aeson.Extra.tests
    ]
