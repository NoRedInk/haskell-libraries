module Spec.Kafka (tests) where

import qualified Expect
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Internal as Internal
import qualified Kafka.Producer as Producer
import qualified Test

tests :: Test.Test
tests =
  Test.describe
    "Kafka"
    [ Test.describe
        "deliveryReportToResult"
        [ Test.test "a successful delivery becomes Ok ()" <| \() ->
            case Internal.deliveryReportToResult
              (Producer.DeliverySuccess exampleRecord (Consumer.Offset 0)) of
              Ok () -> Expect.pass
              other -> Expect.fail ("expected Ok (), got " ++ Debug.toString other),
          Test.test "a broker delivery failure becomes DeliveryFailed carrying the record and error" <| \() ->
            case Internal.deliveryReportToResult
              (Producer.DeliveryFailure exampleRecord exampleError) of
              Err (Internal.DeliveryFailed payload) ->
                Expect.equal payload (exampleRecord, exampleError)
              other -> Expect.fail ("expected Err (DeliveryFailed ...), got " ++ Debug.toString other),
          Test.test "a message-less failure becomes NoMessageDelivered carrying the error" <| \() ->
            case Internal.deliveryReportToResult
              (Producer.NoMessageError exampleError) of
              Err (Internal.NoMessageDelivered kafkaError) ->
                Expect.equal kafkaError exampleError
              other -> Expect.fail ("expected Err (NoMessageDelivered ...), got " ++ Debug.toString other)
        ]
    ]

exampleRecord :: Producer.ProducerRecord
exampleRecord =
  Producer.ProducerRecord
    { Producer.prTopic = "the-topic",
      Producer.prPartition = Producer.UnassignedPartition,
      Producer.prKey = Nothing,
      Producer.prValue = Nothing
    }

exampleError :: Producer.KafkaError
exampleError = Producer.KafkaError "boom"
