# Kafka integration

_Reviewed last on 2021-05-28_

This library exposes an Elm-like API to Kafka. It exports two main modules:

- `Kafka`, for writing to Kafka.
- `Kafka.Worker`, For building long-running worker apps that process Haskell
  messages.

At NoRedInk, we use this to power our high-throughput quiz-engine service. If
you work at NoRedInk: look there for a simple example app.

Otherwise: here's the gist of it:

```
import qualified Environment -- from nri-env-parser
import qualified Kafka.Worker

-- your long running app
main :: IO ()
main =
  settings <- Environment.decode Kafka.Worker.decoder
  Kafka.Worker.process
    Kafka.Worker.Description
      settings
      "this worker's group id"
      (Kafka.Worker.subscription "my.topic" processMessage,)

data MyKafkaMessageType =
  ReticulateSplines Int
  AddHiddenAgenda Text
  CalculateLlamaExpectorationTrajectory Llamas
  deriving (generic)

instance Aeson.ToJSON Envelope
instance Aeson.FromJSON Envelope

-- the meat and potatoes: handles all MyKafkaMessageTypes
processMessage ::
  Kafka.Worker.Envelope MyKafkaMessageType ->
  Task Text ()
processMessage record myMessage =
  -- process your message in here
  -- because of our usage of `Task` you probably want to pass in any handlers
  case myMessage of
    AddHiddenAgenda agenda ->
    	Debug.todo "Add the agenda"
    _ ->
    	Debug.todo "and also handle the other cases"
```
