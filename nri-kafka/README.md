# Kafka integration

_Reviewed last on 2021-05-28_

This library exposes an Elm-like API to Kafka. It exports two main modules:

- `Kafka`, for code that needs to write messages to Kafka.
- `Kafka.Worker`, for writing 'workers' that process Kafka messages.

The quiz-engine service is currently the only one making use of Kafka, so it's the best place to see examples of these modules in use.
