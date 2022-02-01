# 0.1.0.4

- Added new `ElsewhereButToKafkaAsWell` mode to `CommitOffsets`, which commits offsets to Kafka once the external Offset storage has been updated. Kafka commits are performed only to keep Kafka informed about consumer lag.

# 0.1.0.3

- Relax version bounds to encompass `text-2.0.x`, `base-4.16.x` and `template-haskell-2.18.x`

# 0.1.0.2

- Support `time-1.x`.

# 0.1.0.1

- Support `aeson-2.0.x`.

# 0.1.0.0

- First release, but we've battle-tested it against significant load for months now!
  Hope you enjoy
