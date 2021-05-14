# Redis

_Reviewed last on 2020-10-14_

This library provides functions for working with [Redis][redis] and
JSON-serialized haskell records.

Because JSON encoding is quite lenient for containers (e.g. lists, maps),
using Hedis alone makes it easy to accidentally write a record to redis, and
only later discover that, when reading, you actually get back a list of that
record.

This library helps avoid those problem by levering specific Key-Value mapping
that we call an API.

At present this library implements a subset of the available [redis commands].
If you miss a command please feel free to add it!

## How to use me

1. make a handler

(optional) Set up some environmental variables.

these are the defaults:

```sh
REDIS_CONNECTION_STRING=redis://localhost:6379
REDIS_CLUSTER=0 # 1 is on
REDIS_DEFAULT_EXPIRY_SECONDS=0 # 0 is no expiration
REDIS_QUERY_TIMEOUT_MILLISECONDS=1000 # 0 is no timeout
```

```haskell
main : IO ()
main =
  -- use nri-env-parser to parse settings from the environment
  settings <- Environment.decode Redis.decoder
  -- get a handler
  Conduit.withAcquire (Redis.handler settings) <| \redisHandler ->
    callSomeRedisFuncs redisHandler
```

2. Define a codec type

```haskell
data Key = Key
  { userId :: Int,
    quizId :: Int
  }

data User =
  User {
    name :: Text,
    favoriteColor :: Text
  }
  deriving (Generic)
-- payload needs this!
instance Aeson.ToJSON User

-- using this enforces this key-value mapping
redisApi :: Redis.Api Key User
redisApi =
  Redis.Hash.jsonApi toKey
  where
    toKey :: Key -> Text
    toKey Key {userId, quizId} =
      Text.join
        "-"
        [ "quiz",
          Text.fromInt userId,
          Text.fromInt quizId,
          "v1" -- good idea to version your keys!
        ]
```

3. use the codec to read and writee!

```haskell
_ <- Redis.query handler (Redis.set redisApi (Key 1 2) (User "alice" "orange"))
Redis.query (Redis.get redisApi (Key 1 2))  -- Just (User "alice" "orange")
```

## FAQ

### Your default env variables cause a collision. What do I do?

add a prefix, and use `decoderWithEnvVarPrefix` instead of `decoder`

[redis]: https://redis.io
[redis commands]: https://redis.io/commands

```

```
