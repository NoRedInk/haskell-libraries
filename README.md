# Redis

_Reviewed last on 2020-10-09_

This library provides functions for working with [Redis][redis].

The `Redis` module has everything you need to set up a Redis connection. You
can then use `Redis.Json`, `Redis.Text`, and `Redis.ByteString` to store and
retrieve data. These modules all implement the same Redis commands, but differ
in the type of data they store and retrieve.

At present this library implements a subset of the available [redis commands].
If you miss a command please feel free to add it!

[redis]: https://redis.i
[redis commands]: https://redis.io/commands
