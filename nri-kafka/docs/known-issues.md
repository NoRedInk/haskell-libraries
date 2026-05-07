# Known issues

## `sendSync` hangs forever on delivery failure

### Where

`src/Kafka.hs`, `mkHandler` → `Internal.sendSync` (around line 199–208) and
`sendHelperAsync` (around line 248–275).

### Symptom

A caller of `Kafka.sendSync` blocks indefinitely if the message ultimately
fails to deliver (e.g. exceeded `delivery.timeout.ms`, retries exhausted,
non-retriable broker error, partition leader unavailable). The caller never
gets an error and never returns.

### Why

`sendSync` blocks on a `TMVar` that is signalled by an `onDeliveryCallback`.
The callback installed in `sendHelperAsync` only signals on the success branch:

```haskell
\deliveryReport -> do
  log <- Platform.silentHandler
  Task.perform log <|
    case deliveryReport of
      Producer.DeliverySuccess _producerRecord _offset -> onDeliveryCallback
      _ -> Task.succeed ()  -- <-- failure path is a silent no-op
```

So when librdkafka emits a `DeliveryFailure` (or any other non-success report),
the TMVar is never written and `TMVar.readTMVar terminator` in `sendSync`
parks forever.

### Why this matters

`acks=all` and `enable.idempotence=true` are both hardcoded, so the success
path is robust — but failure paths exist (timeout, retries exhausted, broker
returning a non-retriable error). In production these are rare but not
impossible, and when they hit, the calling request handler hangs until killed
or its surrounding timeout fires (if any). It also mutes errors from
observability — the caller doesn't get to log/report the failure.

### What a fix looks like

The callback should signal the TMVar in *both* branches and the `Internal.Msg`
path should carry the error to the caller. Roughly:

1. Make the terminator carry a `Result Internal.Error ()` instead of a unit
   `Terminate`, or use a second TMVar for the error.
2. In the failure branch, package the `DeliveryReport` (which contains the
   `KafkaError`) and write it to the terminator.
3. `sendSync` reads the result and returns `Task.fail` on the error case.

Whatever shape the fix takes, it must preserve the existing success-path
contract (TMVar signalled exactly once after broker confirmation).

### What we're NOT doing yet

This was discovered while diagnosing the unrelated 100ms-poll-interval latency
problem (see `pollEvents` rewrite in `src/Kafka.hs`). It is **not fixed** in
that change because the latency fix is purely a polling-loop change and we
wanted to land it without expanding scope. The hang-forever issue should be
its own follow-up so the test surface stays small and the change is
auditable on its own.

### Links

- librdkafka delivery-report semantics: <https://github.com/confluentinc/librdkafka/blob/master/INTRODUCTION.md#producer-api>
- hw-kafka-client `DeliveryReport` constructors: see
  `Kafka.Producer.Types` in hw-kafka-client (hidden in 4.x but the type is
  re-exported).
