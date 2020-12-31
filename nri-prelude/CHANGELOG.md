# 0.3.0.0

Breaking changs:

- `Test.fromTestTree` has been removed.
- `Fuzz.Fuzzer` is now an opague type and no a synonomy for `Hedgehog.Gen`.
- `Expect.Task.TestFailure` renamed to `Expect.Task.failure`.
- `Test.Runner.Tasy.main` renamed to `Test.run`.

Enhancements:

- Test reports now show source locations of failing tests.
- `Fuzz` module has been extended and now covers almost the entire API of its Elm counterpart.
- Dependency on `tasty` has been dropped.

# 0.2.0.0

- Breaking change: drop `Platform.TracingSpan` constructor.
- Add `Platform.emptyTracingSpan` export.
- Relax version bounds to encompas `tasty-1.4`.

# 0.1.0.4

- Relax version bounds to encompass `time-1.11`.

# 0.1.0.3

- Fix tests to allow inclusion in stackage.

# 0.1.0.2

- Relax version bounds to encompass `pretty-diff-0.2.0.0` and `base-4.14.0.0`.

# 0.1.0.1

- Relax version bounds to be compatible with `bytestring-0.11`.

# 0.1.0.0

- Initial release.
