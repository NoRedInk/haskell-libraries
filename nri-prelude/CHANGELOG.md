# Unreleased next version

### Enhancements:

- JUnit report includes failure location (#60)
- Debug.todo returns callstack of where it was called, not inside of library (#65)

# 0.5.0.0

### Breaking changes:

- replace Log.userIs\* functions with more standard debug, info, warn, error (#48)

### Enhancements:

- Improved diff representation in failing tests. (#40, #41, #43, #44)
- Ported floating point comparison expectations from elm-test. (#41, #42)
  - FloatingPointTolerance (..), within, notWithin
- Expect.equalToContentsOf now includes proper stack traces (is this a bugfix? Let's call it an enhancement.) (#40)

# 0.4.0.0

### Breaking changes:

- `Expect.Task` has been removed. Most of it's functionality has been moved into `Expect`.
- `Test.task` has been removed. Regular `Test.test` now supports monadic-style test writing.
- `Expect.concat` has been removed. `do`-notation can now be used to run multiple expectations.

### Enhancements:

- Test failure diffs now look much nicer if they contain multi-line output.
- Test failures now show a snippet of the source code around the location of the failure.
- Tests now write to a log file that can be inspected using a new `log-explorer` tool.
- Added `Platform.writeSpanToDevLog` to write to `log-explorer` from your own code.

# 0.3.1.0

### Enhancements:

- `Platform.summary` can be used to decorate tracing spans with a text summary for use in dev tooling.
- `Platform.writeSpanToDevLog` can be used to write a span for consumption by the new `log-explorer` tool.

# 0.3.0.0

### Breaking changes:

- `Test.fromTestTree` has been removed.
- `Fuzz.Fuzzer` is now an opague type and no a synonomy for `Hedgehog.Gen`.
- `Expect.Task.TestFailure` renamed to `Expect.Task.failure`.
- `Test.Runner.Tasy.main` renamed to `Test.run`.

### Enhancements:

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
