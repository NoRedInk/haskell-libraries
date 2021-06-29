# nri-observability

A library for reporting observability data collected in [nri-prelude][]-backed Haskell applications to various backends.

This library exposes two categories of modules:

- `Reporter` modules for sending data to various backends.
- `Log` modules defining `Details` types for common kinds of tracing spans.
- `Observability` module for a unified way of reporting to multiple targets.

`Reporter` and `Log` can be mixed and matched with `Reporter` and `Details` types defined elsewhere.

[nri-prelude]: https://github.com/NoRedInk/haskell-libraries/tree/trunk/nri-prelude
