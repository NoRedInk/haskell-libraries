# Memory Leak Test

Testing for memory leaks in the Observability library.

A commit in this repo caused multiple NRI service to leak memory.

This script is an attempt to reproduce the leak in a controlled environment.

## Running

```sh
LOG_ENABLED_LOGGERS="stdout" cabal run --enable-profiling memory-leak-test -- +RTS -hy -l-au > /dev/null
```

This will:
- Run using the `stdout` logger only, so we don't write millions of events to `log-explorer`.
- Enable GHC to copile with profiling enabled
- Collect heap profile data grouped by data type (use `-hm` to group by module)
- Use the new eventlog output format (`-l-au`)
- Discard stdout

After running for a good while, you can inspect the eventlog with:

```sh
nix-shell -p haskellPackages.eventlog2html --run "eventlog2html memory-leak-test.eventlog"
```

Then open `memory-leak-test.eventlog.html` in your browser.