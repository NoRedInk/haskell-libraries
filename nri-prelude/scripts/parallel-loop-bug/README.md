# parallel-loop-bug

`Test.run` runs ungrouped tests in parallel (`Task.parallel` →
`Async.forConcurrently`). Under the threaded RTS with multiple capabilities
(`+RTS -N`), a suite that forces a *variety of distinct decoder CAFs*
concurrently intermittently crashes with an uncaught `<<loop>>`
(`NonTermination`) before printing a report — i.e. a CI flake.

This program is a minimal reproduction: a dozen ungrouped tests, each decoding a
tiny document to a *distinct* type (a distinct `FromJSON` decoder CAF).

## Run

The crash is rare, so loop the binary on a multi-core box and count crashes:

```sh
cabal build parallel-loop-bug -fparallel-loop-bug
BIN=$(cabal list-bin parallel-loop-bug -fparallel-loop-bug)
loops=0
for i in $(seq 1 10000); do
  "$BIN" +RTS -N -RTS >/dev/null 2>err || grep -q '<<loop>>' err && loops=$((loops+1))
done
echo "loop crashes: $loops / 10000"
```

You should see a handful of `parallel-loop-bug: <<loop>>` crashes at `-N >= 4`,
and zero at `-N1`.

## What we know

- ~0.2–0.3% of runs at `-N12` (12-core); **0 at `-N1`/`-N2`**; the rate scales
  with `-N`.
- Needs **both** >1 capability **and** a *variety of distinct decoders, each
  doing a small but real parse*. Identical/duplicated decoders don't trip it,
  nor do pure `Expect.pass` tests; ~12 *distinct* ones do. The document can be
  tiny (each test decodes `"{}"`), but the parse must do real work: a dozen
  distinct YAML decoders reproduce it, whereas a dozen distinct *aeson*
  `eitherDecodeStrict' "{}"` decodes did **not** (0/10000) — that decode is
  essentially free. So the driver is the number of distinct decoder CAFs forced
  concurrently; we didn't map exactly which codecs/sizes qualify.
- Wrapping the suite in `Test.serialize` avoids it (sequential execution) — the
  current workaround for affected suites.
- The exception escapes **uncaught** (outside the per-test bodies that
  `Test.run` would catch and report).
- **Masked by profiling**: a `-fprof-late` build run with `+RTS -xc` does not
  reproduce it, so we have no Haskell-level stack. That's the signature of a
  low-level threaded-RTS black-hole / deadlock-detector race rather than a
  cyclic binding (and `-N1` cleanliness rules out a real cycle).
- **Not** GHC #13751 (`<<loop>>` under concurrent STM) — fixed in 8.2.1; this is
  GHC 9.8.4.
