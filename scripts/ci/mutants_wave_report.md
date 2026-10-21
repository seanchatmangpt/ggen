# Mutants Wave Report — G12 finisher lane, ggen v26.10.20

Date: 2026-10-20 (UTC). Lane: report-only, read-only wrt source. Repo: /Users/sac/ggen.

## Scope

Bounded slice: `cargo mutants -p ggen-engine --file crates/ggen-engine/src/envelope.rs
--timeout-multiplier 3`, budget 20 minutes wall.

## Headline numbers (real)

| Metric | Count |
|---|---|
| Mutants generated (`--list`) | 25 |
| Mutants executed | 0 |
| Caught | 0 |
| Missed | 0 |
| Unviable / timeout | 0 / 0 |

**The non-vacuity proof did NOT complete: zero mutants were executed.** The cargo-mutants
baseline (`--baseline run`, default) refused to proceed because the unmutated tree has two
pre-existing defects (below). cargo-mutants 27.1.0 exits with
`ERROR cargo {build,test} failed in an unmutated tree, so no mutants were tested`.

## Pre-existing blockers found (report-only, not fixed)

1. **Compile break (blocks the default cargo test-tool entirely)** —
   `crates/ggen-engine/tests/fresh_epoch_lease_court_test.rs:256:30`:
   `error[E0308]: mismatched types` — `assert_typed_stale_lease(err)` expects `AppError`,
   `plan_write(...)` now returns `WriteOutcome` (helper defined at
   `fresh_epoch_lease_court_test.rs:85`). Any full-test-package build (`cargo test`,
   the `just mutate-replay` passes A/B) fails to compile. Pre-existing: not touched by
   this lane; present before this session's runs.
2. **Failing refusal gate (real behavioral finding)** —
   `crates/ggen-engine/tests/envelope_undeclared_cap_e2e.rs:118`:
   `cli_refuses_undeclared_cap_with_typed_stderr_and_nonzero_exit` FAILS in the unmutated
   tree — assertion `!out.status.success()` tripped, i.e. `ggen sync run` **exits 0** in a
   project whose ontology declares an undeclared `res:` envelope cap, where the test
   requires a typed `E-ENVELOPE-UNDECLARED` non-zero refusal. The undeclared-envelope
   refusal surface is currently not firing end-to-end. **This is a REAL finding on the
   new envelope surface** (report-only per lane contract; not fixed here).
   Other 8 envelope tests passed in the same baseline run (envelope_receipt_binding_test 2/2,
   envelope_caps_e2e, envelope_otel_span_test 2/2).

## Workarounds attempted (disclosed)

- `cargo mutants --cargo-test-arg --test <name>`: cargo-mutants still compiles the whole
  test package for the build phase; the fresh_epoch compile break blocks it.
- `--test-tool nextest` with `--test envelope_receipt_binding_test envelope_caps_e2e
  envelope_undeclared_cap_e2e envelope_otel_span_test`: build succeeded, but the baseline
  test run failed on finding 2 above. Retried without envelope_undeclared_cap_e2e:
  build phase exceeded the remaining budget (kill at 600 s; nextest build of the package
  takes ~13 min on this machine) — killed, 0 mutants executed.

To complete the proof once the tree is green, the exact invocation is:

```
cargo mutants -p ggen-engine --file crates/ggen-engine/src/envelope.rs \
  --timeout-multiplier 3 --output mutants.out -j 6 --test-tool nextest \
  --cargo-test-arg --test --cargo-test-arg envelope_receipt_binding_test \
  --cargo-test-arg --test --cargo-test-arg envelope_caps_e2e \
  --cargo-test-arg --test --cargo-test-arg envelope_otel_span_test
```

(Re-add `envelope_undeclared_cap_e2e` only after finding 2 is fixed; it must be part of
the killing oracle — it is the refusal-path test.)

## mutants-replay.toml parse verification

`python3 -c "import tomllib; tomllib.load(open('mutations/mutants-replay.toml','rb'))"`
→ OK. Keys: `examine_globs` (5 entries incl. the new `envelope.rs`, `write.rs`,
`court/*.rs`, `generated/*.rs`), `exclude_re: ["fn main"]`.

**Exact explicit filter command for `envelope_receipt_binding_test`** (per the toml
comment; extends the `just mutate-replay` `run_pass` pattern):

```
cargo mutants --config mutations/mutants-replay.toml \
  -p ggen-engine --test-package ggen-engine \
  --output mutations/outcome-replay-envelope --copy-target true \
  --timeout-multiplier 3 --minimum-test-timeout 60 -j 6 -- \
  envelope_receipt_binding
```

(`envelope_receipt_binding` after `--` is the cargo-test name filter; it matches both
tests in the binary: `unchanged_envelope_caps_replay_identically` and
`envelope_cap_change_forks_the_receipt_chain`.) NOTE: this pass will also hit blocker 1
until `fresh_epoch_lease_court_test.rs:256` compiles again, because
`--test-package ggen-engine` builds the full test suite.

## Cleanup / leftovers

- No mutant scratch dirs in the repo tree; cargo-mutants scratch lives under
  `/Users/sac/.cache/tmp/cargo-mutants-ggen-*.tmp` (killed run's tmp dir may remain at
  `/Users/sac/.cache/tmp/cargo-mutants-ggen-ij7GBs.tmp` — rm was permission-denied for
  this lane; safe to delete manually).
- Results dir left in place per instructions: `/Users/sac/ggen/mutants.out/mutants.out/`
  (nested `mutants.out` from this lane's `--output mutants.out` inside the pre-existing
  outer `mutants.out/`). Contains only baseline-failure logs + `--list`-era metadata;
  caught.txt/missed.txt/timeout.txt/unviable.txt all empty (0 lines each).

## Verdict

NON-VACUITY NOT PROVEN for envelope.rs this wave — 25 mutants generated, 0 executed,
0 caught, 0 missed. Blocked by two pre-existing tree defects (compile break at
`fresh_epoch_lease_court_test.rs:256`; failing gate at `envelope_undeclared_cap_e2e.rs:118`).
The second is itself a real finding against the new undeclared-envelope refusal surface:
the CLI currently exits 0 where a typed non-zero refusal is specified.
