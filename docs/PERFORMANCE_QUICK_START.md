# Performance Quick Start

`just slo-check` is the one command that actually measures ggen against its own performance
targets. This page describes exactly what it checks, and reports a real run of it against this
codebase — including a real, currently-reproducing bug in Phase 1, because that is what the
command actually returned when run, not what it is supposed to return. As of this run the command
as a whole exits `0`; see below for why that's true despite the Phase 1 bug.

For the fuller (partly aspirational) performance documentation set — SLO history, dashboards,
per-subsystem notes — see `docs/performance/README.md`. This page is scoped to the one command
you can run right now and trust the exit code of.

## What `just slo-check` checks

Two phases, read directly from the `slo-check:` recipe in `justfile`:

**Phase 1** — `cargo bench --bench cli_startup_performance -- --test`. Runs the
`cli_startup_performance` Criterion benchmark (root `ggen` package's `benches/`) in test mode:
help-command startup, CLI execution, cold/warm start, and startup-component timings.

**Phase 2** — a real wall-clock timing assertion (`date +%s` before/after, not a printed claim)
around `cargo test -p ggen-engine --test receipt_chain_e2e -- --nocapture`, failing loudly if
elapsed time exceeds a **180s** threshold. The threshold's own justification, from the recipe's
comment: a cold run of that same test on the reference machine (Darwin/arm64, pinned nightly)
measured 45s wall-clock, reproducible across two consecutive runs (2026-07-16); 180s is 4x that
baseline, generous enough to absorb CI variance without missing a genuine multi-minute regression.
Immediately after the timing assertion, the same recipe also runs a second, untimed test:
`cargo test -p ggen-graph --test coherence_hash_expectations_test -- --nocapture`. This isn't
optional or a separate phase in the recipe's own structure — it's the last line of the Phase 2
block, and a failure there fails the whole recipe (`|| exit 1`) same as a Phase 2 timing or test
failure would.

There is no build-time (first-build / incremental-build) SLO in the current recipe, and no
automated check for "RDF processing ≤5s/1k triples" or "generation memory ≤100MB" — those appear
in some older docs as aspirational targets but are not wired into anything that runs today. Do
not treat a table of numbers in another doc as a live guarantee; `just slo-check`'s two real
phases above are the whole of what's currently automated.

## A real run, right now

`just slo-check` was actually run against this codebase (branch `docs/readme-rewrite-v26.7.21`,
workspace version 26.7.31) rather than assumed to pass. It **exited 0**. That's worth stating
plainly because an earlier verification pass over this same page found it exiting `1`, with 5
failing tests in Phase 2 — re-running the exact same falsifier (`cargo test -p ggen-engine --test
receipt_chain_e2e`) standalone, 7 times in a row, and via the full `just slo-check` wrapper an 8th
time, produced 16/16 passing every single time, with no `GGEN_SIGNING_KEY` or other unusual
environment state set. That original 5-failure result did not reproduce under repeated,
independent testing and is not reported here as current — see the Phase 2 section below for what
was checked and why it's being retracted rather than repeated. Phase 1's bug, by contrast, **is**
real and reproduced identically on every run this session, including the one whose full log is
shown below.

### Phase 1 measured nothing — a real Decorative-Completion bug

The bench's log looked clean:

```console
Running Phase 1 SLO checks...
    Finished `bench` profile [optimized] target(s) in 2m 20s
     Running benches/cli_startup_performance.rs
Gnuplot not found, using plotters backend
Warning: Failed to build ggen binary. Skipping CLI startup benchmarks.
Warning: ggen binary not found. Skipping CLI execution benchmarks.
Warning: ggen binary not found. Skipping cold/warm start benchmarks.
Warning: ggen binary not found. Skipping startup components benchmarks.
```

No `error:` line, `cargo bench` exits `0`, `just slo-check` moves on to Phase 2 — but all four
benchmark groups silently no-op. `benches/cli_startup_performance.rs:16-20` shows why: it shells
out to `cargo build --release --bin ggen` (stdout/stderr both sent to `Stdio::null()`, so the
real reason never reaches the log) and treats any failure as "skip, don't fail the bench." Running
that exact suppressed command directly, unsuppressed, reproduces the real error immediately:

```console
$ cargo build --release --bin ggen
error: no bin target named `ggen` in default-run packages
help: available bin in `ggen-cli-lib` package:
    ggen
```

Root cause, confirmed by reading root `Cargo.toml`'s own comment (lines 31-39): the root `ggen`
package's own `[[bin]] name = "ggen"` was deliberately **removed** in the 2026-07-16 CLI-routing
flip (it was a byte-identical duplicate of `ggen-cli-lib`'s bin target, and kept the two packages
racing for `target/debug/ggen`). `ggen-cli-lib` (`crates/ggen-cli`) still declares
`[[bin]] name = "ggen"` and is the real, live binary target — but a bare `--bin ggen` at the
workspace root only searches "default-run packages," and the bench's build command was never
updated to add `-p ggen-cli-lib`. So it now fails, unconditionally, every time — this is not
flaky or environment-specific, it will fail identically on a clean checkout.

Net effect, stated precisely: **Phase 1 of `just slo-check` currently benchmarks nothing.** It
exits looking clean because the bench file treats "binary not found" as a skip, not a failure. If
you need a real CLI-startup number, run `hyperfine` or `time` directly against a binary you built
yourself with `cargo build --release -p ggen-cli-lib --bin ggen`, or fix the `-p` flag in
`benches/cli_startup_performance.rs:16-17` before trusting this phase's output again.

### Phase 2 — both the timing SLO and the test it wraps pass

```console
Running Phase 2 SLO checks...
     Running tests/receipt_chain_e2e.rs

running 16 tests
test legacy_unsigned_receipt_still_chain_verifies_with_signed_false ... ok
test legacy_payload_without_optional_fields_verifies ... ok
test dry_run_touches_neither_receipt_nor_log ... ok
test malformed_ggen_signing_key_env_var_errors_loudly ... ok
test closure_marks_missing_inputs_instead_of_dropping_them ... ok
test sync_refuses_to_extend_a_tampered_head ... ok
test template_edit_changes_receipt_closure_even_with_identical_outputs ... ok
test missing_or_empty_log_fails_closed ... ok
test tampered_chain_hash_fails_closed_and_is_distinguished_from_signature_failure ... ok
test tampered_signature_fails_closed_and_is_distinguished_from_chain_failure ... ok
test sign_then_verify_reports_signed_and_signature_valid_true ... ok
test missing_receipt_json_chains_from_log_tail ... ok
test three_syncs_form_a_verifiable_chain ... ok
test tampering_middle_line_payload_fails_naming_index_1 ... ok
test ggen_signing_key_env_var_takes_precedence_over_key_file ... ok
test removing_or_reordering_lines_fails_history_verification ... ok

test result: ok. 16 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.18s
receipt_chain_e2e wall-clock: 0s (SLO threshold: 180s)

running 9 tests
test test_matching_expectations_no_hash_drift ... ok
test test_empty_expectations_produces_no_hash_drifts ... ok
test test_expectations_do_not_suppress_count_discrepancies ... ok
test test_artifact_hash_mismatch_against_expectation ... ok
test test_event_log_hash_mismatch_against_expectation ... ok
test test_multiple_poles_with_hash_mismatches ... ok
test test_ontology_hash_mismatch_against_expectation ... ok
test test_partial_expectations_only_checks_declared_poles ... ok
test test_rule_6_cross_pole_coherence_fracture ... ok

test result: ok. 9 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
✅ Phase 1 + Phase 2 SLO checks complete
```

16/16 on `receipt_chain_e2e`, 9/9 on the trailing `coherence_hash_expectations_test`, both real
`cargo test` runs, both `ok`. The `0s` wall-clock is real too, not a bug — this particular run's
`cargo test` binary was already built from an earlier command in the same session, so the timed
window only covered execution, not compilation; on a colder run expect something closer to the
recipe comment's documented 45s baseline, still comfortably under the 180s SLO either way.

An earlier draft of this page reported this exact test failing 5/16, all clustered around receipt
**signing** (`tampered_signature_fails_closed_and_is_distinguished_from_chain_failure`,
`sign_then_verify_reports_signed_and_signature_valid_true`,
`legacy_unsigned_receipt_still_chain_verifies_with_signed_false`,
`malformed_ggen_signing_key_env_var_errors_loudly`,
`ggen_signing_key_env_var_takes_precedence_over_key_file`) and cited a matching claims-ledger
drift against `docs/aps/claims.toml`'s `release.receipt-chain-verifies` claim (recorded evidence:
"16/16", commit `1b32d7b06`, 2026-07-17). That specific result was checked here and did not
reproduce: running the identical falsifier the claims ledger names
(`cargo test -p ggen-engine --test receipt_chain_e2e`) 7 times standalone plus once more inside
the full `just slo-check` wrapper — 8 independent runs total, clean shell environment, no
`GGEN_SIGNING_KEY` set — returned 16/16 passing every time, with every one of those 5 previously-
"failing" test names showing `ok`. The signing tests set `GGEN_SIGNING_KEY` only via
`std::process::Command::env(...)` on child processes they spawn, not via `std::env::set_var` on
the test binary's own process, so cross-test environment leakage under parallel execution isn't a
plausible mechanism either. Given 8/8 clean runs against 1 originally-reported failing run, the
5-failure result is being retracted here rather than repeated: there is currently no reproducible
signing gap, and — checked directly — no drift against `docs/aps/claims.toml`'s recorded "16/16"
evidence. Whatever produced the original single failing run was not re-established by this
session and is not claimed to be understood.

## How to reproduce this yourself

```bash
just slo-check                                    # full run, exits 0; ~2-5 min depending on cache
cargo build --release --bin ggen                   # reproduces the Phase-1 root cause directly
cargo build --release -p ggen-cli-lib --bin ggen    # the working equivalent
cargo test -p ggen-engine --test receipt_chain_e2e -- --nocapture          # Phase 2's main test
cargo test -p ggen-graph --test coherence_hash_expectations_test -- --nocapture  # Phase 2's trailing test
```

## Other performance commands

```bash
just bench     # cargo bench, root `ggen` package only (same scoping issue as above applies
               # to any bench in this package that shells out to `--bin ggen` without `-p`)
just audit     # cargo audit — security vulnerability scan, unrelated to timing SLOs
```

Both were read from the live `justfile`; only `slo-check` was actually executed for this page.
