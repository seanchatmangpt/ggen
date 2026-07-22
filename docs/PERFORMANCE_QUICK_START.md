# Performance Quick Start

`just slo-check` is the one command that actually measures ggen against its own performance
targets. This page describes exactly what it checks, and reports a real run of it against this
codebase — including a real, currently-reproducing failure, because that is what the command
actually returned when run, not what it is supposed to return.

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

There is no build-time (first-build / incremental-build) SLO in the current recipe, and no
automated check for "RDF processing ≤5s/1k triples" or "generation memory ≤100MB" — those appear
in some older docs as aspirational targets but are not wired into anything that runs today. Do
not treat a table of numbers in another doc as a live guarantee; `just slo-check`'s two real
phases above are the whole of what's currently automated.

## A real run, right now

`just slo-check` was actually run against this codebase (branch `docs/readme-rewrite-v26.7.21`,
workspace version 26.7.31) rather than assumed to pass. It **exited 1** — both phases surfaced
real, current, reproducible problems, for two unrelated reasons. Neither was invented for this
doc; both are shown with their exact command, exact message, and file/line.

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

### Phase 2 — the real 180s SLO passed; the test it wraps did not

```console
Running Phase 2 SLO checks...
    Finished `test` profile [unoptimized + debuginfo] target(s) in 1m 05s
     Running tests/receipt_chain_e2e.rs

running 16 tests
test dry_run_touches_neither_receipt_nor_log ... ok
test closure_marks_missing_inputs_instead_of_dropping_them ... ok
test sync_refuses_to_extend_a_tampered_head ... ok
test template_edit_changes_receipt_closure_even_with_identical_outputs ... ok
test legacy_payload_without_optional_fields_verifies ... ok
test missing_receipt_json_chains_from_log_tail ... ok
test three_syncs_form_a_verifiable_chain ... ok
test tampered_signature_fails_closed_and_is_distinguished_from_chain_failure ... FAILED
test sign_then_verify_reports_signed_and_signature_valid_true ... FAILED
test legacy_unsigned_receipt_still_chain_verifies_with_signed_false ... FAILED
test malformed_ggen_signing_key_env_var_errors_loudly ... FAILED
test ggen_signing_key_env_var_takes_precedence_over_key_file ... FAILED

test result: FAILED. 11 passed; 5 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.27s
receipt_chain_e2e wall-clock: 67s (SLO threshold: 180s)
❌ receipt_chain_e2e reported test failures (see output above); timing SLO was measured (67s,
   within threshold) but the test itself did not pass
```

The **timing SLO genuinely passed** — 67s is real wall-clock, well under the 180s threshold, and
the recipe reports it honestly either way (it doesn't hide a slow-but-under-threshold run, and it
doesn't hide a fast-but-failing one). But the recipe treats a fast failing run as an overall
failure, correctly — a test suite failing in 67s is not evidence of health.

The 5 failures are not scattered; every one is about receipt **signing**, and two distinct
symptoms recur: `receipt verify`'s real JSON output in this run has no `"signed"` /
`"signature_valid"` keys at all (not `false` — absent), e.g.:

```json
{"valid": true, "chain_hash": "3de0b2...", "payload_hash": "1a26e1...", "graph_hash": "93d2b7...", "outputs": 1}
```

and a deliberately-tampered signature that the test expects to fail closed instead exits `0`
("expected non-zero exit but process succeeded"). This session did not root-cause the signing gap
itself (that would mean auditing `crates/ggen-engine/src/keys.rs` and the `receipt verify`
handler's field-population logic, which is out of scope here) — flagging the exact symptom and
its exact reproduction is the honest stopping point, not a diagnosis this doc doesn't have
evidence for.

**This is also a live claims-ledger drift, worth naming explicitly rather than only reporting
the raw test output.** `docs/aps/claims.toml`'s `release.receipt-chain-verifies` claim — which
gates publish (`gates = ["publish"]`) — currently reads `standing = "ALIVE"` with evidence
`"16/16 incl. legacy-unsigned, env-var key precedence, and history tamper cases"` recorded at
commit `1b32d7b06` (2026-07-17), using the exact falsifier
`cargo test -p ggen-engine --test receipt_chain_e2e`. Re-running that exact falsifier today
(branch `docs/readme-rewrite-v26.7.21`, off `main` at `7351e14af`, well after that recorded
commit) returns 11/16, not 16/16 — four of the five newly-failing tests are the signing/env-var
ones the recorded evidence explicitly named as covered. Per `docs/aps/README.md`'s own
maintenance rule ("if they disagree with this file, one of them is drift — fix the divergence,
don't paper over it"), this is exactly that situation, caught live. This page does not edit
`claims.toml` — that's a separate, deliberate action for whoever owns the release-gate decision,
not a side effect of a docs pass — but the drift should not go unreported once found.

## How to reproduce this yourself

```bash
just slo-check                                    # full two-phase run, ~4-5 min from a warm cache
cargo build --release --bin ggen                   # reproduces the Phase-1 root cause directly
cargo build --release -p ggen-cli-lib --bin ggen    # the working equivalent
cargo test -p ggen-engine --test receipt_chain_e2e -- --nocapture  # Phase 2's test, standalone
```

## Other performance commands

```bash
just bench     # cargo bench, root `ggen` package only (same scoping issue as above applies
               # to any bench in this package that shells out to `--bin ggen` without `-p`)
just audit     # cargo audit — security vulnerability scan, unrelated to timing SLOs
```

Both were read from the live `justfile`; only `slo-check` was actually executed for this page.
