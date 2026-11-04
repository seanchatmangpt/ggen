# SX-1 Red Fixes Receipt — v26.11.3 Seal

Standing: Red 1/2/3 FIXED; ALL BENCH GATES PASS, EXIT=0

Lane: SX-1 (implementation, small fixes + classified carries). Date: 2026-11-04.
Repos: /Users/sac/ggen, /Users/sac/arcus. No git operations run.

## Red 1 — ggen-lsp manifest_contract_test — FIXED

- **Subject**: `crates/ggen-lsp/tests/manifest_contract_test.rs` ::
  `manifest_declares_the_ggen_lsp_server`.
- **Root cause**: `.claude-plugin/marketplace.json` plugin version `26.10.10` vs
  crate (workspace) version `26.10.12` — the marketplace.json drift GL-1
  disclosed as pre-existing.
- **Fix**: bumped `.claude-plugin/marketplace.json` plugin version → `26.10.12`
  (one line; test reads the manifest at `../../.claude-plugin/marketplace.json`
  relative to the crate dir, confirmed before editing).
- **Gate (green ×2)**: `cargo test -p ggen-lsp --test manifest_contract_test`
  → `2 passed; 0 failed` (run 1 and run 2, both exits 0).
- **Full suite**: `cargo test -p ggen-lsp` — every test binary `test result: ok`,
  0 failures.

## Red 2 — perf_cold_start_with_config — FIXED

- **Subject**: `crates/ggen-cli/tests/performance.rs` ::
  `perf_cold_start_with_config` failing `[FM-LAW-015] ea.graph.json is absent`.
- **Root cause**: the perf test drives `ggen sync run --dry-run` from a temp
  cwd without the EA fixture; WO-01's fail-closed EA admission gate refuses.
- **Fix**: added the same minimal valid-but-empty EA graph fixture the
  cli_boundary scaffold uses (`crates/ggen-engine/tests/cli_boundary.rs`:
  `{"schema":"ggen.ea.graph.v1",...,zero candidate SBBs}`) to the temp project
  root before the timed `sync run`.
- **Gate (green ×2)**: `cargo test -p ggen-cli-lib --test performance`
  → `10 passed; 0 failed; 6 ignored` (run 1 and run 2, both exits 0).
- **Full suite**: `cargo test -p ggen-cli-lib` — 57 test binaries `ok`; one file
  red, classified pre-existing out-of-scope (see Pre-existing section).

## Red 3 — arcus bench_gates.sh G3 — FIXED (build-if-absent + fail-loud)

- **Subject**: `/Users/sac/arcus/scripts/bench_gates.sh` G3 EA fence.
- **State found on disk**: the typed `EA-BIN-MISSING` exit-1 already existed at
  the binary-existence check (line 99 at time of edit) — the fail-loud half of
  this red had already landed between census and edit (disclosed; census
  observed the pre-fix state).
- **Fix**: build-if-absent. On a missing binary the script now attempts
  `cargo build -p ggen-abb-sbb --example validate_ea_graph --release` from
  `~/ggen`, time-boxed via `timeout 600` when `timeout(1)` exists (unbounded
  run disclosed in the message when it does not), then re-checks executability,
  exiting 1 with typed `EA-BIN-MISSING` + the build command on failure.
  Cold-path latency disclosed in the script comment: a run that starts without
  the binary pays the release build (~12s witnessed this session).
- **Gates**:
  - Green with binary absent→built: full `scripts/bench_gates.sh` run with the
    real binary missing — G1/G2 green, G3 built the binary (11.57s, exit 0),
    EA fence OK min-of-3 0.0s, G4 120 records 0 chain breaks,
    `ALL BENCH GATES PASS`, EXIT=0.
  - Fail branch witnessed verbatim: G3 block extracted unchanged (`sed -n '/==
    G3/,/EA-FENCE-FAIL/p'`) and run under `HOME=/tmp/sx1-fakehome` (no
    `~/ggen`): typed `EA-BIN-MISSING ... build failed or timed out (600s)`
    message, EXIT=1. (The full-script sandbox route was not testable: G1's
    `ggen sync run` fails under a fake HOME — ggen's portable envelope needs
    rustc/rustup under the real HOME — so the block-level witness is the
    documented route.)
- **Files**: `/Users/sac/arcus/scripts/bench_gates.sh` (G3 block only).

## Red 4 — diagnostic_relay_probe — CARRIED-OPEN (recorded only)

- **Status**: CARRIED-OPEN per instructions — not fixed.
- **Attribution**: per cl-f: hardcoded external binary; current failure mode:
  20s publishDiagnostics timeout, 2/2 runs.
- **Seal open list**: add diagnostic_relay_probe (CL-H's suspected regression,
  attribution CL-E/F/G per receipts/cl-close.md) to the v26.11.3 seal's open
  list.

## Pre-existing, out of SX-1 scope (disclosed, not fixed)

`cargo test -p ggen-cli-lib` full-suite run surfaced 4 failures in
`crates/ggen-cli/tests/sync_surface_test.rs` (`bare_sync_defaults_to_run`,
`dry_run_exits_zero_and_writes_nothing`, `real_run_writes_outputs_and_receipt`,
`watch_starts_and_runs_until_killed`), all `[FM-LAW-015] ea.graph.json is
absent` — the same class the 45-remediation pass (K2/cli_boundary scaffold)
fixed elsewhere but this file never received. Not named in SX-1 scope; not in
SE-2b's 3-red census; my diff (performance.rs + marketplace.json +
bench_gates.sh) cannot affect it. Candidate for the identical one-fixture
remediation in a follow-up lane.

## Commands + exits summary

| Command | Exit | Result |
|---|---|---|
| `cargo test -p ggen-lsp --test manifest_contract_test` ×2 | 0, 0 | 2/2 passed |
| `cargo test -p ggen-cli-lib --test performance` ×2 | 0, 0 | 10 passed, 6 ignored |
| `cargo test -p ggen-lsp` (full) | 0 | all binaries ok, 0 failed |
| `cargo test -p ggen-cli-lib` (full) | — | 57 binaries ok; sync_surface_test 4 failed (pre-existing FM-LAW-015, out of scope) |
| `zsh scripts/bench_gates.sh` (binary absent→built) | 0 | ALL BENCH GATES PASS |
| G3 fail branch (verbatim block, fake HOME) | 1 | typed EA-BIN-MISSING |
