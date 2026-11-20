# Build-Time Cut Receipt (S1-S5, Phase 2) — 2026-11-04

Standing: ALIVE (S1-S5 landed, lock 890→854 exact; wall-clock directional-only)

Branch: `v26-11-3-wave`, baseline `032d6c958` (tree clean), CODER lane.
Commits: S1 `377442907`, S2 `9e6a77fb1`, S3, S4, S5 (see git log).

## Measurements (all run this session, real `time` output)

| Metric | Before | After | Delta |
|---|---|---|---|
| `cargo build --workspace` (Phase-0 baseline run) | 2m05.9s | — | — |
| Full workspace-crate recompile (all workspace lib.rs touched, deps cached) | not measured at baseline | 13.1s | — |
| Warm no-op build | 1.13s | 0.40s | -65% |
| `grep -c '^name = '` Cargo.lock | 890 | 854 | **-36 packages** |

Measurement-scope caveat: the 2m05.9s baseline run invalidated mid-level
crates (ggen-cli-lib, ggen-mcp, ggen-lsp), while the 13.1s after-number
rebuilt all workspace crates with third-party deps cached — the two are
not apples-to-apples. The exact-scope numbers are the warm builds
(1.13s -> 0.40s) and the lockfile delta (890 -> 854, -36). A clean
`cargo build --workspace` from empty target/ was not run on either side
(would double the session cost); treat the large wall-clock drop as
directional, the lockfile delta as exact.

## What was cut, and why it was lawful

### S1 — root package prod deps = tokio only (`377442907`)
- Deleted from root [dependencies]: human-panic, better-panic, log,
  env_logger, genai (root prod code = main.rs + VERSION const; zero
  consumers, grepped).
- Moved to [dev-dependencies]: chrono, serde, serde_yaml, tera, uuid,
  tokio, anyhow, serde_json, tempfile, tracing-subscriber. tokio REMAINS
  in [dependencies] — deviation from the dispatch (which said move it):
  src/main.rs uses `#[tokio::main]` and dev-deps do not cover bin
  targets; moving it would have failed the build gate. Documented in the
  S1 commit message.
- Deleted crates/ggen-cli [dependencies.genai] (zero usage).
- Retired the stale genai proc-macro-duplication comment block.
- Pre-existing fix (confirmed broken at the baseline manifests before any
  S1 edit, replayed byte-identical from HEAD): tests/integration/
  ontology_workflows_e2e.rs formatted `Sha256::finalize()` output with
  `{:x}` — GenericArray has no LowerHex impl, un-compilable under sha2
  0.11 under `--features integration`. Replaced with hex::encode.

### S2 — 4 crates excluded, not deleted (`9e6a77fb1`)
- powl2-decompose, bcinr-mfw-ir, bcinr-pddl, pm4pytest-cli: members ->
  exclude. Zero reverse deps outside the group (bcinr-pddl ->
  bcinr-mfw-ir only), zero justfile/CI consumers. Code on disk.
- Companion REQUIRED edit done: .specify/repo-facts.ttl — 4 individuals
  reclassified rf:Crate -> rf:ExcludedCrate (+ rf:excludedFromWorkspace /
  rf:exclusionReason), rf:memberCount 13->11, rf:crateCount 14->12
  (matches `cargo metadata --no-dev-deps`... = 12 packages exactly),
  crateMapIntro recount. Regenerated CLAUDE.md + architecture.md via
  `ggen sync run` (the producer).
- justfile unexpected_cfgs carve-out comment deleted; flag retained with
  a dated note (flag's sole original reason — bcinr-pddl's dangling
  mfw-planner cfg — left the members in the same cut).
- The parity test (system_crate_map_parity_test) caught pm4pytest-cli
  still in members on the first pass — the court working; fixed, re-run
  2/2 PASS.

### S3 — one notify-debouncer-full (`S3` commit)
- crates/ggen-engine: notify + notify-debouncer-full -> workspace = true.
  Lockfile now carries exactly one copy, 0.7.0.
- Gates: watch_module_test 5/5, watcher_e2e_test 1/1, lockfile grep == 1.

### S4 — workspace.dependencies retirements
- rig-core, cargo-cicd (git), genai deleted from
  [workspace.dependencies]; zero consumers verified per entry (also
  removed the dead root dev-dep `genai = { workspace = true }` left by
  S1's split — it referenced the now-removed workspace entry).

### S5 — mockall + process_mining
- Live-import gate run FIRST per dispatch: zero compiled `use mockall` /
  `use process_mining` in tests/ benches/ crates/ src/. Only hits are
  ggen-cheat-scanner's CHEAT-T04 string fixtures (read as text, never
  compiled) and doc mentions. Both deleted; `cargo test -p ggen
  --no-run` green.

## Gate ledger (all executed this session, actual outputs above)

- cargo build --workspace: PASS (after each step)
- cargo check -p ggen --all-targets / -p ggen-cli-lib: PASS
- cargo publish --dry-run --allow-dirty -p ggen: PASS (dry-run upload abort)
- cargo test -p ggen --no-run: PASS; --features integration: PASS after
  the pre-existing-fix (failed at baseline — verified pre-existing, not
  S1-introduced)
- cargo test -p ggen-config --test system_crate_map_parity_test: 2/2 PASS
- just check: PASS
- cargo test -p ggen-engine --test watch_module_test: 5/5 PASS
- cargo test -p ggen-mcp --test watcher_e2e_test: 1/1 PASS
- Cargo.lock notify-debouncer-full count: 1 (0.7.0)

## Not touched (lane boundary)
examples/, marketplace/, .github/ — other lanes own those. The two
untracked files that predate this lane (nv6-transitive-overrides.md,
tsconfig.tsbuildinfo) were left alone and not committed.
