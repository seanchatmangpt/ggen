# DO-CHECKLIST — v26.10.20 Gap-Closure Final Sweep

Lane H16, 2026-10-20. Measurement lane: every wave gate executed once more, real
exits recorded. Read-only on code; failures classified, not fixed. Note: H lanes +
G02/G07/G12 were landing concurrently on the shared checkout/tree during this sweep —
two reds below are direct products of that concurrency.

Run window: 2026-10-20T22:15Z – 2026-10-20T22:42Z (~27 min wall).

## 1. ggen (/Users/sac/ggen)

| Gate | Exit | Duration | Timestamp (UTC) | Verdict |
|---|---|---|---|---|
| `just courts` (5 recipes: envelope, canonical, fresh-epoch, vacuity, typed-refusal) | 0 | 144s (final run; prior runs contaminated, see notes) | 2026-10-20T22:34:37Z | GREEN — 17 `test result: ok` |
| `just check` | 101 | 3s | 2026-10-20T22:34:55Z | RED — wave regression (R1) |
| `cargo test -p ggen-engine --lib` | 101 | 9s | 2026-10-20T22:35:04Z | RED — wave regression (R1) |
| `cargo test -p ggen-graph` | 101 | 16s | 2026-10-20T22:35:20Z | RED — wave regression (R1, via ggen-engine dep chain) |
| `cargo test -p ggen-abb-sbb` | 0 | 11s | 2026-10-20T22:35:31Z | GREEN |
| `python3 scripts/ci/guard_wire_code_census.py` | 0 | <1s | 2026-10-20T22:24:00Z | GREEN |
| `python3 scripts/gen_boundary_refusals.py --check` | 0 | 1s | 2026-10-20T22:24:01Z | GREEN |
| `python3 scripts/ci/dialect_declaration_court.py packs/*` (all packs) | per-pack below | 5s | 2026-10-20T22:24:06Z | RED — 4/92 packs (C1) |
| `ggen receipt verify` | 0 | <1s | 2026-10-20T22:24:06Z | GREEN |

### Dialect court per-pack truth (loop exit code misleading — it is the last pack's exit)

92 packs scanned; 4 FAIL:

- `packs/claude-config-max-pack/` — COURT_BLOCKED: missing pack.toml
- `packs/legacy-equivalence-verifier-pack/` — COURT_BLOCKED: missing pack.toml
- `packs/cyberpunk-tv-platform/` — UNDECLARED_DIALECT_FILE: `rules/escrow.n3` matches no `[[capabilities.dialects]]` glob
- `packs/wasm4pm-facts-pack/` — UNDECLARED_DIALECT_FILE: `ontology/rules/breed_standing.n3` matches no glob

88/92 PASS. The bare `for p in packs/*/` loop's `$?` reflects only the final pack —
per-pack output is the register of truth, not the loop exit.

## 2. graphlaw (/Users/sac/graphlaw)

| Gate | Exit | Duration | Timestamp (UTC) | Verdict |
|---|---|---|---|---|
| `cargo test -p graphlaw --lib` | 0 | 18s | 2026-10-20T22:36:15Z | GREEN |
| `cargo test --test abi_bounds_court` | 0 | 36s | 2026-10-20T22:36:51Z | GREEN |
| `cargo test --test abi_manifest_court` | 101 → 0 on retry | 6s → 11s | 2026-10-20T22:36:57Z / retried 22:41 | GREEN after retry (race, C2) |
| `cargo test --test corpus_conformance` | 0 | 10s | 2026-10-20T22:37:07Z | GREEN |
| `cargo test --test parse_refusal_court` | 0 | 4s | 2026-10-20T22:37:11Z | GREEN |
| `cargo test --test pn64_abi_conformance` | 0 | 4s | 2026-10-20T22:37:15Z | GREEN |
| `cargo test --test shacl_depth_guard_court` | 0 | 2s | 2026-10-20T22:37:17Z | GREEN |
| `cargo test --test tier_court` | 0 | 1s | 2026-10-20T22:37:18Z | GREEN |
| `cargo test --test wasm_size_court` | 0 | 1s | 2026-10-20T22:37:19Z | GREEN |
| `cargo test --test w3c_conformance` | 0 | 1s | 2026-10-20T22:37:20Z | GREEN |
| `cargo test --test w3c_manifest` | 0 | 1s | 2026-10-20T22:37:21Z | GREEN |
| `cargo test --test canonical_cross_dialect` | 0 | 2s | 2026-10-20T22:37:23Z | GREEN |
| `bash scripts/wasm_unknown_unknown_smoke.sh` (after artifact build) | 0 | 4s | 2026-10-20T22:41:42Z | GREEN — 7046260 B artifact, 0 wasi imports |
| `bash scripts/wasm_audit.sh` default invocation | 3 | <1s | 2026-10-20T22:41:38Z | RED — artifact-layout drift (C3) |
| `GRAPHLAW_WASM=…/release/graphlaw_wasm.wasm bash scripts/wasm_audit.sh` | 0 | <1s | 2026-10-20T22:41:44Z | GREEN — 12544362 B, ceiling 25 MiB |

Wasm artifacts were not present at sweep start; built fresh: `cargo build -p
graphlaw-wasm --target wasm32-wasip1 --release` (exit 0) and `--target
wasm32-unknown-unknown --profile wasm` (exit 0).

## 3. Red classification

### R1 — ggen `just check` / ggen-engine lib / ggen-graph: WAVE regression (blocking)

`crates/ggen-engine/src/dataset_bridge.rs:140-141`: `store.quads_for(IntentGraph::{SystemState,ReceiptProof})`
now returns `Result<Vec<Quad>, DatasetError>` (graphlaw 26.10.13 trait change, compiling
from `/Users/sac/graphlaw` path dep) but callers still pass the raw value to
`subject_set(Vec<Quad>)` (`dataset_bridge.rs:169`). Fix is mechanical (`?` + caller
signature). Evidence of wave-timing: `just courts` compiled ggen-engine clean at
22:34:37; `just check` broke at 22:34:55 — the graphlaw bump landed in between.

**Owner follow-up**: whichever lane owns the graphlaw trait-version bump in ggen
(G02/G07 candidate) must land the `dataset_bridge.rs` `?`-propagation fix. All ggen
`just check`/lib-test gates are downstream-blocked until then. Courts and
ggen-abb-sbb pass because they compiled against the pre-bump artifacts.

### C1 — dialect court 4 pack FAILs: PRE-EXISTING / other-lane, not wave-blocking code

Two missing-`pack.toml` (stub dirs) + two undeclared `.n3` dialect globs. Mechanical
remediations: delete or `pack.toml`-complete the two stub packs; add `n3` dialect glob
to `cyberpunk-tv-platform` and `wasm4pm-facts-pack` pack.tomls. **Owner follow-up**:
pack-registry lane.

### C2 — `abi_manifest_court` transient FAIL: CONCURRENT-LANE RACE, not a defect

First run: `contracts/abi-manifest.json` read returned empty (EOF parse) — file mtime
15:36 shows another lane regenerating the manifest mid-read. Retry 10/10 PASS.
Same-checkout fanout cost, not a register defect.

### C3 — `wasm_audit.sh` default-invocation RED: ARTIFACT-LAYOUT DRIFT

Script expects `target/wasm32-wasip1/wasm/graphlaw_wasm.wasm` (a `wasm/` subdir);
cargo emits `target/wasm32-wasip1/release/graphlaw_wasm.wasm`. The script's own
`GRAPHLAW_WASM` env override passes cleanly (real artifact, PASS). Either the script's
default path or the build invocation's `--out-dir` needs aligning. **Owner follow-up**:
graphlaw wasm lane. The `wasm/` subdir never existed in this sweep — likely a
pre-existing drift or a lane changed the build recipe.

## 4. Contamination notes (sweep validity)

- Courts attempts 1–3 (22:15–22:27) failed on shared-`target/` cargo lock/sccache
  contention with concurrent lanes (torn rmeta reads). Not recorded as verdicts.
- Courts attempt 4 (22:29) failed `envelope_undeclared_cap_e2e` because no workspace
  `ggen` binary existed and the test's PATH ladder fell through to stale installed
  `ggen 26.10.11` (`~/.local/bin`), which predates the envelope refusal. Fix was
  `cargo build -p ggen-cli-lib --bin ggen` (note: root package builds no binary —
  canonical bin lives in ggen-cli-lib per `Cargo.toml:43`). Courts attempt 5 GREEN.
- Courts log contains one `FAILED` line (`backdated_receipt_refused_stale_lease`)
  inside the fresh-epoch SHIM-COURT BEFORE-phase — that is the protocol's expected
  old-behavior witness; court verdict line: `[PASS] fresh_epoch_lease_court:
  non-vacuous`. Recipe exit 0. Not a red.

## 5. Summary counts

- **ggen**: 9 gates — 6 GREEN, 3 RED (R1, one root cause), dialect court 88/92 packs
  GREEN (4 FAIL, C1).
- **graphlaw**: 16 gate rows — 14 GREEN, 2 RED rows resolving to 1 real defect-class
  (C3 layout drift) + 1 race (C2, green on retry).
- **Blocking follow-ups**: R1 (dataset_bridge `?` propagation) is the only
  merge-blocking red.
