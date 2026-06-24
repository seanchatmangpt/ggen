# Post-Chatman Phase 1 — Verification Runbook

**Status of this document:** This runbook is the deliverable. Everything it
verifies is **UNVERIFIED** until the steps below are executed in a *building*
environment. The container that produced the Phase 1 patches **cannot compile
the workspace** (see §1), so no command in this file has been run. Treat every
"expected outcome" as a contract the patch must satisfy, not as an observed fact.

**Audience:** a human or CI runner with a checkout that can compile ggen.

**Scope:** verify the 9 Phase 1 units listed in §3. Do not assume a unit passes
because it compiles; run its specific gate and confirm the observable outcome.

**Provenance of this runbook** — grounded against these real files (read, not
guessed):

- `justfile` (recipe names)
- `CLAUDE.md` (command table, OTEL table, Definition of Done)
- `.claude/rules/coding-agent-mistakes.md` (§4 invariants, §5 sabotage table)
- `.claude/rules/otel-validation.md` (span/attribute requirements)
- `Cargo.toml` lines 786–799 (the `[patch.crates-io]` block)
- `.specify/specs/post-chatman/post_chatman.ttl` and `list_poles.rq`
- `crates/ggen-graph/src/coherence.rs`, `.../src/ocel/pack_events.rs`
- `crates/ggen-graph/tests/post_chatman_coherence_integration.rs`
- `crates/ggen-core/src/reverse_sync/inverse_pipeline.rs`
- `crates/ggen-core/tests/lockfile_persistence_test.rs`
- `crates/ggen-cli/tests/sabotage_tests.rs`, `.../e2e_pack_workflow_test.rs`

> **Drift flags found while writing this runbook** (read §7 before trusting any
> command verbatim). The biggest one: **`just test-unit` does not exist** — the
> real recipe is `just test-lib`. Several test *files* named in the Phase 1
> brief also do not exist yet; they are patch deliverables, and §3 marks each.

---

## 1. BUILD UNBLOCK — resolve the `[patch.crates-io]` redirect

### 1.1 The situation

`Cargo.toml` lines **790–799** contain:

```toml
# Point lsp-max family to the local workspace — the crates.io 26.6.9 release
# still carried tower-lsp symbols internally; the local checkout has them stripped.
[patch.crates-io]
lsp-max          = { path = "../lsp-max" }
lsp-max-protocol = { path = "../lsp-max/lsp-max-protocol" }
lsp-max-macros   = { path = "../lsp-max/lsp-max-macros" }
lsp-max-client   = { path = "../lsp-max/crates/lsp-max-client" }
# wasm4pm-compat and wasm4pm must be patched here so that lsp-max (which
# is itself a non-member path dep) resolves their transitive uses from
# the local sources rather than crates.io, which lacks newer modules.
wasm4pm-compat   = { path = "../wasm4pm-compat" }
wasm4pm          = { path = "../wasm4pm/wasm4pm" }
```

These six entries redirect the `lsp-max` family and the `wasm4pm` family to
sibling directories *next to* the ggen checkout (`../lsp-max`, `../wasm4pm`,
`../wasm4pm-compat`). If those siblings are absent, `cargo` fails to resolve the
patched paths and the whole workspace will not configure — which is exactly why
the patch-producing container could not build.

### 1.2 Option A — restore the sibling repos (preferred, matches committed intent)

Place the sibling checkouts so the relative paths resolve. Given the ggen
checkout at `<...>/ggen`, the siblings must live at:

```
<parent>/
├── ggen/                      # this repo
├── lsp-max/                   # provides lsp-max, lsp-max-protocol, lsp-max-macros
│   ├── lsp-max-protocol/
│   ├── lsp-max-macros/
│   └── crates/lsp-max-client/
├── wasm4pm-compat/
└── wasm4pm/
    └── wasm4pm/
```

Clone/copy the canonical sources for each. After they exist, verify the paths
resolve **without building** anything heavy:

```bash
# From the ggen checkout root. Resolves the dependency graph (incl. patches)
# but does not compile crates. Fast, and proves the patch paths are valid.
cargo metadata --format-version 1 >/dev/null && echo "patch paths resolve"
```

If `cargo metadata` errors with "failed to load source for dependency
`lsp-max`" or a path-not-found, a sibling is missing or mis-placed. Fix the
layout above before proceeding to §2.

This option preserves the committed warning: crates.io `lsp-max 26.6.9` "still
carried tower-lsp symbols internally; the local checkout has them stripped."
Using the local checkout is the intended state.

### 1.3 Option B — crates.io workaround (when siblings are unavailable)

Comment out the entire `[patch.crates-io]` block (lines 790–799 of
`Cargo.toml`). With the patch removed, Cargo resolves `lsp-max = "26.6.9"` and
the `wasm4pm` family from crates.io per the version declared in
`workspace.dependencies`.

```toml
# [patch.crates-io]
# lsp-max          = { path = "../lsp-max" }
# lsp-max-protocol = { path = "../lsp-max/lsp-max-protocol" }
# lsp-max-macros   = { path = "../lsp-max/lsp-max-macros" }
# lsp-max-client   = { path = "../lsp-max/crates/lsp-max-client" }
# wasm4pm-compat   = { path = "../wasm4pm-compat" }
# wasm4pm          = { path = "../wasm4pm/wasm4pm" }
```

**WARNING (from the committed comment, lines 786–788):** crates.io `lsp-max
26.6.9` may carry **tower-lsp symbols** internally. This affects the **LSP
crates specifically** — `ggen-lsp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` — which are
the consumers of `lsp-max`. Under Option B, expect possible build/symbol
breakage isolated to those crates.

**Scoping note — this does NOT block Phase 1 verification.** The Phase 1 units
in §3 live in `ggen-core`, `ggen-marketplace`, `ggen-graph`, `ggen-cli-lib`, and
the spec tree. **None of these depend on `lsp-max`.** If you only need to verify
Phase 1, Option B is sufficient: run the per-crate gates in §3 with `-p <crate>`
so a broken LSP crate (if any) does not gate your Phase 1 result. Editing
`Cargo.toml` is a build-host action — do not commit the comment-out on a branch
that ships LSP crates.

### 1.4 Confirm the workspace configures

After Option A or B:

```bash
cargo metadata --format-version 1 >/dev/null && echo "workspace configures"
```

Only once this succeeds do the gates in §2 and §3 become meaningful.

---

## 2. GATES — Definition-of-Done commands

These are the real recipes in `justfile`. Run them from the ggen checkout root.

| Recipe | Underlying command (from `justfile`) | Purpose |
|--------|--------------------------------------|---------|
| `just check` | `timeout 300s cargo check --workspace` | Compilation check, all 15 crates |
| `just lint` | `timeout 90s cargo clippy --all-targets -- -D warnings` | Clippy, warnings are errors |
| `just test` | `cargo test --workspace --tests` (30s → escalate 120s) | Integration test gate |
| `just test-lib` | `timeout 30s cargo test --lib --workspace` | Unit/lib tests inside each crate |
| `just slo-check` | `cargo bench --bench cli_startup_performance -- --test` | Performance SLO validation |
| `just pre-commit` | `fmt-check → check → lint → test-lib` | Full pre-commit gate, fail-fast |

> **Drift flag (see §7):** `CLAUDE.md` lists `just test-unit` for "Unit tests
> only" and describes `pre-commit` as `check → lint → test-unit`. **There is no
> `test-unit` recipe in `justfile`.** The actual recipe is **`just test-lib`**,
> and `pre-commit` actually runs `fmt-check check lint test-lib`. Use
> `just test-lib` wherever the brief or CLAUDE.md says `test-unit`.

Full-workspace gate sequence (mirrors the Definition of Done):

```bash
just check && just lint && just test && just slo-check
```

If Option B (§1.3) was used and an LSP crate fails to compile, the
`--workspace` gates above may go red on the LSP crate alone. In that case, gate
Phase 1 with the per-crate commands in §3 and record the LSP failure separately
(it is an Option-B artifact, not a Phase 1 defect).

---

## 3. PER-PATCH VERIFICATION TABLE

One row per Phase 1 unit. **"File status"** distinguishes test files that exist
in this checkout from files that are *patch deliverables* (the patch must create
them; if the command reports "no test target", the patch was not applied or the
file name differs — investigate, do not pass the row).

| # | Unit | Crate | Test command | Expected observable outcome | File status |
|---|------|-------|--------------|-----------------------------|-------------|
| 1 | Digest re-verify | `ggen-cli-lib` | `cargo test -p ggen-cli-lib --test proof_digest_reverify_test` | All tests in the file pass; a re-run of `sync --locked` against an installed pack re-hashes the pack TOML and matches the lockfile `digest`. | **DELIVERABLE** — `crates/ggen-cli/tests/proof_digest_reverify_test.rs` does **not** exist yet. The sabotage half is covered today by `crates/ggen-cli/tests/sabotage_tests.rs::test_sabotage_remove_pack_toml_sync_locked_exits_nonzero` (see §5). |
| 2 | Lockfile invariants | `ggen-core` | `cargo test -p ggen-core lockfile` | Name-filter runs `crates/ggen-core/tests/lockfile_persistence_test.rs` (e.g. `test_lockfile_save_and_load`, `test_lockfile_format_correctness`, `test_lockfile_validation_detects_missing_dependencies`, `test_lockfile_updates_timestamp`) plus any `lockfile` unit tests in `crates/ggen-core/src/packs/lockfile.rs`. All pass; saved lockfile round-trips and `validate()` rejects missing dependencies. | **EXISTS** — `lockfile_persistence_test.rs`, `src/packs/lockfile.rs`. |
| 3 | Marketplace namespace | `ggen-marketplace` | `cargo test -p ggen-marketplace --test namespace_consistency_test` | All tests pass; marketplace identifiers use the canonical public namespace consistently (no private/sentinel namespace leakage). | **DELIVERABLE** — `crates/ggen-marketplace/tests/namespace_consistency_test.rs` does **not** exist yet. Existing marketplace tests are `m2_challenger_tests.rs`, `m2_challenger_stress_tests.rs`, `milestone2_challenger_tests.rs`. |
| 4 | Inverse pipeline coverage | `ggen-core` | `cargo test -p ggen-core --test inverse_pipeline_coverage_test` | All tests pass; coverage exercises each μ⁻¹ stage of the A→O inverse pipeline. | **DELIVERABLE** — `crates/ggen-core/tests/inverse_pipeline_coverage_test.rs` does **not** exist yet. The implementation under test is `crates/ggen-core/src/reverse_sync/inverse_pipeline.rs`; see row 8 for its in-module tests. |
| 5 | Coherence Phase 1 | `ggen-graph` | `cargo test -p ggen-graph coherence` | Name-filter runs the integration file `crates/ggen-graph/tests/post_chatman_coherence_integration.rs` and the in-module tests in `crates/ggen-graph/src/coherence.rs`. Expect: `test_three_pole_full_coherence_all_poles_present` (3 poles, no `Missing` drift, `admitted == true`), `test_coherence_empty_event_log_produces_count_discrepancy` (A→L `CountDiscrepancy`, `admitted == false`), `test_coherence_fingerprints_are_deterministic`, `test_coherence_missing_ontology_pole_not_admitted`, `test_coherence_report_operation_ids_are_unique_across_calls`, `test_coherence_zero_artifacts_with_triples_emits_o_to_a_discrepancy`, `test_coherence_realistic_multi_triple_ontology`. | **EXISTS** — `coherence.rs`, `post_chatman_coherence_integration.rs`. |
| 6 | OCEL pack events | `ggen-graph` | `cargo test -p ggen-graph pack_events` | In-module tests in `crates/ggen-graph/src/ocel/pack_events.rs` pass: `lifecycle_round_trips_through_json` (3 events, 3 objects survive JSON round-trip), `required_ocel_attributes_are_present_and_non_empty` (every event has non-empty `ocel:activity`, `ocel:timestamp`, and an `ocel:object-id`/`ocel:object-type` pair). | **EXISTS** — `src/ocel/pack_events.rs`. |
| 7 | Pack test-drift (migrated) | `ggen-cli-lib` | `cargo test -p ggen-cli-lib --test e2e_pack_workflow_test` | All tests pass: e.g. `test_pack_install_creates_lockfile`, `test_pack_install_tracks_packs`, `test_pack_install_returns_valid_json`, `test_pack_install_fails_on_unknown_pack`, `test_pack_list_shows_installed_packs`, `test_pack_validate_checks_pack`, `test_capability_enable_updates_lockfile`, `test_lockfile_created_after_pack_install`. Pack install produces a lockfile and valid JSON; unknown pack fails. | **EXISTS** — `e2e_pack_workflow_test.rs`. Other migrated pack files to run alongside: `packs_test.rs`, `packs_install_improvement_test.rs`, `pack_cache_test.rs`, `proof_pack_test.rs`, `marketplace_sync_e2e.rs`. |
| 8 | Inverse receipt | `ggen-core` | `cargo test -p ggen-core inverse_pipeline` | In-module tests in `crates/ggen-core/src/reverse_sync/inverse_pipeline.rs` pass: `test_scan_empty_paths_returns_error`, `test_run_real_rust_file_produces_receipt` (receipt has non-empty `operation_id`, non-empty `output_hash`, non-empty `input_hashes`), `test_unknown_extension_skipped_gracefully`, `test_nonexistent_path_skipped_gracefully`, `test_receipt_operation_id_is_unique`, `test_input_hash_is_blake3_hex`. The `InverseReceipt` is a real provenance object (BLAKE3 output hash, real UUID, real timestamp), not a sentinel. | **EXISTS** — `src/reverse_sync/inverse_pipeline.rs`. |
| 9 | Spec hardening | spec tree | `ggen validate .specify/specs/post-chatman/post_chatman.ttl` | SHACL/spec validation exits 0; `post_chatman.ttl` parses and is conformant. Optionally exercise the companion query `list_poles.rq` (a `SELECT ... WHERE { ?pole a pc:Pole ; rdfs:label ?label } ORDER BY ?label`) — it must return the three poles (Ontology/Artifact/EventLog instances) the ontology declares. | **EXISTS** — `post_chatman.ttl`, `list_poles.rq`. |

### Notes on running rows

- Rows whose **File status is DELIVERABLE** will report something like
  `error: no test target named '<name>'` if the patch has not been applied.
  That is the signal the unit is *not yet implemented*, not a pass.
- `cargo test -p <crate> <substr>` (rows 2, 5, 6, 8) is a **name filter**: it
  runs every test whose path/name contains `<substr>`, across both `--lib` and
  `--tests`. Confirm in the output that the *expected* test names (listed above)
  actually appeared and passed; a filter that matches zero tests still exits 0.
- `--test <file>` (rows 1, 3, 4, 7) targets a single integration-test file by
  its stem (no `.rs`).

---

## 4. OTEL VALIDATION

Per `.claude/rules/otel-validation.md`, OTEL span verification is **mandatory
only** for features that cross an external/service boundary: LLM calls, MCP tool
execution, external APIs, DB operations, **the five-stage pipeline stages**, and
quality gates.

### 4.1 Honest applicability for Phase 1

Most Phase 1 units are **pure-type / in-process / spec** work and produce **no
external-boundary spans**. For these, OTEL validation does **not** apply — a
passing Chicago-TDD test on observable state is the proof:

| Unit | Boundary crossed? | OTEL required? |
|------|-------------------|----------------|
| 2 Lockfile invariants | No (filesystem round-trip, in-process) | No |
| 3 Marketplace namespace | No (string/identifier invariants) | No |
| 4 Inverse pipeline coverage | No (in-process transform) | No |
| 5 Coherence Phase 1 | No (BLAKE3 fingerprint + compare, in-process) | No |
| 6 OCEL pack events | No (OCEL log build + JSON round-trip, in-process) | No |
| 8 Inverse receipt | No (BLAKE3 hash + UUID + timestamp, in-process) | No |
| 9 Spec hardening | No (RDF/SHACL validation, in-process) | No |
| 1 Digest re-verify | Only if exercised through a full `ggen sync` | Conditional — see §4.2 |
| 7 Pack test-drift | Only if exercised through a full `ggen sync` | Conditional — see §4.2 |

> Do **not** fabricate spans for the "No" rows. Claiming an OTEL trace for a
> pure in-process unit is the NARRATION failure mode called out in the rule.

### 4.2 When a full `ggen sync` is in the path

If verifying digest re-verify (1) or the pack workflow (7) **end to end through
`ggen sync`** (i.e. the μ₁–μ₅ pipeline actually runs), then the pipeline-stage
spans from `.claude/rules/otel-validation.md` apply. Capture and grep:

```bash
# Enable trace logging (exact vars from the rule + CLAUDE.md OTEL section)
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run the sync-exercising test with output captured
cargo test -p ggen-cli-lib --test e2e_pack_workflow_test -- --nocapture 2>&1 | tee otel_output.txt

# Pipeline-stage spans (required attributes: pipeline.stage, pipeline.duration_ms, pipeline.files_generated)
grep -E "pipeline\.(load|extract|generate|validate|emit)" otel_output.txt

# Quality-gate spans, if a gate fires (required attributes: gate.name, gate.result)
grep -E "quality_gate\.(validate|pass_fail)" otel_output.txt
```

LLM spans (`llm.complete`, `llm.model`, `llm.*_tokens`) are **not** expected for
any Phase 1 unit — none of them call an LLM. The LLM grep procedure in the rule
applies to `cargo test -p ggen-cli-lib --test llm_e2e_test`, which is **out of
scope** for Phase 1. Interpret results with the rule's three buckets:
PROVEN (spans + populated attributes), UNVERIFIED (no spans), OBSERVED (span
present, attributes incomplete).

---

## 5. SABOTAGE / NEGATIVE-PATH TESTS

Consolidated from `.claude/rules/coding-agent-mistakes.md` §5, restricted to the
checks relevant to the Phase 1 patches (lockfile + digest + receipt). These are
negative-path checks: success means the system **fails loudly** (non-zero exit
or `is_valid: false`), not that it proceeds.

### 5.1 Automated sabotage (already in-tree)

`crates/ggen-cli/tests/sabotage_tests.rs` exists and encodes the sabotage
contract as real Chicago-TDD tests (real `ggen` binary via `assert_cmd`, real
`TempDir`, no mocks). Run them:

```bash
cargo test -p ggen-cli-lib --test sabotage_tests
```

Expected: `test_sabotage_remove_pack_toml_sync_locked_exits_nonzero` passes —
a lockfile referencing a pack whose TOML is absent makes `ggen sync --locked`
exit non-zero. This is the digest-re-verify sabotage (unit 1) in executable
form. Confirm in output that the test ran and passed.

### 5.2 Manual sabotage matrix (from §5 of the rule)

Run inside a disposable project dir with a real `.ggen/` and `$GGEN_PACKS_DIR`
set. Each row must produce the required outcome:

| Sabotage | Command | Required outcome |
|----------|---------|------------------|
| Remove pack TOML after install | `rm "$GGEN_PACKS_DIR/acme/base.toml" && ggen sync --locked; echo "exit: $?"` | Exit non-zero; error references digest mismatch or missing pack |
| Corrupt the lockfile | `echo 'garbage' > .ggen/packs.lock && ggen sync --locked; echo "exit: $?"` | Exit non-zero; error references an invalid lockfile |
| Corrupt the latest receipt | `echo '{}' > .ggen/receipts/latest.json && ggen receipt verify .ggen/receipts/latest.json` | Output shows `is_valid: false` |
| Delete the verifying key | `rm .ggen/keys/verifying.key && ggen receipt verify .ggen/receipts/latest.json` | `is_valid: false` or error; must **not** return `is_valid: true` |
| Empty packs dir | `GGEN_PACKS_DIR=/tmp/empty ggen packs add acme/base; echo "exit: $?"` | Exit non-zero; error references "pack not found" |

### 5.3 Invariant spot-checks (from §4 of the rule)

After a real `ggen sync`, confirm the proof objects are not drifting:

```bash
# Lockfile (§4.1): no empty/null digest entries
jq '.packs | to_entries[] | select(.value.integrity == "" or .value.integrity == null)' .ggen/packs.lock
#   → empty output (no offending entries)

# Receipt (§4.2): signature is non-empty
jq -e '.signature | length > 0' .ggen/receipts/latest.json
#   → exit 0

# Receipt (§4.5): a second sync with a changed input yields a different receipt
ggen sync && cp .ggen/receipts/latest.json /tmp/r1.json
touch <some_input_file>
ggen sync && diff /tmp/r1.json .ggen/receipts/latest.json   # must differ
```

> Field names above (`packs`, `integrity`, `signature`) follow what the in-tree
> lockfile JSON and receipt invariants use. The §4 examples in the rule show a
> `packages[].digest` shape; the in-tree lockfile in
> `crates/ggen-cli/tests/sabotage_tests.rs` uses `packs.<id>.integrity`. If `jq`
> returns nothing because a key name differs, inspect the real
> `.ggen/packs.lock` and adjust the filter — do not record a pass on an empty
> match caused by a wrong key. (See §7.)

---

## 6. "DONE" CHECKLIST (mirrors CLAUDE.md Definition of Done)

A Phase 1 verification run is complete only when **every** box is checked.

```
Build unblock
[ ] §1: workspace configures — `cargo metadata` succeeds (Option A or B)
[ ] §1: if Option B used, LSP-crate breakage (if any) recorded separately

Gates (Definition of Done — CLAUDE.md / justfile)
[ ] just check       — no compiler errors (15 crates, or per-crate under Option B)
[ ] just lint        — clippy clean (-D warnings)
[ ] just test        — integration tests pass
[ ] just test-lib    — unit/lib tests pass   (NOT `test-unit`; see §7)
[ ] just slo-check   — performance SLOs met

Per-patch (§3) — each row PASSES with its expected test names observed
[ ] 1 Digest re-verify          (deliverable file present + green; §5.1 sabotage green)
[ ] 2 Lockfile invariants       (ggen-core lockfile)
[ ] 3 Marketplace namespace     (deliverable file present + green)
[ ] 4 Inverse pipeline coverage (deliverable file present + green)
[ ] 5 Coherence Phase 1         (ggen-graph coherence — named tests observed)
[ ] 6 OCEL pack events          (ggen-graph pack_events — named tests observed)
[ ] 7 Pack test-drift           (e2e_pack_workflow_test + migrated files)
[ ] 8 Inverse receipt           (ggen-core inverse_pipeline — named tests observed)
[ ] 9 Spec hardening            (ggen validate post_chatman.ttl exits 0)

OTEL (§4) — only where a boundary is crossed
[ ] Pure in-process / spec units: no spans claimed (correct — do not fabricate)
[ ] If any unit run through full `ggen sync`: pipeline.* spans captured & shown

Sabotage / negative path (§5)
[ ] sabotage_tests pass (§5.1)
[ ] Manual sabotage matrix produces required failures (§5.2)
[ ] Invariant spot-checks clean: no empty digest, non-empty signature, receipts differ (§5.3)

Evidence
[ ] No Andon signals open (no error[E...], no FAILED, no clippy warnings)
[ ] Every DELIVERABLE row confirmed to have a real test target (not "no test target")
```

Per CLAUDE.md and `.claude/rules/andon/signals.md`: a partial pass is a
**failure**. If any gate is red, stop the line and fix the cause before
recording the run as done.

---

## 7. THINGS THIS RUNBOOK COULD NOT CONFIRM (flag list)

Honest gaps. Each is a place where a command may need adjustment in the live
environment; none was guessed silently.

1. **`just test-unit` does not exist.** `CLAUDE.md` (command table line 121, and
   workflow lines 280/282) and the brief use `just test-unit`; the real
   `justfile` recipe is **`just test-lib`** (`cargo test --lib --workspace`), and
   `just pre-commit` runs `fmt-check check lint test-lib`. This runbook uses
   `test-lib`. **CLAUDE.md is stale on this point.**

2. **Three named test files do not exist yet** (they are Phase 1 patch
   deliverables, marked DELIVERABLE in §3):
   - `crates/ggen-cli/tests/proof_digest_reverify_test.rs` (unit 1)
   - `crates/ggen-marketplace/tests/namespace_consistency_test.rs` (unit 3)
   - `crates/ggen-core/tests/inverse_pipeline_coverage_test.rs` (unit 4)
   Their `cargo test --test <name>` commands will error with "no test target"
   until the patches land. The expected test *names inside* these files are
   unknown to this runbook (the files weren't authored yet), so §3 specifies
   the *outcome contract* rather than per-test names for those three rows.

3. **Marketplace namespace specifics unverified.** No existing marketplace test
   references a namespace constant I could cite; existing files are the
   `m2_challenger*` / `milestone2_challenger` set. The exact public-vs-private
   namespace string the unit-3 patch enforces is not yet in-tree, so the §3
   expected outcome is stated at the contract level.

4. **Receipt/lockfile JSON key names differ between the rule and the in-tree
   fixture.** `.claude/rules/coding-agent-mistakes.md` §4 shows
   `packages[].digest`; the real lockfile JSON in
   `crates/ggen-cli/tests/sabotage_tests.rs` uses `packs.<id>.integrity` and
   `ggen_version`. The §5.3 `jq` filters follow the in-tree shape but may still
   need a tweak against a real generated `.ggen/packs.lock`. Verify the actual
   keys before trusting an empty `jq` result.

5. **`ggen validate` / `ggen receipt verify` CLI surface assumed from docs.**
   `CLAUDE.md` documents `ggen validate <ttl>` and
   `ggen receipt verify <path> --public-key ...`; `justfile` documents
   `ggen sync` flags. The exact flag spelling for `receipt verify` (`--public-key`
   vs `--verifying-key`, and the default verifying-key path) was not read from
   the CLI source for this runbook — confirm with `ggen receipt verify --help`
   before running §5.2.

6. **`slo-check` benchmark is unrelated to Phase 1 logic.** `just slo-check`
   runs `cli_startup_performance`; it gates CLI startup, not the Phase 1 units.
   It is included because it is part of the Definition of Done, but a green
   `slo-check` is not evidence about any specific Phase 1 patch.

7. **Option B blast radius not measured.** The claim that commenting out
   `[patch.crates-io]` only risks the LSP crates is taken from the committed
   `Cargo.toml` comment plus the crate dependency map (Phase 1 crates do not
   depend on `lsp-max`). It was not empirically confirmed by a build in this
   container (which cannot build). Re-confirm under §1.4 in the live host.
