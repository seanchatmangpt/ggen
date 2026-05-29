# Additive Finish Plan
## "Find the nearest implemented capability seed and complete it."
## NON-DELETION COMPLETION PROTOCOL — No rewrite, no remove, no clean slate.

---

## Principle

Every finish action below is:
- **additive** (new code added around existing code)
- **smallest patch** (wrapper, alias, adapter, export, test harness)
- **receipted** (every change produces verifiable evidence)

No existing code is removed. If a seed is superseded, it becomes a `LEGACY_NAME` or `DORMANT` artifact — aliased, documented, preserved.

---

## Priority 1: Register genesis-construct8 in workspace

**Why:** genesis-construct8 contains the richest Shard/Corpus/Replay/OCEL/SHACL projection seeds. They are currently invisible to the workspace.

**Action:**
- Add `crates/genesis-construct8` to `[workspace] members`
- Add missing deps (`blake3`, `serde_json`, etc.) to workspace.dependencies
- Run `cargo check -p knhk-construct8`
- Classify all modules: admission→CAPABILITY_SEED, forge→CAPABILITY_SEED, hierarchy→CAPABILITY_SEED

**Connection mechanism:** `ModuleExport`

---

## Priority 2: Register genesis-lockchain as Truex substrate

**Why:** genesis-lockchain has `src/merkle.rs`, `src/storage.rs`, receipt chain logic. This is the nearest seed for the DOC_ONLY Truex lifecycle crate.

**Action:**
- Add `crates/genesis-lockchain` to workspace
- Connect its receipt chain to `genesis-core-v2::Receipt` as the canonical BLAKE3 surface
- Alias `genesis-lockchain::Receipt` → `genesis-core-v2::Receipt` via `pub type` or `GenesisAdapter`
- Add `TruexLedger` wrapper that promotes via BLAKE3 receipt

**Connection mechanism:** `CompatibilityAlias` → `Adapter`

---

## Priority 3: Connect OCEL projection to receipt chain

**Why:** `ggen-graph/src/ocel/projection.rs` is a CAPABILITY_SEED. The OCEL output is not connected to real receipt evidence — it is a `SeaDeath` plague risk.

**Action:**
- Add `Receipt` parameter to `ocel::projection::project_from_receipt(receipt: &Receipt) -> OcelEvent`
- Wire `PlagueRecord::from_refusal()` output to OCEL event type
- Add `tests/ocel_projection.rs` using the existing `ocel_diagnostics_doctor_test.rs` as specification

**Connection mechanism:** `Adapter` (Receipt → OcelEvent)

---

## Priority 4: Wire SHACL violations to PlagueRecord

**Why:** `ggen-core/src/validation/shacl.rs` validates but silently accepts violations. The boundary law requires `Refusal`/`PlagueRecord` on violation.

**Action:**
- Change SHACL validator return to `Result<(), PlagueRecord>`
- Use `Plague::Hail` for invariant violation
- Add `PlagueRecord::from_shacl_violation()` constructor
- Existing test suite (`shacl_validation_test.rs`, `normalization_shacl_tests.rs`) specifies behavior

**Connection mechanism:** `Wrapper` (existing validator → PlagueRecord output)

---

## Priority 5: Register genesis-wasm-shell in workspace

**Why:** genesis-wasm-shell is the AtomVM/WASM custody boundary — the most closed church. It must compile before it can carry Genesis.

**Action:**
- Audit `crates/genesis-wasm-shell/Cargo.toml` for missing workspace deps
- Add to workspace members
- Run `cargo check -p genesis-wasm-shell`
- Classify every `BROKEN_BUT_REAL` import and add minimum fix
- Do not rewrite — wrap what exists

**Connection mechanism:** `TestHarness` → `Wrapper`

---

## Priority 6: Emit ChurchJudgment from ggen-graph doctor

**Why:** `ggen-graph/src/doctor/mod.rs` is PARTIAL. It diagnoses but does not emit the formal `ChurchJudgment` type.

**Action:**
- Add `ChurchJudgment` output to `doctor::diagnose()` return type
- Import `genesis-core::ChurchJudgment` (now available in workspace)
- Connect `ocel_diagnostics_doctor_test.rs` (TEST_ONLY) as the specification

**Connection mechanism:** `ModuleExport` + `Wrapper`

---

## Non-Negotiable Laws for All Finish Work

1. No file deleted.
2. No module removed.
3. No test quarantined without `// STATUS: BROKEN_BUT_REAL — reason: X` comment.
4. Every change produces a receipt: files changed + tests run + results.
5. Every seed classified before touching.
6. Connection mechanism documented alongside the change.
7. Dormant code marked with `// STATUS: DORMANT — preserved per Non-Deletion Protocol`.
