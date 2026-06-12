# GALL-CHECKPOINT-002 — Pre-Implementation Inventory

**Mission:** Activate the GGEN-HARNESS-001 diagnostic species from Phase-2 metadata-only to a LIVING diagnostic, mirroring the proven GGEN-TPL-001 living loop. ggen LOCAL law (diagnostic species + route + living loop). NOT process mining.

**Status:** READY (not BLOCKED). The one scoping ambiguity surfaced by discovery (Makefile task-reference noise) is resolved below; no actuation beyond `inspect_only` is required; harness_mismatch is fully resolvable from Cargo.toml manifest declarations vs. on-disk reality.

**Verified against real code** (read 2026-05-30):
- `crates/ggen-lsp/src/route/diagnostic_species.rs` (species table L50-76; unit tests L94-148)
- `crates/ggen-lsp/src/state.rs` (`FileType::from_path` L31-43; `analyze_and_observe` L314-384; `detect_tpl_001_for` L409-417; `project_root_for` L423-440; `tpl_clears_for` L132-141; `tpl_flagged` L88; `observe_diagnostics` L156-260 keys generically on `diag_code`)
- `crates/ggen-lsp/src/analyzers/mod.rs` (`detect_tpl_001` L32-46; `build_analyzer` L54-71; `FileType::Toml` arm)
- `crates/ggen-lsp/src/check.rs` (`check_files_in_root` L321-378; `fold_tpl_001` L394-443 returns ERROR count; `discover_law_surfaces` L511-529 filters `FileType != Unknown` → Cargo.toml/Makefile.toml ARE discovered)
- `crates/ggen-lsp/src/route/registry.rs` (`family_of_code` L120-138; `seed_routes` L142-240 — seeds only ConfigValue/ParseFailure/TemplateFailure/DanglingReference)
- `crates/ggen-lsp/src/route/model.rs` (`RepairFamily` L22-41 — **`AdmissionFailure` L34 is UNSEEDED**; `EditTemplate::NoOp`; `Provenance::Seeded`)
- `crates/ggen-lsp/src/project_index.rs` (`ProjectIndex::from_root` L70-96 — reads `ggen.toml` only; ManifestNotFound when absent)
- `crates/ggen-lsp/tests/ggen_tpl_001_regression.rs` (`harness_001_remains_metadata_only` L160-168; `invalid_fixture_emits_only_tpl_001` L217-244 with forbidden list including `GGEN-HARNESS-001` L236)

---

## 1. THE RELATION (harness_mismatch) — exact detection predicate

**Authoritative side = Cargo.toml `[[test]]`/`[[bench]]` tables carrying an explicit `path = "..."`.**

> A `harness_mismatch` (GGEN-HARNESS-001, ERROR/release_blocking) is raised IFF a `[[test]]` or `[[bench]]` table declares an explicit `path = P` and the file `<manifest_dir>/P` does NOT exist on disk.

- The diagnostic anchors on the offending manifest's `path = "..."` line (the **producer/declaration surface**).
- The proof file on disk (`tests/proof/*.rs`, `tests/*.rs`, `benches/*.rs`) is the **consumer/expected-artifact surface** — `FileType::Unknown`, so it does NOT trigger the seam. Detection fires on the **declaration edit** (the `.toml`), exactly as the mission predicted.
- **Source-law repair only:** fix the `path`/remove the dead table OR create/rename the source file. NEVER fabricate a passing proof; NEVER point a declaration at an emitted/generated output. (cf. real commit `47656dbf` "replace non-existent benchmark targets".)

### Scoping decision (resolves the one discovery ambiguity — NOT blocking)
**EXCLUDE Makefile.toml `cargo test/bench --test/--bench NAME` shell strings from the predicate.** The live clean tree references targets that resolve to nothing yet ship green because the tasks are guarded (`--no-run 2>/dev/null` probes, `|| echo skipped`): `security_tests`, `rdf_validation_integration`, bench `slo_validation`, `yawl_generation_slo`. Treating Makefile strings as declared targets would FALSE-POSITIVE on the clean tree (FAKE-LIVE / wrong gate). Makefile.toml stays in the species `surfaces` list (legitimate repair surface; an edit there re-triggers the Toml seam), but the **mismatch predicate is defined over Cargo.toml manifest declarations vs. on-disk files only.** Makefile drift = a separate future species.

**Implementer choice (both keep the live tree clean):** the minimal-safe predicate is "explicit `path =` only". Validating path-less `[[bench]]` against the convention `benches/<name>.rs` is optional; all current convention benches resolve, so either way the live tree is CLEAN. **Use explicit-`path`-only for the initial implementation.**

---

## 2. THE FILETYPE DECISION (load-bearing)

`FileType::from_path` (state.rs:38) classifies `ggen.toml`, `Cargo.toml`, AND `Makefile.toml` all as `FileType::Toml` (the `|| path.ends_with(".toml")` arm). **Do NOT add a new FileType variant** — that forces a match-arm change at every `DocumentAnalyzer`/`build_analyzer` dispatch site and risks TPL regression.

**Decision: route by BASENAME inside the existing `Toml` branch.** Two private predicates in state.rs (mirror in check.rs):
- `is_ggen_manifest(path) = path.ends_with("ggen.toml")` → TPL-001 path.
- `is_harness_surface(path) = path.ends_with("Cargo.toml") || path.ends_with("Makefile.toml")` → HARNESS-001 path.

Self-selection is clean because:
- TPL-001's `detect_tpl_001_for` calls `ProjectIndex::from_root` which requires `ggen.toml` (project_index.rs:71-76 → `ManifestNotFound` otherwise). A `Cargo.toml`-rooted dir without `ggen.toml` already yields `Vec::new()`. So **tightening the TPL `Toml` trigger to `ggen.toml`-only cannot regress TPL** (TPL only ever fired meaningfully on `ggen.toml`).
- HARNESS detection no-ops on `ggen.toml`. The two never overlap on the same basename → no leak in either direction.

---

## 3. FILE-BY-FILE DIFF PLAN (implementer follows verbatim)

### NEW `crates/ggen-lsp/src/analyzers/harness_analyzer.rs` (pure detector — mirror tera_analyzer pure-fn)
```rust
use std::collections::BTreeSet;
use std::path::PathBuf;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

pub const GGEN_HARNESS_001: &str = "GGEN-HARNESS-001";

/// A declared proof/test/bench target from a harness manifest (Cargo.toml).
#[derive(Debug, Clone)]
pub struct DeclaredTarget {
    pub name: String,
    pub path: PathBuf,        // resolved absolute path the declaration points at
    pub manifest: PathBuf,    // the Cargo.toml the table lives in (squiggle surface)
    pub line: u32,            // 0-based line of the `path = "..."` entry
}

/// PURE detector: for each declared target whose resolved `path` is NOT in
/// `existing_files`, emit a GGEN-HARNESS-001 ERROR anchored on its declaration
/// line. Reads NO files (paths pre-resolved by HarnessIndex — mirrors
/// detect_tpl_001's no-I/O contract).
#[must_use]
pub fn harness_mismatch_diagnostics(
    targets: &[DeclaredTarget],
    existing_files: &BTreeSet<PathBuf>,
) -> Vec<Diagnostic> {
    targets.iter()
        .filter(|t| !existing_files.contains(&t.path))
        .map(|t| Diagnostic {
            range: Range {
                start: Position { line: t.line, character: 0 },
                end:   Position { line: t.line, character: 0 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String(GGEN_HARNESS_001.to_string())),
            source: Some("ggen-lsp".to_string()),
            message: format!(
                "harness mismatch: declared target `{}` points at `{}` which does not exist \
                 on disk. Fix the Cargo.toml [[test]]/[[bench]] path or create/rename the proof \
                 file (source law). Never fabricate a passing proof.",
                t.name, t.path.display()
            ),
            ..Default::default()
        })
        .collect()
}
```
Co-located unit tests: declared-but-missing → 1 diag at ERROR; declared-and-present → 0 diags; multiple mismatches → N diags; empty inputs → empty.

### NEW `crates/ggen-lsp/src/harness_index.rs` (I/O boundary — mirror project_index.rs)
```rust
pub struct HarnessIndex {
    pub root: PathBuf,
    pub targets: Vec<DeclaredTarget>,         // explicit-path [[test]]/[[bench]] tables
    pub existing_files: BTreeSet<PathBuf>,     // tests/**/*.rs + benches/**/*.rs on disk
}
impl HarnessIndex {
    /// Parse `<root>/Cargo.toml` [[test]]/[[bench]] tables with an explicit
    /// `path`, resolve each relative to `root`, capture the declaration line.
    /// Enumerate existing tests/**/*.rs + benches/**/*.rs. Best-effort: a missing
    /// Cargo.toml yields an empty index (Ok with no targets) — never panics.
    pub fn from_root(root: &Path) -> Result<HarnessIndex, IndexError> { ... }
}
```
Implementation notes:
- Parse Cargo.toml with the `toml` crate already in the dep tree (used by `ggen_core::manifest`). Deserialize `[[test]]`/`[[bench]]` arrays; keep only entries with an explicit `path`.
- For the **declaration line**: `toml` (serde) does not give spans. Do a second cheap pass — read the raw Cargo.toml text and find the line of each kept entry's `path = "<P>"` literal (search for the literal `path` string within the table region, or fall back to the table-header line). This keeps detection pure-of-child-LSP and avoids a new dep. Best-effort line (0 if not found) is acceptable; the squiggle still lands on the manifest.
- Resolve `path` relative to `root`; canonicalize is not required (use `existing_files` built from the same `root` walk so comparison is consistent). Recommended: store both `targets[i].path` and `existing_files` as `root.join(rel)` non-canonicalized, OR canonicalize both — be consistent.
- `existing_files`: `WalkDir` over `root/tests` and `root/benches` collecting `*.rs` (skip on missing dir).
- Reuse `IndexError` from `project_index` or define a local minimal error; do NOT panic.

### EDIT `crates/ggen-lsp/src/analyzers/mod.rs`
Add after the TPL block:
```rust
pub mod harness_analyzer;
pub use harness_analyzer::{harness_mismatch_diagnostics, DeclaredTarget, GGEN_HARNESS_001};

/// Cross-surface GGEN-HARNESS-001 detection over a harness index. Groups
/// diagnostics by the manifest path (the squiggle surface). Mirror detect_tpl_001's
/// Vec<(PathBuf, Vec<Diagnostic>)> shape. Reads no files (HarnessIndex did the I/O).
#[must_use]
pub fn detect_harness_001(
    index: &crate::harness_index::HarnessIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let diags = harness_mismatch_diagnostics(&index.targets, &index.existing_files);
    if diags.is_empty() { return Vec::new(); }
    // All diagnostics anchor on the same Cargo.toml manifest in this index.
    vec![(index.root.join("Cargo.toml"), diags)]
}
```

### EDIT `crates/ggen-lsp/src/lib.rs`
Add `pub mod harness_index;` (mirror `pub mod project_index;`). Confirm `analyzers` re-export pattern so tests can `use ggen_lsp::analyzers::detect_harness_001`.

### EDIT `crates/ggen-lsp/src/route/registry.rs`
1. `family_of_code` (L120-138): add arm
```rust
"GGEN-HARNESS-001" => Some(RepairFamily::AdmissionFailure),
```
`AdmissionFailure` is currently UNSEEDED (model.rs:34) → HARNESS owns it exclusively, exactly as TPL-001 owns `DanglingReference`. `select_for_diagnostic` keys only on family → zero cross-contamination.

2. `seed_routes()` (L142-240): add to the returned vec
```rust
RepairRoute {
    id: RouteId("proof-topology.repair".into()),
    family: RepairFamily::AdmissionFailure,
    steps: PartialOrder {
        nodes: vec![
            RepairStep { id: StepId("fix-cargo-toml-declaration".into()),
                title: "Correct the Cargo.toml [[test]]/[[bench]] declaration: align the `path` \
                        to the real proof file, or remove the non-existent target (source law)".into(),
                edit: EditTemplate::NoOp },
            RepairStep { id: StepId("fix-makefile-toml-reference".into()),
                title: "Correct the Makefile.toml proof/test task target reference to name only \
                        existing targets (source law)".into(),
                edit: EditTemplate::NoOp },
            RepairStep { id: StepId("inspect-proof-file-path".into()),
                title: "Inspect/create the missing proof file at tests or tests/proof so the \
                        declared path resolves (source law)".into(),
                edit: EditTemplate::NoOp },
        ],
        edges: vec![],  // three independent source-law surfaces, concurrent
    },
    description: "Harness mismatch — reconcile the declared proof/test topology (Cargo.toml \
                  [[test]]/[[bench]], Makefile.toml task targets) with the proof files on disk. \
                  Repair the declaration or the file path; NEVER fabricate or force a passing \
                  proof, NEVER target an emitted or generated artifact. Advisory only (inspect_only).".into(),
    provenance: Provenance::Seeded,
    priority: 10,
},
```
**Source-law-only invariant:** all `NoOp`; no step title contains an emitted-output marker (`out/`,`output/`,`dist/`,`gen/`,`emitted`) or a fabrication phrase (`fabricate`,`force`,`make the proof pass`,`pass the test`,`stub`); every step title references `Cargo.toml`/`Makefile.toml`/`tests`.

3. Co-located route tests (in registry.rs `tests` mod):
```rust
#[test] fn ggen_harness_001_maps_to_its_own_family() {
    assert_eq!(family_of_code("GGEN-HARNESS-001"), Some(RepairFamily::AdmissionFailure)); }
#[test] fn ggen_harness_001_selects_the_proof_topology_route() {
    let reg = RouteRegistry::seeded();
    let r = reg.select_for_diagnostic(&diag("GGEN-HARNESS-001","harness mismatch")).expect("route");
    assert_eq!(r.id.0, "proof-topology.repair");
    assert_eq!(r.provenance, Provenance::Seeded);
    assert!(r.steps.is_sound()); }
#[test] fn ggen_harness_001_route_is_source_law_only() { /* NoOp + forbidden-token scan, mirror L424 */ }
#[test] fn ggen_harness_001_does_not_contaminate_tpl_001() {
    let reg = RouteRegistry::seeded();
    let r = reg.select_for_diagnostic(&diag("GGEN-TPL-001","unbound projection")).expect("route");
    assert_eq!(r.id.0, "source-law.bind-projection"); }
```

### EDIT `crates/ggen-lsp/src/route/diagnostic_species.rs`
- L74: `detector_active: false` → `true`.
- L110-125 test `ggen_harness_001_is_metadata_only` → rename `ggen_harness_001_is_active`; flip assertion to `assert!(species.detector_active, "GGEN-HARNESS-001 detector must be active")`. Keep all other field assertions unchanged.
- `registry_contains_exactly_two_species` (L134) — UNCHANGED (stays 2).
- `actuation_boundary_is_inspect_only_for_all_species` (L139) — UNCHANGED (HARNESS stays inspect_only).

### EDIT `crates/ggen-lsp/src/state.rs` (the live seam)
1. Add private predicates near `FileType` (free fns or assoc):
```rust
fn is_ggen_manifest(path: &str) -> bool { path.ends_with("ggen.toml") }
fn is_harness_surface(path: &str) -> bool {
    path.ends_with("Cargo.toml") || path.ends_with("Makefile.toml") }
```
2. Add `harness_flagged: Arc<Mutex<HashSet<Url>>>` field (mirror `tpl_flagged` L88); init in `with_root` (L117).
3. Add `detect_harness_001_for` + `harness_root_for` (mirror `detect_tpl_001_for` L409 / `project_root_for` L423):
```rust
fn detect_harness_001_for(&self, uri: &Url) -> Vec<(PathBuf, Vec<Diagnostic>)> {
    let Some(root) = self.harness_root_for(uri) else { return Vec::new(); };
    match crate::harness_index::HarnessIndex::from_root(&root) {
        Ok(index) => crate::analyzers::detect_harness_001(&index),
        Err(_) => Vec::new(),
    }
}
fn harness_root_for(&self, uri: &Url) -> Option<PathBuf> {
    // walk up to nearest dir containing Cargo.toml; fallback self.root if it has one
}
```
4. Add `harness_clears_for` (mirror `tpl_clears_for` L132) over `harness_flagged`.
5. In `analyze_and_observe` (L329): tighten the TPL branch and add the HARNESS branch:
```rust
let tpl_groups = if is_ggen_manifest(uri.path())
    || matches!(file_type, FileType::Tera | FileType::Sparql) {
    self.detect_tpl_001_for(uri)
} else { Vec::new() };

let harness_groups = if is_harness_surface(uri.path()) {
    self.detect_harness_001_for(uri)
} else { Vec::new() };
```
Then run the SAME merge-once/publish-once + `observe_diagnostics` loop for `harness_groups` that the existing code runs for `tpl_groups` (the edited Cargo.toml IS its own surface → merge its TomlAnalyzer single-file diags once, publish once). Add a HARNESS stale-clear block mirroring L372-381 using `harness_clears_for` + `residual_single_file_diags` (NOT a blunt empty publish — residual preservation).
**Critical:** `observe_diagnostics` (L156) needs NO change — it keys generically on `diag_code` and `select_for_diagnostic` returns the HARNESS route once `family_of_code` is wired, so the 6-link chain flows automatically.

### EDIT `crates/ggen-lsp/src/check.rs` (headless gate fold)
Add `fold_harness_001(root, &mut files, registry) -> usize` mirroring `fold_tpl_001` (L394): build `HarnessIndex::from_root(root)`, run `detect_harness_001`, append diags to the matching `Cargo.toml` `FileReport` (or push new), resolve routes when `registry.is_some()`, return the ERROR count. Then in `check_files_in_root` after L368:
```rust
error_count += fold_harness_001(root, &mut files, registry.as_ref());
```
`discover_law_surfaces` (L511, filters `FileType != Unknown`) already includes `Cargo.toml`/`Makefile.toml` (both `.toml`) → they reach the gate. Every HARNESS ERROR bumps `error_count` → trips `has_errors()`/`exit_code()` (release_blocking).

---

## 4. SINGLE-WRITER OWNERSHIP

**ONE integration writer owns ALL of these files** (src + tests together, to avoid a compile race — new symbols and the tests that import them must land in one coherent commit-window):
- NEW: `analyzers/harness_analyzer.rs`, `harness_index.rs`
- EDIT: `analyzers/mod.rs`, `lib.rs`, `route/registry.rs`, `route/diagnostic_species.rs`, `state.rs`, `check.rs`
- NEW tests: `tests/ggen_harness_001_living_loop.rs`, `tests/fixtures/ggen_harness_001_living_loop/**`
- EDIT test: `tests/ggen_tpl_001_regression.rs` (the barrier flip)
- This receipt: `docs/receipts/GALL_CHECKPOINT_002_PRE_INVENTORY.md` (already written)

No two agents write any of these. NOTHING outside `crates/ggen-lsp/` except this receipt. Do NOT `git commit` (conductor handles the PR).

---

## 5. RED PROOF DESIGN

### Fixtures (under `crates/ggen-lsp/tests/fixtures/ggen_harness_001_living_loop/`)
- `invalid_project/Cargo.toml`: a minimal package manifest with
  `[[test]]\nname = "proof"\npath = "tests/proof/nonexistent.rs"` and NO such file → mismatch. Include a real existing `tests/<something>.rs` so the dir exists. **No `ggen.toml`** in this fixture (HARNESS fixture must raise ZERO TPL).
- `valid_project/Cargo.toml`: same `[[test]]` but the declared `path` file EXISTS → clean.

### Headless gate test (`ggen_harness_001_living_loop.rs`)
1. **Live raise:** `check_files_in_root(invalid_root, &surfaces, true)` → a GGEN-HARNESS-001 ERROR present; `report.error_count >= 1`; `report.exit_code() != 0`.
2. **Live clear:** create the missing proof file (or fix the path) → re-run → no GGEN-HARNESS-001; `error_count == 0`.
3. **Route source-law-only:** for a HARNESS diag, `select_for_diagnostic` → `proof-topology.repair`, `Provenance::Seeded`, all steps `NoOp`, no emitted-output/fabrication tokens, every step references a harness source-law surface.
4. **No artifact:** assert analysis writes nothing under the temp root except the OCEL log path it is supposed to write (see 5).

### Living-loop 6-link test (via `analyze_and_observe`)
- Build `ServerState::with_root(temp_invalid_project)`; URI = the fixture `Cargo.toml`.
- `analyze_and_observe(&cargo_uri, &cargo_contents_with_dangling_path)` → RAISE.
- Then make the declaration lawful (write the missing proof file OR rewrite Cargo.toml `path` to the existing file) and `analyze_and_observe` again → CLEAR.
- Read the EXTERNAL on-disk OCEL log `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` and assert ALL SIX activities appear for a line naming `Cargo.toml` + `GGEN-HARNESS-001`: `DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted` (`receipt_requirement = boundary_receipt`). Read from DISK, never an in-process bool.

### Regression / barrier flips
- `tests/ggen_tpl_001_regression.rs` L160-168 `harness_001_remains_metadata_only` → flip to `harness_001_is_active` asserting `species.detector_active == true`.
- `invalid_fixture_emits_only_tpl_001` (L217-244, forbidden list incl. `GGEN-HARNESS-001` L236) — **KEEP UNCHANGED**: a TPL fixture (only `ggen.toml`, no Cargo/Makefile mismatch) must still raise ZERO HARNESS (no-leak barrier).
- ADD symmetric barrier in the HARNESS test: a HARNESS fixture (Cargo.toml mismatch, no `ggen.toml`) raises ZERO GGEN-TPL-001.

---

## 6. ALIVE / FAKE-LIVE / BLOCKED GATES (concrete)

**ALIVE (accept):**
- Editing a harness declaration surface (`Cargo.toml`/`Makefile.toml`) raises GGEN-HARNESS-001 through `analyze_and_observe`.
- Headless gate (`check_files_in_root`) FAILS (`error_count >= 1`, nonzero exit) on a real mismatch and PASSES when repaired.
- Route is source-law-only (never references a generated/emitted target, never a "make the proof pass" step); selected via the seeded `proof-topology.repair` on the exclusively-owned `AdmissionFailure` family.
- Full 6-link chain (`DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted`, `boundary_receipt`) proven by reading the EXTERNAL on-disk OCEL log.
- `detector_active == true`; analysis writes no artifact other than the OCEL log.
- TPL-001 does NOT regress; HARNESS does NOT leak into TPL gates and vice versa.
- `cargo make check` + `cargo make test` green; `cargo clippy -p ggen-lsp --no-deps -- -D warnings` == 0 (no `#[allow]` dodges).

**FAKE-LIVE (reject):**
- Detector flipped to `true` but no real cross-surface detection (e.g. detection reads an in-process bool, not Cargo.toml-vs-disk).
- Clear done via blunt empty-publish (no residual single-file diags preserved).
- 6-link chain incomplete, or read from an in-process bool instead of disk.
- Route references a generated/emitted target or a "make the proof pass" / fabricate step.
- Makefile shell-string references folded into the predicate → false positives on the clean tree.

**BLOCKED (none surfaced — would be):**
- harness_mismatch unresolvable from Cargo/Makefile reality (NOT the case — explicit-path vs disk is exact).
- Activation needing actuation beyond `inspect_only` (NOT the case — diagnostic advises; agent/human applies).
- A forbidden/foreign surface needing patching (NOT the case — all edits inside `crates/ggen-lsp/`).
- Unavoidable TPL-001 regression/leakage (NOT the case — basename self-selection + ggen.toml-only tightening proven safe).

**Checkpoint is READY. No blocking ambiguity. The one scoping risk (Makefile noise) is resolved by pinning the predicate to Cargo.toml explicit-`path` declarations.**
