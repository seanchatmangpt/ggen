<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN-TPL-001 — Pre-Implementation Inventory (Phase 0)](#ggen-tpl-001--pre-implementation-inventory-phase-0)
  - [The 7 facts to verify — RESULTS](#the-7-facts-to-verify--results)
  - [Path corrections (mission assumed flat `src/`)](#path-corrections-mission-assumed-flat-src)
  - [Module wiring facts](#module-wiring-facts)
  - [Manifest types (reuse, do not re-parse) — `ggen_core::manifest::types`](#manifest-types-reuse-do-not-re-parse--ggen_coremanifesttypes)
  - [`ggen.toml` rule shape (root, ggen.toml:45-66)](#ggentoml-rule-shape-root-ggentoml45-66)
  - [Example projects available as real fixtures](#example-projects-available-as-real-fixtures)
  - [SHARED API CONTRACT (all agents code to this — prevents integration drift)](#shared-api-contract-all-agents-code-to-this--prevents-integration-drift)
    - [Canonical values for the two species](#canonical-values-for-the-two-species)
    - [Ownership-collision resolutions (decided now)](#ownership-collision-resolutions-decided-now)
  - [Next](#next)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN-TPL-001 — Pre-Implementation Inventory (Phase 0)

**Mission:** GGEN-TPL-001-FIVE-AGENT-WORKSPLIT
**Branch:** `feat/ggen-tpl-001-living-lsp` (off `main`)
**commit_before:** `9a9fb646`
**Date:** 2026-05-29
**Verified by:** orchestrator (direct file reads, not agent-reported)

---

## The 7 facts to verify — RESULTS

| # | Claim | Verdict | Evidence |
|---|---|---|---|
| 1 | `TeraAnalyzer` has `available_vars` | ✅ TRUE | `crates/ggen-lsp/src/analyzers/tera_analyzer.rs:64` — `available_vars: HashSet<String>` marked `#[allow(dead_code)]` (computed in ctor, never read) |
| 2 | `extract_sparql_vars()` exists | ✅ TRUE | `tera_analyzer.rs:77-90` parses `SELECT … WHERE` → `?var` names. **Private** to that file. Also `SparqlAnalyzer::variables()` `sparql_analyzer.rs:175` (all `?`/`$` tokens, private). |
| 3 | `new_from_content(content, sparql_bindings)` exists | ✅ TRUE | `tera_analyzer.rs:68-75` — ctor already takes `sparql_bindings: &str` and computes `available_vars` from it |
| 4 | Construction passes empty bindings / files analyzed in isolation | ✅ TRUE | `analyzers/mod.rs:34` — `TeraAnalyzer::new_from_content(content, "")`. No rule context. **This is the gap.** |
| 5 | `ServerState::observe_diagnostics` observes transitions | ✅ TRUE | `state.rs:127-231` — emits `DiagnosticRaised→RouteSelected→RepairSuggested→RepairApplied→GatePassed→ReceiptEmitted` (BLAKE3 id at `state.rs:209`) into OCEL intel log |
| 6 | `RouteRegistry` maps diagnostics→routes | ✅ TRUE | `route/registry.rs` (`RouteRegistry`, `family_of_code`, `family_of_diagnostic`); entry points `action_route_for` / `route_plan_for_diagnostic` / `select_for_diagnostic` in `route/mod.rs:38-67`. Seeded at `state.rs:103` via `RouteRegistry::seeded()` |
| 7 | Existing tests extendable without moving code | ✅ TRUE | Inline `#[cfg(test)] mod tests` in each analyzer; integration dir `crates/ggen-lsp/tests/` available for `ggen_tpl_001.rs` |

**Conclusion:** the cross-surface template-binding nerve is **dormant, not absent**. MVP = connect existing wires + add a project/rule index.

---

## Path corrections (mission assumed flat `src/`)

| Mission said | Reality |
|---|---|
| `src/tera_analyzer.rs` | `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` |
| `src/mod.rs` | `crates/ggen-lsp/src/analyzers/mod.rs` (the `build_analyzer` seam, line 25/34) |
| `src/route.rs` | `crates/ggen-lsp/src/route/` **directory** (8 files: mod, registry, model, edit, envelope, plan, promoted, compact) → new file = `src/route/diagnostic_species.rs` |
| `src/state.rs` | correct |

## Module wiring facts
- `crates/ggen-lsp/src/lib.rs:1-13` declares modules: `analyzers, check, error, features, handlers, init, intel, pack, protocol, route, server, state, utils`.
- **New modules `project_index` + `rule_index` need declaring in `lib.rs`** → Agent 1's *only* `lib.rs` edit (two `pub mod` lines).
- Agent 3's `diagnostic_species` lives **under `route/`**, declared in `route/mod.rs` (Agent 3-owned) → **no `lib.rs` collision**.

## Manifest types (reuse, do not re-parse) — `ggen_core::manifest::types`
- `GgenManifest { project, ontology, inference, generation, validation }`
- `GenerationConfig { rules: Vec<GenerationRule>, output_dir, … }`
- `GenerationRule { name, query: QuerySource, template: TemplateSource, output_file: String, mode, when, skip_empty }`
- `QuerySource` (untagged): `File { file: PathBuf }` | `Inline { inline: String }`
- `TemplateSource` (untagged): `File { file }` | `Inline { inline }` | `Git {…}` | `Package {…}`
- Parser: `crates/ggen-core/src/manifest/parser.rs` (Agent 1 reads for the exact parse entry point).

## `ggen.toml` rule shape (root, ggen.toml:45-66)
`[[generation.rules]]` → `query = { inline = "…" }` **or** `{ file = "…" }`; `template = { file = "…" }`; `output_file = "…"`; `mode`. Example rule SELECTs `?class ?label ?comment`.

## Example projects available as real fixtures
`examples/rust-structs/ggen.toml`, `examples/rest-api-advanced/`, `examples/grpc-service/`, `examples/workspace-project/`, `examples/llm-full-integration/`, … (Agent 4 may mine these for realistic fixtures, but should build hermetic TempDir fixtures it fully controls).

---

## SHARED API CONTRACT (all agents code to this — prevents integration drift)

```rust
// ── Agent 1: crate::rule_index / crate::project_index ──────────────
pub struct RuleIndexEntry {
    pub rule_id: String,
    pub manifest_path: std::path::PathBuf,
    pub query_inline: bool,                 // true=inline, false=file
    pub query_content: String,              // resolved SPARQL text
    pub template_path: Option<std::path::PathBuf>, // None if inline/non-file source
    pub template_content: Option<String>,   // resolved Tera text; None if file missing
    pub output_file: String,
    pub selected_vars: std::collections::BTreeSet<String>, // SELECT vars, no '?'
    pub issues: Vec<String>,                // e.g. "template file missing: …" (index-level, NOT GGEN-TPL-001)
}
pub struct ProjectIndex {
    pub root: std::path::PathBuf,
    pub rule_entries: Vec<RuleIndexEntry>,
}
impl ProjectIndex {
    pub fn from_root(root: &std::path::Path) -> Result<ProjectIndex, /*structured error*/ _>;
}

// ── Agent 2: crate::analyzers::tera_analyzer + crate::analyzers (mod.rs) ──
pub const GGEN_TPL_001: &str = "GGEN-TPL-001"; // canonical code (Agents 2,3,4 use this literal)
/// Pure detector: which template-consumed vars are NOT produced by the query.
pub fn unbound_projection_diagnostics(
    template: &str,
    available_vars: &std::collections::BTreeSet<String>,
) -> Vec<tower_lsp::lsp_types::Diagnostic>; // each: code=GGEN-TPL-001, severity=ERROR
/// Project-level cross-surface entry (what Agent 4 tests call):
pub fn detect_tpl_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<tower_lsp::lsp_types::Diagnostic>)>;
// build_analyzer stays back-compatible (single-file path still passes ""), so
// server.rs/check.rs callers do NOT break. Live-server trigger wiring → handoff note.

// ── Agent 3: crate::route::diagnostic_species ──────────────────────
pub struct DiagnosticSpecies {
    pub code: &'static str,
    pub failure_class: &'static str,
    pub surfaces: &'static [&'static str],
    pub severity_policy: &'static str,   // "error" | "warning" | "release_blocking" | "advisory"
    pub route: &'static str,             // "source_law_repair" | "proof_topology_repair"
    pub origin: &'static str,
    pub actuation_boundary: &'static str,// "inspect_only" | …
    pub receipt_requirement: &'static str,// "diagnostic_receipt" | "boundary_receipt" | "none"
    pub detector_active: bool,           // GGEN-TPL-001 = true; GGEN-HARNESS-001 = false (Phase 2)
}
pub fn species_registry() -> &'static [DiagnosticSpecies]; // GGEN-TPL-001 + GGEN-HARNESS-001
// Also: register a source-law route for code "GGEN-TPL-001" in RouteRegistry::seeded()
// so select_for_diagnostic() returns it; route must NEVER target emitted output.
```

### Canonical values for the two species
- **GGEN-TPL-001:** failure_class `unbound_projection`; surfaces `[ggen.toml, SPARQL, Tera]`; severity `error`; route `source_law_repair`; origin `ark-covenant / living-lsp MVP`; actuation_boundary `inspect_only`; receipt_requirement `diagnostic_receipt`; **detector_active = true**.
- **GGEN-HARNESS-001:** failure_class `harness_mismatch`; surfaces `[Cargo.toml, tests/proof, Makefile.toml]`; severity `release_blocking`; route `proof_topology_repair`; origin `ark-covenant ProofPack runner failure`; actuation_boundary `inspect_only`; receipt_requirement `boundary_receipt`; **detector_active = false (Phase 2, not implemented)**.

### Ownership-collision resolutions (decided now)
1. `lib.rs` touched **only** by Agent 1 (two `pub mod` lines). All others use `crate::…` paths without editing `lib.rs`.
2. SELECT-var extraction: Agent 1 writes its **own** minimal extractor (Agent 2's `extract_sparql_vars` is private; no cross-edit). Consolidation deferred to Phase 1.5 (known gap).
3. Template content resolution belongs to **Agent 1** (index does the I/O). Agent 2's detector is a **pure function** over `(template_content, selected_vars)` — never reads files. "Missing template" → Agent 1 `issues`, not GGEN-TPL-001.
4. Concurrent compile may be RED until all of Agents 1–3 land; Agent 4 expects RED; Agent 5 (coordinator) reconciles and runs the gates. This is expected, not a failure.

---

## Next
Launch 5 concurrent agents (disjoint file surfaces per the table in the approved plan). Orchestrator wires any server.rs/check.rs live-trigger seam flagged in Agent 2's handoff after merge, then runs `cargo make check/lint/test`.
