<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN-LSP Living Project Graph](#ggen-lsp-living-project-graph)
  - [1. What changed conceptually](#1-what-changed-conceptually)
  - [2. The project-graph components (real files)](#2-the-project-graph-components-real-files)
    - [2.1 RuleIndex — `crates/ggen-lsp/src/rule_index.rs`](#21-ruleindex--cratesggen-lspsrcrule_indexrs)
    - [2.2 ProjectIndex — `crates/ggen-lsp/src/project_index.rs`](#22-projectindex--cratesggen-lspsrcproject_indexrs)
    - [2.3 The detector — `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` + `analyzers/mod.rs`](#23-the-detector--cratesggen-lspsrcanalyzerstera_analyzerrs--analyzersmodrs)
    - [2.4 DiagnosticSpecies registry — `crates/ggen-lsp/src/route/diagnostic_species.rs`](#24-diagnosticspecies-registry--cratesggen-lspsrcroutediagnostic_speciesrs)
  - [3. How GGEN-TPL-001 enters the existing observe→route→receipt loop](#3-how-ggen-tpl-001-enters-the-existing-observe%E2%86%92route%E2%86%92receipt-loop)
  - [4. Native-first / child-LSP-deferred decision](#4-native-first--child-lsp-deferred-decision)
  - [5. The deferred live-wiring seam (next lawful action)](#5-the-deferred-live-wiring-seam-next-lawful-action)
  - [6. Species roadmap](#6-species-roadmap)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN-LSP Living Project Graph

**Mission:** GGEN-TPL-001-FIVE-AGENT-WORKSPLIT
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Date:** 2026-05-29
**Status:** Phase 1 landed (GGEN-TPL-001 active); live-trigger wiring deferred to next lawful action.

> The product is CodeManufactory; RevOps is merely proof that CodeManufactory works.

This document records the architecture as it actually exists on disk after Agents 1–4 landed. Every claim below cites a real file path that was read during the coordination audit. It is not a sketch — where the original mission sketch diverged from the real code, the real code wins and is noted.

---

## 1. What changed conceptually

The `ggen-lsp` analyzers already understood single documents in isolation: a `.tera` file was linted for Tera syntax, a `.rq` file for SPARQL, each on its own surface. The cross-surface "nerve" — knowing that a Tera template consumes a variable that the rule's SPARQL `SELECT` does not produce — existed only as dormant wiring (`available_vars` was computed in the `TeraAnalyzer` constructor and then never read; see `crates/ggen-lsp/src/analyzers/tera_analyzer.rs`, the field was `#[allow(dead_code)]` before Agent 2).

GGEN-TPL-001 lifts the living loop **from intra-document to project-graph**. The unit of analysis is no longer a file; it is a `ggen.toml` project whose `[[generation.rules]]` bind a SPARQL query surface to a Tera template surface to an output-file surface. The diagnostic `GGEN-TPL-001 unbound_projection` fires when a template consumes a variable the bound query's projection cannot supply.

The defining constraint: **analysis is read-only**. The project graph is built by reading `ggen.toml`, `.rq`, and `.tera` from disk; no emitted output file is ever materialized. This is proven by the integration test `analysis_never_materializes_output_file` (`crates/ggen-lsp/tests/ggen_tpl_001.rs:157`), which copies a valid project into a `TempDir`, runs the full index + detector, and asserts the declared `output_file` (`out.txt`) never appears on disk.

---

## 2. The project-graph components (real files)

### 2.1 RuleIndex — `crates/ggen-lsp/src/rule_index.rs`

`RuleIndexEntry` (one per `[[generation.rules]]` rule) resolves a single rule into the three source-law surfaces, performing all file I/O here so the detector downstream stays pure:

```rust
pub struct RuleIndexEntry {
    pub rule_id: String,
    pub manifest_path: std::path::PathBuf,
    pub query_inline: bool,
    pub query_content: String,
    pub template_path: Option<std::path::PathBuf>,
    pub template_content: Option<String>,
    pub output_file: String,
    pub selected_vars: std::collections::BTreeSet<String>, // SELECT vars, no leading '?'
    pub issues: Vec<String>,                                // non-fatal index-level problems
}
impl RuleIndexEntry {
    pub fn from_rule(rule: &ggen_core::manifest::GenerationRule, manifest_path: &std::path::Path) -> RuleIndexEntry;
}
```

`from_rule` never fails: missing query/template files, `SELECT *`, and unsupported Git/Package template sources are collected into `issues`, not raised as errors. File reads use only `std::fs::read_to_string` (`rule_index.rs:69`, `:88`) — reads, never writes. SELECT-variable extraction is a lexical scanner (case-insensitive `SELECT … WHERE`, strips `?`, handles `DISTINCT`, treats `SELECT *` as "all vars unknown" + an info issue).

### 2.2 ProjectIndex — `crates/ggen-lsp/src/project_index.rs`

```rust
pub struct ProjectIndex {
    pub root: std::path::PathBuf,
    pub rule_entries: Vec<RuleIndexEntry>,   // manifest order
}
impl ProjectIndex {
    pub fn from_root(root: &std::path::Path) -> Result<ProjectIndex, IndexError>;
}
pub enum IndexError { ManifestNotFound { .. }, ManifestParse { .. } }
```

`from_root` discovers `<root>/ggen.toml`, parses it via `ggen_core::manifest::ManifestParser::parse` (the real entry point — there is no `parse_manifest_file`), and reads `manifest.generation.rules` directly (`generation` is a required, non-`Option` field). It errors only on a missing or unparsable manifest; every per-rule file/source problem is non-fatal and surfaces in `entry.issues`, so the index is always usable. Both modules are declared in `crates/ggen-lsp/src/lib.rs` (Agent 1's only `lib.rs` edit: two `pub mod` lines).

### 2.3 The detector — `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` + `analyzers/mod.rs`

The pure, file-free core:

```rust
pub const GGEN_TPL_001: &str = "GGEN-TPL-001";

pub fn unbound_projection_diagnostics(
    template: &str,
    available_vars: &std::collections::BTreeSet<String>,
) -> Vec<tower_lsp::lsp_types::Diagnostic>;
```

It extracts consumed variables from the template (`row["k"]`, `row['k']`, `{{ name }}`, `{{ row.name }}`, with trailing filter chains stripped and `{% for %}` / `{% set %}` locals subtracted), then emits one `GGEN-TPL-001` diagnostic (severity `ERROR`, source `ggen-lsp`, whole-line range, deterministic `BTreeSet` order) per consumed var not in `available_vars`.

The project-level entry point (`analyzers/mod.rs`):

```rust
pub fn detect_tpl_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<tower_lsp::lsp_types::Diagnostic>)>;
```

It iterates `project.rule_entries`, runs `unbound_projection_diagnostics(template_content, selected_vars)` for each entry whose template resolved (`template_content: Some`), and groups the result per template path. Entries with `template_content: None` (missing file / Git / Package) are skipped — a missing source is an index issue, **not** GGEN-TPL-001 (proven by `missing_template_file_is_index_issue_not_tpl_001`, `tests/ggen_tpl_001.rs`).

The live instance path is wired too: `TeraAnalyzer::available_vars()` is now read, and `diagnostics()` appends `unbound_projection_diagnostics` when the analyzer was built with real SPARQL bindings. The single-file `build_analyzer` seam (`analyzers/mod.rs`, passes `""`) is left back-compatible so existing server.rs / check.rs callers do not change behavior.

### 2.4 DiagnosticSpecies registry — `crates/ggen-lsp/src/route/diagnostic_species.rs`

A `&'static` taxonomy that classifies each diagnostic code by failure class, surfaces, severity policy, route, actuation boundary, and receipt requirement:

```rust
pub struct DiagnosticSpecies {
    pub code: &'static str, pub failure_class: &'static str,
    pub surfaces: &'static [&'static str], pub severity_policy: &'static str,
    pub route: &'static str, pub origin: &'static str,
    pub actuation_boundary: &'static str, pub receipt_requirement: &'static str,
    pub detector_active: bool,
}
pub fn species_registry() -> &'static [DiagnosticSpecies]; // exactly 2 entries
pub fn species_for(code: &str) -> Option<&'static DiagnosticSpecies>;
```

Re-exported from `route::` (`route/mod.rs:19`). Two species are registered:

| code | failure_class | surfaces | severity | route | actuation | receipt | detector_active |
|------|---------------|----------|----------|-------|-----------|---------|-----------------|
| `GGEN-TPL-001` | `unbound_projection` | `[SPARQL, Tera]` | `error` | `source_law_repair` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-OUT-001` | `unbound_output_path`| `[SPARQL, ggen.toml]` | `error` | `source_law_repair` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-YIELD-001`| `layer_violation` | `[ggen.toml, OS]` | `error` | `security_repair` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-RULE-001` | `unbound_rule_file` | `[ggen.toml, OS]` | `error` | `source_law_repair` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-QUERY-002`| `blindspot` | `[SPARQL]` | `warning` | `advisory` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-PACK-001` | `pack_indirection` | `[ggen.toml]` | `warning` | `advisory` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-SRC-001`  | `source_caste` | `[ggen.toml]` | `error` | `source_law_repair` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-HARNESS-001` | `harness_mismatch` | `[Cargo.toml, tests/proof]` | `release_blocking` | `proof_topology_repair` | `inspect_only` | `boundary_receipt` | **true** |

`GGEN-HARNESS-001` is now active via `detect_harness_001`.

---

## 3. How GGEN-TPL-001 enters the existing observe→route→receipt loop

The project-level detectors in `crates/ggen-lsp/src/analyzers/mod.rs` now cover:
- `detect_tpl_001`: Unbound template projections.
- `detect_out_001`: Unbound output-path variables.
- `detect_rule_001`: Missing query/template files.
- `detect_yield_001`: Output paths escaping project root.
- `detect_src_001`: Rules targeting source-caste directories.
- `detect_query_002`: Advisory for `SELECT *`.
- `detect_pack_001`: Advisory for pack-sourced queries/templates.
- `detect_harness_001`: Cross-surface harness mismatch.

The existing living loop in `crates/ggen-lsp/src/state.rs` now triggers these via `ProjectIndex` and `HarnessIndex` refreshes.

---

## 4. Native-first / child-LSP-deferred decision

The detector is implemented **natively inside `ggen-lsp`** — a pure Rust function over `(template, available_vars)` plus an in-process project index — rather than by spawning or proxying a child language server for Tera or SPARQL. Rationale grounded in the real code:

- The cross-surface signal (template-consumes-var vs query-produces-var) spans three surfaces that no single off-the-shelf child LSP understands jointly; the binding lives in `ggen.toml`, which only ggen-lsp models.
- The existing analyzer surface (`TeraAnalyzer`, `SparqlAnalyzer`) already parses both surfaces in-process; reusing it keeps the detector zero-IPC and deterministic.
- A child-LSP proxy would reintroduce an I/O and process boundary into a path whose defining invariant is read-only, in-process analysis with no artifact materialization.

Delegating individual surfaces to dedicated child LSPs (e.g. a richer Tera or SPARQL server) remains a future option but is **deferred** — it is not required to satisfy the GGEN-TPL-001 acceptance criteria and would add a process boundary that the current pure-function design avoids.

---

## 5. The deferred live-wiring seam (next lawful action)

`detect_tpl_001` is **not yet called** by the interactive server (`crates/ggen-lsp/src/server.rs`) or the headless gate (`crates/ggen-lsp/src/check.rs`) — verified by grep: neither file references `detect_tpl_001`. This was an explicit orchestrator decision: the `detect_tpl_001` entry point plus the integration tests satisfy all acceptance criteria, and live wiring touches the per-URI `observe_diagnostics` loop, which deserves its own increment.

Two seams remain (from Agent 2's handoff):

1. **Headless gate (`check.rs`):** build a `ProjectIndex::from_root(root)` and fold `detect_tpl_001(&project)` results into the per-file diagnostic map; any GGEN-TPL-001 (severity `ERROR`) must fail the run. This is the authoritative path that lets `ggen lsp check` block on unbound projections.
2. **Interactive server (`server.rs`) on `didOpen`/`didChange`:**
   - **(A, minimal)** give the analyzer-construction seam a rule-aware path (e.g. `build_analyzer_with_bindings(path, content, sparql)`) so a `.tera` doc is analyzed with its owning rule's SPARQL text instead of `""`; the existing `diagnostics()` then includes GGEN-TPL-001 automatically.
   - **(B, project-level)** on change of any `.tera`/`.rq`/`ggen.toml`, refresh the `ProjectIndex`, run `detect_tpl_001`, `publishDiagnostics` per `(template_path, diags)`, and feed each into `ServerState::observe_diagnostics` (`state.rs:127`) so the OCEL intel log records the full transition chain and the route registry resolves `source-law.bind-projection`.

---

## 6. Species roadmap

| Phase | Species / item | State | Notes |
|-------|----------------|-------|-------|
| **Phase 1 (landed)** | `GGEN-TPL-001` / `unbound_projection` | **Active** | Detector live; route seeded; integration-tested. |
| **Phase 1.5 (GGEN-OUT-001)** | unbound var in templated **output path** (`out/{{ slug }}.txt`) | Fixture-locked, test `#[ignore]` | `output_path_unbound_emits_out_001_next_phase` (`tests/ggen_tpl_001.rs:186`, ignored: "GGEN-OUT-001 not active yet (next phase)"). Also consolidate the duplicated SELECT extractor and add `RepairFamily::SourceLaw`. |
| **Phase 2 (GGEN-HARNESS-001)** | `harness_mismatch` (ProofPack runner failure) | Metadata only in species registry; **no detector** | `detector_active = false`; `release_blocking`; `boundary_receipt`. Locks the shape for the proof-topology repair family. |

**Phase 1.5 consolidation items** (recorded as known gaps): the SELECT-var extractor is currently duplicated (Agent 1's minimal extractor in `rule_index.rs` vs the private `extract_sparql_vars` in `tera_analyzer.rs`); add a dedicated `RepairFamily::SourceLaw` variant in `route/model.rs` (today GGEN-TPL-001 maps to the unseeded `DanglingReference` family — functional and contamination-free, but the family name does not match the `source_law_repair` route slug); and address the Tera dialect gaps in Agent 2's handoff (macros, deep member access, exotic `{{ }}` expressions) — all conservative fail-safe misses, not false positives.
