<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN-TPL-001 — Agent 3 Handoff: Diagnostic Species + Route Registry](#ggen-tpl-001--agent-3-handoff-diagnostic-species--route-registry)
  - [Files Changed (only my owned files)](#files-changed-only-my-owned-files)
  - [Species API (as implemented — matches the shared contract verbatim)](#species-api-as-implemented--matches-the-shared-contract-verbatim)
    - [Registered species (canonical values, verbatim from inventory §"Canonical values")](#registered-species-canonical-values-verbatim-from-inventory-%C2%A7canonical-values)
  - [How GGEN-TPL-001 routes — and proof it is source-law-only](#how-ggen-tpl-001-routes--and-proof-it-is-source-law-only)
    - [IMPORTANT: how the real route model selects (not what the contract sketch implied)](#important-how-the-real-route-model-selects-not-what-the-contract-sketch-implied)
    - [Registration (in `seed_routes()`)](#registration-in-seed_routes)
    - [Proof: targets only source law, never emitted output](#proof-targets-only-source-law-never-emitted-output)
  - [RepairFamily / model.rs change requested but NOT made (orchestrator request)](#repairfamily--modelrs-change-requested-but-not-made-orchestrator-request)
  - [Tests added + results (ALL PASS)](#tests-added--results-all-pass)
    - [In `diagnostic_species.rs`](#in-diagnostic_speciesrs)
    - [In `registry.rs`](#in-registryrs)
  - [Blockers / notes](#blockers--notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN-TPL-001 — Agent 3 Handoff: Diagnostic Species + Route Registry

**Branch:** `feat/ggen-tpl-001-living-lsp`
**Agent:** 3 of 5 (GGEN-TPL-001-FIVE-AGENT-WORKSPLIT)
**Scope:** `crates/ggen-lsp/src/route/` — diagnostic species taxonomy + GGEN-TPL-001 route registration
**Date:** 2026-05-29
**Status:** COMPLETE. Crate compiles (`cargo check -p ggen-lsp` exit 0); all route tests green (28/28).

---

## Files Changed (only my owned files)

| File | Change |
|------|--------|
| `crates/ggen-lsp/src/route/diagnostic_species.rs` | **NEW** — `DiagnosticSpecies` struct, `species_registry()`, `species_for()`, 5 inline tests |
| `crates/ggen-lsp/src/route/mod.rs` | Added `pub mod diagnostic_species;` + `pub use diagnostic_species::{species_for, species_registry, DiagnosticSpecies};` (2 lines, nothing else) |
| `crates/ggen-lsp/src/route/registry.rs` | Added a seeded `RepairRoute` `source-law.bind-projection` to `seed_routes()`; wired `family_of_code` for `"GGEN-TPL-001"`; added 5 inline tests |

Not touched: `model.rs`, `state.rs`, `server.rs`, `check.rs`, `lib.rs`, `analyzers/`, `rule_index.rs`, `project_index.rs`, `Cargo.toml`.

---

## Species API (as implemented — matches the shared contract verbatim)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticSpecies {
    pub code: &'static str,
    pub failure_class: &'static str,
    pub surfaces: &'static [&'static str],
    pub severity_policy: &'static str,
    pub route: &'static str,
    pub origin: &'static str,
    pub actuation_boundary: &'static str,
    pub receipt_requirement: &'static str,
    pub detector_active: bool,
}
pub fn species_registry() -> &'static [DiagnosticSpecies]; // exactly 2 entries
pub fn species_for(code: &str) -> Option<&'static DiagnosticSpecies>;
```

`&'static` slice in `rodata`, zero-alloc lookup. Re-exported from `route::`.

### Registered species (canonical values, verbatim from inventory §"Canonical values")

| code | failure_class | surfaces | severity_policy | route | origin | actuation_boundary | receipt_requirement | detector_active |
|------|---------------|----------|-----------------|-------|--------|--------------------|---------------------|-----------------|
| `GGEN-TPL-001` | `unbound_projection` | `["ggen.toml","SPARQL","Tera"]` | `error` | `source_law_repair` | `ark-covenant / living-lsp MVP` | `inspect_only` | `diagnostic_receipt` | **true** |
| `GGEN-HARNESS-001` | `harness_mismatch` | `["Cargo.toml","tests/proof","Makefile.toml"]` | `release_blocking` | `proof_topology_repair` | `ark-covenant ProofPack runner failure` | `inspect_only` | `boundary_receipt` | **false** (Phase 2 — metadata only, NO detector) |

---

## How GGEN-TPL-001 routes — and proof it is source-law-only

### IMPORTANT: how the real route model selects (not what the contract sketch implied)

`route/model.rs` does NOT have a `RepairRoute.code` field, a `SparqlBinding`/`SourceLaw`
family, or `RepairStep { action, target, rationale }`. The real model:

- `RouteRegistry::seeded()` collects routes from the free fn `seed_routes() -> Vec<RepairRoute>`.
- `select_for_diagnostic(diag)` resolves **by `RepairFamily` only** (`family_of_diagnostic` → `family_of_code`), then prefers a promotable mined route, else the seeded route, else first. **There is NO per-code disambiguation within a family.**
- `RepairRoute { id: RouteId, family: RepairFamily, steps: PartialOrder, description, provenance, priority }`.
- `RepairStep { id: StepId, title: String, edit: EditTemplate }`; `EditTemplate::NoOp` = advisory (no textual edit).

Because selection keys only on family, **GGEN-TPL-001 must own a family exclusively** or it contaminates the other codes in that family. I mapped it to `RepairFamily::DanglingReference` — an otherwise-unseeded family, and semantically apt (an unbound projection is a dangling reference from the Tera template to a non-existent SPARQL binding).

### Registration (in `seed_routes()`)

```
RepairRoute {
    id: RouteId("source-law.bind-projection"),
    family: RepairFamily::DanglingReference,   // owned exclusively by GGEN-TPL-001
    steps: PartialOrder { nodes: [3 NoOp steps], edges: [] },  // concurrent, advisory
    description: "Unbound projection — bind the variable at its source law … Advisory only; never edits emitted output.",
    provenance: Provenance::Seeded,
    priority: 10,
}
```

Three advisory (`EditTemplate::NoOp`) steps, each titled for a source-law surface:

| step id | title | source-law surface |
|---------|-------|--------------------|
| `edit-sparql-select` | "Project the variable in the SPARQL SELECT (source law)" | (a) SPARQL SELECT vars |
| `edit-template-ref` | "Fix the Tera template variable reference (source law)" | (b) Tera template var reference |
| `inspect-ggen-toml-rule` | "Inspect/edit the ggen.toml rule binding (source law)" | (c) ggen.toml rule binding |

`family_of_code("GGEN-TPL-001") == Some(RepairFamily::DanglingReference)`.

### Proof: targets only source law, never emitted output

Enforced by test `ggen_tpl_001_route_is_source_law_only`, which for every step asserts:
1. `matches!(step.edit, EditTemplate::NoOp)` — the route never produces an output-mutating edit (consistent with the `inspect_only` actuation boundary).
2. The step title contains no emitted-output marker (`out/`, `output/`, `dist/`, `gen/`, `emitted`).
3. The step title references a source-law surface (`sparql` | `template` | `ggen.toml`).

`ggen_tpl_001_route_covers_three_source_law_surfaces` proves all three surfaces are covered.
`ggen_tpl_001_does_not_contaminate_template_failure_codes` proves E0010 still selects `template.values-inline` (no regression).

---

## RepairFamily / model.rs change requested but NOT made (orchestrator request)

The species-level route slug is `"source_law_repair"`, but `model.rs::RepairFamily` (which I do **not** own) has no matching variant. I reused `RepairFamily::DanglingReference` (unseeded, semantically apt) so GGEN-TPL-001 owns a family exclusively and avoids cross-code contamination.

**Request for orchestrator / model.rs owner:** add a dedicated `RepairFamily::SourceLaw` variant so the family name matches the `"source_law_repair"` slug. If added, update in `registry.rs`:
- the route's `family` field → `RepairFamily::SourceLaw`
- the `family_of_code` arm for `"GGEN-TPL-001"`
- test `ggen_tpl_001_maps_to_its_own_family` (currently asserts `DanglingReference`)

Non-blocking; the current `DanglingReference` mapping is correct, functional, and contamination-free.

---

## Tests added + results (ALL PASS)

`cargo test -p ggen-lsp --lib route::` → **28 passed; 0 failed**.

### In `diagnostic_species.rs`
1. `ggen_tpl_001_is_active_with_canonical_values` — active entry, `detector_active == true`, all canonical fields. ✅
2. `ggen_harness_001_is_metadata_only` — exists, `detector_active == false`. ✅
3. `unknown_code_has_no_species` — `None` for unknown codes. ✅
4. `registry_contains_exactly_two_species`. ✅
5. `actuation_boundary_is_inspect_only_for_all_species`. ✅

### In `registry.rs`
6. `ggen_tpl_001_maps_to_its_own_family` — `family_of_code` → `DanglingReference`. ✅
7. `ggen_tpl_001_does_not_contaminate_template_failure_codes` — E0010 still → `template.values-inline`. ✅
8. `ggen_tpl_001_selects_the_source_law_route` — GGEN-TPL-001 diagnostic → `source-law.bind-projection`, `Seeded`, sound. ✅
9. `ggen_tpl_001_route_is_source_law_only` — **the load-bearing invariant**: every step `NoOp`, no emitted-output marker, references a source-law surface. ✅
10. `ggen_tpl_001_route_covers_three_source_law_surfaces` — SPARQL + template + ggen.toml all covered. ✅

`cargo check -p ggen-lsp` exit 0. `rustfmt --edition 2021` clean on all three files.

---

## Blockers / notes

1. **No blockers.** Crate compiles and all route tests pass. (An earlier `cargo check` failed on Agent 1's `rule_index.rs` `PathBuf`-Display error; that was fixed by Agent 1 before my final run.)
2. **One regression I introduced and fixed in-session:** my first attempt mapped GGEN-TPL-001 to `TemplateFailure` with `priority: 20`, which broke the existing `unproven_mined_route_loses_to_seed` test (my route out-prioritized `template.values-inline` for E0010). Root cause: `select_for_diagnostic` keys on family only. Fixed by giving GGEN-TPL-001 its own family (`DanglingReference`). Net result: +1 test (28 vs 27), all green.
3. **Optional faithfulness request** (non-blocking): add `RepairFamily::SourceLaw` in `model.rs` (see section above).
4. **Concurrent-author note:** consistent with `memory/concurrent-author-cron-loop.md`, the working tree changed under me mid-session (Agent 1 landed `rule_index.rs`/`project_index.rs` fix). Recommend orchestrator re-check HEAD and re-run `cargo make test -p ggen-lsp` after all agents land before commit.
