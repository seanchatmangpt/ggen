# Supersessions — Generation-First Audit (2026-07)

Dispositions of prior audit artifacts per METHODOLOGY.md §"Relationship to prior audits".
Verified at git `013bee436` on 2026-07-02.

| Artifact | Disposition | Basis |
|---|---|---|
| `docs/crate-audits/AUDIT_DASHBOARD.md` | **Superseded** | Stale roster and stale P0 status (evidence below). P0s carried forward into `machinery-gaps.md` with fresh verification. |
| `/CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` (repo root) | **Consumed** | Seeds the `DEAD-DELETE` class in `classification.tsv` (e.g. `genesis-core` 0 dependents / 0 tests, `stpnt` 0 dependents — its §1 REMOVE table with dependent-grep evidence is exactly the "consolidation-analysis citation" METHODOLOGY requires for DEAD-DELETE). |
| `/DOCUMENTATION_AUDIT_REPORT.md` (repo root) | **Consumed** | Raw per-file inventory (path / purpose / completeness / action) used as input to `docs-audit.md`. Not authoritative on its own — it is a flat table with no roster or generation analysis. |
| `docs/MASTER_TODO.md` | **Cross-referenced** | P0-01/02/03 definitions (MASTER_TODO.md:70-104) are the anchor for the carried-forward items; each was re-verified in code and two of three have materially changed status (see below and `machinery-gaps.md`). Remains the live task ledger; not superseded. |

## Why AUDIT_DASHBOARD.md is superseded

**1. Roster is wrong by 14 packages.** The dashboard states
"**Scope:** All 31 workspace crates (excludes `vendors/`)" and "Total Crates Audited | 31"
(`docs/crate-audits/AUDIT_DASHBOARD.md:26,35`, generated 2026-04-01). Ground truth from
`cargo metadata --no-deps` (2026-07-02) is **17 packages**:

```
cpmp, genesis-core, genesis-core-v2, genesis-schema, genesis-types, ggen,
ggen-a2a-mcp, ggen-cli-lib, ggen-config, ggen-core, ggen-graph, ggen-lsp,
ggen-lsp-a2a, ggen-lsp-mcp, ggen-marketplace, star-toml, stpnt
```

The 31-crate figure predates the workspace prune; the dormant `crates/` directories
(genesis-construct8, genesis-lockchain, genesis-wasm-shell, ggen-membrane,
ggen-projection) are no longer members, and several crates the dashboard audits
individually (e.g. `ggen-domain`, cited at MASTER_TODO P0-03) no longer exist as
packages. Any per-crate percentage or dependency graph in the dashboard is computed over
a roster that does not match the build.

**2. P0 statuses are stale.** Re-verification (full evidence in `machinery-gaps.md`):

- **P0-01** "SHACL validation always passes (no-op)" (AUDIT_DASHBOARD.md:241) — the stub
  was replaced by a real SPARQL implementation
  (`crates/ggen-core/src/validation/shacl.rs`, 344 lines; `validator.rs` `SparqlValidator`).
  Only a fail-open residue remains at `shacl.rs:137-140`.
- **P0-03** "Three competing URIs" (AUDIT_DASHBOARD.md:243) — consolidated; the fix is
  labeled in-code at `crates/ggen-marketplace/src/marketplace/rdf/ontology.rs:7`
  ("Single Canonical Namespace (P0-03 fix)"), and `ggen-core/src/rdf/schema.rs:46` now
  matches `MARKETPLACE_NS`.
- **P0-02** remains open, but the file paths moved: the staged pipeline is now
  `crates/ggen-core/src/pipeline_engine/pipeline.rs:163`, not `v26.5.19/pipeline.rs:329`
  as the dashboard/MASTER_TODO cite.

**Carried forward**: the three P0s themselves (as re-verified defects/residues) and the
54-stub classification method. **Discarded**: the roster, the per-crate dashboards' crate
list, and the P0 file:line citations, all of which now mislead.

## Notes on the consumed artifacts

- `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` sits at the **repo root** (violating the
  no-root-files rule) alongside the older, never-executed `CRATE_CONSOLIDATION_PROPOSAL.md`
  which it explicitly cross-checks and in one case overrides (`stpnt` KEEP→REMOVE). The
  2026-07-01 analysis is the DEAD-DELETE seed; the older PROPOSAL is superseded by it and
  should not be consulted directly.
- `DOCUMENTATION_AUDIT_REPORT.md` (repo root) enumerates every `.md` with a
  completeness state; the generation audit reuses that inventory but re-derives
  dispositions, since "Placeholder/Stub → Expand into full documentation" is often the
  wrong action under Gap 5 of `machinery-gaps.md` (no TTL→md pipeline exists — many stubs
  should become TTL specs or be deleted, not hand-expanded).
