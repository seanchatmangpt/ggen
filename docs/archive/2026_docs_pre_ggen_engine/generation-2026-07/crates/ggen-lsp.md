# ggen-lsp — Generation-First Audit (2026-07)

**Purpose:** Language server for ggen's four law surfaces (RDF/SPARQL/TOML/Tera) plus the
headless `check` gate, the repair-route (POWL) engine, OCEL intel mining/promotion, and the
Agent Admissibility Pack emitter. Per `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` (§2, lines
27–37, 54), this crate is the **designated absorption target** for `ggen-a2a-mcp`,
`ggen-lsp-mcp`, and `ggen-lsp-a2a` (13,361 LOC to arrive behind `mcp`/`a2a` features) — its
generatable share will grow when the thin-adapter crates land here.

## LOC baseline

Command: `tokei crates/ggen-lsp/src --output json | jq '.Rust.code'` (tokei 14.0.0)

| Language | Files | Code LOC | Comments | Blanks |
|---|---|---|---|---|
| Rust | 61 | 11,687 | 557 (+1,363 doc-Markdown) | 1,117 |

Per-file LOC: `for f in $(find crates/ggen-lsp/src -name '*.rs'); do tokei $f --output json | jq '.Rust.code'; done`

## Class breakdown

| Class | LOC | % of 11,687 |
|---|---|---|
| IRREDUCIBLY-CUSTOM | 8,281 | 70.9% |
| GENERATABLE-WITH-SPEC | 3,406 | 29.1% |
| DEAD-DELETE | 0 (1 file, comment-only) | 0.0% |
| GENERATED / GENERATABLE-NOW | 0 | 0.0% |

Prior estimate of ~75% custom verifies at **70.9%** (full-file assignment, not sampling —
every file was individually classified; heads of 15 representative files read).

## Per-module rationale

### IRREDUCIBLY-CUSTOM — 8,281 LOC (E-ledger candidate: "LSP analyzers" are on METHODOLOGY's Engine-core allowlist; route/ and intel/ mining should be reviewed onto the same allowlist as the repair-engine core)

- **analyzers/ — 2,952 LOC** (tera_analyzer 970, mod 517, rdf_analyzer 490, toml_analyzer 329,
  sparql_analyzer 289, diag 131, harness_analyzer 115, source_law_analyzer 111). Real parsers
  and cross-surface law checks (GGEN-TPL-001/OUT-001 unbound-projection analysis, E0024 Tera
  syntax, SPARQL SELECT-variable extraction). Explicitly named in METHODOLOGY.md's E-ledger
  allowlist ("LSP analyzers").
- **route/ (minus diagnostic_species) — 1,566 LOC** (registry 673, model 232, edit 189,
  compact 174, promoted 120, plan 66, envelope 60, mod 52). POWL partial-order repair-route
  model with structural soundness check (acyclic single-entry/exit WF-net test, route/model.rs
  header), priority-sorted family registry with conformance-gated promotion. Algorithmic
  repair-engine core — claim E-ledger membership alongside the analyzers.
- **intel/ (minus events, receipt) — 1,323 LOC** (mine 390, metrics 337, replay 219, log 129,
  history 126, field 106, mod 16). Offline failure-edge mining: OCEL log → RDF projection →
  DFG discovery + per-family conformance via SPARQL → evidence-carrying route promotion
  (intel/mine.rs header). Emission + promotion policy, not analysis (analysis lives in
  ggen-graph/wasm4pm per the process-intelligence boundary), but the promotion/replay logic
  is algorithmic. E-ledger candidate.
- **features/ — 1,692 LOC** (semantic_tokens 538, inlay_hint 359, code_lens 341,
  formatting 246, workspace_symbol 203, mod 5). Genuine line-by-line tokenizers/formatters for
  the four surfaces (semantic_tokens.rs implements delta-encoded LSP tokens with a fixed
  legend contract). Tokenizer logic is algorithmic; only the legend/table fragments are
  spec-shaped, too small to split.
- **indexes — 748 LOC** (rule_index 290, harness_index 257, project_index 201). Resolve
  `[[generation.rules]]` into analyzable entries (SPARQL variable extraction, buffer overlay,
  issue collection) — the analyzers' substrate.

### GENERATABLE-WITH-SPEC — 3,406 LOC

- **handlers/ — 315 LOC** (16 files, 12–53 LOC each). Textbook LSP dispatch: each handler
  unwraps params, calls `state.get_analyzer(uri)`, maps to a response
  (handlers/definition.rs is 15 code lines). Classic schema-derivable protocol surface.
  Nearest template family: `templates/lsp-max/` (9 templates incl. `backend.rs.tera`,
  which already renders `{{ server_name | pascal }}Backend` handler wiring). Those templates
  target a lsp-max *rule-pack* server shape, not this crate's analyzer-dispatch shape, so no
  file here is GENERATABLE-NOW — a new TTL spec of (method → analyzer call → response map)
  is needed, but no novel algorithm.
- **server wiring — 368 LOC** (server.rs 261, lib.rs 53, error.rs 32, main.rs 22).
  Capability declaration + LanguageServer impl delegating to handlers; thiserror enum.
- **state.rs — 546 LOC.** Document store, FileType extension mapping, analyzer cache —
  registry/dispatch glue over the custom analyzers.
- **check.rs — 747 LOC.** Headless gate: walkdir discovery, per-file analyzer invocation,
  serde `CheckReport`. Orchestration + report types; the law itself lives in analyzers/.
- **pack/mod.rs — 433 LOC.** Admissibility-pack emitter driven entirely by `include_str!`
  assets — an asset-manifest TTL spec would generate the emitter.
- **route/diagnostic_species.rs — 225 LOC.** `&'static` metadata table of diagnostic species
  (code, failure_class, surfaces, severity, route slug) whose values "are fixed by the
  GGEN-TPL-001 pre-implementation inventory" — a registry begging to be a TTL spec.
- **intel/events.rs — 305 LOC** (OCEL activity-name consts + event builders reusing
  ggen-graph types verbatim) and **intel/receipt.rs — 106 LOC** (serde `RepairReceipt`
  struct). Schema-derivable type/const definitions.
- **init.rs — 148 LOC, utils.rs — 213 LOC.** Scaffolding init and position/range helpers.

### DEAD-DELETE — 0 code LOC

- **protocol.rs** — a single comment line (`// LSP protocol message handling`), 0 code LOC.
  Declared `pub mod protocol;` in lib.rs:23 but defines nothing; grep shows no other
  reference (`grep -rn "protocol::" crates/ggen-lsp/src/` — empty). Delete file + mod decl.

## Consolidation note

When the §2 consolidation lands, `ggen-lsp/src/mcp/`, `a2a_mcp/`, `a2a/` arrive as adapter
code that CRATE_CONSOLIDATION_ANALYSIS calls "no independent logic" — expect the crate's
generatable share to rise well above 29% post-absorption (ggen-a2a-mcp already contains the
workspace's only GENERATED tree, `a2a_generated/`).
