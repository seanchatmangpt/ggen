# GGEN-TPL-001 — Agent 2 Handoff: Tera Binding Detector

**Mission:** GGEN-TPL-001-FIVE-AGENT-WORKSPLIT
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Agent:** 2 (SPARQL→Tera binding path + cross-surface detector)
**Date:** 2026-05-29

---

## Files changed (the only two I own/wrote)

- `crates/ggen-lsp/src/analyzers/tera_analyzer.rs`
- `crates/ggen-lsp/src/analyzers/mod.rs`

No other files touched. `project_index.rs` / `rule_index.rs` were NOT created (Agent 1's surface).

---

## What changed

### `tera_analyzer.rs`

1. **`available_vars` is now LIVE.** Removed `#[allow(dead_code)]`. Type changed
   `HashSet<String>` → `BTreeSet<String>` (matches the contract's `selected_vars`
   type, gives deterministic diagnostic ordering). Added accessor
   `pub fn available_vars(&self) -> &BTreeSet<String>`. `new_from_content(content, sparql_bindings)`
   is unchanged in signature; it still computes `available_vars` via
   `extract_sparql_vars` (now returning `BTreeSet`).
2. **`diagnostics()` now surfaces GGEN-TPL-001 when bindings are present.** If
   `available_vars` is non-empty (i.e. the analyzer was built with real SPARQL
   bindings, not the single-file `""` path), `diagnostics()` appends the output of
   `unbound_projection_diagnostics(&self.source, &self.available_vars)` after the
   existing E0024 syntax check. With empty bindings (the `build_analyzer` single-file
   path) only E0024 is checked — **fully back-compatible**, server.rs/check.rs callers
   unaffected.
3. **Consumed-var extractor** `pub fn consumed_vars(template: &str) -> BTreeSet<String>`:
   detects `row["name"]`, `row['name']`, `{{ name }}`, `{{ row.name }}`, and strips a
   trailing filter chain (`{{ name | upper }}` → `name`). Subtracts local vars.
4. **Local-var subtraction** (private `local_vars`): collects identifiers introduced by
   `{% for X in … %}` (incl. `{% for K, V in … %}`) and `{% set X = … %}`, and removes
   them from the consumed set so loop/set bindings are never flagged.
5. **Pure detector** (contract signature, exact):
   ```rust
   pub fn unbound_projection_diagnostics(
       template: &str,
       available_vars: &std::collections::BTreeSet<String>,
   ) -> Vec<tower_lsp::lsp_types::Diagnostic>
   ```
   Reads/writes no files. Emits one diagnostic per consumed var ∉ available_vars.
6. **`pub const GGEN_TPL_001: &str = "GGEN-TPL-001";`** defined and used.

### `analyzers/mod.rs`

- Re-exports `unbound_projection_diagnostics`, `GGEN_TPL_001`, `TeraAnalyzer`.
- **Project-level entry** (contract signature, exact):
  ```rust
  pub fn detect_tpl_001(
      project: &crate::project_index::ProjectIndex,
  ) -> Vec<(std::path::PathBuf, Vec<tower_lsp::lsp_types::Diagnostic>)>
  ```
  Iterates `project.rule_entries`; for each entry with `template_content: Some(t)`,
  calls `unbound_projection_diagnostics(&t, &entry.selected_vars)`; collects
  `(entry.template_path.clone().unwrap_or_default(), diags)` when non-empty. Entries
  with `template_content: None` are skipped (index-level issue, not GGEN-TPL-001).
- `build_analyzer` unchanged (single-file Tera path still passes `""`).

---

## Detector API (exact signatures)

```rust
pub const GGEN_TPL_001: &str = "GGEN-TPL-001";

// crate::analyzers::tera_analyzer (re-exported at crate::analyzers)
pub fn unbound_projection_diagnostics(
    template: &str,
    available_vars: &std::collections::BTreeSet<String>,
) -> Vec<tower_lsp::lsp_types::Diagnostic>;

pub fn consumed_vars(template: &str) -> std::collections::BTreeSet<String>; // helper, also pub

// crate::analyzers (mod.rs)
pub fn detect_tpl_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<tower_lsp::lsp_types::Diagnostic>)>;

// instance accessor (live)
impl TeraAnalyzer { pub fn available_vars(&self) -> &std::collections::BTreeSet<String>; }
```

---

## Exact diagnostic fields emitted

| Field | Value |
|-------|-------|
| `code` | `Some(NumberOrString::String("GGEN-TPL-001"))` |
| `severity` | `Some(DiagnosticSeverity::ERROR)` |
| `source` | `Some("ggen-lsp")` (via `diag::whole_line`) |
| `message` | `` GGEN-TPL-001 unbound_projection: template consumes `{var}` which the rule's SPARQL SELECT does not produce `` |
| `range` | whole line 0: `start {line:0,character:0}` → `end {line:0,character:u32::MAX}` (via `diag::whole_line`; whole-file/line-0 per MVP plan) |
| other | `code_description/related_information/tags/data = None` |

Diagnostics are emitted in `BTreeSet` order (deterministic, alphabetical by var).

---

## Tests added (inline `#[cfg(test)] mod tests` in tera_analyzer.rs)

All pure over `(BTreeSet<String>, &str)` — Chicago style, real inputs, observable output.

1. `available_name_consumed_name_bracket_no_diagnostic` — available `{name}`, `row["name"]` → 0 diags.
2. `available_name_consumed_title_one_tpl_001` — available `{name}`, `row["title"]` → exactly 1 GGEN-TPL-001 mentioning `title`.
3. `both_vars_available_no_diagnostic` — available `{name,title}`, `{{ name }} - {{ row.title }}` → 0 diags.
4. `empty_available_with_consumed_var_triggers_tpl_001` — available `{}`, `{{ x }}` → 1 GGEN-TPL-001.
5. `loop_local_var_is_not_consumed` (guard) — available `{items}`, `{% for x in items %}{{ x }}{% endfor %}` → 0 diags (x is loop-local).
6. `set_local_var_is_not_consumed` (extra guard) — available `{name}`, `{% set y = name %}{{ y }}` → 0 diags.
7. `live_diagnostics_surface_tpl_001_with_bindings` — `TeraAnalyzer::new_from_content(r#"{{ row["missing"] }}"#, "SELECT ?name WHERE")` → `diagnostics()` contains GGEN-TPL-001 (proves the live wiring through `available_vars`).

Pre-existing tests retained: `valid_template_has_no_diagnostics`, `unclosed_block_reports_e0024`.

### Test results — GREEN

Agent 1's `project_index` + `rule_index` modules landed during this session, so the crate
now compiles and the full suite is green. Verified field-name match against the contract:
`ProjectIndex::rule_entries`, `RuleIndexEntry::{template_content, template_path, selected_vars}`
all present as the contract specifies — `detect_tpl_001` compiles against the real types.

```
$ cargo check -p ggen-lsp
    Finished `dev` profile in 0.31s            # 0 errors

$ cargo test -p ggen-lsp --lib analyzers::tera_analyzer
running 9 tests ... test result: ok. 9 passed; 0 failed     # 7 new + 2 pre-existing

$ cargo test -p ggen-lsp --lib
test result: ok. 132 passed; 0 failed; 0 ignored            # whole crate green
```

All 7 GGEN-TPL-001 unit tests pass; no regression in the other 125 lib tests.

---

## Known dialect gaps (extractor may mis-handle)

The consumed-var extractor is a lightweight scanner, not a full Tera parser. Documented gaps for Phase 1.5:

- **Macros:** `{% macro m(a, b) %}` parameters are NOT treated as locals — args used inside a macro body could be falsely flagged as consumed-from-query. (No macro fixtures in MVP scope.)
- **Nested member access depth:** `{{ row.a.b }}` reports `a` (segment after first dot). `{{ obj.row.name }}` also reports the first post-dot segment. Acceptable for the canonical `row.<key>` shape; deeper graphs may need a real parser.
- **Filters on bracket keys:** `{{ row["name"] | upper }}` — the bracket-key scanner still finds `name` (correct); but the `{{ }}` head-parser's filter strip only handles the `name | filter` bare form. Combined exotic expressions (`{{ (a or b) | d }}`) are not parsed and yield no consumed var (silent miss, not a false positive — fail-safe).
- **`{{ … }}` with leading function call / literal / arithmetic** (`{{ "x" }}`, `{{ 1 + n }}`, `{{ fn(x) }}`): `leading_var` returns `None` for non-bare-identifier heads → these are NOT flagged (conservative; avoids false positives, may miss `n`/`x` inside expressions).
- **`{%- … -%}` whitespace-control trim markers:** the `for`/`set` matcher trims inner text so `{%- for x in items -%}` is handled, but unusual spacing around `in`/`=` could slip; covered cases use standard spacing.
- **String keys that are not identifiers** (`row["full name"]`, `row["a-b"]`) are ignored by `is_identifier` (not flagged). Intentional — they cannot be SPARQL SELECT var names anyway.
- **Detector currently uppercases nothing for SELECT parsing in `extract_sparql_vars`** (kept as-is from original): relies on literal `SELECT`/`WHERE`. Lowercase `select`/`where` in bindings would yield empty available_vars (Agent 1 owns the authoritative SELECT-var extraction per the contract; `extract_sparql_vars` is only the single-file fallback). This is the contract's known SELECT-extraction duplication gap (Phase 1.5 consolidation).

---

## ORCHESTRATOR WIRING NOTE (server.rs / check.rs — I could not edit these)

`detect_tpl_001` and the live `diagnostics()` GGEN-TPL-001 emission are in place but
nothing in the live server or headless gate calls the project-level path yet. To make
GGEN-TPL-001 fire end-to-end, the orchestrator must wire (after Agents 1+2 merge):

1. **Headless gate (`crates/ggen-lsp/src/check.rs`):**
   Build a `ProjectIndex` for the project root and fold in cross-surface diagnostics:
   ```rust
   let project = crate::project_index::ProjectIndex::from_root(root)?;
   for (template_path, diags) in crate::analyzers::detect_tpl_001(&project) {
       // attribute `diags` to `template_path` in the gate's per-file diagnostic map;
       // any GGEN-TPL-001 diag is severity ERROR → gate must fail the run.
   }
   ```
   This is the authoritative path that lets `ggen lsp check` block on unbound projections.

2. **Interactive server (`crates/ggen-lsp/src/server.rs`) on `didOpen`/`didChange`:**
   Two options — pick one:
   - **(A, minimal)** When a `.tera` doc is analyzed, resolve its owning rule's SPARQL
     bindings from a cached `ProjectIndex` and build the analyzer with
     `TeraAnalyzer::new_from_content(content, &sparql_bindings)` instead of `""`. Then the
     existing `diagnostics()` automatically includes GGEN-TPL-001 (already implemented).
     This requires `build_analyzer` (or the server's analyzer-construction seam) to receive
     the rule's SPARQL text — currently it passes `""` at `analyzers/mod.rs:34`. **I left
     `build_analyzer` back-compatible on purpose; the server needs a rule-aware construction
     path or a `build_analyzer_with_bindings(path, content, sparql)` variant.**
   - **(B, project-level)** On `didChange`/`didOpen` of any `.tera`/`.rq`/`ggen.toml`,
     rebuild/refresh the `ProjectIndex` and run `detect_tpl_001`, then `publishDiagnostics`
     per `(template_path, diags)`. Also feed each raised diagnostic into
     `ServerState::observe_diagnostics` (state.rs:127) so the OCEL intel log records the
     `DiagnosticRaised → RouteSelected → …` transitions, and so Agent 3's route registry
     (`select_for_diagnostic` for code `GGEN-TPL-001`) maps it to `source_law_repair`.

3. **Route attachment (Agent 3 dependency):** ensure `RouteRegistry::seeded()` (state.rs:103)
   registers a `source_law_repair` route for code `"GGEN-TPL-001"` so the observed
   diagnostic resolves to a route. Route must target the SOURCE law surface
   (ggen.toml/SPARQL/Tera), never emitted output. (Agent 3 owns this.)

**Constraint reminder honored:** no sync calls, no artifact writes, route = source-law repair,
emitted output never targeted. The detector is pure; all I/O lives in Agent 1's index.

---

## Blockers

- **None for my surface.** Agent 1's modules landed; crate compiles, all 132 lib tests pass.
- Live end-to-end firing depends only on the orchestrator wiring above (server.rs/check.rs —
  outside my ownership). Confirmed neither file calls `detect_tpl_001` yet.
