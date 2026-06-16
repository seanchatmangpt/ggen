<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN-TPL-001 — Agent 1 Handoff: Rule / Project Index Layer](#ggen-tpl-001--agent-1-handoff-rule--project-index-layer)
  - [Files Changed (only my owned files)](#files-changed-only-my-owned-files)
  - [Public API Exposed (exact signatures — honors the shared contract)](#public-api-exposed-exact-signatures--honors-the-shared-contract)
  - [CORRECTIONS to the inventory (verified against real ggen-core source — read these)](#corrections-to-the-inventory-verified-against-real-ggen-core-source--read-these)
  - [Behavior Summary](#behavior-summary)
  - [Known Gaps](#known-gaps)
  - [Tests Added (Chicago TDD — real `TempDir`, real files, no mocks)](#tests-added-chicago-tdd--real-tempdir-real-files-no-mocks)
  - [Test Results (real output)](#test-results-real-output)
  - [Integration Notes for Agent 2](#integration-notes-for-agent-2)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN-TPL-001 — Agent 1 Handoff: Rule / Project Index Layer

**Branch:** `feat/ggen-tpl-001-living-lsp`
**Agent:** 1 of 5 (rule indexing layer)
**Date:** 2026-05-29
**Build status:** GREEN — `cargo check -p ggen-lsp` exit 0; 12/12 unit tests pass.

## Files Changed (only my owned files)

| File | Change |
|------|--------|
| `crates/ggen-lsp/src/rule_index.rs` | **NEW** — `RuleIndexEntry` + `from_rule` + minimal SELECT-var extractor + 7 tests |
| `crates/ggen-lsp/src/project_index.rs` | **NEW** — `ProjectIndex` + `IndexError` + `from_root` + 5 tests |
| `crates/ggen-lsp/src/lib.rs` | Added `pub mod project_index;` (after `pack`) and `pub mod rule_index;` (after `route`). No other change. |

No other files touched. No Cargo.toml, tests/, analyzers/, route/, state.rs, server.rs, check.rs edits.

## Public API Exposed (exact signatures — honors the shared contract)

```rust
// crate::rule_index
pub struct RuleIndexEntry {
    pub rule_id: String,
    pub manifest_path: std::path::PathBuf,
    pub query_inline: bool,                              // true=inline, false=file
    pub query_content: String,                           // resolved SPARQL text ("" if file missing)
    pub template_path: Option<std::path::PathBuf>,       // None for inline / Git / Package sources
    pub template_content: Option<String>,                // resolved Tera text; None if file missing / unsupported
    pub output_file: String,
    pub selected_vars: std::collections::BTreeSet<String>, // SELECT vars WITHOUT leading '?'
    pub issues: Vec<String>,                             // non-fatal index-level problems
}
impl RuleIndexEntry {
    // Never fails; fs/parse problems collected into `issues`.
    pub fn from_rule(rule: &ggen_core::manifest::GenerationRule, manifest_path: &std::path::Path) -> RuleIndexEntry;
}

// crate::project_index
pub struct ProjectIndex {
    pub root: std::path::PathBuf,
    pub rule_entries: Vec<RuleIndexEntry>,   // one per [[generation.rules]], in manifest order
}
impl ProjectIndex {
    pub fn from_root(root: &std::path::Path) -> Result<ProjectIndex, IndexError>;
}

#[derive(Debug)]
pub enum IndexError {
    ManifestNotFound { path: std::path::PathBuf },
    ManifestParse    { path: std::path::PathBuf, message: String },
}
// Implements std::fmt::Display + std::error::Error (hand-rolled; thiserror not needed).
```

Contract compliance: type names, field names, and `from_root` signature match
`GGEN_TPL_001_PRE_IMPLEMENTATION_INVENTORY.md` exactly.

## CORRECTIONS to the inventory (verified against real ggen-core source — read these)

1. **Parse entry point is `ggen_core::manifest::ManifestParser::parse(&Path)`**, returning
   `Result<GgenManifest, ggen_core::utils::error::Error>`. There is **no** `parse_manifest_file`.
2. **`GgenManifest.generation` is a required, non-`Option` field** (`GenerationConfig`), and
   `GenerationConfig.rules: Vec<GenerationRule>` is required. A valid `ggen.toml` must therefore
   contain `[project]`, `[ontology]` (with `source`), and `[generation]`. The rule set may be
   empty via `[generation]\nrules = []`. My `from_root` does NOT match on an `Option` — it reads
   `manifest.generation.rules` directly.
3. **`GenerationRule` real fields** (`manifest/types.rs:150-174`):
   `name, query, template, output_file, skip_empty: bool, mode: GenerationMode, when: Option<String>`.
   The inventory's mention of `merge`/`prefixes` was inaccurate.
4. **`QuerySource::File { file: PathBuf }`**, **`TemplateSource::File { file: PathBuf }`** —
   the `file` field is `PathBuf`, not `String`.
   `TemplateSource::Git { git: String, branch: Option<String>, path: PathBuf }`;
   `TemplateSource::Package { package: String, version: Option<String>, path: PathBuf }`.

## Behavior Summary

- Manifest discovery: `<root>/ggen.toml` (must be a file). Parsed via `ManifestParser::parse`
  (no hand-rolled TOML).
- Per `[[generation.rules]]` rule → one `RuleIndexEntry`:
  - `rule_id = rule.name`, `manifest_path = ggen.toml path`, `output_file = rule.output_file`.
  - Query: `Inline{inline}` → `query_content=inline`, `query_inline=true`.
    `File{file}` → path resolved relative to ggen.toml's dir, read → `query_content`,
    `query_inline=false`. Missing file → `issues` push (`"query file missing: ..."`), empty content.
  - Template: `File{file}` → `template_path=Some(resolved)`, read → `template_content=Some`;
    missing → `template_content=None` + `"template file missing: <path>"`. `Inline{inline}` →
    `template_path=None`, `template_content=Some(inline)`. `Git`/`Package` → both `None` +
    `"unsupported template source for MVP: ..."`.
  - `selected_vars`: own minimal extractor (see Known Gaps). Case-insensitive `SELECT`, projection
    up to `WHERE`, collect `?`-prefixed tokens (strip `?`, keep `[A-Za-z0-9_]` run), `DISTINCT`
    handled, `SELECT *` → empty set + info issue.
- No panics / no `unwrap` / no `expect` in non-test code. All paths return `Result` or collect
  into `issues`.

## Known Gaps

1. **Duplicated SELECT extractor (flagged for Phase-1.5).** `extract_select_vars` in
   `rule_index.rs` intentionally duplicates the *private* `extract_sparql_vars` in
   `crate::analyzers` (not `pub`, cannot import). Consolidate into one shared helper when the
   analyzer surface stabilizes. Documented in-code.
2. **Extractor is lexical, not a SPARQL parser.** Handles `SELECT ?a ?b WHERE` and
   `SELECT DISTINCT`. Expression projections like `SELECT (COUNT(?x) AS ?n)` will capture both
   `?x` and `?n` — acceptable for MVP.
3. **Git/Package template sources unsupported for MVP** — recorded as issues, no fetch.

## Tests Added (Chicago TDD — real `TempDir`, real files, no mocks)

`rule_index.rs` (7 tests): `inline_query_extracts_selected_vars`,
`query_file_is_read_and_vars_extracted`, `missing_template_file_is_reported_as_issue`,
`missing_query_file_yields_empty_content_and_issue`, `select_star_pushes_info_issue_and_empty_vars`,
`lowercase_select_is_handled`, `unsupported_git_template_source_records_issue`.

`project_index.rs` (5 tests): `missing_manifest_returns_not_found_error`,
`project_with_inline_rule_builds_index`, `project_with_query_file_rule_reads_file`,
`manifest_with_no_rules_yields_empty_entries`, `rule_with_missing_template_file_surfaces_issue_not_error`.

The 3 contractually-required tests (inline-query vars, query-file vars, missing-template issue)
are all present.

## Test Results (real output)

```
$ cargo test -p ggen-lsp --lib rule_index
test result: ok. 7 passed; 0 failed; 0 ignored; 0 measured; 124 filtered out; finished in 0.00s

$ cargo test -p ggen-lsp --lib project_index
test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 126 filtered out; finished in 0.00s
```

`cargo check -p ggen-lsp` → exit 0 (JSON message-format parse: TOTAL_ERRORS=0, MY_FILE_ERRORS=0).
`cargo build -p ggen-lsp --lib` → compiled clean.

Note: at the moment I ran the gates, the sibling `route`/`analyzers` files (Agents 2/3) were
already present enough for the whole crate to compile — the earlier transient
`crate::project_index` missing-symbol error was resolved by my `lib.rs` declaration, exactly as
the contract intended.

## Integration Notes for Agent 2

Agent 2 consumes the resolved rules to drive template-variable diagnostics. Usage:

```rust
use ggen_lsp::project_index::{ProjectIndex, IndexError};
use ggen_lsp::rule_index::RuleIndexEntry;

let index = ProjectIndex::from_root(project_root)?; // Result<_, IndexError>

for entry in &index.rule_entries {
    // SPARQL SELECT vars available to the template (no leading '?'):
    let available: &std::collections::BTreeSet<String> = &entry.selected_vars;

    // Template body to lint (None if file missing / unsupported source):
    if let Some(tera_src) = &entry.template_content {
        // diff Tera {{ var }} usages against `available` here → GGEN-TPL-001
    } else {
        // entry.issues explains why (template missing / Git/Package) — surface as needed
    }

    for issue in &entry.issues { /* non-fatal: "template file missing: ...", "SELECT * ...", etc. */ }
    // Also: entry.rule_id, entry.output_file, entry.query_content, entry.query_inline,
    //       entry.template_path, entry.manifest_path.
}
```

Contracts Agent 2 can rely on:
- `selected_vars` never contains a leading `?`.
- `from_root` only errors on missing/unparsable manifest (`IndexError`); per-rule file/source
  problems are **non-fatal** and live in `entry.issues` — the index is always usable.
- `template_content == None` means "nothing to lint" (missing file or Git/Package source);
  check `entry.issues` for the reason.
- `SELECT *` yields an empty `selected_vars` plus an info issue — treat as "all vars unknown,"
  do not emit missing-var diagnostics for those rules.
- The detector should be the **pure function** `detect_tpl_001(&ProjectIndex)` per the contract;
  do all file I/O on the index side (already done here), keep the detector reading only
  `template_content` + `selected_vars`.
