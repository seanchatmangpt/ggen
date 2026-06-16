<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGEN-TPL-001 — Agent 4 (Integration Tests) Handoff](#ggen-tpl-001--agent-4-integration-tests-handoff)
  - [1. Files created (Agent 4 ownership only)](#1-files-created-agent-4-ownership-only)
  - [2. Tests created and what each proves](#2-tests-created-and-what-each-proves)
    - [Assertion design notes (typed against the real landed API)](#assertion-design-notes-typed-against-the-real-landed-api)
  - [3. Key fixture file contents](#3-key-fixture-file-contents)
  - [4. Exact API symbols depended on (from Agents 1 & 2)](#4-exact-api-symbols-depended-on-from-agents-1--2)
  - [5. Command results](#5-command-results)
    - [A. The Agent-4 integration test — GREEN](#a-the-agent-4-integration-test--green)
    - [B. Full crate `cargo test -p ggen-lsp` — RED, but NOT in any Agent-4 file](#b-full-crate-cargo-test--p-ggen-lsp--red-but-not-in-any-agent-4-file)
  - [6. Guardrails honored](#6-guardrails-honored)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGEN-TPL-001 — Agent 4 (Integration Tests) Handoff

**Branch:** `feat/ggen-tpl-001-living-lsp`
**Lane:** failing-test-first (RED expected until Agents 1 & 2 land their `src/`)
**Methodology:** Chicago TDD — real `ggen.toml` project trees on disk, real `ProjectIndex`, real analyzer. No mocks, no test doubles, no fabricated diagnostics.
**Date:** 2026-05-29

> The product is CodeManufactory; RevOps is merely proof that CodeManufactory works.

---

## 1. Files created (Agent 4 ownership only)

```
crates/ggen-lsp/tests/ggen_tpl_001.rs                                      (new)
crates/ggen-lsp/tests/fixtures/ggen_tpl_001/
  valid_rule/
    ggen.toml
    schema/domain.ttl
    queries/items.rq
    templates/item.tera
  invalid_unbound_template_var/
    ggen.toml
    schema/domain.ttl
    queries/items.rq
    templates/item.tera
  inline_query_valid/
    ggen.toml
    schema/domain.ttl
    templates/item.tera
  missing_template_file/
    ggen.toml
    schema/domain.ttl
    queries/items.rq
    # NOTE: templates/missing.tera is intentionally absent (drives the index issue)
  output_path_unbound_next_phase/        # OPTIONAL — GGEN-OUT-001 (next phase)
    ggen.toml
    schema/domain.ttl
    queries/items.rq
    templates/item.tera
```

No `src/`, no `Cargo.toml`, no other crate's example/fixture dirs were touched.

---

## 2. Tests created and what each proves

| # | Test fn | Fixture | Proves |
|---|---------|---------|--------|
| 1 | `valid_rule_emits_no_tpl_001` | `valid_rule` | Template consumes only the bound projection var `name` → **NO** GGEN-TPL-001. |
| 2 | `unbound_template_var_emits_tpl_001_error` | `invalid_unbound_template_var` | Template consumes `row["title"]` but query binds only `?name` → GGEN-TPL-001 fires, code `== "GGEN-TPL-001"`, severity renders as `Error`. |
| 3 | `inline_query_valid_emits_no_tpl_001` | `inline_query_valid` | Inline-query binding path (`query = { inline = "..." }`) resolves bindings like the file path → **NO** GGEN-TPL-001. |
| 4 | `missing_template_file_is_index_issue_not_tpl_001` | `missing_template_file` | Missing template surfaces as an **index issue** (`RuleIndexEntry.issues` non-empty) and `detect_tpl_001` does **NOT** emit GGEN-TPL-001 (a missing source is an index/source diagnostic, not unbound-projection). |
| 5 | `analysis_never_materializes_output_file` | `valid_rule` (TempDir copy) | After `ProjectIndex::from_root` + `detect_tpl_001`, the declared `output_file` (`out.txt`) does **NOT** exist on disk → analysis is read-only and never materializes artifacts. Uses a `tempfile::TempDir` copy so the absence assertion is unambiguous and repo fixtures stay pristine. |
| 6 | `output_path_unbound_emits_out_001_next_phase` `#[ignore]` | `output_path_unbound_next_phase` | OPTIONAL / next phase. GGEN-OUT-001 (unbound var in templated output path `out/{{ slug }}.txt`). Marked `#[ignore]` — rule not active; fixture only locks the shape. |

### Assertion design notes (typed against the real landed API)
- `detect_tpl_001` returns `Vec<(PathBuf, Vec<tower_lsp::lsp_types::Diagnostic>)>` — a per-file grouping. Helpers `count_tpl_001` / `has_tpl_001_error` flatten the inner `Vec<Diagnostic>`.
- The load-bearing assertion is **`code == "GGEN-TPL-001"`**, checked typed via `matches!(&d.code, Some(NumberOrString::String(s)) if s == "GGEN-TPL-001")`.
- Severity is asserted typed: `d.severity == Some(DiagnosticSeverity::ERROR)`.
- `ProjectIndex::from_root` returns `Result<ProjectIndex, IndexError>`; the `load()` helper `.unwrap_or_else(panic!)`s on error (real I/O failure surfaces loudly).
- Test #4 reads the public field `project.rule_entries` and each entry's public `issues: Vec<String>` (matches Agent 1's struct).

---

## 3. Key fixture file contents

**`valid_rule/ggen.toml`** (template uses only bound `name`):
```toml
[project]
name = "tpl001-valid-rule"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out.txt"
```

**`queries/items.rq`** (binds only `?name`):
```sparql
PREFIX ex: <http://example.org/tpl001#>
SELECT ?name WHERE {
    ?s a ex:Item .
    ?s ex:name ?name .
}
```

**`valid_rule/templates/item.tera`** (consumes only `name`):
```tera
Item: {{ name }}
Row name: {{ row["name"] }}
```

**`invalid_unbound_template_var/templates/item.tera`** (TPL-001 trigger — `title` unbound):
```tera
Item title: {{ row["title"] }}
```

**`inline_query_valid/ggen.toml`** (inline query path):
```toml
[[generation.rules]]
name = "items"
query = { inline = "PREFIX ex: <http://example.org/tpl001#> SELECT ?name WHERE { ?s a ex:Item . ?s ex:name ?name . }" }
template = { file = "templates/item.tera" }
output_file = "out.txt"
```

**`missing_template_file/ggen.toml`** (template file does not exist → index issue):
```toml
[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/missing.tera" }   # no such file on disk
output_file = "out.txt"
```

**`output_path_unbound_next_phase/ggen.toml`** (GGEN-OUT-001 candidate, ignored test):
```toml
[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out/{{ slug }}.txt"   # `slug` unbound by query — next-phase OUT-001
```

All `schema/domain.ttl` files are minimal valid Turtle declaring `ex:Item`s with `ex:name`, so `[ontology] source` resolves for `ggen_core` manifest parsing.

---

## 4. Exact API symbols depended on (from Agents 1 & 2)

Import block at top of `ggen_tpl_001.rs`:
```rust
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};
use ggen_lsp::analyzers::detect_tpl_001;
use ggen_lsp::project_index::ProjectIndex;
```

| Symbol | Owner | Real signature / shape (verified against landed src) |
|--------|-------|-------------------------------------------------------|
| `ggen_lsp::project_index::ProjectIndex` | Agent 1 | struct with public fields `root: PathBuf`, `rule_entries: Vec<RuleIndexEntry>` (`project_index.rs:52`) |
| `ProjectIndex::from_root(&Path) -> Result<ProjectIndex, IndexError>` | Agent 1 | **Result-returning** (`project_index.rs:70`) — test `load()` helper unwraps with a panic on error |
| `ProjectIndex.rule_entries` (public field) | Agent 1 | iterated in test #4 via `project.rule_entries.iter()` |
| `RuleIndexEntry.issues: Vec<String>` (public field) | Agent 1 | `rule_index.rs:46` — used in test #4: `!entry.issues.is_empty()` |
| `ggen_lsp::analyzers::detect_tpl_001(&ProjectIndex) -> Vec<(PathBuf, Vec<Diagnostic>)>` | Agent 2 | `analyzers/mod.rs:32` — per-file grouping of `tower_lsp::lsp_types::Diagnostic` |
| `Diagnostic.code == Some(NumberOrString::String("GGEN-TPL-001"))` + `severity == Some(DiagnosticSeverity::ERROR)` | Agent 2 | asserted typed (see §2 notes) |

The test file was corrected to the REAL API above after inspecting the landed `src/` (initial draft had assumed a bare `ProjectIndex` return and a `.rules()` method; both corrected). Fixtures are API-agnostic and unchanged.

---

## 5. Command results

Commands to run:
```
cargo test -p ggen-lsp --test ggen_tpl_001 -- --nocapture
cargo test -p ggen-lsp
```

**REAL captured output (the branch evolved during the session — agents landed concurrently).**

### A. The Agent-4 integration test — GREEN

`cargo test -p ggen-lsp --test ggen_tpl_001 -- --nocapture` (exit 0):
```
     Running tests/ggen_tpl_001.rs (target/debug/deps/ggen_tpl_001-7d97fa291265ebb5)
running 6 tests
test result: ok. 5 passed; 0 failed; 1 ignored; 0 measured; 0 filtered out; finished in 0.00s
___EXIT=0___
```
All 5 active tests pass; the GGEN-OUT-001 next-phase test is correctly `ignored` (1 ignored). This proves GGEN-TPL-001 end-to-end from real project context: `ProjectIndex::from_root` → `detect_tpl_001` over hermetic on-disk fixtures.

> Note: an earlier mid-session run showed `--test ggen_tpl_001` failing with 3 lib errors in Agent-1 `src/` (`parse_manifest_file` not in scope @ `project_index.rs:79`; two `PathBuf` Display errors @ `rule_index.rs:99,106`). Agent 1 fixed those concurrently; the dedicated test target is now GREEN as shown above.

### B. Full crate `cargo test -p ggen-lsp` — RED, but NOT in any Agent-4 file

`cargo test -p ggen-lsp` (exit 101):
```
   Compiling ggen-lsp v26.5.29 (/Users/sac/ggen/crates/ggen-lsp)
error[E0308]: mismatched types  --> crates/ggen-lsp/src/rule_index.rs:260:23
    | file: "query.rq".to_string(),        expected `PathBuf`, found `String`
error[E0308]: mismatched types  --> crates/ggen-lsp/src/rule_index.rs:287:23
    | file: "does_not_exist.tera".to_string(),  expected `PathBuf`, found `String`
error[E0308]: mismatched types  --> crates/ggen-lsp/src/rule_index.rs:314:23
    | file: "absent.rq".to_string(),       expected `PathBuf`, found `String`
error: could not compile `ggen-lsp` (lib test) due to 3 previous errors
___EXIT=101___
```

**Interpretation / ownership:**
- All 3 `E0308` errors are inside **Agent-1-owned `src/rule_index.rs`** — its inline `#[cfg(test)] mod tests` unit tests construct `QuerySource::File { file: "...".to_string() }` / `TemplateSource::File { ... }` with `String` where the type now expects `PathBuf`. (Likely Agent 1 changed the field to `PathBuf` but missed updating its own unit-test literals.)
- These live in the **`--lib`** compilation unit, which is separate from the **`--test ggen_tpl_001`** integration target (section A) — that is why the integration target is GREEN while the full crate run is RED.
- Per guardrails, **Agent 4 must NOT edit `src/`**. Fix is trivial and Agent-1-owned: append `.into()` (or wrap in `PathBuf::from(...)`) on the three string literals at `rule_index.rs:260,287,314`, exactly as `rustc` suggests.
- **No Agent-4-owned file (`tests/ggen_tpl_001.rs`, fixtures) contains any error.** The Agent-4 deliverable is complete and passing on its own target.

**Next action for the coordinator (Agent 5 / Agent 1):** apply the 3 `.into()` fixes in `src/rule_index.rs`, then re-run `cargo test -p ggen-lsp` to confirm the whole crate (lib unit tests + all integration targets) is GREEN.

---

## 6. Guardrails honored
- No `src/` edits, no `Cargo.toml` edits, no other-crate example/fixture mutation.
- No commit / push / tag.
- No mocks / test doubles — real files on disk, real `ProjectIndex`, real `detect_tpl_001`, real TempDir filesystem I/O.
- `tempfile` confirmed present in `crates/ggen-lsp/Cargo.toml` `[dev-dependencies]` (used by test #5).
