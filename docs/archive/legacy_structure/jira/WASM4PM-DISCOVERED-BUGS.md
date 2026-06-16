<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [WASM4PM-DISCOVERED-BUGS: ggen Defects Fixed & Gaps Addressed](#wasm4pm-discovered-bugs-ggen-defects-fixed--gaps-addressed)
  - [BUG-001: Manifest silently ignores unknown fields](#bug-001-manifest-silently-ignores-unknown-fields)
  - [BUG-002: Frontmatter machinery unreachable from `ggen sync`](#bug-002-frontmatter-machinery-unreachable-from-ggen-sync)
  - [BUG-003: GGEN-YIELD-001 detector missing in ggen-lsp](#bug-003-ggen-yield-001-detector-missing-in-ggen-lsp)
  - [BUG-004: mode="Create" implementation silently skips](#bug-004-modecreate-implementation-silently-skips)
  - [BUG-005: No `ggen verify` CLI subcommand](#bug-005-no-ggen-verify-cli-subcommand)
  - [BUG-006: SELECT row order nondeterministic (No ORDER BY)](#bug-006-select-row-order-nondeterministic-no-order-by)
  - [BUG-007: SELECT * disables provision checks](#bug-007-select--disables-provision-checks)
  - [BUG-008: Pack &#91;pack.outputs&#93; indirection not resolved](#bug-008-pack-packoutputs-indirection-not-resolved)
  - [BUG-009: Identity CONSTRUCT accepted silently](#bug-009-identity-construct-accepted-silently)
  - [Additional Fixes (2026-06-11)](#additional-fixes-2026-06-11)
    - [BUG-010: LSP completion/hover docs vs Bare Context](#bug-010-lsp-completionhover-docs-vs-bare-context)
    - [BUG-011: Pack query/template source resolution](#bug-011-pack-querytemplate-source-resolution)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# WASM4PM-DISCOVERED-BUGS: ggen Defects Fixed & Gaps Addressed

**Status:** ALL ADDRESSED / FIXED (2026-06-11)
**ggen version:** 26.6.9

This document records the resolution of the 9 primary gaps and bugs discovered during the wasm4pm breed scaffold research.

---

## BUG-001: Manifest silently ignores unknown fields
- **Status:** FIXED
- **Fix:** Added `#[serde(deny_unknown_fields)]` to `OntologyConfig` and all manifest structs.
- **Verification:** `ggen sync` now fails immediately if `additional` is used instead of `imports` in `ggen.toml`.

## BUG-002: Frontmatter machinery unreachable from `ggen sync`
- **Status:** FIXED
- **Fix:** Wired `TemplateWithMeta` into the `codegen/pipeline.rs` rendering loop. Frontmatter is now stripped before Tera rendering, and `to:` paths are respected.
- **Verification:** Breed templates with frontmatter now render correctly into their target paths.

## BUG-003: GGEN-YIELD-001 detector missing in ggen-lsp
- **Status:** ADDRESSED
- **Fix:** Implemented `detect_yield_001` in `ggen-lsp`. It flags `output_file` patterns that escape the project root.
- **Verification:** LSP now emits `GGEN-YIELD-001` error for `../escaped/out.rs`.

## BUG-004: mode="Create" implementation silently skips
- **Status:** FIXED
- **Fix:** Updated `codegen/pipeline.rs` to return a hard error (`E0011`) if the output file already exists in `Create` mode.
- **Verification:** `ggen sync` fails if a scaffolded file would be overwritten in Create mode.

## BUG-005: No `ggen verify` CLI subcommand
- **Status:** ADDRESSED
- **Fix:** Added `ggen receipt verify` (and `info`) verbs via `clap-noun-verb`.
- **Verification:** `ggen receipt verify .ggen/receipts/latest.json` validates Ed25519 signatures and chain integrity.

## BUG-006: SELECT row order nondeterministic (No ORDER BY)
- **Status:** ADDRESSED
- **Fix:** Added `E0013` (GGEN-QUERY-001 equivalent) warning in `SparqlAnalyzer`.
- **Verification:** LSP surfaces warning: "SELECT query lacks ORDER BY — required when strict_mode is enabled".

## BUG-007: SELECT * disables provision checks
- **Status:** ADDRESSED
- **Fix:** Added `GGEN-QUERY-002` (SELECT * advisory) in `TeraAnalyzer` and `detect_query_002` in project index.
- **Verification:** LSP surfaces warning: "SELECT * disables provision checks — use explicit projections".

## BUG-008: Pack [pack.outputs] indirection not resolved
- **Status:** ADDRESSED
- **Fix:** Added `GGEN-PACK-001` diagnostic in `detect_pack_001` to inform authors that pack-sourced contents disable author-time checks. Added `PackageToml::resolve_output_key` in `types.rs`.
- **Verification:** LSP emits warning when using `{ pack = ... }` sources.

## BUG-009: Identity CONSTRUCT accepted silently
- **Status:** ADDRESSED
- **Fix:** Added `E0015` (Identity CONSTRUCT) warning in `SparqlAnalyzer`.
- **Verification:** LSP surfaces warning: "Identity CONSTRUCT query detected — this is a no-op mapping".

---

## Additional Fixes (2026-06-11)

### BUG-010: LSP completion/hover docs vs Bare Context
- **Status:** FIXED
- **Fix:** Updated `TeraAnalyzer` hover/completion text to reflect the actual context shape: bare `{{ name }}` for per-row variables. Retained `row.name` as a legacy alias for backwards compatibility.

### BUG-011: Pack query/template source resolution
- **Status:** ADDRESSED
- **Fix:** Integrated `PackageToml` resolution into the pipeline and added `GGEN-PACK-001` diagnostics to the LSP to surface resolution issues as warnings.
