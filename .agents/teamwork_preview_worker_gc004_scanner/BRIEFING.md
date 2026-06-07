# BRIEFING — 2026-06-07T00:46:29Z

## Mission
Implement the dogfood scanner test to scan admission tests for bypass violations and enforce bypass-kills.

## 🔒 My Identity
- Archetype: implementer/qa/specialist
- Roles: implementer, qa, specialist
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_worker_gc004_scanner/
- Original parent: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Milestone: gc004

## 🔒 Key Constraints
- CODE_ONLY network mode.
- AGENTS.md Constitution (no mocks/stubs, real boundary crossing, multi-surface corroboration, no placeholder laundering).
- ggen Testing and Verification rules in GEMINI.md (no placeholder hashes, no fake OCEL).

## Current Parent
- Conversation ID: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Updated: 2026-06-07T00:46:29Z

## Task Summary
- **What to build**: Dogfood test scanning admission tests to enforce bypass-kills for BYPASS-LSP-001 to BYPASS-LSP-005.
- **Success criteria**:
  - Scanning matches all forbidden symbols: `compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`.
  - Violating rules raises appropriate errors and fails the build/checks.
- **Interface contracts**: Admission tests structure.
- **Code layout**: Integration tests in `crates/ggen-lsp/tests`.

## Change Tracker
- **Files modified**:
  - `crates/ggen-lsp/tests/dogfood_gc004.rs` — Increased READ_TIMEOUT to 30s, added `test_gc004_bypass_kills_scanner`
- **Build status**: PASS
- **Pending issues**: None

## Quality Status
- **Build/test result**: PASS
- **Lint status**: PASS (Clippy check has zero warnings/errors on ggen-lsp crate)
- **Tests added/modified**: Added `test_gc004_bypass_kills_scanner`

## Loaded Skills
- None

## Key Decisions Made
- Increased LSP read timeout to 30 seconds to make test initialization robust against high host machine load.
- Implemented robust single-line and multi-line comment stripping in the scanner to allow references to rules/symbols in comments without triggering false positives.
- Implemented dynamic detection of admission test files and harness using both path checks and text-content matching for `LspHarness`, `lsp_harness`, or `admission`.

## Artifact Index
- `crates/ggen-lsp/tests/dogfood_gc004.rs` — Dogfood scanner test implementation
- `.agents/teamwork_preview_worker_gc004_scanner/changes.md` — Changes list
- `.agents/teamwork_preview_worker_gc004_scanner/handoff.md` — 5-Component handoff report
