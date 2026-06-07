# BRIEFING — 2026-06-06T23:02:05Z

## Mission
Investigate crates/ggen-lsp/ interaction with equation enforcement receipts and verify diagnostic triggers (GGEN-EVIDENCE-001, GGEN-DRIFT-001).

## 🔒 My Identity
- Archetype: Teamwork explorer
- Roles: Teamwork explorer, Read-only investigator
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_explorer_gc003_2/
- Original parent: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Milestone: crates/ggen-lsp investigation

## 🔒 Key Constraints
- Read-only investigation — do NOT implement
- Do not write or edit any source files (only write to our own folder)
- CODE_ONLY network mode: no external requests, no curl/wget/lynx

## Current Parent
- Conversation ID: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Updated: 2026-06-06T23:02:05Z

## Investigation State
- **Explored paths**:
  - `crates/ggen-lsp/src/handlers/diagnostics.rs`
  - `crates/ggen-lsp/src/state.rs`
  - `crates/ggen-lsp/src/check.rs`
  - `crates/ggen-projection/src/receipt.rs`
  - `crates/ggen-projection/tests/f4_lsp_diagnostics.rs`
- **Key findings**:
  - `ggen-lsp` does not import or use `ReceiptIndex` or `validate_sync` from `crates/ggen-projection`.
  - Diagnostics like `GGEN-EVIDENCE-001` and `GGEN-DRIFT-001` in `ggen-lsp` do not read `receipts.json` / `receipts.jsonl` from disk.
  - Instead, the diagnostics rely entirely on simple editor-buffer pattern-matching heuristics (e.g. checking for string `"drifted"` in `main.rs`, `"random_corrupt_bytes"` in `receipts.json`, or absence of `"pub mod cli"` in `lib.rs`).
- **Unexplored areas**: None.

## Key Decisions Made
- Confirmed decoupling of LSP diagnostics from projection-sync validation via source analysis and test review.

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_explorer_gc003_2/analysis.md — Detailed investigation report.
- /Users/sac/ggen/.agents/teamwork_preview_explorer_gc003_2/handoff.md — 5-component handoff report.
