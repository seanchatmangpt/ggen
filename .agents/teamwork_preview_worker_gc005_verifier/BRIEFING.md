# BRIEFING — 2026-06-06T17:34:29-07:00

## Mission
Implement and align wasm4pm-lsp and gc005-wasm4pm-adapter to verify diagnostics against sealed read-only authorities.

## 🔒 My Identity
- Archetype: teamwork_preview_worker
- Roles: implementer, qa, specialist
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_worker_gc005_verifier
- Original parent: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Milestone: gc005_verification

## 🔒 Key Constraints
- CODE_ONLY network mode.
- AGENTS.md / Verification Constitution compliance.
- No mocks, stubs, TODOs, or cheating.

## Current Parent
- Conversation ID: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Updated: not yet

## Task Summary
- **What to build**: Implement/align wasm4pm-lsp, gc005-wasm4pm-adapter, dogfood_gc005.rs template projection, and verify diagnostics.
- **Success criteria**: FIT -> WASM4PM-VERDICT-FIT, Missing boundary -> WASM4PM-CONFORMANCE-BLOCKED, Artifact-before-staging -> WASM4PM-REPLAY-DEVIATION, Broken receipt chain -> WASM4PM-DIGEST-CHAIN-BROKEN or WASM4PM-CONFORMANCE-BLOCKED.
- **Interface contracts**: PROJECT.md, AGENTS.md
- **Code layout**: crates/wasm4pm-lsp, crates/gc005-wasm4pm-adapter

## Key Decisions Made
- Added `gc005-wasm4pm-adapter` and `wasm4pm-lsp` to workspace members so they are compiled as first-class citizens.
- Copied `playground` ocel fixtures from `tower-lsp-max` to `crates/playground` in `ggen` to allow relative paths to resolve correctly in tests.
- Aligned `wasm4pm-lsp` path resolution logic to pop `deps/` dynamically when run as an integration test.
- Implemented structural/JSON-based OCEL checking in `gc005-wasm4pm-adapter` to avoid substring check errors.
- Disabled automatic test discovery in `crates/ggen-projection/Cargo.toml` (`autotests = false`) to prevent duplicate target errors for projected tests.

## Change Tracker
- **Files modified**:
  - `Cargo.toml`: Added workspace members.
  - `crates/gc005-wasm4pm-adapter/src/main.rs`: Implemented sealed authority checks and ocel verdict translations.
  - `crates/wasm4pm-lsp/src/main.rs`: Updated to delegate to `gc005-wasm4pm-adapter` with dynamic target path checking.
  - `crates/ggen-pack-proofs/templates/dogfood_gc005.rs.tmpl`: Changed lsp bin path resolution to be dynamic.
  - `crates/ggen-projection/Cargo.toml`: Added `tower-lsp` dev-dependency, added `autotests = false`.
- **Build status**: PASS
- **Pending issues**: None

## Quality Status
- **Build/test result**: PASS (All 26/26 tests passed)
- **Lint status**: 0 clippy warnings/violations
- **Tests added/modified**: Projected `dogfood_gc005.rs` from template, validating stdio LSP wire flow and GC005 diagnostics.

## Artifact Index
- `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/src/main.rs` — Adapter source code
- `/Users/sac/ggen/crates/wasm4pm-lsp/src/main.rs` — LSP observer source code
- `/Users/sac/ggen/crates/ggen-projection/tests/dogfood_gc005.rs` — Projected test file
