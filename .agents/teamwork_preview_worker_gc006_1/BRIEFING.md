# BRIEFING — 2026-06-06T17:47:14-07:00

## Mission
Remove hardcoded paths in dogfood_gc006 templates, sync projection, run/verify tests, verify source laws and check workspace constraints.

## 🔒 My Identity
- Archetype: teamwork_preview_worker
- Roles: implementer, qa, specialist
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_worker_gc006_1/
- Original parent: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Milestone: dogfood_gc006

## 🔒 Key Constraints
- CODE_ONLY network mode. No external HTTP/curl/wget.
- No mocks, stubs, or hardcoded test results.
- Sealed workspaces ~/wasm4pm and ~/wasm4pm-compat must remain 100% read-only.

## Current Parent
- Conversation ID: 6ad094c2-b1ff-4d0a-8070-a705c371409d
- Updated: yes

## Task Summary
- **What to build**: Resolve hardcoded paths in templates/dogfood_gc006.rs.tmpl, project it using sync_target, run the generated test, and perform the source laws verification.
- **Success criteria**: All cargo make check/test runs compile and pass cleanly; verification of no shadow wasm4pm crates, no conformance/replay in wasm4pm-lsp, neutral adapter gc005-wasm4pm-adapter does not emit fake FIT and calls sealed wasm4pm authority.
- **Interface contracts**: `/Users/sac/ggen/crates/ggen-pack-proofs/`
- **Code layout**: Rust project crates structure.

## Key Decisions Made
- Use relative path and environment variables (`CARGO_MANIFEST_DIR`) to resolve paths dynamically inside `dogfood_gc006.rs.tmpl` template, removing all hardcoded `/Users/sac` values.

## Artifact Index
- `/Users/sac/ggen/.agents/teamwork_preview_worker_gc006_1/changes.md` — Changes made.
- `/Users/sac/ggen/.agents/teamwork_preview_worker_gc006_1/handoff.md` — Handoff report.

## Change Tracker
- **Files modified**:
  - `crates/ggen-pack-proofs/templates/dogfood_gc006.rs.tmpl`: Converted hardcoded paths to dynamic logic based on workspace root search.
  - `crates/ggen-projection/Cargo.toml`: Registered `dogfood_gc006` integration test.
  - `crates/ggen-projection/tests/dogfood_gc006.rs`: Generated/projected dynamically.
- **Build status**: Pass
- **Pending issues**: None

## Quality Status
- **Build/test result**: Pass
- **Lint status**: 0 violations/warnings
- **Tests added/modified**: `crates/ggen-projection/tests/dogfood_gc006.rs`

## Loaded Skills
- **Source**: None
- **Local copy**: None
- **Core methodology**: None
