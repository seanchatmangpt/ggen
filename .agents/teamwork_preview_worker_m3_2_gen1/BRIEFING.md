# BRIEFING — 2026-06-09T06:40:11Z

## Mission
Remediate cargo test and clippy errors in `ggen` codebase by ignoring obsolete tests, fixing feature gating for benchmarks, and implementing serialize/deserialize derivations.

## 🔒 My Identity
- Archetype: teamwork_preview_worker
- Roles: implementer, qa, specialist
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_worker_m3_2_gen1
- Original parent: 6fd56682-eed8-4195-a712-b264ed30c178
- Milestone: m3_2_gen1

## 🔒 Key Constraints
- CODE_ONLY network mode: No external websites/services, no HTTP clients targeting external URLs.
- No cheating (genuine implementations only).
- Do not use in-place stream editing (like sed/awk). Use replace or write_file.
- Verification command: `cargo test --workspace --all-targets` and `cargo clippy --workspace --all-targets --all-features -- -D warnings`.

## Current Parent
- Conversation ID: 6fd56682-eed8-4195-a712-b264ed30c178
- Updated: not yet

## Task Summary
- **What to build**: Ignore obsolete tests, adjust feature gates, add serde traits.
- **Success criteria**: Code compiles, clippy passes cleanly, workspace test suite passes.
- **Interface contracts**: Obsolete subcommands (ai, template, marketplace, project, graph, ontology) tests are ignored.
- **Code layout**: Source in crates, metadata in .agents/.

## Key Decisions Made
- None yet

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_worker_m3_2_gen1/handoff.md — Handoff report documenting modifications and verification results.
