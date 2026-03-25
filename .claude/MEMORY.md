# ggen Team Memory

Curated institutional knowledge. Add entries when Claude makes a correctable mistake — each correction prevents that class of error permanently.

**Auto-memory** (Claude-managed): `~/.claude/projects/ggen/memory/MEMORY.md`
**This file** (team-managed): compounding corrections checked into git.

---

## Compounding Corrections

> Format: **[Date] Problem → Fix**

<!-- Add entries here as they are discovered. Example:
[2026-02-10] Claude used `cargo test` directly → Always use `cargo make test`
[2026-02-15] Claude saved output to root directory → Always use crates/*/src/, tests/, docs/
-->

## Patterns to Avoid

- Direct `cargo` commands (always `cargo make`)
- `unwrap()`/`expect()` in production code (use `Result<T,E>`)
- Editing `.md` files in `.specify/` (source of truth is `.ttl`)
- Saving files to project root (use proper subdirectories)
- Single-message sequential agent launches (always batch)

## Project-Specific Context

- RDF pipeline: μ₁-normalize → μ₂-extract → μ₃-emit → μ₄-canonicalize → μ₅-receipt
- 30 workspace crates; core: `ggen-core`, `ggen-cli`, `ggen-domain`, `ggen-utils`
- Test target: 80%+ coverage, mutation score ≥60%
- SLOs: first build ≤15s, incremental ≤2s, RDF processing ≤5s/1k triples
- Rust edition 2021, version 1.91.1
- Chicago TDD (state-based, real collaborators, no mocks)

## Recently Resolved Issues

<!-- Log significant debugging sessions here so future agents don't repeat them -->
