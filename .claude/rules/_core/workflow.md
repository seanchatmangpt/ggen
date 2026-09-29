---
auto_load: true
priority: high
version: 6.0.0
---

# 🔧 Development Workflow (4 Steps)

1. **RDF spec**: edit `.specify/specs/NNN-feature/feature.ttl` (source) →
   `ggen graph validate --files <path>` (bare `ggen validate` doesn't exist) →
   `ggen sync run --dry-run` (preview; `--dry_run true` doesn't work).
2. **Chicago TDD**: write failing test in `crates/ggen-engine/tests/` (RED, `ggen-engine` is
   live, `ggen-core` deleted) → `just test` (fails; `test-lib` is `--lib` only) → implement →
   `just test` (GREEN) → `just pre-commit` (refactor, maintain GREEN).
3. **Generate**: `ggen sync run --dry-run` (preview) → `ggen sync run` (full sync w/ receipt;
   `just sync`/`sync-dry` currently run broken internal commands — use `ggen sync run` directly).
4. **Commit with evidence**: `just pre-commit` → commit message citing real gate/test counts,
   e.g. `[Receipt] just pre-commit: ✓ N/N gates`, `[Receipt] just test: ✓ N/N tests`.
