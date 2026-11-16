---
auto_load: true
priority: high
version: 7.0.0
---

# 🔧 Development Workflow (4 Steps)

1. **RDF spec**: edit `.specify/specs/NNN-feature/feature.ttl` (source) →
   `ggen graph validate --files <path>` (bare `ggen validate` doesn't exist) →
   `ggen sync run --dry-run` (preview; `--dry_run true` doesn't work).
2. **Chicago TDD**: write failing test in `crates/ggen-engine/tests/` (RED, `ggen-engine` is
   live, `ggen-core` deleted) → `just test` (fails; `test-lib` is `--lib` only) → implement →
   `just test` (GREEN) → `just pre-commit` (refactor, maintain GREEN). The `pre-commit:`
   dependency line in the `justfile` is the sole source of truth for the gate list. The line ends with the promoted courts — `court-canonical court-vacuity
   court-envelope court-typed-refusal court-fresh-epoch` (each witnessed GREEN under 60s
   before wiring; compile-fail probes are batched into one cargo invocation).
   Standalone: `just courts` runs all five (also in `pre-commit`).
3. **Generate**: `ggen sync run --dry-run` (preview) → `ggen sync run` (full sync w/ receipt;
   `just sync`/`sync-dry` currently run broken internal commands — use `ggen sync run`
   directly). Gate authoring: `ggen court compile` compiles a mutant catalog (TTL, one
   `vc:Mutant` per catalog) into shim-court artifacts with typed exits.
4. **Commit with evidence**: `just pre-commit` → commit message citing real gate/test counts,
   e.g. `[Receipt] just pre-commit: ✓ N/N gates`, `[Receipt] just test: ✓ N/N tests`.
   Refusals are typed and fail closed: parse errors surface as `[E-PARSE-{DIALECT}]`
   (position-bearing) and `[FD-*]` codes; a stale/unreadable FreshEpochLease slot refuses
   with `[FM-WRITE-012]`, mirrored by the CLI as exit code 5. Treat any exit-5 output as an
   Andon signal, not a retryable warning.
