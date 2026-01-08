# ggen: Specification-Driven Code Generation

**Core Equation**: $A = \mu(O)$ — Code (A) precipitates from RDF ontology (O) via transformation pipeline (μ).

## Three Paradigms (Non-Negotiable)

| Paradigm | Practice |
|----------|----------|
| **Big Bang 80/20** | Verify `.specify/*.ttl` closure (100%) before code gen. Single-pass. Receipts prove closure. |
| **EPIC 9** | Non-trivial tasks: 10 parallel agents → collision detection → convergence synthesis. |
| **Deterministic Receipts** | Evidence replaces narrative: `[Receipt] cargo make test: ✓ 347/347, <30s` |

## Constitutional Rules

```
🔴 RED (Compilation/test error)        → STOP immediately
🟡 YELLOW (Warnings/deprecations)      → Investigate before release
🟢 GREEN (All checks pass)             → Proceed safely
```

| Rule | Requirement |
|------|-------------|
| **Cargo Make Only** | `cargo make [target]` always (never raw `cargo`). Enforces SLOs, gates. |
| **Result<T,E>** | Production: `Result<T,E>` throughout. Tests: `unwrap()` OK. |
| **No Unwrap/Expect** | Zero in production code. Language-enforced via clippy `-D warnings`. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated artifacts). |
| **Type-First** | Constraints in types, compiler verifies. NewType for domains. Generic + zero-cost. |
| **TTL is Immutable** | Once closed, don't iterate — fix source, regenerate. |

## Essential Commands (SLO Targets)

```bash
cargo make check       # <5s    (compile check)
cargo make test-unit   # <16s   (fast feedback)
cargo make test        # <30s   (full test suite)
cargo make lint        # <60s   (clippy, format)
cargo make pre-commit  # <2min  (all three)

ggen sync              # RDF → generated code (60-80% faster than hand-coding)
```

## File Organization

```
ggen/
├── .specify/           # RDF specs (.ttl = source of truth)
├── crates/*/src/       # Rust source
├── crates/*/tests/     # Integration tests
├── docs/, templates/, benches/
└── .claude/            # Minimal: settings.json + skills/
```

**Rule**: Never create working files in root directory.

## Stack

| Component | Version | Purpose |
|-----------|---------|---------|
| Rust | 1.91.1 | Core language (type-safe, zero-cost) |
| Tokio | 1.47 | Async runtime |
| Oxigraph | 0.5.1 | RDF store (unrdf substrate) |
| Tera | 1.20 | Template engine (code generation) |
| chicago-tdd-tools | 1.4.0 | AAA testing (Arrange/Act/Assert, real objects) |
| proptest, criterion | latest | Property testing, benchmarks |

**Crates**: 17 (ggen-core, ggen-cli, ggen-domain, ggen-infer, ggen-marketplace, ggen-config, ggen-test-audit, ggen-e2e, ggen-node, ggen-macros, ...).

## Automatic (No Reminders Needed)

I implement these by default:
- `Result<T,E>` for all fallible operations
- Zero `unwrap/expect` in production
- Chicago TDD pattern (AAA: real objects, no mocks)
- Type-safe design (constraints in types, compiler verification)
- Error context mapping (`map_err`, custom types)
- Idiomatic Rust (clippy compliance, naming conventions)
- Performance awareness (SLO targets, complexity analysis)

## Holographic Factory Metaphor

**Substrate** (unrdf): RDF ontology as high-dimensional film encoding domain knowledge. **History** (KGC-4D): Git snapshots as temporal coherence waypoints. **Transformation** (ggen): Five-stage pipeline (Normalize → Extract → Emit → Canonicalize → Receipt) precipitates code. **Corollary**: Bug in code? Fix the RDF spec (interference pattern), not the output (projection).

## Quality Gates (Pre-Commit)

1. `cargo make check` passes (zero errors)
2. `cargo make lint` passes (zero warnings, `-D warnings`)
3. `cargo make test` passes (all tests green)
4. No `unwrap/expect` in production
5. All APIs return `Result<T, E>`
6. Deterministic outputs (same input → same output)

## When to Use EPIC 9

- Non-trivial tasks (affects multiple files/systems)
- Multiple valid approaches with trade-offs
- Large architectural decisions
- Unclear requirements needing exploration

**Workflow**: Specification closure → 10 parallel agents (fan-out) → collision detection → convergence → receipts.

## Remember

- **Spec Closure First**: Verify 100% coverage before code generation (non-negotiable).
- **Receipts Over Narratives**: Always produce evidence (test counts, compile times, SLO metrics).
- **RDF is Reality**: Edit `.ttl`; everything else (code, docs, configs) is generated projection.
- **Andon Stops Work**: 🔴 RED = halt immediately; 🟡 YELLOW = investigate; 🟢 GREEN = proceed.
- **Cargo Make is Law**: All validation through Makefile, never raw cargo (prevents bypassing gates).
- **Parallel First**: For non-trivial work, always use EPIC 9 (10 agents + collision + convergence).

## .claude/ Configuration

**Essential files**:
- `settings.json`: Permissions, hooks, MCP stubs
- `skills/`: bb80-* (spec closure, parallel, receipts), cargo-make-protocol, chicago-tdd-pattern, poka-yoke-patterns, rdf-ontologies, mcp-servers

**Not in critical path** (reference only, not loaded during sessions).
