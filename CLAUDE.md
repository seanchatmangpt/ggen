# SPR: ggen Project Configuration (v6.0.0)

## Core Identity
ggen = specification-driven code generator in Rust implementing equation A = μ(O) where code precipitates from RDF ontology via five-stage deterministic pipeline.

## Fundamental Laws
1. RDF specifications (.ttl files) are immutable source of truth; all artifacts (code, docs, markdown) are generated projections
2. Cargo make commands exclusively; direct cargo invocations prohibited (enforces SLOs, timeouts, quality gates)
3. Andon signals (compiler errors, test failures, warnings) trigger immediate work stoppage until root cause resolved
4. Parallel execution mandatory: batch all operations (todos 10+, agents, file ops) in single message
5. Result<T,E> throughout production; unwrap/expect forbidden (clippy -D warnings enforced)

## Architecture Equation
μ = five-stage transformation pipeline:
- μ₁ (Normalize): RDF validation, SHACL shapes, dependency resolution
- μ₂ (Extract): SPARQL queries, OWL inference, rule execution
- μ₃ (Emit): Tera template rendering, code generation
- μ₄ (Canonicalize): Deterministic formatting, content hashing
- μ₅ (Receipt): Cryptographic proof generation, audit trail

## Quality Enforcement (Poka-Yoke)
- Warnings-as-errors (clippy -D warnings)
- Chicago TDD mandatory: state-based, real collaborators, AAA pattern (Arrange/Act/Assert)
- Six pre-flight gates prevent defects before μ₁
- Deterministic receipts with SHA-256 hashing for reproducibility verification
- SLO targets: first build ≤15s, RDF processing ≤5s/1k triples, incremental ≤2s

## Workspace Structure
30 crates organized in domains: Core System (8), CLI/Validation (3), Testing (3), AI Orchestration (2), Ontology (1), KNHK Systems (6), RevOps (4), Marketplace (1), Folk Strategy (1). Root never stores working files; subdirectories mandatory.

## Agent Coordination Protocol
Task tool spawns real agents concurrently; MCP tools coordinate topology only. Every agent executes pre-task hooks, coordinates via memory, runs post-task validation, verifies Andon signals cleared.

## Holographic Factory Metaphor
Substrate = RDF ontology as high-dimensional holographic film encoding domain knowledge. Transformation = five-stage pipeline precipitates code from interference patterns. Corollary: bugs fixed in RDF spec, not generated output.

## Type-First Thinking
Types encode invariants; compiler as design tool. Invalid states unrepresentable. PhantomData for type-level state machines. Zero-cost abstractions via generics/const generics/macros. Memory safety through ownership semantics.

## 80/20 Elite Mindset
Generate three ideas: (1) immediate solution (2) 80% of related problems with 20% effort (sweet spot) (3) maximum value with type-level guarantees. Quality includes maintainability/consistency—not optional.

## Definition of Done
Seven mandatory validations: timeout-check exists, cargo make check passes (zero errors/warnings), cargo make test passes (all green), cargo make lint clean, SLOs met, signals cleared, deterministic receipts generated.

## EPIC 9 (Big Bang 80/20)
For non-trivial tasks: (1) Specification closure in .ttl (2) Fan-out 10 parallel agents (3) Collision detection (4) Convergence via selection pressure (5) Refactoring (6) Cryptographic closure. Eight specialized agents, eleven domain skills available.

## Prohibited Patterns
Direct cargo commands, unwrap/expect in production, root directory file creation, TODO comments without FUTURE prefix, placeholders, unimplemented!(), meaningless tests, suppressing Andon signals, proceeding with signals present.

## Critical Reminders
Specs before code. Receipts over narratives. RDF is reality. Andon stops work. Cargo make is law. Parallel first. V6 unified command: ggen sync. Quality gates non-negotiable. Determinism always. Tests are truth.

---

## Essential Commands (v6)

```bash
# V6 UNIFIED COMMAND
ggen sync                           # Full pipeline (μ₁-μ₅)
ggen sync --dry_run true            # Preview without writing
ggen sync --validate_only true      # Pre-flight gates only
ggen sync --audit true              # Cryptographic audit trail
ggen init                           # Initialize ggen project

# CARGO MAKE (MANDATORY - NEVER DIRECT CARGO)
cargo make check       # <5s    compile check, warnings-as-errors
cargo make test-unit   # <16s   fast feedback, Chicago TDD
cargo make test        # <30s   full suite with timeouts
cargo make lint        # <60s   clippy -D warnings, rustfmt
cargo make pre-commit  # <2min  check → lint → test-unit

# SPECIFICATION WORKFLOW (RDF-FIRST)
cargo make speckit-check     # Verify TTL specs exist
cargo make speckit-validate  # SHACL validation
cargo make speckit-render    # Regenerate markdown from TTL
cargo make speckit-full      # Full workflow: validate + render
```

## File Organization

```
ggen/
├── .specify/              # RDF SPECIFICATIONS (SOURCE OF TRUTH)
│   └── specs/NNN-*/       # feature.ttl, entities.ttl, plan.ttl (EDIT)
│                          # spec.md (GENERATED - DO NOT EDIT)
├── crates/                # 30 RUST CRATES
│   ├── ggen-core/         # RDF, SPARQL, templates (4.2M)
│   ├── ggen-cli/          # CLI entry (1.8M)
│   ├── ggen-domain/       # Business logic (1.6M)
│   ├── ggen-utils/        # Utilities (431K)
│   └── [26 more crates]
├── tests/                 # Workspace tests (integration, BDD)
├── benches/              # Performance benchmarks (15+ suites)
├── .claude/              # Claude Code config (agents, hooks, skills)
├── Makefile.toml         # 70+ build targets (timeouts enforced)
└── Cargo.toml            # Workspace manifest (30 members)
```

## Andon Signal Workflow

```
🔴 RED (Compilation/test error)    → STOP immediately
🟡 YELLOW (Warnings/deprecations)   → Investigate before release
🟢 GREEN (All checks pass)          → Proceed safely
```

### Mandatory Checks Before Completion:
1. `cargo make timeout-check` - Verify timeout exists
2. `cargo make check` - Zero errors/warnings
3. `cargo make test` - All tests pass
4. `cargo make lint` - Clippy clean
5. `cargo make slo-check` - SLOs met
6. Batch create 10+ todos for systematic fixing if signals found
7. Only mark complete when ALL signals cleared

## Development Workflow (v6)

```bash
# 1. Create RDF specification (SOURCE)
mkdir -p .specify/specs/013-feature
cp .specify/templates/rdf-helpers/user-story.ttl.template \
   .specify/specs/013-feature/feature.ttl
vim .specify/specs/013-feature/feature.ttl
cargo make speckit-render

# 2. Chicago TDD (RED → GREEN → REFACTOR)
vim crates/ggen-core/tests/feature_test.rs  # Write failing test
cargo make test-unit                         # Verify RED
vim crates/ggen-core/src/feature.rs         # Minimal implementation
cargo make test-unit                         # Verify GREEN
cargo make pre-commit                        # Refactor

# 3. Generate from ontology
ggen sync --audit true

# 4. Commit with evidence
cargo make pre-commit
git commit -m "feat(013): Feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates
[Receipt] cargo make test: ✓ 347/347 tests, 28.3s
[Receipt] ggen sync --audit: ✓ 12 files, 0 conflicts"
```

## Stack (v6)

| Component | Version | Purpose |
|-----------|---------|---------|
| Rust | 1.91.1 | Type-safe, zero-cost abstractions |
| Oxigraph | 0.5.1 | RDF store (SPARQL 1.1, inference) |
| Tera | 1.20 | SPARQL-aware template engine |
| Clap | 4.5 | CLI framework (derive API) |
| genai | 0.5 | Multi-provider LLM client |
| chicago-tdd-tools | 1.4.0 | AAA testing, real objects |
| proptest | 1.8 | Property-based testing |
| criterion | 0.7 | Performance benchmarking |
| testcontainers | 0.25 | Integration testing |
| OpenTelemetry | 0.21 | Observability (optional: --features otel) |

## EPIC 9 Workflow

**Trigger**: Non-trivial tasks (5+ files, 3+ systems, multiple approaches, unclear requirements)

```
1. Specification Closure (100% in .specify/*.ttl)
   ↓
2. Fan-Out (10 parallel agents)
   ↓
3. Collision Detection (overlap analysis)
   ↓
4. Convergence (coverage, invariants, minimality)
   ↓
5. Refactoring (DRY, type-safety, performance)
   ↓
6. Closure (receipts, cryptographic proof)
```

**Specialized Agents**: bb80-specification-validator, bb80-parallel-task-coordinator, bb80-collision-detector, bb80-convergence-orchestrator, rust-coder, reviewer, speckit-architect, test-engineer

**Domain Skills**: cargo-make-protocol, chicago-tdd-pattern, poka-yoke-patterns, rdf-ontologies, bb80-specification-closure, bb80-parallel-agents, bb80-deterministic-receipts, bb80-invariant-construction

## Key Associations

- Types = invariants = compile-time guarantees
- Zero-cost = generics/macros/const generics
- Performance = references/stack/minimize allocations
- Ownership = explicit = memory safety
- APIs = type-safe = ergonomic = composable
- Tests = observable outputs = behavior verification
- 80/20 = second idea = sweet spot = maximum value
- Andon Signals = stop = fix = verify
- DfLSS = prevent defects AND waste from start

## Remember

**Claude Flow coordinates, Claude Code creates!**

**Stop the line when Andon signals appear - fix root cause before proceeding!**

**Always use `cargo make` - NEVER direct cargo commands!**

**TodoWrite always has 10+ todos in a single batch!**

---

## Quality Checklist (Production Standard)

1. `cargo make check` passes (zero errors, warnings-as-errors)
2. `cargo make lint` passes (zero warnings, clippy -D warnings)
3. `cargo make test` passes (all tests green, <30s SLO)
4. No unwrap/expect in production code (clippy enforced)
5. All APIs return Result<T, E> (no naked errors)
6. Deterministic outputs (same input → same output, verified by receipts)
7. SHACL validation passes for all .specify/*.ttl files

---

**Last Updated**: 2026-01-25 (v6.0.0 SPR compression from 1326 lines → ~250 lines)
