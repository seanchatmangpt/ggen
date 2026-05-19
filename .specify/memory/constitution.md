<!--
═══════════════════════════════════════════════════════════════════════════════
SYNC IMPACT REPORT - ggen Constitution v1.1.0 (Sprint Learnings Integration)
═══════════════════════════════════════════════════════════════════════════════

Version Change: 1.0.0 → 1.1.0 (MINOR: new principles + guidance sections)

Version Bump Rationale (MINOR):
- Added new principle II (Infrastructure-Domain Separation) from Sprint 4 learning
- Added new section: Crate Extraction & Consolidation Patterns
- Enhanced principle I with circular dependency anti-pattern guidance
- All existing principles remain compatible (backward compatible)

Modified Principles:
- I. Crate-First Architecture: Enhanced with consolidation-ready design + circular dependency anti-pattern
- II. Infrastructure-Domain Separation: NEW principle (Sprint 4 blocker learning)
- III-X: Renumbered (Deterministic RDF Projections through Lean Six Sigma)

Added Sections:
- Crate Extraction & Consolidation Patterns: Guidelines based on ggen-receipt/ggen-config/ggen-a2a-mcp successes and ggen-domain blocker

Removed Sections:
- None (no breaking changes)

Sprint Learnings Incorporated:
- ✅ ggen-receipt → ggen-config consolidation (successful micro-crate merge)
- ✅ ggen-config extraction (6.5k LOC, clean, stable, reusable)
- ✅ ggen-a2a-mcp extraction (10k LOC, isolated, no infrastructure deps)
- ⚠ ggen-domain blocker (21.5k LOC, blocked by pack_resolver circular dependency)
- 🔄 Marketplace feature-gating pattern (optional crate documentation)

Templates Requiring Updates:
- ✅ .specify/templates/plan-template.md: Already aligned
- ✅ .specify/templates/spec-template.md: Already aligned
- ✅ .specify/templates/tasks-template.md: Already aligned
- ⚠ CLAUDE.md: Update crate count (now 6 crates: cli, config, core, a2a-mcp, marketplace, prolog8)
- ⚠ README.md: Link to constitution and clarify current architecture

Follow-up TODOs:
- Update project README.md to reference 6-crate architecture
- Document in architecture guide why ggen-domain extraction is blocked (circular dependency with pack_resolver)

═══════════════════════════════════════════════════════════════════════════════
-->

# ggen Constitution

## Core Principles

### I. Crate-First Architecture

Every feature MUST start as a standalone crate within the workspace. Crates MUST be:
- **Self-contained**: No circular dependencies between workspace crates (verified at compile time)
- **Independently testable**: Each crate has its own test suite achieving 80%+ coverage
- **Clearly scoped**: Single responsibility principle—domain logic separated from CLI, core separated from utilities
- **Documented**: Public APIs MUST have rustdoc comments with examples
- **Consolidation-ready**: Design for future consolidation (e.g., ggen-receipt merged into ggen-config successfully)

**Circular Dependency Anti-Pattern**: If extracting a new crate creates a cycle (e.g., new-crate depends on ggen-core which depends on new-crate), DO NOT extract. Instead:
1. Identify shared types/traits causing the cycle
2. Extract shared types to separate `ggen-types` crate (if justified by 80/20 analysis)
3. Or accept the module staying in ggen-core with clear documentation of scope

**Rationale**: Workspace structure enforces modularity, prevents tight coupling, and enables parallel development. The 6-crate architecture (ggen-cli, ggen-config, ggen-core, ggen-a2a-mcp, ggen-marketplace, prolog8) demonstrates this principle. Circular dependencies block compilation; they are unresolvable architectural problems, not fixable by refactoring.

### II. Infrastructure-Domain Separation

Code in ggen-core MUST maintain clear boundaries between **infrastructure** and **domain**:

**Infrastructure** (lives in ggen-core, reusable across crates):
- RDF graph management (`graph/`, `rdf/`)
- Pipeline execution framework (`pipeline/`, `parallel_generator/`)
- Template resolution and rendering (`templates/`, `template/`)
- Code generation traits (`codegen/`, `codegen_lib/`)
- Utilities and error handling (`utils/`, reusable types)

**Domain** (business logic, can be extracted to separate crates):
- Pack management and validation (`packs/`, `pack_resolver/`)
- Capability registry and resolution (`domain/packs/capability_registry.rs`)
- Validation rules and policies (`domain/validation/`)
- Marketplace package management (`marketplace/` — optional feature)

**Extraction Rule**: Domain modules MAY be extracted to separate crates IF they don't create circular dependencies. If infrastructure code depends on the domain logic, the extraction is blocked.

**Example (Sprint 4 blocker)**: `ggen-domain` extraction blocked because `pack_resolver.rs` (infrastructure in ggen-core) depends on `packs/` module. Attempted extraction created: `ggen-domain` → `ggen-core` → `ggen-domain` cycle.

**Lesson**: Before extracting a domain module, verify NO infrastructure code depends on it. If infrastructure depends on domain, accept the module staying in ggen-core.

**Rationale**: Clear boundaries prevent architectural tangles. Infrastructure must be stable and reusable; domain logic can evolve and be reorganized independently—IF it doesn't create cycles.

### III. Deterministic RDF Projections

Code generation MUST be deterministic and reproducible:
- **Same input → same output**: Identical RDF ontology + template MUST produce identical code across runs
- **Version-locked**: Dependencies in Cargo.lock MUST be tracked and reproducible
- **Content-addressable**: Generated code SHOULD be verifiable via checksums
- **Idempotent operations**: Running generation twice MUST NOT produce different results

**Rationale**: ggen's core value proposition is deterministic code generation from knowledge graphs. Non-determinism breaks trust and makes debugging impossible.

### IV. Chicago TDD (Zero Tolerance)

Test-Driven Development using Chicago School methodology is MANDATORY:
- **State-based testing**: Tests verify observable outputs, return values, and side effects (not mocks or internal calls)
- **Real collaborators**: Use actual dependencies—no mocking of Oxigraph, Tera, or file system
- **Red-Green-Refactor**: Tests MUST be written first, MUST fail before implementation, then pass after implementation
- **80%+ coverage**: Critical paths (RDF parsing, template rendering, CLI commands) MUST achieve minimum 80% code coverage
- **1,168+ tests passing**: Full test suite MUST pass before any commit

**Rationale**: Chicago TDD validates actual system behavior rather than implementation details, catching integration issues early. State-based tests survive refactoring better than mock-heavy tests.

**Exemption**: Test code (`#[cfg(test)]`, `#[test]`, `tests/`, `benches/`) MAY use `unwrap()` and `expect()` to fail fast on setup issues.

### V. cargo make Protocol

Direct `cargo` commands are PROHIBITED in development and CI. Use `cargo make` targets exclusively:
- **Fast feedback**: `cargo make check` (<5s compilation check)
- **Test execution**: `cargo make test-unit` (unit tests), `cargo make test` (full suite with timeouts)
- **Quality gates**: `cargo make lint` (Clippy), `cargo make pre-commit` (all checks)
- **Timeout enforcement**: All commands MUST respect SLO timeouts (compilation ≤15s first build, ≤2s incremental)
- **Hook integration**: Commands trigger pre-task/post-task hooks for coordination

**Rationale**: `cargo make` provides consistent timeouts, prevents hanging builds, enforces SLOs, and integrates with Claude-Flow hooks. Direct cargo can hang indefinitely on certain errors.

### VI. Type-First Thinking

Leverage Rust's type system for compile-time guarantees:
- **Express invariants in types**: Use newtypes, enums, and type states to make invalid states unrepresentable
- **Zero-cost abstractions**: Prefer generics and const generics over trait objects for performance-critical code
- **Explicit ownership**: Clone should be deliberate; prefer references and lifetimes
- **API ergonomics**: Public APIs MUST be type-safe AND easy to use (not just safe)

**Rationale**: Type-first thinking shifts errors from runtime to compile-time, reducing defects. If it compiles, invariants are enforced.

### VII. Andon Signal Protocol

Stop the line immediately when quality signals appear:
- **RED (Compilation error, test failure)**: STOP all work. Fix immediately before proceeding.
- **YELLOW (Warning, clippy lint)**: Investigate before release. May indicate technical debt.
- **GREEN (Clean output)**: Safe to continue development.

**Process**:
1. Monitor `cargo make` output for signals
2. On RED: Stop, diagnose root cause, fix, verify with `cargo make test`, continue only when GREEN
3. On YELLOW: Assess impact, decide if fix-now or defer with tracking issue
4. Definition of Done: All signals must be GREEN before marking task complete

**Rationale**: Andon signals (from Toyota Production System) prevent defects from propagating downstream. Fixing at source is 10x cheaper than fixing in production.

### VIII. Error Handling Standards

Production code MUST use `Result<T, E>` error handling:
- **NO `unwrap()` in production**: Causes panic, violates fail-safe principle
- **NO `expect()` in production**: Same as unwrap with message—still panics
- **Rich error context**: Use `thiserror` for domain errors with context chains
- **Fail-safe design**: Errors MUST be propagated with `?` operator or handled explicitly

**Exemption**: Test code (`#[cfg(test)]`, `#[test]`, `tests/`, `benches/`) MAY use `unwrap()`/`expect()` to fail fast on test setup issues.

**Rationale**: Production systems MUST handle errors gracefully. Panics crash the process and lose work. Test code SHOULD fail fast to surface issues immediately.

### IX. Concurrent Execution

All operations MUST be batched in single messages:
- **"1 MESSAGE = ALL RELATED OPERATIONS"**: TodoWrite (10+ todos), Task tool (all agents), file operations, bash commands
- **Parallel agent execution**: Use Claude Code's Task tool to spawn multiple agents concurrently (not sequentially)
- **MCP for coordination only**: MCP tools set up topology; Task tool executes actual work
- **File organization**: NEVER save working files, texts, tests to root folder—use `src/`, `tests/`, `docs/`, `scripts/`

**Rationale**: Concurrent execution provides 2.8-4.4x speed improvement and prevents coordination failures. Sequential operations waste time and risk inconsistency.

### X. Lean Six Sigma Quality

Enforce manufacturing-grade quality standards (99.99966% defect-free):
- **Poka-Yoke**: Compiler warnings treated as errors (`#![deny(warnings)]`) prevent defects at compile time
- **Comprehensive linting**: 400+ Ruff-equivalent Clippy rules with ALL categories enabled (pedantic, nursery, cargo)
- **Type coverage**: 100% type annotations on all functions—NO untyped code
- **Security scanning**: Continuous vulnerability analysis (would use `cargo audit` if integrated)
- **Mandatory pre-commit**: Git hooks run format check + cargo check automatically—CANNOT skip

**Rationale**: Design for Lean Six Sigma (DfLSS) prevents defects AND waste from the start. Prevention is cheaper than detection, which is cheaper than correction.

## Crate Extraction & Consolidation Patterns

### When to Extract a Crate

Extraction is justified when a module satisfies ALL of these (80/20 Pareto analysis):

1. **Size**: ≥5k LOC (small extractions create maintenance burden without benefit)
2. **Stability**: Module API is stable (major API changes indicate uncertain scope)
3. **Independence**: Zero infrastructure dependencies (or clear boundary in ggen-core infra layer)
4. **Reusability**: Module is used by ≥2 other crates or is a standalone library
5. **No circular deps**: Verify extraction doesn't create ggen-core ↔ new-crate cycle

**Historical Examples**:
- ✅ **ggen-config** (6.5k LOC): Stable, reusable, no infrastructure deps → Successfully extracted
- ✅ **ggen-a2a-mcp** (10k LOC): Isolated MCP protocol, no infra deps → Successfully extracted
- ✅ **ggen-receipt consolidated** (2.5k LOC): Small + simple, merged into ggen-config → Successful consolidation
- ❌ **ggen-domain** (21.5k LOC): Blocked—pack_resolver.rs (infra) depends on packs/ module → Circular dependency, extraction blocked

### When to Consolidate Crates

Consolidation is justified when a crate satisfies ANY of these:

1. **Micro-crate**: <2k LOC with few dependents (maintenance overhead > value)
2. **Tight coupling**: >90% of functionality depends on another crate (merge into dependent)
3. **Shared types only**: Exists primarily to share types with parent crate (merge or extract types to `ggen-types`)
4. **Stable and small**: <5k LOC, stable API, well-tested → Can consolidate without risk

**Recent consolidation**: ggen-receipt (2.5k LOC) → ggen-config (maintains re-exports for backward compatibility)

### Feature-Gating Optional Crates

Optional functionality (e.g., ggen-marketplace) MUST be feature-gated:

```toml
[dependencies.ggen-marketplace]
path = "../ggen-marketplace"
optional = true

[features]
default = ["marketplace"]
marketplace = ["ggen-marketplace"]
```

**Benefits**:
- Reduces default build time and binary size
- Allows marketplace to evolve independently
- Users can disable if not needed (embedded systems, minimal builds)
- Clear dependency boundary (no marketplace code in critical paths)

**Marketplace Status**: Currently feature-gated and optional. Enabled by default but can be disabled.

## Build & Quality Standards

### cargo make Targets (MANDATORY)

**Quick Feedback Loop** (<20s total):
- `cargo make check` - Fast compilation check (<5s, 1.95s measured)
- `cargo make test-unit` - Unit tests only (<16s, 15.82s measured)
- `cargo make lint` - Clippy with strict rules (<10s)

**Full Validation** (<60s total):
- `cargo make test` - Complete test suite with timeouts (31.60s measured, 1,168+ tests)
- `cargo make pre-commit` - Format + lint + tests (runs before every commit)
- `cargo make ci` - Full CI pipeline with quality gates

**Performance Verification**:
- `cargo make slo-check` - Verify Service Level Objectives
- `cargo make bench` - Run criterion benchmarks

### Service Level Objectives (SLOs)

- **First build**: ≤15s (measured: 0.79s, 84% under target)
- **Incremental build**: ≤2s (measured: sub-second)
- **Startup time**: ≤50ms (measured: 2.0ms, 96% under target)
- **Binary size**: ≤5MB (measured: 2.8MB, 44% under target)
- **Template parsing**: ≤5ms (measured: 115ns, 43,480x under target)
- **RDF triple processing**: ≤5s for 1,000+ triples (measured: <1µs per triple)
- **SPARQL queries**: ≤10ms for complex queries (measured: <10ms)

### Quality Gates (Zero Defects)

**CANNOT commit code that**:
- Has compiler errors or warnings (cargo check fails)
- Has failing tests (cargo test fails)
- Lacks type annotations on public APIs
- Uses `unwrap()`/`expect()` in production code (outside `#[cfg(test)]`)
- Has unaddressed Clippy lints (cargo clippy fails)
- Lacks test coverage on critical paths (<80%)
- Saves working files to root folder (violates organization principle)

## Development Workflow

### Speckit Integration (Spec-First Development)

**MANDATORY**: NO implementation without specification. Use GitHub Spec Kit workflow:

1. **Project Principles** (one-time): `/speckit.constitution`
2. **Feature Specification** (per feature): `/speckit.specify "Feature description"`
3. **Technical Planning**: `/speckit.plan` (establishes architecture)
4. **Task Breakdown**: `/speckit.tasks` (generates actionable tasks)
5. **Implementation**: `/speckit.implement` (execute with evidence)

**Branch Naming**: `NNN-feature-name` (e.g., `001-rdf-validation`)
**Evidence Directory**: `.specify/specs/NNN-feature/evidence/` (tests, benchmarks, OTEL spans)
**Spec Artifacts**: `.specify/specs/NNN-feature/{spec,plan,tasks,data-model}.md`

### TDD Cycle (Red-Green-Refactor)

**Chicago School State-Based Testing**:

1. **RED**: Write test that verifies observable behavior
   - Use AAA pattern (Arrange-Act-Assert)
   - Test return values, state changes, side effects
   - Use real collaborators (Oxigraph, file system, Tera)
   - Test MUST fail before implementation exists

2. **GREEN**: Implement minimum code to pass test
   - Use `cargo make test-unit` for fast feedback
   - Add proper error handling with `Result<T, E>`
   - NO `unwrap()` in production code
   - Watch for YELLOW/RED Andon signals

3. **REFACTOR**: Improve code while keeping tests green
   - Extract abstractions as patterns emerge
   - Apply type-first thinking for invariants
   - Run `cargo make check` continuously (<5s)
   - Commit when GREEN

### File Organization Rules

**NEVER save to root folder**. Use these directories:

- `/crates/*/src/` - Source code (per crate)
- `/crates/*/tests/` - Integration tests (per crate)
- `/tests/` - Workspace-level integration tests
- `/docs/` - Documentation and guides
- `/scripts/` - Build and automation scripts
- `/benches/` - Performance benchmarks
- `/templates/` - Code generation templates (Tera)
- `/examples/` - Working example projects (48 examples)
- `/.specify/specs/NNN-feature/` - Feature specifications and evidence

### Agent Coordination (Claude-Flow Hooks)

**Every agent spawned via Task tool MUST**:

**Before work**:
```bash
npx claude-flow@alpha hooks pre-task --description "[task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"
```

**During work**:
```bash
cargo make check  # Monitor Andon signals
cargo make test-unit  # Quick validation
npx claude-flow@alpha hooks post-edit --file "[file]"
```

**After work**:
```bash
cargo make test  # All tests must pass
npx claude-flow@alpha hooks post-task --task-id "[task]"
```

## Governance

### Amendment Procedure

1. **Proposal**: Submit amendment via GitHub issue or PR with rationale
2. **Impact Analysis**: Assess effect on existing specs, templates, codebase
3. **Template Sync**: Update `.specify/templates/` to reflect new principles
4. **Version Bump**:
   - **MAJOR**: Backward-incompatible principle removals or redefinitions
   - **MINOR**: New principle/section added or materially expanded guidance
   - **PATCH**: Clarifications, wording, typo fixes, non-semantic refinements
5. **Approval**: Constitution changes require maintainer sign-off
6. **Migration Plan**: Provide guidance for updating existing code/specs

### Versioning Policy

- **Format**: MAJOR.MINOR.PATCH (Semantic Versioning)
- **Ratification Date**: Original adoption date (first version 1.0.0)
- **Last Amended Date**: Updated on MAJOR/MINOR changes (not PATCH)
- **Version in File**: Single source of truth at bottom of constitution
- **Compatibility**: MAJOR versions may have breaking changes to principles

### Compliance Review

- **Pull Requests**: MUST verify compliance with all principles
- **Pre-Commit Hooks**: Automated checks for cargo make, file organization, compilation
- **CI Pipeline**: Runs `cargo make ci` with full quality gates
- **Complexity Justification**: MUST document in plan.md if violating principles (e.g., "why 4th crate needed?")
- **Escalation**: Non-compliance flagged in code review; blocking for RED signals

### Relationship with CLAUDE.md

This constitution supersedes CLAUDE.md where principles overlap. CLAUDE.md provides runtime development guidance and tool usage patterns. Constitution defines non-negotiable architectural principles.

**Precedence**:
1. Constitution (`.specify/memory/constitution.md`) - Architectural principles
2. CLAUDE.md (project root) - Development workflow and tool usage
3. README.md - User-facing documentation

**Version**: 1.1.0 | **Ratified**: 2025-12-11 | **Last Amended**: 2026-05-13
