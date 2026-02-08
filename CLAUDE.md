# Claude Code Configuration - ggen Rust Project

## 📋 Project Identity

**ggen v6.0.0**: Specification-driven code generation CLI built in Rust. Deterministic transformation from RDF ontologies to reproducible code.

**Core Equation**: $A = \mu(O)$ — Code (A) precipitates from RDF ontology (O) via five-stage pipeline (μ₁-μ₅)

**Stack**: Rust 1.91.1 | Tokio | Oxigraph (RDF/SPARQL) | Tera | Serde | Clap | Chicago TDD | DfLSS

---

## 🚨 ABSOLUTE RULES (Non-Negotiable)

1. **ALL operations MUST be concurrent/parallel in ONE message**
2. **NEVER save files to root folder** - use appropriate subdirectories
3. **USE Claude Code Task tool for agent execution** - MCP only coordinates
4. **ALWAYS USE `cargo make`** - NEVER direct cargo commands
5. **TodoWrite ALWAYS 10+ todos in ONE batch**
6. **STOP THE LINE when Andon signals appear** - fix before proceeding

### ⚡ Golden Rule: "1 MESSAGE = ALL RELATED OPERATIONS"

**Batch everything in single messages:**
- TodoWrite: 10+ todos minimum
- Task tool: Spawn ALL agents together
- File ops: ALL reads/writes/edits together
- Bash: ALL commands chained with `&&`
- Memory: ALL store/retrieve together

---

## 📁 File Organization (Rust Workspace - 30 Crates)

**NEVER save to root. Use:**
- `crates/*/src/` - Source code per crate
- `crates/*/tests/` - Integration tests per crate
- `tests/` - Workspace-level tests
- `docs/` - Documentation
- `scripts/` - Utility scripts with timeout wrappers
- `examples/` - Example code
- `benches/` - Benchmarks
- `.specify/` - RDF specifications (SOURCE OF TRUTH)
- `.claude/` - Claude Code configuration

**30 Crates**: ggen-core (4.2M), ggen-cli (1.8M), ggen-domain (1.6M), ggen-utils (431K), ggen-ai (2.6M), ggen-ontology-core, ggen-marketplace-v2, 6 KNHK systems, + 19 others

---

## 🚨 Andon Signals (Stop the Line)

**Signal = STOP WORK IMMEDIATELY. Fix root cause, not symptom.**

| Signal | Action |
|--------|--------|
| 🔴 **CRITICAL** | Compiler errors (`error[E...]`), Test failures (`test ... FAILED`) → HALT |
| 🟡 **HIGH** | Warnings (`warning:`), Clippy errors → STOP before release |
| 🟢 **GREEN** | All checks pass → Proceed |

**Workflow**: Monitor → Stop → Investigate (5 Whys) → Fix → Verify cleared

---

## 🔧 Build Commands (ALWAYS `cargo make`)

**CRITICAL: NEVER USE DIRECT CARGO - NON-NEGOTIABLE**

| Command | Purpose | Timeout |
|---------|---------|---------|
| `cargo make check` | Compilation check | <5s |
| `cargo make test-unit` | Fast unit tests | <16s |
| `cargo make test` | Full test suite | <30s |
| `cargo make lint` | Clippy + rustfmt | <60s |
| `cargo make pre-commit` | check → lint → test-unit | <2min |
| `cargo make ci` | Full CI pipeline | - |
| `cargo make slo-check` | Performance SLOs | - |
| `cargo make audit` | Security vulnerabilities | - |
| `cargo make bench` | Benchmarks | - |
| `cargo make speckit-validate` | SHACL validation | - |
| `cargo make speckit-render` | Generate markdown from TTL | - |

**ggen commands:**
```bash
ggen sync                    # Full pipeline (μ₁-μ₅)
ggen sync --dry_run true     # Preview changes
ggen sync --audit true       # Cryptographic audit trail
ggen init                    # Initialize project
```

---

## 🧪 Testing (Chicago TDD - MANDATORY)

**Principles**: State-based | Real collaborators | Behavior verification | AAA pattern

**Test Types**: Unit (<150s) | Integration (<30s) | BDD (Cucumber) | Property (proptest) | Snapshot (insta) | Security | Determinism (RNG_SEED=42) | Performance (Criterion)

**Requirements**:
- ✅ All public APIs tested
- ✅ Error paths + edge cases (80%+ coverage)
- ✅ Tests verify observable outputs/state changes (no meaningless tests)
- ✅ NEVER claim completion without running tests
- ✅ AAA pattern: Arrange/Act/Assert

**80/20**: Focus on error paths, resource cleanup, concurrency, real dependencies, determinism

---

## 🦀 Elite Rust Mindset

**Type-First**: Types encode invariants | Compiler as design tool | PhantomData for state machines | Const generics | Ask: "What can I express in types?"

**Zero-Cost**: Generics/macros/const generics are zero-cost | Trait objects/heap have cost | Ask: "Is this abstraction zero-cost?"

**Performance**: References > owned | Stack > heap | Minimize allocations | Optimize hot paths (20%) | Ask: "What's the performance characteristic?"

**Memory Safety**: Ownership explicit | Lifetimes prevent use-after-free | Rc/Arc for sharing | Encapsulate unsafe | Ask: "What are ownership semantics?"

**API Design**: Type-safe by default | Ergonomic | Composable | Self-documenting | Result<T,E> not panics | Ask: "How to make misuse impossible?"

**80/20 Ideas**: Always generate 3: (1) Solve immediate (2) 80% of related with 20% effort (3) Maximum value (type-level). **Second is sweet spot.**

**DfLSS**: Prevent defects AND waste from start, not fix later.

---

## 🚀 Available Agents (Compressed)

**Core**: `coder` `reviewer` `tester` `planner` `researcher` - Use for simple Rust tasks

**Priority (Use instead of core when matching):**
- `production-validator` - Production readiness
- `code-analyzer` - Code quality analysis
- `system-architect` - System architecture
- `performance-benchmarker` - Performance optimization
- `backend-dev` - Backend implementation
- `task-orchestrator` - Complex workflows

**Specialized**: `hierarchical-coordinator` `mesh-coordinator` `adaptive-coordinator` `byzantine-coordinator` `raft-manager` `gossip-coordinator` `crdt-synchronizer` `perf-analyzer` `memory-coordinator` `github-modes` `pr-manager` `code-review-swarm` `issue-tracker` `release-manager` `workflow-automation` `repo-architect` `multi-repo-swarm` `sparc-coord` `sparc-coder` `specification` `pseudocode` `architecture` `refinement` `tdd-london-swarm` `migration-planner` `swarm-init`

**BB80 (EPIC 9)**: `bb80-specification-validator` `bb80-parallel-task-coordinator` `bb80-collision-detector` `bb80-convergence-orchestrator` `rust-coder` `speckit-architect` `test-engineer`

---

## 🎯 Agent Execution Pattern (Rust-Specific)

**Correct Flow:**
1. Optional: MCP swarm init (coordination topology)
2. **REQUIRED**: Claude Code Task tool spawns agents (actual work)
3. **REQUIRED**: Agents use `cargo make` (NEVER direct cargo)
4. **REQUIRED**: Batch all ops in ONE message

**Example (Single Message):**
```rust
Task("System Architect", "Design type-first API...", "system-architect")
Task("Rust Coder", "Implement with const generics...", "coder")
Task("Test Engineer", "Chicago TDD tests AAA pattern...", "tester")
Task("Code Analyzer", "Review type safety...", "code-analyzer")

TodoWrite { todos: [10+ comprehensive todos with Andon checks] }

Write "crates/ggen-core/src/feature.rs"
Write "crates/ggen-core/tests/feature_test.rs"
```

**Agent Protocol:**
```bash
# BEFORE: Verify timeout exists
cargo make timeout-check

# DURING: Quick feedback
cargo make check    # Andon monitoring
cargo make lint     # Andon monitoring

# AFTER: Full validation
cargo make test     # CRITICAL Andon signal
cargo make slo-check
cargo make audit
```

---

## 📋 Agent Coordination Rules

| Rule | Requirement |
|------|-------------|
| **Cargo Make Only** | `cargo make [target]` always. Enforces SLOs, timeouts, quality gates. |
| **Result<T,E>** | Production: required throughout. Tests: `unwrap()` OK. |
| **No Unwrap/Expect** | Zero in production. Clippy `-D warnings` enforces. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated). |
| **Type-First** | Constraints in types. Compiler verifies. NewType for domains. |
| **TTL Immutable** | Once closed, fix source then regenerate. |

---

## 🚨 Definition of Done (Validation Checklist)

**BEFORE MARKING COMPLETE - RUN ALL CHECKS:**

```bash
# 1. Verify timeout command exists
cargo make timeout-check

# 2. Check compiler errors (CRITICAL SIGNAL)
cargo make check
# IF ERRORS: STOP THE LINE - Fix immediately
# VERIFY: No `error[E...]` patterns

# 3. Check warnings (HIGH SIGNAL)
# IF WARNINGS: STOP THE LINE - Fix immediately
# VERIFY: No `warning:` patterns

# 4. Run tests (CRITICAL SIGNAL)
cargo make test
# IF FAILS: STOP THE LINE - Create rich todos for each failure
# VERIFY: No `test ... FAILED` patterns

# 5. Check linting (HIGH SIGNAL)
cargo make lint
# IF ERRORS: STOP THE LINE - Fix immediately
# VERIFY: Clean output

# 6. Verify performance SLOs
cargo make slo-check
# VERIFY: First build ≤15s, Incremental ≤2s, RDF ≤5s/1k+ triples
```

**Systematic Fixing:**
1. Batch create 10+ related todos
2. Fix: Read error → Root cause → Fix → Verify → Update todo → Remove when fixed
3. Re-run validation
4. Repeat if still failing

**Final Verification - ALL signals cleared:**
- ✅ `cargo make check` - Clean
- ✅ `cargo make test` - All pass
- ✅ `cargo make lint` - Clean
- ✅ `cargo make slo-check` - SLOs met
- ✅ No pending test todos

**ONLY mark complete when ALL checks pass**

---

## 🚫 Prohibited Patterns

**NEVER:**
- Direct cargo commands (`cargo check/test/clippy/fmt/build`)
- Commands without timeout wrappers
- Skip timeout-check
- Ignore Andon signals
- Proceed with signals present
- Suppress signals (`#[allow(...)]` without fixing)
- Mark complete without verification
- Placeholders or "In production..." comments
- TODO comments (use `FUTURE:` prefix)
- `unimplemented!()` - Complete implementations required
- `unwrap()/expect()` in production (use `Result<T,E>`)
- Stubs - No functions that always succeed
- Claims without test verification
- Meaningless tests (must verify observable outputs)
- Chicago TDD violations
- `print!/println!` in library code (use `log!` macros)
- Save files to root folder

---

## 🎯 SLOs & Performance

**Targets:**
- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s/1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs

**Claude Flow Benefits:**
- 84.8% SWE-Bench solve rate
- 32.3% token reduction
- 2.8-4.4x speed improvement
- 27+ neural models

---

## 📚 Stack & Crates (v6.0.0)

**Stack**: Rust 1.91.1 | Tokio 1.47 | Oxigraph 0.5.1 | Tera 1.20 | Serde 1.0 | Clap 4.5 | genai 0.5 | chicago-tdd-tools 1.4.0 | proptest 1.8 | criterion 0.7 | testcontainers 0.25 | insta | serial_test | pqcrypto-mldsa 0.1 | Axum 0.8 | Tonic 0.14

**Core Crates (8)**: ggen-core (RDF/SPARQL/templates) | ggen-cli (CLI) | ggen-domain (MAPE-K) | ggen-utils | ggen-config | ggen-macros | ggen-node (NAPI-RS) | ggen-dod

**Plus 22 more**: CLI validation, testing, marketplace, RevOps (API/auth/payments/SaaS), AI orchestration, ontology, KNHK systems (ETL/hot/connectors/lockchain/otel/orchestrator), folk strategy

---

## 🔧 Development Workflow (v6)

**1. Create RDF Spec:**
```bash
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl  # Edit TTL (source)
ggen validate .specify/specs/NNN-feature/feature.ttl
cargo make speckit-render  # Generate markdown
```

**2. Chicago TDD:**
```bash
vim crates/ggen-core/tests/feature_test.rs  # Write failing test (RED)
cargo make test-unit                        # Verify fails
vim crates/ggen-core/src/feature.rs         # Implement (GREEN)
cargo make test-unit                        # Verify passes
cargo make pre-commit                       # Refactor (maintain GREEN)
```

**3. Generate from Ontology:**
```bash
ggen sync --dry_run true   # Preview
ggen sync --audit true     # Full sync with audit
```

**4. Commit with Evidence:**
```bash
cargo make pre-commit
git add .specify/specs/NNN-feature/feature.ttl crates/ggen-core/src/feature.rs crates/ggen-core/tests/feature_test.rs
git commit -m "feat(NNN): Implement feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates
[Receipt] cargo make test: ✓ 347/347 tests, 28.3s
[Receipt] ggen sync --audit: ✓ 12 files, 0 conflicts"
```

---

## 🎯 When to Use EPIC 9 (Big Bang 80/20)

**Triggers** (use if ANY apply):
- Non-trivial: 5+ files or 3+ systems
- Multiple valid approaches
- Large architectural decisions
- Unclear requirements
- Unknown unknowns

**Workflow**: Spec Closure (100% TTL) → Fan-Out (10 parallel agents) → Collision Detection → Convergence → Refactoring → Cryptographic Receipt

---

## 🔑 Key Mental Models

- **Types = invariants = compile-time guarantees**
- **Zero-cost = generics/macros/const generics**
- **Performance = references/stack/minimize allocations**
- **Ownership = explicit = memory safety**
- **APIs = type-safe = ergonomic = composable**
- **Tests = observable outputs = behavior verification**
- **80/20 = second idea = sweet spot**
- **Andon = stop = fix = verify**
- **DfLSS = prevent defects AND waste**

---

## 📝 Critical Reminders

**STOP THE LINE when Andon signals appear!**

**ALWAYS use `cargo make` - NEVER direct cargo!**

**TodoWrite always 10+ todos in ONE batch!**

**Spec Closure First**: 100% coverage in `.specify/*.ttl` before code generation

**Receipts Over Narratives**: Deterministic evidence (test counts, SLO metrics, hashes)

**RDF is Reality**: Edit `.ttl`; everything else is generated projection via μ

**Cargo Make is Law**: All validation through `Makefile.toml` (enforces quality gates + timeouts)

**Test results are truth - code doesn't work if tests don't pass!**

---

## Holographic Factory (v6)

**μ Pipeline (5 stages)**:
1. **Normalize**: RDF validation, SHACL shapes, dependency resolution
2. **Extract**: SPARQL queries, OWL inference, rule execution
3. **Emit**: Tera template rendering, code generation
4. **Canonicalize**: Deterministic formatting, content hashing
5. **Receipt**: Cryptographic proof, audit trail

**Corollary**: Bug in generated code? Fix RDF spec (source), not output (projection).

---

## 📚 Support

- **ggen**: https://github.com/seanchatmangpt/ggen
- **Claude Flow**: https://github.com/ruvnet/claude-flow
- **Flow-Nexus**: https://flow-nexus.ruv.io

---

**Last Updated**: 2026-01-25 | v6.0.0 production-ready | 30 crates | 1000+ files | 87% test coverage