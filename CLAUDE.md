# Claude Code Configuration - ggen Rust Project

## 📋 Project Identity
# ggen: Specification-Driven Code Generation (v6.0.0)

**Core Equation**: $A = \mu(O)$ — Code (A) precipitates from RDF ontology (O) via five-stage transformation pipeline (μ).

**Version**: 6.0.0 (Production-Ready Core) | **Release**: January 2026

---

**ggen** is a language-agnostic, deterministic code generation CLI built in Rust that turns ontologies + RDF-like metadata into reproducible code projections.

- **Language**: Rust (stable toolchain)
- **Architecture**: Workspace with multiple crates (ggen-core, ggen-cli, ggen-domain, ggen-utils, ggen-ontology-core, etc.)
- **Methodology**: SPARC + Chicago TDD + DfLSS (Design for Lean Six Sigma)
- **Core Principles**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

## 🔗 Ontology Layer (v0.2.0 - Production-Ready)

**ggen-ontology-core** provides enterprise-grade RDF/SPARQL processing:

- **TripleStore**: In-memory RDF storage with SPARQL 1.1 execution (<1s load, <100ms queries)
- **Entity Mapper**: Bidirectional RDF ↔ Rust type conversion with zero-copy references
- **SPARQL Generator**: Type-safe query construction with compile-time validation
- **Validators**: RDF schema validation, entity relationships, type safety, SHACL shape validation
- **Domain Ontologies**: Legal (contracts, compliance), IT Infrastructure (systems, services), Cloud Security (access control, encryption)
- **Cloud Bindings**: AWS CloudFormation, GCP Terraform, Azure ARM templates
- **Test Coverage**: 64 Chicago TDD tests (100% passing, 87% coverage)
- **Documentation**: Complete integration guide with 5+ patterns and 4 working examples
- **Release Status**: v0.2.0 production-ready (Jan 19, 2026)

**See Also**: `/docs/releases/v0.2.0/INDEX.md` for comprehensive guide

## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT

**ABSOLUTE RULES**:
1. ALL operations MUST be concurrent/parallel in a single message
2. **NEVER save working files, text/mds and tests to the root folder**
3. ALWAYS organize files in appropriate subdirectories
4. **USE CLAUDE CODE'S TASK TOOL** for spawning agents concurrently, not just MCP
5. **NEVER USE DIRECT CARGO COMMANDS - ALWAYS USE `cargo make`**

### ⚡ GOLDEN RULE: "1 MESSAGE = ALL RELATED OPERATIONS"

**MANDATORY PATTERNS:**
- **TodoWrite**: ALWAYS batch ALL todos in ONE call (10+ todos minimum per .cursorrules)
- **Task tool (Claude Code)**: ALWAYS spawn ALL agents in ONE message with full instructions
- **File operations**: ALWAYS batch ALL reads/writes/edits in ONE message
- **Bash commands**: ALWAYS batch ALL terminal operations in ONE message
- **Memory operations**: ALWAYS batch ALL memory store/retrieve in ONE message

### 🎯 CRITICAL: Claude Code Task Tool for Agent Execution

**Claude Code's Task tool is the PRIMARY way to spawn agents:**
```rust
// ✅ CORRECT: Use Claude Code's Task tool for parallel agent execution
[Single Message]:
  Task("Research agent", "Analyze Rust patterns and RDF requirements...", "researcher")
  Task("Rust Coder", "Implement core features with zero-cost abstractions...", "coder")
  Task("Test Engineer", "Create Chicago TDD tests with behavior verification...", "tester")
  Task("Code Reviewer", "Review for type safety and Andon signals...", "reviewer")
  Task("System Architect", "Design type-first architecture...", "system-architect")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Initialize coordination topology
- `mcp__claude-flow__agent_spawn` - Define agent types for coordination
- `mcp__claude-flow__task_orchestrate` - Orchestrate high-level workflows

### 📁 File Organization Rules (Rust Workspace)

**NEVER save to root folder. Use these directories:**
- `/crates/*/src` - Source code files (per crate)
- `/crates/*/tests` - Integration tests (per crate)
- `/tests` - Workspace-level integration tests
- `/docs` - Documentation and markdown files
- `/scripts` - Utility scripts (bash scripts with timeout wrappers)
- `/examples` - Example code and demos
- `/benches` - Benchmark suites
- `/resources` - Configuration templates (e.g., default_config.toml)
- `/templates` - Code generation templates (.tmpl files)

**Workspace Structure:**
```
ggen/
├── crates/                     # 30 total crates
│   ├── Core System (8)
│   │   ├── ggen-core/          # RDF processing, SPARQL, templates (4.2M)
│   │   ├── ggen-cli/           # CLI entry point and routing (1.8M)
│   │   ├── ggen-domain/        # Business logic, MAPE-K loop (1.6M)
│   │   ├── ggen-utils/         # Shared utilities (431K)
│   │   ├── ggen-config/        # Configuration management
│   │   ├── ggen-macros/        # Procedural macros
│   │   ├── ggen-node/          # Node.js bindings (NAPI-RS)
│   │   └── ggen-dod/           # Data-oriented design patterns
│   ├── CLI & Validation (3)
│   │   ├── ggen-cli-validation/
│   │   ├── ggen-config-clap/
│   │   └── ggen-spec-validator/
│   ├── Testing & Quality (3)
│   │   ├── ggen-test-audit/
│   │   ├── ggen-test-opt/
│   │   └── ggen-e2e/
│   ├── Marketplace (1)
│   │   └── ggen-marketplace-v2/ (596K)
│   ├── RevOps/Monetization (4)
│   │   ├── ggen-api/
│   │   ├── ggen-auth/
│   │   ├── ggen-payments/
│   │   └── ggen-saas/
│   ├── AI Orchestration (2)
│   │   ├── ggen-ai/ (2.6M)
│   │   └── ggen-dspy/ (439K)
│   ├── Ontology (1)
│   │   └── ggen-ontology-core/
│   └── KNHK Systems (6)
│       ├── knhk-etl/
│       ├── knhk-hot/
│       ├── knhk-connectors/
│       ├── knhk-lockchain/
│       ├── knhk-otel/ (704K)
│       └── knhk-orchestrator/
├── .specify/              # RDF specification system (source of truth)
├── tests/                 # Workspace-level integration & BDD tests
├── benches/              # Performance benchmarks (15+ suites)
├── examples/             # 40+ production-grade examples
├── docs/                 # 50+ documentation subdirectories
├── scripts/              # Build and utility scripts
├── templates/            # Tera code generation templates
├── Cargo.toml           # Workspace manifest (30 members)
├── Makefile.toml        # Build automation (70+ targets)
└── .claude/             # Claude Code configuration
```

## 🚨 CRITICAL: Andon Signals (Stop the Line)

**Andon signals are visual problem indicators - treat compiler errors, test failures, and warnings as stop signals.**

### Signal Types:
- **CRITICAL (Red) - Must stop immediately**:
  - Compiler errors (`error[E...]`)
  - Test failures (`test ... FAILED`)
- **HIGH (Yellow) - Should stop**:
  - Compiler warnings (`warning:`)
  - Linting errors (clippy warnings/errors)
- **MEDIUM (Yellow) - Investigate**:
  - Performance regressions
  - Code quality warnings

### Andon Signal Workflow:
1. **Monitor**: Run `cargo make check`, `cargo make test`, `cargo make lint` to check for signals
2. **Stop**: When signal appears, immediately stop current work - do not proceed
3. **Investigate**: Use root cause analysis (5 Whys) to understand why signal appeared
4. **Fix**: Address root cause, not just symptom
5. **Verify**: Re-run checks to confirm signal cleared - signal must be cleared before work continues

### Signal Verification Before Completion:
- ✅ No compiler errors: `cargo make check` passes cleanly
- ✅ No test failures: `cargo make test` - all tests pass
- ✅ No warnings: `cargo make lint` - no clippy warnings/errors
- ✅ No performance regressions: `cargo make slo-check` - meets SLOs

**Andon Principle**: "Stop the line" - Any problem visible through signals must be fixed immediately. Don't hide signals, don't ignore them, don't proceed with them present. This prevents defects from propagating and waste from accumulating (DfLSS alignment).

## 🔧 Build Commands (ALWAYS USE `cargo make`)

**CRITICAL: NEVER USE DIRECT CARGO COMMANDS - THIS IS NON-NEGOTIABLE**

### Quick Feedback (Fast iteration):
- `cargo make check` - Quick compilation check (timeout 5s)
- `cargo make test-unit` - Unit tests only (timeout 10s)
- `cargo make test test_name` - Run single test
- `cargo make lint` - Clippy linting (NEVER use `cargo clippy` directly)

### Full Validation:
- `cargo make test` - All tests (timeout 10s unit + 30s integration)
- `cargo make pre-commit` - Format + lint + unit tests
- `cargo make ci` - Full CI pipeline
- `cargo make release-validate` - Comprehensive release checks

### Performance & Profiling:
- `cargo make slo-check` - Verify performance SLOs
- `cargo make bench` - Run benchmarks
- `cargo make profile` - Performance profiling

### Security & Quality:
- `cargo make audit` - Security vulnerability checks
- `cargo make validate-templates` - Template security validation
- `cargo make validate-rdf` - RDF validation

### Development Utilities:
- `cargo make completions` - Generate shell completions
- `cargo make watch` - Live development with auto-rebuild
- `cargo make debug` - Debugging mode
- `cargo make timeout-check` - Verify timeout command exists

### Timeout SLAs (CRITICAL - Poka-Yoke Enforced):
Every CLI command MUST have timeout wrapper to prevent freezing:
- Quick checks: `timeout 5s` (cargo check, cargo fmt, cargo clippy)
- Compilation (debug): `timeout 10s` (cargo build)
- Compilation (release): `timeout 30s` (cargo build --release)
- Unit tests: `timeout 150s` (cargo test --lib - allows rebuild time)
- Integration tests: `timeout 30s` (cargo test --test)
- Full test suite: `timeout 30s-120s escalation` (cargo make test)
- Pre-push hooks: `timeout 30s` (cargo make check-pre-push)
- RDF processing: `timeout 5s` (1k+ triples, critical SLO)
- CLI scaffolding: `timeout 3s end-to-end` (project generation)

**SLO Targets:**
- First build: ≤15s
- Incremental: ≤2s
- RDF processing: ≤5s/1k+ triples
- Generation memory: ≤100MB
- CLI scaffolding: ≤3s end-to-end
- 100% reproducible outputs (deterministic)

## 🧪 Testing Strategy (Chicago TDD - MANDATORY)

**Chicago TDD**: State-based testing with real collaborators and behavior verification.

### Core Principles:
1. **State-based testing** - Verify outputs, not implementation
2. **Real collaborators** - Use real objects, minimize mocks
3. **Behavior verification** - Verify what code does (observable outputs/state changes)
4. **AAA pattern required** - Arrange-Act-Assert
5. **Tests verify**: Return values, state changes, side effects, execution order, actual effects on system

### Test Categories (Comprehensive):
- **Unit tests**: Colocated with source (`crates/*/src/*_test.rs` or `#[cfg(test)] mod tests`) - <150s timeout
- **Integration tests**: In `/tests` and `crates/*/tests` - <30s timeout
- **BDD/Feature tests**: Cucumber `.feature` files with 13+ step modules in `/tests/bdd/`
- **Property tests**: Using `proptest` for parsers, RDF, templating edge cases
- **Snapshot tests**: Using `insta` for deterministic code generation outputs
- **Security tests**: Input validation, SPARQL injection prevention (`tests/security/`)
- **Determinism tests**: Fixed seeds (RNG_SEED=42), reproducible outputs verification
- **Testcontainer tests**: Production-readiness validation with Docker containers
- **Performance tests**: Benchmarks in `/benches` (15+ suites with Criterion HTML reports)
- **Consolidated quality tests**: Generic trait-based testing patterns

### Test Infrastructure:
```bash
cargo make test            # Full suite (30s timeout with escalation)
cargo make test-unit       # Fast feedback (<150s)
cargo make test-integration # Integration only
cargo make test-doc        # Doc tests
cargo make test-single-threaded # Deterministic async (RNG_SEED=42)
cargo make test-bdd        # Cucumber BDD features
cargo make validate-rdf    # RDF/SPARQL validation tests
```

### Test Requirements:
- ✅ All public APIs must be tested
- ✅ Test error paths, edge cases, critical paths (80%+ coverage target)
- ✅ **No meaningless tests** - Tests must verify observable outputs/state changes, not just `assert_ok!()`
- ✅ **Behavior verification** - Tests verify what code does, not just that functions exist
- ✅ **Never claim completion without running tests** - Tests must pass before work is done
- ✅ Run deterministic tests with `--test-threads=1` for concurrent safety
- ✅ Use `serial_test` for deterministic async validation

### Expert Testing (80/20):
Focus on the 20% that catches 80% of bugs:
- Error paths and boundary conditions (SPARQL injection, invalid RDF)
- Resource cleanup and lifecycle (tempfiles, allocations)
- Concurrency and race conditions (async/await patterns)
- Real dependencies (testcontainers, actual RDF stores)
- Determinism verification (same seed = identical results)

## 🦀 Elite Rust Mindset & 80/20 Thinking

### Type-First Thinking:
- Types encode invariants; compiler as design tool
- Use types to make invalid states unrepresentable
- PhantomData for type-level state machines
- Const generics over runtime values
- Ask: **"What can I express in types?"** before "What values do I need?"

### Zero-Cost Awareness:
- Generics monomorphize (zero-cost)
- Const generics are zero-cost
- Macros expand efficiently
- References are zero-cost
- Trait objects have dynamic dispatch cost
- Heap allocation has cost
- Ask: **"Is this abstraction zero-cost?"**

### Performance Intuition:
- References over owned values
- Stack over heap
- Cache locality matters
- Minimize allocations
- Optimize hot paths (20% that matters)
- Ask: **"What's the performance characteristic?"**

### Memory Safety:
- Ownership is explicit
- Borrowing enables zero-cost
- Lifetimes prevent use-after-free
- Rc/Arc for shared ownership
- Encapsulate unsafe in safe APIs
- Ask: **"What are the ownership semantics?"**

### API Design:
- Type-safe by default (errors impossible through types)
- Ergonomic interfaces (easy to use correctly, hard to misuse)
- Composable design
- Self-documenting types
- Explicit error handling (Result types, not panics)
- Ask: **"How can I make misuse impossible?"**

### 80/20 Idea Generation:
Always generate 3 ideas:
1. **First**: Solve immediate problem
2. **Second**: Go bigger (80% of related problems with 20% effort) while maintaining quality
3. **Third**: Maximum value (type-level solutions, compile-time guarantees)

**Second idea is usually the sweet spot.**

### Quality-First 80/20:
Value includes quality, consistency, maintainability - not optional. Quality prevents defects, maintains consistency, improves maintainability. Consistency is high value, not "extra effort".

### DfLSS Alignment:
Design for Lean Six Sigma - addresses efficiency (Lean waste elimination) AND quality (Six Sigma defect prevention) from start. Prevent defects AND waste rather than fixing later.

## 🚀 Available Agents (54 Total)

### Core Development (Use for simple Rust tasks)
`coder`, `reviewer`, `tester`, `planner`, `researcher`

**Use when:** Task is straightforward and doesn't require specialized expertise.

### Hyper-Advanced Agents (PRIORITY for ggen)
**CRITICAL:** When task matches these agents' specializations, ALWAYS use them instead of basic agents.

- **`production-validator`** - Production readiness validation (dependencies, infrastructure, release readiness)
- **`code-analyzer`** - Advanced code quality analysis (technical debt, architecture assessment)
- **`system-architect`** - System architecture design (integration patterns, architectural decisions)
- **`performance-benchmarker`** - Performance measurement & optimization
- **`backend-dev`** - Backend implementation (APIs, databases, infrastructure)
- **`task-orchestrator`** - Complex workflow orchestration

### Swarm Coordination
`hierarchical-coordinator`, `mesh-coordinator`, `adaptive-coordinator`, `collective-intelligence-coordinator`, `swarm-memory-manager`

### Consensus & Distributed
`byzantine-coordinator`, `raft-manager`, `gossip-coordinator`, `consensus-builder`, `crdt-synchronizer`, `quorum-manager`, `security-manager`

### Performance & Optimization
`perf-analyzer`, `performance-benchmarker`, `task-orchestrator`, `memory-coordinator`, `smart-agent`

### GitHub & Repository
`github-modes`, `pr-manager`, `code-review-swarm`, `issue-tracker`, `release-manager`, `workflow-automation`, `project-board-sync`, `repo-architect`, `multi-repo-swarm`

### SPARC Methodology
`sparc-coord`, `sparc-coder`, `specification`, `pseudocode`, `architecture`, `refinement`

### Specialized Development
`backend-dev`, `mobile-dev`, `ml-developer`, `cicd-engineer`, `api-docs`, `system-architect`, `code-analyzer`, `base-template-generator`

### Testing & Validation
`tdd-london-swarm`, `production-validator`

### Migration & Planning
`migration-planner`, `swarm-init`

## 🎯 Claude Code vs MCP Tools

### Claude Code Handles ALL EXECUTION:
- **Task tool**: Spawn and run agents concurrently for actual work
- File operations (Read, Write, Edit, Glob, Grep)
- Rust code generation and programming
- Bash commands with timeout wrappers
- Implementation work
- Project navigation and analysis
- TodoWrite and task management (10+ todos per batch)
- Git operations
- Cargo make commands (NEVER direct cargo)
- Testing and debugging

### MCP Tools ONLY COORDINATE:
- Swarm initialization (topology setup)
- Agent type definitions (coordination patterns)
- Task orchestration (high-level planning)
- Memory management
- Neural features
- Performance tracking
- GitHub integration

**KEY**: MCP coordinates the strategy, Claude Code's Task tool executes with real agents.

## 🚀 Quick Setup

```bash
# Add MCP servers (Claude Flow required, others optional)
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp add ruv-swarm npx ruv-swarm mcp start  # Optional: Enhanced coordination
claude mcp add flow-nexus npx flow-nexus@latest mcp start  # Optional: Cloud features
```

## 🚀 Agent Execution Flow with Claude Code (Rust-Specific)

### The Correct Pattern:

1. **Optional**: Use MCP tools to set up coordination topology
2. **REQUIRED**: Use Claude Code's Task tool to spawn agents that do actual work
3. **REQUIRED**: Each agent uses `cargo make` commands (NEVER direct cargo)
4. **REQUIRED**: Each agent runs hooks for coordination
5. **REQUIRED**: Batch all operations in single messages

### Example: Rust Feature Development with Andon Signals

```rust
// ✅ CORRECT: Rust-specific agent swarm with Andon signal workflow
[Single Message - Parallel Agent Execution]:

  // Specialized agents for Rust development
  Task("System Architect", "Design type-first API for RDF processing with zero-cost abstractions. Store architecture in memory.", "system-architect")
  Task("Rust Coder", "Implement RDF parser with const generics and Result<T,E>. Use cargo make lint.", "coder")
  Task("Code Analyzer", "Review for type safety, zero-cost abstractions, memory safety patterns.", "code-analyzer")
  Task("Test Engineer", "Create Chicago TDD tests: state-based, real collaborators, behavior verification. Use cargo make test.", "tester")
  Task("Performance Benchmarker", "Benchmark RDF parsing against SLOs (≤5s for 1k+ triples). Use cargo make bench.", "performance-benchmarker")
  Task("Production Validator", "Validate production readiness: dependencies, security audit, SLO compliance. Use cargo make audit.", "production-validator")

  // Batch ALL todos in ONE call (10+ minimum per .cursorrules)
  TodoWrite { todos: [
    {content: "Design type-first RDF API with PhantomData state machine", status: "in_progress", activeForm: "Designing type-first RDF API"},
    {content: "Implement RDF parser with const generics for compile-time validation", status: "pending", activeForm: "Implementing RDF parser"},
    {content: "Run cargo make check to verify no compiler errors (Andon signal)", status: "pending", activeForm: "Running cargo make check"},
    {content: "Fix any compiler errors immediately (Stop the Line)", status: "pending", activeForm: "Fixing compiler errors"},
    {content: "Create Chicago TDD unit tests with AAA pattern and state verification", status: "pending", activeForm: "Creating Chicago TDD unit tests"},
    {content: "Run cargo make test to verify all tests pass (Andon signal)", status: "pending", activeForm: "Running cargo make test"},
    {content: "Fix failing tests immediately (Stop the Line)", status: "pending", activeForm: "Fixing failing tests"},
    {content: "Run cargo make lint to check for clippy warnings (Andon signal)", status: "pending", activeForm: "Running cargo make lint"},
    {content: "Fix clippy warnings immediately (Stop the Line)", status: "pending", activeForm: "Fixing clippy warnings"},
    {content: "Run cargo make slo-check to verify performance SLOs", status: "pending", activeForm: "Running cargo make slo-check"},
    {content: "Run cargo make audit for security vulnerability check", status: "pending", activeForm: "Running cargo make audit"},
    {content: "Verify all Andon signals cleared before marking complete", status: "pending", activeForm: "Verifying all Andon signals cleared"}
  ]}

  // Parallel file operations
  Write "crates/ggen-core/src/rdf/parser.rs"
  Write "crates/ggen-core/src/rdf/types.rs"
  Write "crates/ggen-core/tests/rdf_parser_tests.rs"
  Write "docs/RDF_ARCHITECTURE.md"
```

### Example: Multi-Crate Rust Workspace Development

```rust
// ✅ CORRECT: Workspace-aware Rust development
[Single Message - Parallel Agent Execution]:

  Task("Core Developer", "Implement ggen-core RDF processing with zero-cost abstractions. Use cargo make check.", "coder")
  Task("CLI Developer", "Implement ggen-cli commands with clap derives. Use cargo make lint.", "coder")
  Task("Utils Developer", "Implement ggen-utils error types with thiserror. Use cargo make test-unit.", "coder")
  Task("Domain Developer", "Implement ggen-domain MAPE-K loop. Use cargo make test.", "coder")
  Task("Integration Tester", "Create workspace-level integration tests. Use cargo make test.", "tester")
  Task("Production Validator", "Validate workspace dependencies and feature flags. Use cargo make ci.", "production-validator")

  TodoWrite { todos: [10+ comprehensive todos with Andon signal checks] }

  // Workspace-aware file operations
  Write "crates/ggen-core/src/rdf.rs"
  Write "crates/ggen-cli/src/cmds/rdf.rs"
  Write "crates/ggen-utils/src/error.rs"
  Write "crates/ggen-domain/src/mape_k.rs"
  Write "tests/integration/rdf_workflow.rs"
```

## 📋 Agent Coordination Protocol (Rust-Specific)

### Every Agent Spawned via Task Tool MUST:

**1️⃣ BEFORE Work:**
```bash
npx claude-flow@alpha hooks pre-task --description "[task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"
cargo make timeout-check  # Verify timeout command exists
```
🔴 RED (Compilation/test error)        → STOP immediately (Andon halt)
🟡 YELLOW (Warnings/deprecations)      → Investigate before release
🟢 GREEN (All checks pass)             → Proceed safely
```

| Rule | Requirement |
|------|-------------|
| **Cargo Make Only** | `cargo make [target]` always (never raw `cargo`). Enforces SLOs, timeouts, quality gates. |
| **Result<T,E>** | Production: `Result<T,E>` throughout. Tests: `unwrap()` OK. |
| **No Unwrap/Expect** | Zero in production code. Language-enforced via clippy `-D warnings`. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated artifacts). |
| **Type-First** | Constraints in types, compiler verifies. NewType for domains. Generic + zero-cost. |
| **TTL is Immutable** | Once closed, don't iterate — fix source, regenerate. |

---

## Essential Commands (v6 SLO Targets)

**2️⃣ DURING Work:**
```bash
# Use cargo make commands (NEVER direct cargo)
cargo make check          # Quick compilation check (Andon signal monitoring)
cargo make lint          # Clippy linting (Andon signal monitoring)
cargo make test-unit     # Quick unit test feedback

# Coordinate via hooks
npx claude-flow@alpha hooks post-edit --file "[file]" --memory-key "swarm/[agent]/[step]"
npx claude-flow@alpha hooks notify --message "[what was done]"
```

**3️⃣ AFTER Work:**
```bash
# Full validation (Andon signal verification)
cargo make test          # All tests must pass (CRITICAL Andon signal)
cargo make slo-check     # Performance SLOs must be met
cargo make audit         # Security vulnerabilities must be addressed
# V6 UNIFIED COMMAND - REPLACES ALL PREVIOUS GENERATE COMMANDS
ggen sync                           # Full pipeline (μ₁-μ₅): Normalize → Extract → Emit → Canonicalize → Receipt
ggen sync --dry_run true            # Preview changes without writing files
ggen sync --validate_only true      # Pre-flight quality gates only (no generation)
ggen sync --audit true              # Generate cryptographic audit trail
ggen sync --watch true              # Continuous regeneration on file changes
ggen sync --force true --audit true # Safe destructive overwrite with audit

ggen init                           # Initialize new ggen project with manifest

# CARGO MAKE TARGETS (POKA-YOKE ENFORCED)
cargo make check       # <5s    (compile check, warnings-as-errors)
cargo make test-unit   # <16s   (fast feedback, Chicago TDD)
cargo make test        # <30s   (full test suite with timeout enforcement)
cargo make lint        # <60s   (clippy -D warnings, rustfmt)
cargo make pre-commit  # <2min  (check → lint → test-unit quality gate)

# SPECIFICATION WORKFLOW (RDF-FIRST)
cargo make speckit-check     # Verify TTL specs exist for current branch
cargo make speckit-validate  # SHACL validation of .specify/*.ttl
cargo make speckit-render    # Regenerate all markdown from TTL sources
cargo make speckit-full      # Full workflow: validate + render
```

---

## File Organization (Complete Workspace Structure)

**Current Project Structure (January 2026):**

```
ggen/
├── .specify/                       # RDF SPECIFICATIONS (SOURCE OF TRUTH)
│   ├── specs/                      # Feature specifications (6+ active specs)
│   │   ├── 001-poka-yoke-patterns/
│   │   ├── 004-optimize-test-concurrency/
│   │   ├── 013-ga-production-release/
│   │   ├── 014-affiliate-routing/
│   │   ├── 999-docs-as-code/
│   │   └── NNN-feature/
│   │       ├── feature.ttl         # User stories, requirements (EDIT THIS)
│   │       ├── entities.ttl        # Domain entities (EDIT THIS)
│   │       ├── plan.ttl            # Architecture plan (EDIT THIS)
│   │       ├── tasks.ttl           # Task breakdown (EDIT THIS)
│   │       ├── spec.md             # Generated from TTL (DO NOT EDIT)
│   │       └── evidence/           # Test artifacts, receipts
│   ├── templates/                  # Tera templates (SPARQL-aware)
│   ├── memory/                     # Project memory & decisions
│   ├── scripts/                    # RDF processing utilities
│   └── examples/                   # Example ontologies
│
├── crates/                         # 30 RUST CRATES (4.2M ggen-core alone)
│   ├── ggen-core/                  # RDF, SPARQL, templates (218 files, 25 modules)
│   ├── ggen-cli/                   # CLI entry point (1.8M)
│   ├── ggen-domain/                # Business logic (1.6M)
│   ├── ggen-utils/                 # Utilities (431K)
│   ├── ggen-config/, ggen-macros/, ggen-node/, ggen-dod/
│   ├── ggen-cli-validation/, ggen-config-clap/, ggen-spec-validator/
│   ├── ggen-test-audit/, ggen-test-opt/, ggen-e2e/
│   ├── ggen-marketplace-v2/        # (596K)
│   ├── ggen-api/, ggen-auth/, ggen-payments/, ggen-saas/
│   ├── ggen-ai/ (2.6M), ggen-dspy/ (439K)
│   ├── ggen-ontology-core/         # v0.2.0 production-ready
│   ├── knhk-etl/, knhk-hot/, knhk-connectors/
│   ├── knhk-lockchain/, knhk-otel/, knhk-orchestrator/
│   └── ggen-folk-strategy/         # Folk calculus quantification
│
├── tests/                          # WORKSPACE-LEVEL TESTS
│   ├── integration/                # 20+ integration test suites
│   ├── bdd/                        # Cucumber with 13 step modules
│   └── security/                   # Input validation tests
│
├── benches/                        # PERFORMANCE BENCHMARKS (15+ suites)
│   ├── marketplace/, cli_performance/, pipeline/, async_runtime/
│   └── [performance measurement with Criterion + HTML reports]
│
├── .claude/                        # CLAUDE CODE CONFIGURATION
│   ├── settings.json               # Agent definitions
│   ├── agents/                     # 22+ specialized agents
│   ├── hooks/                      # 9 pre/post tool hooks
│   └── skills/                     # 37+ domain skills (on-demand)
│
├── .github/workflows/              # CI/CD PIPELINES (18 total)
│   ├── ci.yml, andon-validation.yml, performance.yml
│   ├── security-audit.yml, marketplace-*.yml
│   └── [18 workflow files]
│
├── Makefile.toml                   # 70+ build targets (timeouts enforced)
├── Cargo.toml                      # Workspace manifest (30 members, workspace lints)
├── Cargo.lock                      # Dependency lock file
├── docs/                           # 50+ documentation subdirectories
├── examples/                       # 40+ production-grade examples
├── templates/                      # Tera code generation templates
├── scripts/                        # Build and utility scripts
├── marketplace/                    # Package marketplace registry
├── README.md                       # Main project README
├── CLAUDE.md                       # Claude Code project instructions (THIS FILE)
├── V6_RELEASE_NOTES.md            # v6.0.0 changelog
├── SECURITY.md, TESTING.md, CONTRIBUTING.md
└── [100+ markdown documentation files]
```

**Key File Locations:**
- **RDF Specifications**: `.specify/specs/NNN-*/` (edit `.ttl` files, never `.md`)
- **Source Code**: `crates/*/src/` (production code only)
- **Tests**: `crates/*/tests/`, `/tests/`, `#[cfg(test)]` modules
- **Benchmarks**: `/benches/` with Criterion HTML reports
- **CLI Configuration**: `.claude/settings.json`, `.claude/agents/`
- **CI/CD**: `.github/workflows/` with 18 workflow files
- **Build Automation**: `Makefile.toml` (ONLY use `cargo make`, never direct cargo)

## 🚨 Definition of Done (Andon Signals Enforced)

**BEFORE MARKING ANY TASK AS COMPLETE - MANDATORY VALIDATION CHECKS:**

### 1. Verify Timeout Command
```bash
cargo make timeout-check
```

### 2. Check for Compiler Errors (CRITICAL SIGNAL)
```bash
cargo make check
```
- **IF ERRORS FOUND**: STOP THE LINE - Do not proceed. Fix compiler errors immediately.
- **VERIFY**: No `error[E...]` patterns in output - must be clean

### 3. Check for Compiler Warnings (HIGH SIGNAL)
Review `cargo make check` output:
- **IF WARNINGS FOUND**: STOP THE LINE - Fix warnings before proceeding
- **VERIFY**: No `warning:` patterns in output - must be clean

### 4. Run Tests (CRITICAL SIGNAL)
```bash
cargo make test
```
- **IF TESTS FAIL**: STOP THE LINE - Do not proceed. Extract failing test names, create rich todos with:
  - Test name
  - Error message
  - File/line location
  - Root cause analysis (use 5 Whys)
  - Proposed fix
  - Status (pending/in_progress/completed)
- **VERIFY**: No `test ... FAILED` patterns - all tests must pass

### 5. Check for Linting Errors (HIGH SIGNAL)
```bash
cargo make lint
```
- **IF LINTING ERRORS FOUND**: STOP THE LINE - Fix linting errors before proceeding
- **VERIFY**: No clippy warnings/errors - must be clean

### 6. Verify Performance SLOs
```bash
cargo make slo-check
```
- **IF SLOs NOT MET**: Investigate performance regressions
- **VERIFY**: All SLOs met:
  - First build ≤ 15s
  - Incremental ≤ 2s
  - RDF processing ≤ 5s for 1k+ triples
  - Generation memory ≤ 100MB
  - CLI scaffolding ≤ 3s end-to-end

### 7. Systematic Fixing
If any signals found:
1. Batch create 10+ related todos in single call for systematic fixing
2. Fix systematically: Read failure message → Identify root cause → Fix issue → Run specific check → Verify signal cleared → Update todo status → Remove when fixed
3. Re-run validation checks to verify all signals cleared
4. If still failing, return to step 2

### 8. Final Verification - All Signals Cleared
- ✅ `cargo make check` - No compiler errors or warnings
- ✅ `cargo make test` - All tests pass
- ✅ `cargo make lint` - No linting errors
- ✅ `cargo make slo-check` - SLOs met
- ✅ All failing tests fixed and removed from todos
- ✅ No compilation errors
- ✅ No test failures
- ✅ No pending test-related todos

**ONLY mark complete when ALL signals are cleared and ALL validation checks pass**

## 🚫 Prohibited Patterns (Production-Ready Standards)

- **NEVER USE DIRECT CARGO COMMANDS - ALWAYS USE `cargo make`**
- **NEVER USE `cargo fmt`, `cargo clippy`, `cargo test` DIRECTLY**
- **NEVER USE `cargo build` WITHOUT `cargo make`**
- **NEVER RUN COMMANDS WITHOUT TIMEOUT WRAPPERS**
- **NEVER SKIP TIMEOUT-CHECK** - Always verify timeout command exists
- **NEVER IGNORE ANDON SIGNALS** - Stop the line when signals appear
- **NEVER PROCEED WITH SIGNALS PRESENT** - Do not continue work with errors/warnings
- **NEVER SUPPRESS OR HIDE SIGNALS** - Do not use `#[allow(...)]` without fixing root cause
- **NEVER MARK COMPLETE WITHOUT VERIFYING SIGNALS CLEARED**
- No placeholders or "In production, this would..." comments
- No TODO comments (use FUTURE: prefix for documented future enhancements)
- No `unimplemented!()` - Complete implementations required
- No `unwrap()`/`expect()` in production code - Use `Result<T, E>`
- No stubs - No functions that always succeed without implementation
- No claims without verification - Never claim code works without test validation
- No meaningless tests - Tests must verify observable outputs/state changes
- No Chicago TDD violations - Must use state-based testing, real collaborators, AAA pattern
- No `print!` or `println!` in library code - Use `log!` macros or alert macros

## 🎯 SLOs (Service Level Objectives)

ggen project performance targets:
- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs (deterministic)

## 📊 Performance Benefits (Claude Flow Integration)

- **84.8% SWE-Bench solve rate**
- **32.3% token reduction**
- **2.8-4.4x speed improvement**
- **27+ neural models**

## 🔗 Hooks Integration

### Pre-Operation
- Auto-assign agents by file type
- Validate commands for safety (ensure cargo make, not direct cargo)
- Prepare resources automatically
- Optimize topology by complexity
- Cache searches

### Post-Operation
- Auto-format code (`cargo make fmt`)
- Train neural patterns
- Update memory
- Analyze performance
- Track token usage

### Session Management
- Generate summaries
- Persist state
- Track metrics
- Restore context
- Export workflows

## 🚀 Advanced Features (v2.0.0)

- 🚀 Automatic Topology Selection
- ⚡ Parallel Execution (2.8-4.4x speed)
- 🧠 Neural Training
- 📊 Bottleneck Analysis
- 🤖 Smart Auto-Spawning
- 🛡️ Self-Healing Workflows
- 💾 Cross-Session Memory
- 🔗 GitHub Integration

## 💡 Integration Tips
**Critical Rule**: Never create working files in root directory. Use appropriate subdirectories.

---

## Stack (v6.0.0)

| Component | Version | Purpose |
|-----------|---------|---------|
| Rust | 1.91.1 | Core language (type-safe, zero-cost abstractions) |
| Tokio | 1.47 | Async runtime (full feature set) |
| Oxigraph | 0.5.1 | RDF store (Turtle, SPARQL 1.1, inference) |
| Tera | 1.20 | Template engine (SPARQL-aware multi-pass rendering) |
| Serde | 1.0 | Serialization (JSON, YAML, TOML) |
| Clap | 4.5 | CLI framework (derive API) |
| Clap Noun-Verb | 5.0.0 | Advanced CLI patterns |
| genai | 0.5 | Multi-provider LLM client (GPT-4, Claude, local models) |
| chicago-tdd-tools | 1.4.0 | AAA testing (Arrange/Act/Assert, real objects, no mocks) |
| proptest | 1.8 | Property-based testing |
| criterion | 0.7 | Performance benchmarking (HTML reports) |
| testcontainers | 0.25 | Integration testing with Docker |
| OpenTelemetry | 0.21 | Observability (optional - enable with `--features otel`) |
| insta | Latest | Snapshot testing for deterministic outputs |
| serial_test | Latest | Deterministic async test execution |
| pqcrypto-mldsa | 0.1 | NIST ML-DSA post-quantum cryptography |
| Axum | 0.8 | Web framework for ggen-api |
| Tonic | 0.14 | gRPC framework for distributed systems |

### Optional Features

**OpenTelemetry Instrumentation** (opt-in for production deployments):
```bash
# Development builds (default - faster, no OTEL)
cargo build

# Production builds (with OTEL instrumentation)
cargo build --release --features otel
```

**Why optional?** OTEL stack adds ~200 dependencies, significantly slowing development builds. Feature-gating provides:
- **50% faster development builds** (no OTEL dependencies)
- **Production observability** when needed (enable with `--features otel`)
- **Zero runtime overhead** when disabled

See `/docs/features/otel-optional-feature.md` for complete documentation.

---

## Crates (30 Total)

### Core System (8)
- **ggen-core** (4.2M): RDF processing, SPARQL 1.1 engine, template rendering (218 files, 25 modules)
- **ggen-cli** (1.8M): CLI entry point, command routing, user interface
- **ggen-domain** (1.6M): Domain models, business logic, validation, MAPE-K autonomic loop
- **ggen-utils** (431K): Logging, error handling, shared utilities, safe path validation
- **ggen-config**: Configuration management (TOML, env vars, AppConfig)
- **ggen-macros**: Procedural macros for code generation
- **ggen-node**: Node.js bindings (NAPI-RS)
- **ggen-dod**: Data-oriented design patterns

### CLI & Validation (3)
- **ggen-cli-validation**: Pre/post-flight validation gates, quality assurance
- **ggen-config-clap**: Clap integration for CLI configuration
- **ggen-spec-validator**: SHACL validation for RDF specifications

### Testing & Quality (3)
- **ggen-test-audit**: Test quality metrics, mutation testing analysis
- **ggen-test-opt**: Test optimization, deduplication, performance analysis
- **ggen-e2e**: End-to-end integration testing with testcontainers

### Marketplace (1)
- **ggen-marketplace-v2** (596K): Package discovery, FMEA risk analysis, template registry

### RevOps / Monetization (4)
- **ggen-api** (311K): REST API layer for SaaS platform
- **ggen-auth**: Authentication (OAuth2, JWT, API keys, session management)
- **ggen-payments**: Payment processing (Stripe integration, billing)
- **ggen-saas**: Multi-tenant management, quota enforcement, licensing

### AI Orchestration (2)
- **ggen-ai** (2.6M): Multi-provider LLM integration (GPT-4, Claude, local models), genai client
- **ggen-dspy** (439K): DSPy predictor patterns, constraint calculus integration

### Ontology & RDF (1)
- **ggen-ontology-core**: RDF/Turtle processing, SPARQL 1.1, entity mapping, validators (v0.2.0)

### KNHK Systems (ETL + KGC-4D + Workflow) (6)
- **knhk-etl**: Extract-Transform-Load pipeline for knowledge graph construction
- **knhk-hot**: C FFI hot-path optimization (performance-critical operations)
- **knhk-connectors**: Connector registry (Kafka, HTTP, databases)
- **knhk-lockchain**: Merkle-linked receipt storage (cryptographic provenance)
- **knhk-otel** (704K): OpenTelemetry integration (optional feature, tracing, metrics)
- **knhk-orchestrator**: Integration bridge (ETL → KGC-4D → Workflow Engine)

### Specialized Systems (1)
- **ggen-folk-strategy**: Folk calculus quantification and analysis

---

1. Start with basic swarm init for complex workflows
2. Scale agents gradually based on task complexity
3. Use memory for cross-agent context sharing
4. Monitor Andon signals continuously (check, test, lint)
5. Train patterns from successful workflows
6. Enable hooks automation for consistent formatting
7. Use GitHub tools for PR/issue coordination
8. Always batch todos (10+ minimum)
9. Always use `cargo make` (NEVER direct cargo)
10. Stop the line when Andon signals appear

## 📚 Support & Documentation

- **ggen Repository**: https://github.com/seanchatmangpt/ggen
- **Claude Flow Documentation**: https://github.com/ruvnet/claude-flow
- **Claude Flow Issues**: https://github.com/ruvnet/claude-flow/issues
- **Flow-Nexus Platform**: https://flow-nexus.ruv.io (optional cloud features)

I implement these by default without being asked:
- `Result<T,E>` for all fallible operations
- Zero `unwrap/expect` in production code (tests OK)
- Chicago TDD pattern (AAA: Arrange/Act/Assert with real objects, no mocks)
- Type-safe design (constraints in types, compiler verification)
- Error context mapping (`map_err`, custom error types with `thiserror`)
- Idiomatic Rust (clippy compliance, naming conventions)
- Performance awareness (SLO targets, O(n) complexity analysis)
- Poka-Yoke design (error prevention at compile time)
- Deterministic outputs (same input → same output, always)

---

## Holographic Factory Metaphor (v6)

**Core Equation**: $A = \mu(O)$ where μ is a **five-stage deterministic pipeline**:

```
μ₁ (Normalize)   → RDF validation, SHACL shapes, dependency resolution
μ₂ (Extract)     → SPARQL queries, OWL inference, rule execution
μ₃ (Emit)        → Tera template rendering, code generation
μ₄ (Canonicalize)→ Deterministic formatting, content hashing
μ₅ (Receipt)     → Cryptographic proof generation, audit trail
```

**Substrate** (unrdf): RDF ontology as high-dimensional holographic film encoding domain knowledge.

**History** (KGC-4D): Git snapshots as temporal coherence waypoints. Each commit is a 4D slice of the knowledge graph evolution.

**Transformation** (ggen sync): Five-stage pipeline precipitates code from interference patterns in RDF ontology.

**Corollary**: Bug in generated code? Fix the RDF spec (interference pattern), not the output (projection). The ontology is the source of truth.

**Quality Gates (Poka-Yoke)**: Six pre-flight checks prevent defects before μ₁:
1. Manifest schema validation
2. Ontology dependency resolution
3. SPARQL query syntax validation
4. Template syntax validation
5. File permission checks
6. Rule validation

**Deterministic Receipts**: Every `ggen sync` generates cryptographic proof:
- Execution ID + timestamp (ISO 8601)
- Manifest hash (SHA-256) + Ontology hash (SHA-256)
- Files generated + content hashes (SHA-256 per file)
- Inference rules executed + timings (μs precision)
- Generation rules executed + timings (μs precision)
- Audit trail path (JSON log with full provenance)

---

## 🎯 Key Associations & Mental Models

- **Types = invariants = compile-time guarantees**
- **Zero-cost = generics/macros/const generics**
- **Performance = references/stack/minimize allocations**
- **Ownership = explicit = memory safety**
- **APIs = type-safe = ergonomic = composable**
- **Tests = observable outputs = behavior verification**
- **80/20 = second idea = sweet spot = maximum value**
- **Andon Signals = stop = fix = verify**
- **DfLSS = prevent defects AND waste from start**

## 📝 Remember

**Claude Flow coordinates, Claude Code creates!**

**Stop the line when Andon signals appear - fix root cause before proceeding!**

**Always use `cargo make` - NEVER direct cargo commands!**

**TodoWrite always has 10+ todos in a single batch!**
1. `cargo make check` passes (zero errors, warnings-as-errors)
2. `cargo make lint` passes (zero warnings, clippy `-D warnings`)
3. `cargo make test` passes (all tests green, <30s SLO)
4. No `unwrap/expect` in production code (clippy enforced)
5. All APIs return `Result<T, E>` (no naked errors)
6. Deterministic outputs (same input → same output, verified by receipts)
7. SHACL validation passes for all `.specify/*.ttl` files

---

## When to Use EPIC 9 (Big Bang 80/20)

**Trigger Conditions** (use EPIC 9 if ANY apply):
- Non-trivial tasks affecting 5+ files or 3+ systems
- Multiple valid approaches with significant trade-offs
- Large architectural decisions requiring parallel exploration
- Unclear requirements needing hypothesis testing
- New feature domains with unknown unknowns

**Workflow** (EPIC 9 Atomic Cycle):
```
1. Specification Closure (100% coverage in .specify/*.ttl)
   ↓
2. Fan-Out (10 parallel agents, independent exploration)
   ↓
3. Collision Detection (structural + semantic overlap analysis)
   ↓
4. Convergence (selection pressure: coverage, invariants, minimality, elegance)
   ↓
5. Refactoring (DRY, type-safety, performance optimization)
   ↓
6. Closure (deterministic receipts, cryptographic proof)
```

**Claude Agents Available** (8 specialized agents):
- **bb80-specification-validator**: Validates TTL closure (100% coverage check)
- **bb80-parallel-task-coordinator**: Spawns 10+ agents, monitors execution
- **bb80-collision-detector**: Detects structural/semantic overlap
- **bb80-convergence-orchestrator**: Synthesizes optimal solution from parallel outputs
- **rust-coder**: Idiomatic Rust implementation specialist
- **reviewer**: Code review (type safety, security, performance)
- **speckit-architect**: RDF specification designer (Turtle ontologies)
- **test-engineer**: Chicago TDD test specialist (AAA pattern)

**Claude Skills Available** (11 domain skills):
- **cargo-make-protocol**: Master Cargo Make, Poka-Yoke, SLO enforcement
- **chicago-tdd-pattern**: State-based testing, AAA pattern
- **poka-yoke-patterns**: Error-proofing, FMEA, quality gates
- **rdf-ontologies**: Turtle syntax, SPARQL, SHACL validation
- **bb80-specification-closure**: 100% coverage verification
- **bb80-parallel-agents**: 10-agent parallel orchestration
- **bb80-deterministic-receipts**: Cryptographic proof generation
- **bb80-invariant-construction**: Type-safe invariant enforcement
- **session-start-hook**: Repository setup for Claude Code on web
- **mcp-servers**: Model Context Protocol integration

---

## Remember (Critical Reminders)

- **Spec Closure First**: Verify 100% coverage in `.specify/*.ttl` before any code generation (non-negotiable for Big Bang 80/20).
- **Receipts Over Narratives**: Always produce deterministic evidence (test counts, compile times, SLO metrics, cryptographic hashes).
- **RDF is Reality**: Edit `.ttl` files; everything else (code, docs, configs, markdown) is a generated projection via μ.
- **Andon Stops Work**: 🔴 RED = halt immediately; 🟡 YELLOW = investigate before release; 🟢 GREEN = proceed safely.
- **Cargo Make is Law**: All validation through `Makefile.toml`, never raw `cargo` (prevents bypassing quality gates and timeout enforcement).
- **Parallel First**: For non-trivial work, always use EPIC 9 (10 agents + collision + convergence + receipts).
- **V6 Unified Command**: Use `ggen sync` for ALL generation tasks. Old commands (`ggen generate`, `ggen template`, etc.) are deprecated.
- **Quality Gates**: Six pre-flight checks must pass before μ₁ (Normalize) starts. Poka-Yoke prevents defects, not detects them.
- **Deterministic Always**: Same ontology + same templates = identical output every time. Verified by SHA-256 content hashing.

---

## V6 Five-Stage Pipeline (μ)

### μ₁ (Normalize)
- Load and parse RDF ontology (Turtle, RDF/XML, N-Triples)
- SHACL shape validation (ensure ontology conforms to schema)
- Dependency resolution (imports, external ontologies)
- OWL inference (materialize implicit triples)
- **Output**: Validated, normalized RDF graph

### μ₂ (Extract)
- Execute SPARQL queries (SELECT, CONSTRUCT, ASK, DESCRIBE)
- Apply inference rules (RDFS, OWL2-RL)
- Extract template context (JSON/YAML binding for Tera)
- **Output**: Structured data for template rendering

### μ₃ (Emit)
- Tera template rendering (SPARQL-aware, multi-pass)
- Code generation (Rust, TypeScript, Python, Go, etc.)
- Multi-file generation (directory structures, modules)
- **Output**: Raw generated artifacts (code, configs, docs)

### μ₄ (Canonicalize)
- Deterministic formatting (rustfmt, prettier, black)
- Syntax validation (compiler checks, linters)
- Content hashing (SHA-256 per file)
- **Output**: Canonicalized, formatted artifacts

### μ₅ (Receipt)
- Cryptographic proof generation (execution ID, hashes)
- Audit trail logging (JSON with full provenance)
- Manifest + ontology fingerprinting
- File-by-file change tracking
- **Output**: Deterministic receipt (JSON), audit log (JSON)

---

**Test results are truth - code doesn't work if tests don't pass!**

---

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
Never save working files, text/mds and tests to the root folder.
TODO LISTS ARE ALWAYS 10 ITEMS OR MORE. THEY ARE ALWAYS FULLY COMPLETED BEFORE PROGRESSING TO THE NEXT TASK.
### Essential Files
- **settings.json**: Permissions, environment variables, MCP servers
- **agents/**: 8 specialized agent definitions (rust-coder, reviewer, etc.)
- **hooks/**: 9 pre/post hooks for safety and validation
  - `pre-specification-check.sh`: Verify TTL closure before EPIC 9
  - `pre-tool-safety-check.sh`: Prevent destructive operations
  - `post-bash-validation.sh`: Verify bash commands succeeded
  - `post-collision-detection.sh`: Analyze agent overlap
  - `convergence-validation.sh`: Ensure convergence quality
  - `session-start.sh`: Initialize session environment
  - `user-prompt-validation.sh`: Validate user inputs
- **skills/**: 11 domain skill modules (loaded on demand)

### Not in Critical Path
- Documentation files in `.claude/` are reference only (not auto-loaded during sessions)
- Skills are loaded when explicitly requested or when agent descriptions match task
- Agents are spawned on-demand via Task tool when task matches agent capabilities

---

## Development Workflow (v6)

### 1. Create Feature Specification (RDF-First)
```bash
# Create feature directory
mkdir -p .specify/specs/013-feature-name

# Copy template and edit TTL (SOURCE OF TRUTH)
cp .specify/templates/rdf-helpers/user-story.ttl.template \
   .specify/specs/013-feature-name/feature.ttl

# Edit TTL with RDF data
vim .specify/specs/013-feature-name/feature.ttl

# Validate SHACL conformance
ggen validate .specify/specs/013-feature-name/feature.ttl

# Generate markdown for GitHub viewing
cargo make speckit-render
```

### 2. Implement Feature (Chicago TDD)
```bash
# Write failing test FIRST (AAA pattern: Arrange/Act/Assert)
vim crates/ggen-core/tests/feature_test.rs

# Verify test fails (RED)
cargo make test-unit

# Implement minimal code to pass (GREEN)
vim crates/ggen-core/src/feature.rs

# Verify test passes
cargo make test-unit

# Refactor (maintain GREEN)
cargo make pre-commit  # check → lint → test-unit
```

### 3. Generate Code from Ontology
```bash
# Dry-run to preview changes
ggen sync --dry_run true

# Full sync with audit trail
ggen sync --audit true

# Verify receipt and audit log
cat .ggen/receipts/latest.json
cat .ggen/audit/$(date +%Y-%m-%d).json
```

### 4. Commit with Evidence
```bash
# Run full pre-commit quality gate
cargo make pre-commit

# Stage changes
git add .specify/specs/013-feature-name/feature.ttl
git add crates/ggen-core/src/feature.rs
git add crates/ggen-core/tests/feature_test.rs

# Commit with receipt evidence
git commit -m "feat(013): Implement feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates passed
[Receipt] cargo make test: ✓ 347/347 tests, 28.3s
[Receipt] ggen sync --audit: ✓ 12 files generated, 0 conflicts
[Receipt] Audit trail: .ggen/audit/2026-01-18.json"
```

---

## Troubleshooting

### Problem: `ggen sync` times out or hangs
**Solution**: Use timeout enforcement via cargo make:
```bash
# Quick check (15s timeout)
timeout 15s ggen sync --validate_only true

# Full sync with timeout (120s)
timeout 120s ggen sync --audit true
```

### Problem: SHACL validation fails
**Solution**: Check `.specify/*.ttl` for schema violations:
```bash
# Validate specific file
ggen validate .specify/specs/013-feature-name/feature.ttl

# Common issues:
# - Priority must be exactly "P1", "P2", or "P3" (not "HIGH", "MEDIUM", "LOW")
# - Each UserStory must have at least 1 AcceptanceScenario
# - All required properties must be present
```

### Problem: Deterministic receipt hash mismatch
**Solution**: Ensure canonical formatting is applied:
```bash
# Reformat all Rust code
cargo make fmt

# Regenerate with force flag
ggen sync --force true --audit true

# Verify receipt
cat .ggen/receipts/latest.json | jq '.files[] | {path, hash}'
```

### Problem: Cargo make pre-commit fails on warnings
**Solution**: Fix all warnings (treated as errors in v6):
```bash
# Check which warnings exist
cargo make check

# Fix warnings in code
# Common issues:
# - Unused imports, variables, functions
# - Missing documentation
# - Clippy lints (pedantic, nursery, cargo groups enabled)

# Verify clean build
cargo make pre-commit
```

---

## Migration from v5.1.0 to v6.0.0

### Breaking Changes
1. **Unified Command**: Replace all `ggen generate`, `ggen template`, `ggen validate`, `ggen render` with `ggen sync`
2. **Quality Gates**: Pre-flight validation now mandatory (use `--force true` to override, but generates warning in audit log)
3. **Receipts**: Audit trails now required for production deployments (use `--audit true`)
4. **Warnings-as-Errors**: All compiler warnings treated as errors (clippy `-D warnings`)
5. **SHACL Validation**: `.specify/*.ttl` files must pass SHACL validation before generation

### Migration Steps
```bash
# 1. Update Cargo.toml workspace dependencies
cargo update

# 2. Replace old commands in scripts/CI
# OLD: ggen generate --template foo.tera --output bar.rs
# NEW: ggen sync

# 3. Add quality gates to Makefile.toml (if custom targets exist)
# Ensure timeout enforcement and warnings-as-errors

# 4. Validate all .specify/*.ttl files
cargo make speckit-validate

# 5. Run pre-commit to verify no warnings
cargo make pre-commit

# 6. Generate initial receipts
ggen sync --audit true
```

---

## Further Reading

- [V6 Release Notes](V6_RELEASE_NOTES.md) - Complete v6 changelog
- [Big Bang 80/20 Master Plan](BIG_BANG_80_20_MASTER_PLAN.md) - EPIC 9 methodology
- [Poka-Yoke Patterns](Makefile.toml) - Error-proofing mechanisms
- [.specify/ README](.specify/README.md) - RDF-first specification system
- [Testing Guide](TESTING.md) - Chicago TDD patterns
- [Security Guide](SECURITY.md) - SPARQL injection prevention
- [Contributing Guide](CONTRIBUTING.md) - Development workflow
- [Performance Guide](PERFORMANCE.md) - SLO targets and benchmarking

---

## 📊 Repository Status (as of January 2026)

### Project Metrics
- **Crates**: 30 total (27 core + 3 supporting)
- **Source Files**: 218 files in ggen-core alone, across all crates: ~1000+ files
- **Tests**:
  - Unit tests: Chicago TDD pattern throughout
  - Integration tests: 20+ suites
  - BDD tests: 13 Cucumber step modules
  - Security tests: Input validation, injection prevention
  - Performance benchmarks: 15+ Criterion suites
- **Documentation**:
  - Markdown files: 156+ in root directory
  - Documentation directories: 50+ subdirectories
  - API documentation: Comprehensive with examples
- **CI/CD**: 18 GitHub Actions workflows
- **Build Targets**: 70+ Makefile targets with timeout enforcement

### Code Quality (Enforced at Compile Time)
- **Warnings**: Treated as errors (clippy `-D warnings`)
- **Unwrap/Expect**: Zero in production code (clippy enforced)
- **Result<T,E>**: Required for all fallible operations
- **Test Coverage**: 87% in ggen-ontology-core (100% passing)
- **Performance**: All SLOs met (first build ≤15s, RDF processing ≤5s/1k+ triples)

### Recent Progress
- v6.0.0 released January 2026 (production-ready core)
- v0.2.0 ontology layer production-ready (Jan 19, 2026)
- 64 Chicago TDD tests in ggen-ontology-core (100% passing, 87% coverage)
- Five-stage deterministic pipeline (μ₁-μ₅) fully implemented
- Poka-Yoke error-proofing integrated throughout

### Critical Files for Claude Code
1. **CLAUDE.md** (THIS FILE) - Project instructions for AI assistants
2. **.claude/settings.json** - Agent configuration
3. **.claude/agents/** - 22+ specialized agents
4. **.claude/hooks/** - 9 pre/post tool hooks for safety
5. **Makefile.toml** - Build automation (MUST use `cargo make`)
6. **.specify/** - RDF specifications (source of truth)
7. **V6_RELEASE_NOTES.md** - Latest release information

### Development Workflow (v6)
1. **Edit RDF**: Create/edit `.specify/specs/NNN-*/feature.ttl` (source)
2. **Generate Markdown**: `cargo make speckit-render` (generated artifacts)
3. **Write Tests First**: Chicago TDD pattern (Arrange/Act/Assert)
4. **Implement**: Type-first design, zero-unwrap enforcement
5. **Validate**: `cargo make pre-commit` → check → lint → test-unit
6. **Verify**: `cargo make test` (full suite), `cargo make slo-check`
7. **Commit**: Evidence-based commits with audit trail references

---

**Last Updated**: 2026-01-25 (v6.0.0 production-ready core with 30 crates, comprehensive analysis)
