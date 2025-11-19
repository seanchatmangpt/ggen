# Claude Code Configuration - ggen Rust Project

## 📋 Project Identity

**ggen** is a language-agnostic, deterministic code generation CLI built in Rust that turns ontologies + RDF-like metadata into reproducible code projections.

- **Language**: Rust (stable toolchain)
- **Architecture**: Workspace with multiple crates (ggen-core, ggen-cli, ggen-domain, ggen-utils, etc.)
- **Methodology**: SPARC + Chicago TDD + DfLSS (Design for Lean Six Sigma)
- **Core Principles**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

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
├── crates/
│   ├── ggen-core/         # Core domain logic
│   ├── ggen-cli/          # CLI arg parsing and commands
│   ├── ggen-domain/       # Domain models and MAPE-K loop
│   ├── ggen-utils/        # Shared utilities (config, logging, errors)
│   ├── ggen-ai/           # AI integration
│   ├── ggen-config/       # Configuration management
│   ├── ggen-marketplace/  # Package marketplace
│   └── ggen-node/         # Node.js bindings
├── tests/                 # Integration tests
├── examples/              # Example projects
├── docs/                  # Documentation
├── scripts/               # Build and utility scripts
└── Cargo.toml            # Workspace manifest
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

### Timeout SLAs (CRITICAL):
Every CLI command MUST have timeout wrapper to prevent freezing:
- Quick checks: `timeout 5s` (cargo check, cargo fmt, cargo clippy)
- Compilation: `timeout 10s` (cargo build debug)
- Release builds: `timeout 30s` (cargo build --release)
- Unit tests: `timeout 10s` (cargo test --lib)
- Integration tests: `timeout 30s` (cargo test --test)
- Pre-push hooks: `timeout 30s` (cargo make check-pre-push)

## 🧪 Testing Strategy (Chicago TDD - MANDATORY)

**Chicago TDD**: State-based testing with real collaborators and behavior verification.

### Core Principles:
1. **State-based testing** - Verify outputs, not implementation
2. **Real collaborators** - Use real objects, minimize mocks
3. **Behavior verification** - Verify what code does (observable outputs/state changes)
4. **AAA pattern required** - Arrange-Act-Assert
5. **Tests verify**: Return values, state changes, side effects, execution order, actual effects on system

### Test Categories:
- **Unit tests**: Colocated with source (`crates/*/src/*_test.rs` or `#[cfg(test)] mod tests`)
- **Integration tests**: In `/tests` and `crates/*/tests`
- **Property tests**: Using `proptest` for parsers, RDF, templating
- **Snapshot tests**: Using `insta` for deterministic outputs
- **Performance tests**: Benchmarks in `/benches`

### Test Requirements:
- ✅ All public APIs must be tested
- ✅ Test error paths, edge cases, critical paths (80%+ coverage)
- ✅ **No meaningless tests** - Tests must verify observable outputs/state changes, not just `assert_ok!()`
- ✅ **Behavior verification** - Tests verify what code does, not just that functions exist
- ✅ **Never claim completion without running tests** - Tests must pass before work is done
- ✅ Run with: `--test-threads=1` for deterministic async tests

### Expert Testing (80/20):
Focus on the 20% that catches 80% of bugs:
- Error paths and boundary conditions
- Resource cleanup and lifecycle
- Concurrency and race conditions
- Real dependencies (not mocks)

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

# Coordination completion
npx claude-flow@alpha hooks post-task --task-id "[task]"
npx claude-flow@alpha hooks session-end --export-metrics true
```

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

**Test results are truth - code doesn't work if tests don't pass!**

---

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
Never save working files, text/mds and tests to the root folder.
TODO LISTS ARE ALWAYS 10 ITEMS OR MORE. THEY ARE ALWAYS FULLY COMPLETED BEFORE PROGRESSING TO THE NEXT TASK.
