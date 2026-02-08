# Native Agent Coordination System

**Version**: 1.0.0
**Project**: ggen v6.0.0
**Coordination**: Claude Code Task tool (NO MCP dependencies)

---

## Overview

This directory contains agent definitions for native coordination using Claude Code's built-in Task tool. Each agent is defined as a simple JSON configuration that specifies capabilities, tools, constraints, and workflows.

**Core Principle**: Lightweight, declarative agent definitions that work directly with Claude Code without external dependencies.

---

## Directory Structure

```
.claude/agents/
├── core/                    # General-purpose agents
│   ├── coder.json          # Code implementation
│   ├── reviewer.json       # Quality assurance
│   └── tester.json         # Test creation/validation
├── specialized/            # Domain-specific agents
│   ├── rust-coder.json     # Rust implementation specialist
│   ├── speckit-architect.json  # RDF ontology design
│   └── test-engineer.json  # Chicago TDD specialist
└── README.md               # This file
```

---

## Agent Types

### Core Agents (General Purpose)

**coder** - General-purpose code implementation
- Model: claude-sonnet-4-5
- Role: Implementation of features across languages
- Capabilities: code-writing, refactoring, api-design, error-handling
- Works with: reviewer, tester, planner

**reviewer** - Code quality and best practices
- Model: claude-sonnet-4-5
- Role: Quality assurance and code review
- Capabilities: code-review, security-analysis, performance-review
- Works with: coder, tester

**tester** - Test creation and validation
- Model: claude-sonnet-4-5
- Role: Testing and coverage analysis
- Capabilities: test-writing, test-execution, coverage-analysis
- Works with: coder, reviewer

### Specialized Agents (ggen-Specific)

**rust-coder** - Idiomatic Rust implementation
- Model: claude-sonnet-4-5
- Role: Rust-specific implementation with type-first design
- Capabilities: rust, type-first-design, zero-cost-abstractions, ownership-semantics
- Principles: Zero unwrap/expect, Result<T,E>, cargo make only
- Works with: test-engineer, speckit-architect

**speckit-architect** - RDF ontology and specification design
- Model: claude-sonnet-4-5
- Role: Specification design and ontology architecture
- Capabilities: rdf-ontology-design, shacl-validation, sparql-queries, ttl-authoring
- Principles: RDF is truth, spec closure first, μ pipeline (A = μ(O))
- Works with: rust-coder, test-engineer

**test-engineer** - Chicago TDD specialist
- Model: claude-sonnet-4-5
- Role: Comprehensive testing with Chicago TDD methodology
- Capabilities: chicago-tdd, property-based-testing, integration-testing, determinism-verification
- Principles: State-based, real collaborators, AAA pattern, 80%+ coverage
- Works with: rust-coder, reviewer

---

## Usage with Task Tool

### Single Agent Execution

```rust
// Spawn one agent
Task("Rust Coder", "Implement type-safe parser for RDF triples", "rust-coder")
```

### Multi-Agent Coordination

```rust
// Batch ALL agents in ONE message (Golden Rule)
Task("Speckit Architect", "Design RDF ontology for feature X with SHACL validation", "speckit-architect")
Task("Rust Coder", "Implement parser from spec with Result<T,E> error handling", "rust-coder")
Task("Test Engineer", "Create Chicago TDD tests with AAA pattern and 80%+ coverage", "test-engineer")
Task("Code Reviewer", "Review implementation for type safety and performance", "reviewer")
```

### With TodoWrite (Always 10+ todos)

```rust
// Single message with agents + todos
Task("Rust Coder", "Implement feature", "rust-coder")
Task("Test Engineer", "Test feature", "test-engineer")

TodoWrite {
  todos: [
    { content: "Design type signatures for parser", status: "pending", activeForm: "Designing type signatures" },
    { content: "Implement normalize stage (μ₁)", status: "pending", activeForm: "Implementing normalize stage" },
    { content: "Implement extract stage (μ₂)", status: "pending", activeForm: "Implementing extract stage" },
    { content: "Implement emit stage (μ₃)", status: "pending", activeForm: "Implementing emit stage" },
    { content: "Add Result<T,E> error handling", status: "pending", activeForm: "Adding error handling" },
    { content: "Write unit tests (AAA pattern)", status: "pending", activeForm: "Writing unit tests" },
    { content: "Write integration tests", status: "pending", activeForm: "Writing integration tests" },
    { content: "cargo make check (Andon monitoring)", status: "pending", activeForm: "Running cargo make check" },
    { content: "cargo make lint (Andon monitoring)", status: "pending", activeForm: "Running cargo make lint" },
    { content: "cargo make test (full validation)", status: "pending", activeForm: "Running cargo make test" },
    { content: "Verify SLOs with cargo make slo-check", status: "pending", activeForm: "Verifying SLOs" },
    { content: "Review code quality", status: "pending", activeForm: "Reviewing code quality" }
  ]
}
```

---

## Agent Configuration Schema

```json
{
  "name": "agent-name",
  "description": "Brief description of agent role",
  "model": "claude-sonnet-4-5",
  "role": "category-of-work",
  "capabilities": [
    "capability-1",
    "capability-2"
  ],
  "tools": [
    "Read",
    "Write",
    "Edit",
    "Bash"
  ],
  "constraints": {
    "timeout_seconds": 300,
    "max_file_size": 50000,
    "parallel_operations": true
  },
  "workflow": {
    "before": ["step1", "step2"],
    "during": ["step1", "step2"],
    "after": ["step1", "step2"]
  },
  "coordination": {
    "works_with": ["agent1", "agent2"],
    "handoff_format": "description of output format"
  }
}
```

---

## Workflow Patterns

### Pattern 1: Spec-First Development

```
1. speckit-architect: Design RDF ontology (.specify/*.ttl)
2. rust-coder: Implement from spec (crates/*/src/)
3. test-engineer: Create comprehensive tests (crates/*/tests/)
4. reviewer: Quality assurance
```

### Pattern 2: Chicago TDD Cycle

```
1. test-engineer: Write failing test (RED)
2. rust-coder: Minimal implementation (GREEN)
3. test-engineer: Verify test passes
4. reviewer: Refactor while maintaining GREEN
```

### Pattern 3: Feature Implementation

```
1. planner: Break down feature into tasks
2. rust-coder: Implement with type-first design
3. test-engineer: AAA pattern tests with 80%+ coverage
4. reviewer: Security + performance review
```

---

## ggen-Specific Requirements

### Cargo Make (MANDATORY)

All agents MUST use `cargo make` commands, NEVER direct `cargo`:

```bash
# ✅ CORRECT
cargo make check
cargo make test
cargo make lint
cargo make pre-commit

# ❌ PROHIBITED
cargo check
cargo test
cargo clippy
cargo fmt
```

### Andon Signal Monitoring

Agents must monitor and respond to quality signals:

**🔴 CRITICAL (STOP THE LINE)**:
- `error[E...]` - Compiler errors
- `test ... FAILED` - Test failures

**🟡 HIGH (STOP BEFORE RELEASE)**:
- `warning:` - Compiler warnings
- `clippy::*` - Linter errors

**Action**: Stop work, fix root cause, verify cleared, then proceed.

### Definition of Done (Validation Checklist)

Before marking any task complete, agents MUST verify:

```bash
# 1. Verify timeout command exists
cargo make timeout-check

# 2. Check compiler errors (CRITICAL)
cargo make check
# VERIFY: No error[E...] patterns

# 3. Check warnings (HIGH)
# VERIFY: No warning: patterns

# 4. Run tests (CRITICAL)
cargo make test
# VERIFY: No test ... FAILED patterns

# 5. Check linting (HIGH)
cargo make lint
# VERIFY: Clean output

# 6. Verify SLOs
cargo make slo-check
# VERIFY: Performance targets met
```

### Prohibited Patterns (Production Code)

rust-coder and test-engineer agents enforce:

**❌ NEVER in production code:**
- `unwrap()` / `expect()`
- `panic!()`
- `unimplemented!()`
- `todo!()`
- `println!()` in libraries (use `log!` macros)
- TODO comments (use `FUTURE:` prefix)
- Placeholders or incomplete implementations

**✅ ALLOWED in tests only:**
- `unwrap()` / `expect()`

### File Organization

Agents must respect the 30-crate workspace structure:

```
/home/user/ggen/
├── .specify/          # RDF specifications (SOURCE OF TRUTH)
├── .claude/agents/    # Agent definitions (this directory)
├── crates/
│   ├── ggen-core/
│   │   ├── src/       # Source code
│   │   └── tests/     # Integration tests
│   ├── ggen-cli/
│   ├── ggen-domain/
│   └── [27 more crates...]
├── tests/             # Workspace-level tests
├── docs/              # Documentation
└── scripts/           # Utility scripts
```

**CRITICAL**: NEVER save files to project root.

---

## Coordination Principles

### 1. Golden Rule: "1 MESSAGE = ALL RELATED OPERATIONS"

Batch everything in single messages:
- ✅ TodoWrite: 10+ todos minimum
- ✅ Task tool: Spawn ALL agents together
- ✅ File ops: ALL reads/writes/edits together
- ✅ Bash: ALL commands chained with `&&`

### 2. Handoff Format

Agents coordinate by passing structured information:

```json
{
  "from_agent": "rust-coder",
  "to_agent": "test-engineer",
  "artifacts": [
    "/home/user/ggen/crates/ggen-core/src/parser.rs",
    "/home/user/ggen/crates/ggen-core/src/types.rs"
  ],
  "public_api": [
    "parse_triple() -> Result<Triple, ParseError>",
    "validate_triple() -> Result<(), ValidationError>"
  ],
  "error_types": [
    "ParseError",
    "ValidationError"
  ],
  "test_requirements": [
    "Test happy path with valid RDF",
    "Test error handling for invalid syntax",
    "Test edge cases (empty strings, special chars)",
    "Property-based tests with proptest",
    "Integration tests with Oxigraph"
  ]
}
```

### 3. No MCP Dependencies

This system uses ONLY Claude Code's built-in capabilities:
- ✅ Task tool for agent spawning
- ✅ TodoWrite for task tracking
- ✅ Read/Write/Edit for file operations
- ✅ Bash for command execution
- ❌ NO MCP tools
- ❌ NO external orchestration

---

## Agent Selection Guide

**Use core agents for:**
- Simple, language-agnostic tasks
- Quick prototypes
- General code review
- Basic testing

**Use specialized agents for:**
- Rust implementation → `rust-coder`
- RDF/ontology work → `speckit-architect`
- Comprehensive testing → `test-engineer`
- Complex architecture → `system-architect` (if available)
- Performance optimization → `performance-benchmarker` (if available)

**Rule of thumb**: If the task mentions Rust, types, cargo, or ggen-specific concepts → use specialized agents.

---

## Model Selection

All agents currently use `claude-sonnet-4-5` for consistency. Future versions may support:
- `claude-opus-4-6` for complex reasoning
- `claude-haiku-4-5` for fast, simple tasks
- Model selection based on task complexity

---

## Extending the System

### Adding a New Agent

1. Create JSON config in appropriate directory (core/ or specialized/)
2. Define name, model, role, capabilities
3. Specify tools, constraints, workflow
4. Document coordination with existing agents
5. Update this README with agent description

### Example: Adding a Security Agent

```json
{
  "name": "security-auditor",
  "description": "Security vulnerability analysis and remediation",
  "model": "claude-sonnet-4-5",
  "role": "security",
  "capabilities": [
    "vulnerability-scanning",
    "dependency-auditing",
    "code-security-review"
  ],
  "tools": ["Read", "Bash", "Grep"],
  "workflow": {
    "before": ["cargo make audit"],
    "during": ["Analyze dependencies", "Review unsafe code"],
    "after": ["Generate security report"]
  },
  "coordination": {
    "works_with": ["reviewer", "rust-coder"],
    "handoff_format": "vulnerability_list + severity + remediation_steps"
  }
}
```

---

## Best Practices

1. **Always batch operations** - Follow "1 MESSAGE = ALL RELATED OPERATIONS"
2. **Use specialized agents** - When task matches domain, prefer specialized over core
3. **Monitor Andon signals** - Stop and fix at first sign of quality issues
4. **Verify before completion** - Run full validation checklist
5. **Coordinate explicitly** - Pass structured handoff information between agents
6. **Respect file organization** - Never save to root, use appropriate crate directories
7. **Follow cargo make** - NEVER use direct cargo commands
8. **Create comprehensive todos** - Always 10+ todos in one batch

---

## Performance SLOs

Agents must respect ggen's performance targets:
- First build: ≤ 15s
- Incremental build: ≤ 2s
- RDF processing: ≤ 5s per 1000+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end
- Test execution: < 30s full suite

Verify with: `cargo make slo-check`

---

## Support & Evolution

This agent system evolves with ggen. Current version (1.0.0) supports:
- ✅ Native Task tool coordination
- ✅ Cargo make enforcement
- ✅ Andon signal monitoring
- ✅ Chicago TDD methodology
- ✅ Type-first Rust design
- ✅ RDF ontology architecture

Future enhancements may include:
- Additional specialized agents
- Multi-model support
- Performance optimization agents
- Advanced coordination patterns

---

**Last Updated**: 2026-02-08
**ggen Version**: 6.0.0
**Agent System Version**: 1.0.0

For more information, see `/home/user/ggen/CLAUDE.md`
