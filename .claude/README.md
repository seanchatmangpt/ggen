# .claude/ Configuration

Claude Code configuration for ggen v6.0.0.

**Primary Source**: See `/CLAUDE.md` for all project rules, stack, paradigms, and workflows. This is the single source of truth.

## Directory Structure

```
.claude/
├── settings.json      # Loaded during sessions (permissions, hooks, environment)
├── agents/            # Agent definitions (8 agents, loaded on-demand)
│   ├── rust-coder.md
│   ├── reviewer.md
│   ├── test-engineer.md
│   ├── speckit-architect.md
│   ├── bb80-specification-validator.md
│   ├── bb80-parallel-task-coordinator.md
│   ├── bb80-collision-detector.md
│   └── bb80-convergence-orchestrator.md
├── skills/            # Domain skills (11 skills, loaded when invoked)
│   ├── cargo-make-protocol/
│   ├── chicago-tdd-pattern/
│   ├── poka-yoke-patterns/
│   ├── rdf-ontologies/
│   ├── bb80-specification-closure/
│   ├── bb80-parallel-agents/
│   ├── bb80-deterministic-receipts/
│   ├── bb80-invariant-construction/
│   └── mcp-servers/
└── hooks/             # Pre/post execution hooks (9 hooks, auto-loaded)
    ├── session-start.sh
    ├── pre-tool-use.sh
    ├── pre-tool-safety-check.sh
    ├── pre-specification-check.sh
    ├── post-tool-use.sh
    ├── post-bash-validation.sh
    ├── post-collision-detection.sh
    ├── convergence-validation.sh
    └── user-prompt-validation.sh
```

## What's Loaded vs Reference

### Auto-Loaded (Critical Path)
- **settings.json**: Always loaded during session initialization
- **hooks/**: Automatically executed at appropriate lifecycle events
- **CLAUDE.md**: Primary instructions (always in context)

### Loaded On-Demand
- **agents/**: Spawned via Task tool when task matches agent description
- **skills/**: Invoked via Skill tool when explicitly called (e.g., /cargo-make-protocol)

### Not Loaded (Reference Only)
- This README.md
- Extended documentation in skills/ subdirectories

## Token Budget Impact

Total .claude/ content:
- **Before refactoring**: ~4040 lines (agents + skills)
- **After refactoring**: ~611 lines (agents + skills)
- **Reduction**: 85% fewer tokens

All agent and skill files now reference CLAUDE.md instead of duplicating content.

## 8 Specialized Agents

1. **rust-coder**: Idiomatic Rust implementation
2. **reviewer**: Code review and quality audit
3. **test-engineer**: Chicago TDD tests
4. **speckit-architect**: RDF specification design
5. **bb80-specification-validator**: 100% specification closure validation
6. **bb80-parallel-task-coordinator**: EPIC 9 orchestration
7. **bb80-collision-detector**: Parallel agent overlap analysis
8. **bb80-convergence-orchestrator**: Selection pressure synthesis

## 11 Domain Skills

1. **cargo-make-protocol**: Cargo Make build orchestration
2. **chicago-tdd-pattern**: State-based testing
3. **poka-yoke-patterns**: Error-proofing design
4. **rdf-ontologies**: Turtle syntax and SPARQL
5. **bb80-specification-closure**: 100% closure verification
6. **bb80-parallel-agents**: 10-agent EPIC 9 workflow
7. **bb80-deterministic-receipts**: Evidence over narrative
8. **bb80-invariant-construction**: Type-safe invariants
9. **session-start-hook**: Repository setup for web sessions
10. **mcp-servers**: Model Context Protocol integration

## 9 Safety Hooks

### Session Lifecycle
- **session-start.sh**: Initialize session environment

### Pre-Tool Safety
- **pre-tool-use.sh**: Pre-flight file operation checks
- **pre-tool-safety-check.sh**: Protected file validation
- **pre-specification-check.sh**: EPIC 9 specification gate

### Post-Tool Validation
- **post-tool-use.sh**: Tool execution verification
- **post-bash-validation.sh**: Andon signal detection
- **post-collision-detection.sh**: EPIC 9 overlap analysis
- **convergence-validation.sh**: EPIC 9 synthesis quality
- **user-prompt-validation.sh**: Input validation

## Configuration Notes

### Permissions
- **Allow**: Read, Write, Edit, Glob, Grep, Task, TodoWrite, cargo make, git, gh, ggen
- **Deny**: Secrets (.env*, *.key), destructive ops (rm -rf, sudo)
- **Ask**: File deletion, force push, Cargo.toml modification

### Protected Paths
- Cargo.toml, Cargo.lock (workspace configuration)
- CLAUDE.md (project instructions)
- .claude/settings.json (this configuration)
- .specify/ontology/* (RDF schemas)

### Andon Signals
- 🔴 **RED**: error[E, FAILED, panicked → STOP immediately
- 🟡 **YELLOW**: warning:, clippy::, TODO → Investigate
- 🟢 **GREEN**: test result: ok, 0 violations → Continue

## EPIC 9 Configuration

- **Enabled**: true
- **Default agents**: 10
- **Mandatory phases**: fan_out → construction → collision → convergence → refactoring → closure
- **Target speedup**: 2.8x-4.4x

## SLO Targets

| Command | SLO | Purpose |
|---------|-----|---------|
| cargo make check | ≤5s | Compile check |
| cargo make test | ≤30s | Full test suite |
| cargo make lint | ≤60s | Clippy + rustfmt |
| first build | ≤15s | Initial compile |
| incremental build | ≤2s | Fast feedback |

## Usage

1. **CLAUDE.md is primary**: All project rules, workflows, and conventions
2. **Agents/Skills are concise**: Reference CLAUDE.md for details
3. **Hooks enforce safety**: Poka-Yoke error prevention
4. **Settings configure environment**: Permissions, tools, SLOs

## Updates

**v6.0.0 (2026-01-18)**: Refactored .claude/ for clarity and token efficiency
- Reduced agents/skills from 4040 → 611 lines (85% reduction)
- All content now references CLAUDE.md (single source of truth)
- Updated version metadata (5.2.0 → 6.0.0, 17 → 27 crates)
- Clarified loaded vs reference content
