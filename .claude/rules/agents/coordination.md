# 🤖 Agent Coordination Rules

## Batching (Golden Rule)
**1 MESSAGE = ALL RELATED OPERATIONS**

| Operation | Requirement |
|-----------|-------------|
| **TodoWrite** | ALWAYS 10+ todos in ONE batch at session start |
| **Task launches** | ALL agents together in one message |
| **File ops** | ALL reads/writes/edits in parallel |
| **Bash** | Chain with `&&`, run independents in parallel |

## Execution Pattern

**Task tool** → agent execution (write code, run tests, edit files)
**MCP** → topology coordination only (not execution)

Phased workflow (auto-resume on restart):
```
Phase 1: Explore ×5 → discover unknowns
Phase 2: Plan    ×5 → design strategies
Phase 3: Execute ×20 → implement changes
```

State persisted to `.claude/autonomous/workflow-state.json`. On restart, incomplete phases resume automatically.

## Andon Protocol

🔴 **CRITICAL**: Compiler errors or test failures → STOP ALL AGENTS immediately.
Fix root cause (not symptom). Re-run checks. Only resume when green.

```bash
# Fix cycle
cargo make check && cargo make test && cargo make lint
# Only proceed when ALL pass
```

## Agent Selection

| Need | Agent |
|------|-------|
| Rust implementation | `rust-coder` |
| RDF/SHACL design | `speckit-architect` |
| Testing (Chicago TDD) | `test-engineer` |
| Code review | `reviewer` |
| System design | `system-architect` |
| Performance | `performance-benchmarker` |
| Multi-phase orchestration | `phased-plan-executor` |

## Anti-Patterns

- NEVER launch agents sequentially when parallel is possible
- NEVER use MCP to execute code — use Task tool
- NEVER start without TodoWrite 10+ todos
- NEVER ignore Andon signals and continue working
