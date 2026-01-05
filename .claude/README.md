# Claude Code Configuration for ggen (2026 Edition)

**Version**: ggen 5.2.0 | **Updated**: 2026-01-05

## Three Paradigm Shifts

1. **Big Bang 80/20**: `/speckit-verify` before implementation → single-pass construction
2. **EPIC 9**: 10 parallel agents (2.8-4.4x speedup) via `/bb80-parallel`
3. **Deterministic Validation**: Receipts replace review (`[Receipt] cargo make test: ✓`)

## Directory Structure

```
.claude/
├── commands/     # 9 slash commands (bb80-parallel, collision-detect, convergence, speckit-*, test-audit, review-errors, optimize, bench-compare)
├── agents/       # 8 agents (bb80-*, rust-coder, test-engineer, reviewer, speckit-architect)
├── skills/       # 10 skills (bb80-*, cargo-make-protocol, chicago-tdd-pattern, poka-yoke-patterns, rdf-ontologies, mcp-servers)
├── hooks/        # 7 hooks (session-start, pre-tool-*, post-*, user-prompt-*, convergence-*)
└── settings.json # Permissions, hooks config, MCP stubs
```

## Quick Start

```bash
# Non-trivial task workflow
/speckit-verify [feature]           # 1. Verify spec closure (MANDATORY)
/bb80-parallel "[specification]"    # 2. Launch 10 parallel agents
/collision-detect                   # 3. Find convergence points
/convergence                        # 4. Synthesize best solution
cargo make pre-commit               # 5. Collect receipts
```

## Essential Commands

| Command | Purpose | SLO |
|---------|---------|-----|
| `/speckit-verify` | Verify spec closure | <3s |
| `/bb80-parallel` | Orchestrate EPIC 9 | 2-4h |
| `/collision-detect` | Analyze convergence | <30m |
| `cargo make check` | Compile | <5s |
| `cargo make test` | All tests | <30s |
| `cargo make pre-commit` | Full validation | <60s |

## Hooks (Lifecycle Automation)

| Hook | Trigger | Purpose |
|------|---------|---------|
| `SessionStart` | Session begins | Verify env, load context |
| `PreToolUse` | Before tool | Block dangerous ops |
| `PostToolUse` | After tool | Auto-format, andon signals |

**Config** (settings.json):
```json
{"hooks": {"PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "hooks/pre-tool-use.sh"}]}]}}
```

## Constitutional Rules

1. **Cargo Make**: `cargo make [target]` ONLY (never direct `cargo`)
2. **Error Handling**: Production = `Result<T,E>`, Tests = `unwrap()` OK
3. **Andon Signals**: RED = stop immediately
4. **RDF-First**: Edit `.ttl` (source), never `.md` (generated)
5. **EPIC 9**: Parallel-first for non-trivial tasks
6. **Evidence**: Receipts, not narratives

## ggen sync (Specification Compiler)

```bash
ggen sync              # Generate from TTL spec
ggen sync --watch      # Auto-regenerate on changes
ggen sync --dry-run    # Preview without writing
```

**Pattern**: `spec.ttl` → `ggen sync` → generated code (60-80% faster than hand-coding)

## Skills (Auto-Load by Context)

- **bb80-***: Specification closure, parallel agents, receipts, invariants
- **cargo-make-protocol**: Build orchestration, SLOs, timeouts
- **chicago-tdd-pattern**: State-based testing, AAA pattern
- **poka-yoke-patterns**: Error-proofing, quality gates
- **rdf-ontologies**: TTL, SPARQL, .specify/ workflow
- **mcp-servers**: GitHub, filesystem, custom servers

## Quality Gates

- Before commit: `cargo make pre-commit` ✓
- Before EPIC 9: `/speckit-verify` = 100% ✓
- Before merge: All receipts collected ✓

## Troubleshooting

- **Hook not executing**: `chmod +x hooks/*.sh`
- **Skill not loading**: Mention keywords (e.g., "cargo make")
- **Command not found**: Check `/help` for available commands

---

**Philosophy**: Type-first thinking | Zero-cost abstractions | Deterministic outputs
