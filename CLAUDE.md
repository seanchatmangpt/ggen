# ggen v6.1.0 - Rust Code Generation CLI

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) — Code precipitates from RDF via five-stage pipeline (μ₁-μ₅).
Stack: Rust 1.91.1 | Tokio | Oxigraph | Tera | Serde | Clap | Chicago TDD | 30 crates | 87% test coverage

@.claude/rules/_core/absolute.md
@.claude/rules/_core/workflow.md

## 📋 Agent Coordination Rules

| Rule | Requirement |
|------|-------------|
| **Andon Signals** | 🔴 Compiler errors/test failures = STOP THE LINE. Fix immediately. |
| **Cargo Make Only** | NEVER use direct cargo commands. Always `cargo make [target]`. |
| **Testing** | Chicago TDD mandatory. 80%+ coverage. Mutation score ≥60%. All tests pass. |
| **No Unwrap** | Zero `unwrap()/expect()` in production. `Result<T,E>` required. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated). |
| **File Organization** | NEVER save to root. Use `crates/*/src/`, `tests/`, `docs/`, etc. |
| **Batch Operations** | 1 message = ALL related operations. TodoWrite 10+ todos minimum. |
| **Agent Execution** | Use Claude Code Task tool. MCP only coordinates topology. |
| **Type-First** | Encode invariants in types. Compiler as design tool. |
| **Definition of Done** | check + lint + test + slo-check all pass. No signals. |

## Commands

| Command | Purpose | Timeout |
|---------|---------|---------|
| `cargo make check` | Compilation check | <5s |
| `cargo make test` | Full test suite (unit + integration + property) | <30s |
| `cargo make test-mutation` | Mutation testing (≥60% score) | <5min |
| `cargo make lint` | Clippy + rustfmt | <60s |
| `cargo make pre-commit` | check → lint → test-unit | <2min |
| `cargo make slo-check` | Performance SLOs validation | - |
| `cargo make audit` | Security vulnerabilities scan | - |
| `ggen sync` | Full μ₁-μ₅ pipeline | - |
| `ggen validate <ttl>` | SHACL validation | - |

## Workflow

```bash
# 1. Create RDF Spec (source of truth)
mkdir -p .specify/specs/NNN-feature && vim .specify/specs/NNN-feature/feature.ttl
ggen validate .specify/specs/NNN-feature/feature.ttl

# 2. Chicago TDD (RED → GREEN → REFACTOR)
vim crates/ggen-core/tests/feature_test.rs  # Write failing test (RED)
cargo make test-unit                        # Verify fails
vim crates/ggen-core/src/feature.rs         # Implement (GREEN)
cargo make test-unit                        # Verify passes
cargo make pre-commit                       # Refactor (maintain GREEN)

# 3. Validation (Definition of Done)
cargo make check && cargo make lint && cargo make test && cargo make slo-check

# 4. Generate from Ontology
ggen sync --audit true  # Full sync with cryptographic receipt
```

## Phased Agent Workflows

**Pattern:** Explore → Plan → Execute (auto-resume on restart)

```bash
# Phase 1: Discover (5 Explore agents search codebase)
launch 5 explore agents to search for optimization opportunities

# Phase 2: Design (5 Plan agents create strategies)
launch 5 planning agents to design implementation plans

# Phase 3: Implement (20 agents execute work)
launch 20 agents to implement all changes
```

**Auto-Resume:** State saved to `.claude/autonomous/workflow-state.json`. On restart, continues from last incomplete phase.
**See:** `.claude/autonomous/workflow-pattern.md` for templates and examples.
**Agent Rules:** @.claude/rules/agents/coordination.md
**Git Standards:** @.claude/rules/git/commits.md

## Memory

- **Auto corrections**: stored in `~/.claude/projects/ggen/memory/` (first 200 lines loaded at session start)
- **Team memory**: `.claude/MEMORY.md` — curated lessons, patterns to avoid, compounding corrections
- **claudeMdExcludes**: see `.claude/settings.json` for paths excluded from CLAUDE.md loading

## Support

- **Repository**: https://github.com/seanchatmangpt/ggen
- **Documentation**: docs/
- **Detailed Rules**: .claude/rules/
- **Research**: docs/research/
