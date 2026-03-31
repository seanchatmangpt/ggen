# ggen v6.0.0 - Rust Code Generation CLI

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) - Code precipitates from RDF via five-stage pipeline (μ₁-μ₅).
Stack: Rust 1.91.1 | Tokio | Oxigraph | Tera | Serde | Clap | Chicago TDD ONLY | 30 crates | 87% test coverage

## Rules (see .claude/rules/ for details)

---

## 📋 Agent Coordination Rules

| Rule | Requirement |
|------|-------------|
| **Andon Signals** | 🔴 Compiler errors/test failures = STOP THE LINE. Fix immediately. |
| **Cargo Make Only** | NEVER use direct cargo commands. Always `cargo make [target]`. |
| **Testing** | Chicago TDD ONLY (no mocks, no test doubles). 80%+ coverage. Mutation score ≥60%. All tests pass. |
| **No Unwrap** | Zero `unwrap()/expect()` in production. `Result<T,E>` required. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated). |
| **File Organization** | NEVER save to root. Use `crates/*/src/`, `tests/`, `docs/`, etc. |
| **Batch Operations** | 1 message = ALL related operations. TodoWrite 10+ todos minimum. |
| **Agent Execution** | Use Claude Code Task tool. MCP only coordinates topology. |
| **Type-First** | Encode invariants in types. Compiler as design tool. |
| **Definition of Done** | check + lint + test + slo-check + OTEL traces all pass. No signals. |
| **OTEL Validation** | Verify spans/traces for LLM calls, external services, pipeline stages. |

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

## 🔍 OpenTelemetry (OTEL) Validation

**CRITICAL:** For any feature involving LLM calls or external services, you MUST verify OTEL spans/traces exist. Tests passing is NOT sufficient.

### Required Spans by Feature

| Feature | Required Spans | Required Attributes | Verification Method |
|---------|---------------|---------------------|-------------------|
| **LLM Integration** | `llm.complete`, `llm.complete_stream` | `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens` | `RUST_LOG=trace,ggen_ai=trace cargo test` + grep for spans |
| **MCP Tools** | `mcp.tool.call`, `mcp.tool.response` | `mcp.tool.name`, `mcp.tool.duration_ms` | Check logs after MCP server operations |
| **Pipeline Stages** | `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` | `pipeline.stage`, `pipeline.duration_ms` | Run `ggen sync` with tracing enabled |
| **Quality Gates** | `quality_gate.validate`, `quality_gate.pass_fail` | `gate.name`, `gate.result` | Run quality gate validation |

### How to Verify OTEL Spans

```bash
# Enable trace logging for OTEL spans
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run tests with OTEL output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# Verify expected spans exist
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.model.*groq|llm\.model.*gpt" otel_output.txt
grep -E "llm\.prompt_tokens|llm\.completion_tokens|llm\.total_tokens" otel_output.txt

# If spans are missing, the feature is NOT working correctly
# Example failure: No "llm.complete" span = LLM API was never called
```

### OTEL Validation Checklist

Before claiming any LLM/external service feature is complete:

- [ ] Spans exist for the operation (e.g., `llm.complete`)
- [ ] Attributes are populated (e.g., `llm.model=groq::openai/gpt-oss-20b`)
- [ ] Token counts are present (if applicable)
- [ ] Timing information is recorded
- [ ] Error spans appear if operation failed (with `error=true` attribute)
- [ ] Spans show network latency (not mock/synthetic response times)

### What OTEL Spans Prove

| Observation | Conclusion |
|-------------|------------|
| `llm.complete` span exists | Real LLM API call was made |
| `llm.model=groq::openai/gpt-oss-20b` | Correct endpoint used |
| `llm.total_tokens > 0` | LLM returned actual content |
| `llm.prompt_tokens + llm.completion_tokens = llm.total_tokens` | Valid token accounting |
| Span duration ~2-3 seconds | Real network call (not mock) |
| No spans found | Feature not working (tests may be mocked) |

**Rule:** If OTEL spans are missing, the feature is **NOT complete**, even if tests pass.

## 🧪 Testing Policy: Chicago TDD ONLY

**CRITICAL:** This project uses Chicago TDD EXCLUSIVELY. London TDD patterns are NOT acceptable.

### What This Means

**Chicago TDD (REQUIRED):**
- ✅ Real collaborators: actual databases, filesystems, HTTP clients, LLM APIs
- ✅ State-based verification: assert on observable results
- ✅ Empirical observation: tests verify actual system behavior
- ✅ Real execution: tests make real API calls, real I/O, real concurrent operations
- ✅ OTEL trace verification: prove real external calls were made

**London TDD (FORBIDDEN):**
- ❌ Mocks and test doubles (`mockall::mock!`, `#[automock]`, `MockXxx` structs)
- ❌ Behavior verification (`.expect_x().times(1)`, `.with(eq(...))`)
- ❌ Dependency injection for testability (traits as mocks)
- ❌ Test doubles that simulate real behavior
- ❌ Assertions on mock interactions rather than actual state

### How to Verify Tests Are Chicago TDD

1. **No mockall imports:** `grep -r "mockall" tests/` should return nothing (except archives)
2. **No Mock structs:** `grep -r "struct Mock" tests/` should return nothing (except archives)
3. **No behavior verification:** `grep -r "expect_\|times(" tests/` should return nothing (except archives)
4. **OTEL spans present:** `RUST_LOG=trace cargo test` should show real API calls
5. **Real I/O operations:** Tests use `TempDir`, `SqlitePool`, `reqwest::Client`, etc.

### References

- `/Users/sac/ggen/.claude/rules/rust/testing.md` - Detailed Chicago TDD requirements
- `/Users/sac/ggen/.claude/rules/rust/testing-forbidden.md` - Forbidden London TDD patterns
- `/Users/sac/ggen/TEST_CATEGORIZATION_REPORT.md` - Current test categorization (63% Chicago, 37% London)

### Migration Path

Existing London TDD tests should be:
1. **Converted** to Chicago TDD (replace mocks with real collaborators)
2. **Deleted** if they only test mock wiring (not real behavior)
3. **Archived** to `tests-archive/london_tdd_legacy/` with DEPRECATED notice

**See:** `/Users/sac/ggen/docs/LONDON_TDD_MIGRATION_GUIDE.md` (to be created)

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

# 5. OTEL Trace Validation (REQUIRED for LLM/external service features)
# Verify OpenTelemetry spans/traces exist for critical operations
RUST_LOG=trace,ggen_ai=trace,ggen_core=trace cargo test -p ggen-cli-lib --test llm_e2e_test 2>&1 | grep -E "(llm\.|otel|span|groq|GROQ_API_KEY|complete|request|response)"
# Expected spans: llm.complete, llm.complete_stream
# Expected attributes: llm.model, llm.prompt_tokens, llm.completion_tokens, llm.total_tokens
# If spans are missing, the feature did not call the actual external service
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
**See:** `/home/user/ggen/.claude/autonomous/workflow-pattern.md` for templates and examples.

## Support

- **Repository**: https://github.com/seanchatmangpt/ggen
- **Documentation**: /home/user/ggen/docs/
- **Detailed Rules**: /home/user/ggen/.claude/rules/
- **Research**: /home/user/ggen/docs/research/
