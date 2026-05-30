# ggen v26.5.28 - Rust Code Generation CLI

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) - Code precipitates from RDF via five-stage pipeline (μ₁-μ₅).
Stack: Rust (stable) | Tokio | Oxigraph | Tera | Serde | Clap | Chicago TDD ONLY | 15-crate workspace
**Compressed Architecture:** `docs/architecture/COMPRESSED_REFERENCE.md` — verified C4, real sync flow, stub registry, error map. Load this before modifying any code.

**Recent Audit (2026-04-01):** `docs/crate-audits/AUDIT_DASHBOARD.md` — workspace health assessment with 54 stubs classified, 8,900 lines dead code identified, 4 P0 blockers prioritized.

## Rules (see .claude/rules/ for details)

---

## Architecture Reference

### Crate Map (15 workspace members)

Verified against `Cargo.toml` `members = [...]`. These are the only crates that compile in the workspace.

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-core` | Core graph-aware code generation engine (μ₁–μ₅ pipeline) |
| `ggen-cli` | CLI interface for ggen (binary + `ggen-cli-lib`) |
| `ggen-config` | Configuration parser and validator for `ggen.toml` files |
| `ggen-marketplace` | Marketplace / package management system for ggen |
| `ggen-a2a-mcp` | A2A protocol and MCP server for agent-to-agent communication |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, deltas, validation hooks, transition receipts |
| `ggen-lsp` | Language server for ggen surfaces (analyzers, check, intel, pack, route, repair); also exposes `check`/`init`/`mine` library APIs |
| `ggen-lsp-mcp` | MCP server exposing `ggen-lsp` repair routes as a tool (leaf crate — avoids the `ggen-core`↔`ggen-a2a-mcp` cycle) |
| `ggen-lsp-a2a` | A2A bridge exposing the `ggen-lsp-mcp` route engine as an A2A agent (leaf crate, cycle-free) |
| `genesis-core` | Pure mathematical foundation for the Genesis interchangeable-parts architecture (A = μ(O)); no_std variant for wasm32 targets |
| `genesis-types-v2` | KNHK V2 type system — foundational data structures for the workflow engine (workflow/pattern defs, execution state, errors, config) |
| `genesis-schema-v2` | KNHK V2 schema system — OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, workflow schema validation |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, pattern registry, composition, zero-copy/zero-alloc execution paths |
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts |
| `stpnt` | Stewards of the Pentecost — Canonical Stewardship Cell implementation (canon, cells, governance, membrane, projections, proof) |

#### Dormant (on-disk under `crates/`, NOT workspace members — do not compile)

`genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-membrane`, `ggen-projection`. These directories exist but are excluded from `Cargo.toml` `members`; treat as non-compiled reference material until activated.

### Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| **`pub type Result<T> = std::result::Result<T, CrateError>`** | Most crates | Each crate has its own error enum via `thiserror` |
| **Builder pattern** | `ggen-core` | `with_*()` chain methods for optional config |
| **Typestate** | `ggen-marketplace` | Compile-time state transitions (`Draft`/`Published`) |
| **Newtype wrappers** | `ggen-core`, `ggen-marketplace` | Invariant encoding in types (e.g., `PackageId`) |
| **Async traits** | `ggen-a2a-mcp` | `#[async_trait]` with `Result` returns |
| **RDF/SPARQL foundation** | `ggen-core`, `ggen-graph`, `ggen-marketplace` | Built on `oxigraph` triplestores |
| **Pipeline architecture** | `ggen-core` (μ₁–μ₅) | Multi-stage deterministic transformation |
| **Deterministic hashing + transition receipts** | `ggen-graph` | State-change detection (deltas) and cryptographic receipts |
| **Leaf-crate cycle avoidance** | `ggen-lsp-mcp`, `ggen-lsp-a2a` | Bridge crates kept dependency-cycle-free |
| **Pattern trait / registry** | `genesis-core-v2`, `genesis-schema-v2` | 43 YAWL workflow patterns, zero-copy execution |

---

## Evidence-First Principle

**ABSOLUTE RULE:** Never fabricate examples, OTEL traces, MCP schemas, or documentation.

### Forbidden
- Fabricating JSON schemas (read actual code first)
- Making up OTEL traces (run with `RUST_LOG=trace`, capture real output)
- Generic 5 Whys without real failure data (analyze actual logs/errors)
- Template-filled documentation (do real investigation first)
- Claiming features work without OTEL evidence

### Required Workflow
1. **Gather Evidence** — Read code, run tests, capture real OTEL output
2. **Analyze** — 5 Whys on ACTUAL failures with real data
3. **Document** — Write with real examples, actual file paths, real traces
4. **Verify** — Prove claims with captured output/logs

**Rule:** If you didn't read it from the codebase or capture it from execution, don't write it.

---

## Tool Restrictions

### Forbidden Tools
| Tool | Use Instead | Reason |
|------|-------------|--------|
| `mcp__desktop-commander__*` | `Read`, `Write`, `Edit`, `Glob`, `Grep`, `Bash` | Explicitly forbidden by user |

### Approved Tools
- File ops: `Read`, `Write`, `Edit`, `Glob`, `Grep`
- Terminal: `Bash`
- Rust navigation: `LSP` (rust-analyzer-lsp) — **ALWAYS use LSP over Grep for .rs files** (see `.claude/rules/rust/lsp.md`)
- Agents: `Agent` tool (with output verification)

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
| **Correctness > Speed** | NEVER sacrifice accuracy for speed. Real evidence > fast output. |
| **Evidence-First** | ALL docs/examples MUST reference actual code, real OTEL output, actual errors. No fabrication. |
| **Agent Verification** | ALWAYS verify agent output before accepting. Read files, check for fabrication. |

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

## Git Hooks & Build Gotchas

- `.git/hooks/pre-commit` → `scripts/hooks/pre-commit.sh` builds the **entire workspace**; a doc-only commit still fails if ANY crate (e.g. ggen-cli) doesn't compile. Never `--no-verify` on main; wait for a stable tree.
- `.git/hooks/pre-push` → `scripts/hooks/pre-push.sh` runs `cargo test` (~300s gate).
- `cargo build -p <crate>` is NOT a workspace-health signal: ggen-core can be 0 errors while ggen-cli has 6. Use `cargo build --workspace` to see what the hooks gate.
- Builds can flap nondeterministically when a concurrent session edits shared crates (e.g. a `SyncOptions` field refactor propagating ggen-core→ggen-cli). Re-sample before concluding; never patch another session's in-flight crate to unblock yourself.

## ggen-lsp Intel Log

- The OCEL agent-edit event log lives at `.ggen/ocel/agent-edit-events.ocel.jsonl` (see `intel::log::default_path`); read it via `IntelLog::at_root(root).read()`. Living-loop proofs assert on this external log, not in-process state.

## Remotes

- Canonical remote is `origin` (seanchatmangpt/ggen). `jmanhype` and `upstream` (rust-starter) also exist — push only to `origin`. Verify `git merge-base --is-ancestor` before assuming local main is behind/ahead of origin/main.

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

- `./.claude/rules/rust/testing.md` - Detailed Chicago TDD requirements
- `./.claude/rules/rust/testing-forbidden.md` - Forbidden London TDD patterns
- `./TEST_CATEGORIZATION_REPORT.md` - Current test categorization (63% Chicago, 37% London)

### Migration Path

Existing London TDD tests should be:
1. **Converted** to Chicago TDD (replace mocks with real collaborators)
2. **Deleted** if they only test mock wiring (not real behavior)
3. **Archived** to `tests-archive/london_tdd_legacy/` with DEPRECATED notice

**See:** `./docs/LONDON_TDD_MIGRATION_GUIDE.md` (to be created)

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
**See:** `./.claude/autonomous/workflow-pattern.md` for templates and examples.

### Agent Verification Protocol

After agents complete, ALWAYS:
1. **Read** each output file
2. **Check** — Does it reference real code/files? Or is it generic/template?
3. **Validate** — Are OTEL traces plausible? Cross-reference with actual code
4. **Reject** — If fabricated, delete and redo with grounded prompts

Before launching agents:
- Provide actual context (file paths, real errors, actual OTEL output)
- Include "read code first, use real examples only" in every agent prompt
- Never ask agents to document features without pointing to real implementation

## Support

- **Repository**: https://github.com/seanchatmangpt/ggen
- **Documentation**: ./docs/
- **Detailed Rules**: ./.claude/rules/
- **Research**: ./docs/research/
