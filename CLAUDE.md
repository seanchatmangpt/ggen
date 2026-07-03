# ggen v26.7.1 - Rust Code Generation CLI

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) - Code precipitates from RDF via five-stage pipeline (μ₁-μ₅).
Stack: Rust (nightly, pinned via `rust-toolchain.toml`) | Tokio | Oxigraph | Tera | Serde | Clap | Chicago TDD ONLY | 10-crate workspace

> **Toolchain:** the workspace currently requires **nightly** Rust, pinned to a specific date in `rust-toolchain.toml`. Two crates.io dependencies use nightly-only features: `wasm4pm-compat` (via `ggen-lsp`→`lsp-max`→`lsp-max-runtime`; `#![feature(...)]`) and `libsqlite3-sys` 0.38.x (via `cpmp`→`rusqlite` "bundled"; `cfg_select!`). Run `cargo`/`just` as usual — the toolchain file selects nightly automatically. `Cargo.lock` is committed for reproducible builds. Move back to stable here if/when those dependencies support it.
**Compressed Architecture:** `docs/architecture/COMPRESSED_REFERENCE.md` — verified C4, real sync flow, stub registry, error map. Load this before modifying any code.

**Recent Audit (2026-04-01):** `docs/crate-audits/AUDIT_DASHBOARD.md` — workspace health assessment with 54 stubs classified, 8,900 lines dead code identified, 4 P0 blockers prioritized.

## Process Intelligence Boundary — CRITICAL

**ggen EMITS process evidence. ggen does NOT analyse it.**

ggen is a code generator. It observes its own operations by emitting OCEL events (like OTel spans). wasm4pm-compat and wasm4pm own all analysis: discovery, conformance, fitness, precision, variants.

The split calculus: after a feature is extracted into its own project, the original project keeps the **emission surface** and deletes the **analysis surface**.

| Concern | Owner | Forbidden in ggen |
|---------|-------|-------------------|
| OCEL event emission | `ggen-graph/ocel/pack_events.rs` | Analysis, discovery, conformance |
| Lifecycle order (SPARQL over RDF) | `ggen-graph/ocel/lifecycle.rs` | pm4py, subprocess, Python |
| DFG discovery | `wasm4pm-compat::dfg::discover_ocel_dfg` | Any local discovery impl |
| Conformance checking | `wasm4pm-compat::dfg::{dfg_fitness, dfg_precision}` | Any local fitness/precision impl |
| Process variants | `wasm4pm-compat::dfg::extract_ocel_variants` | Any local variant counting |
| OCEL types (authority) | `wasm4pm-compat::ocel::{OCEL, OCELEvent, OCELObject}` | Local redefinition |
| DFG shapes (authority) | `wasm4pm-compat::models::{DFG, DFGNode, DFGEdge}` | Local redefinition |
| WASM execution | `wasm4pm` crate (WASM only) | Native dep — wasm-bindgen version conflict |

`wasm4pm` requires `wasm-bindgen = "=0.2.100"`. It **cannot** be a direct dep in any ggen crate. Always use `wasm4pm-compat` for native Rust code.

**If you find process analysis code in ggen:** it is a post-split residue. Delete it.  
**If you need process analysis from ggen:** you are doing the wrong thing. Use wasm4pm externally.

## mode=Create Semantics (v26.6.25+)

`mode = "Create"` silently skips existing files. It is the correct mode for bootstrap scaffolds (analyzer stubs, breed stubs) that are hand-completed after first generation. Do NOT use `mode = "Overwrite"` on files with hand-written logic.

## Rules (see .claude/rules/ for details)

---

## Architecture Reference

### Crate Map (10 workspace members)

Verified against `Cargo.toml` `members = [...]`. These are the only crates that compile in the workspace. Trimmed from 17 packages / 24 disk dirs to 10 packages / 9 disk dirs by the 2026-07 crate-consolidation pass — see `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` for the full evidence base and phase-by-phase history.

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-core` | Core graph-aware code generation engine (μ₁–μ₅ pipeline) |
| `ggen-cli` | CLI interface for ggen (binary + `ggen-cli-lib`) |
| `ggen-config` | Configuration parser and validator for `ggen.toml` files (depends on the published `star-toml` crate, not an embedded copy) |
| `ggen-marketplace` | Marketplace / package management system for ggen |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, deltas, validation hooks, transition receipts |
| `ggen-lsp` | Language server for ggen surfaces (analyzers, check, intel, pack, route, repair); also exposes `check`/`init`/`mine` library APIs. Absorbed `ggen-lsp-mcp`, `ggen-a2a-mcp`, and `ggen-lsp-a2a` as feature-gated modules (`mcp`, `a2a`) in the 2026-07 consolidation |
| `genesis-types-v2` | KNHK V2 type system — foundational data structures for the workflow engine (workflow/pattern defs, execution state, errors, config); absorbed `genesis-schema-v2` as its `schema` module (OpenAPI specs, RDF ontology, 43 YAWL pattern definitions) |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, pattern registry, composition, zero-copy/zero-alloc execution paths |
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts |
| `ggen` (root) | Workspace root package |

#### Removed in the 2026-07 consolidation pass

`ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` (absorbed into `ggen-lsp`), `genesis-schema-v2` (absorbed into `genesis-types-v2`), `star-toml` (removed from the workspace — now an external published dependency), `stpnt` and `genesis-core` (dead code, zero dependents). Prior dormant non-member directories were deleted earlier in the same pass.

### Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| **`pub type Result<T> = std::result::Result<T, CrateError>`** | Most crates | Each crate has its own error enum via `thiserror` |
| **Builder pattern** | `ggen-core` | `with_*()` chain methods for optional config |
| **Typestate** | `ggen-marketplace` | Compile-time state transitions (`Draft`/`Published`) |
| **Newtype wrappers** | `ggen-core`, `ggen-marketplace` | Invariant encoding in types (e.g., `PackageId`) |
| **Async traits** | `ggen-lsp` (`a2a_mcp` module, feature `mcp`/`a2a`) | `#[async_trait]` with `Result` returns |
| **RDF/SPARQL foundation** | `ggen-core`, `ggen-graph`, `ggen-marketplace` | Built on `oxigraph` triplestores |
| **Pipeline architecture** | `ggen-core` (μ₁–μ₅) | Multi-stage deterministic transformation |
| **Deterministic hashing + transition receipts** | `ggen-graph` | State-change detection (deltas) and cryptographic receipts |
| **Feature-gated module absorption** | `ggen-lsp` (`mcp`/`a2a` features), `genesis-types-v2` (`schema` module) | Former sibling crates folded in behind Cargo features/modules, cycle-free |
| **Pattern trait / registry** | `genesis-core-v2`, `genesis-types-v2::schema` | 43 YAWL workflow patterns, zero-copy execution |

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
| **just Is the Entry Point** | ALWAYS `just <task>`. Never call `cargo make` or bare `cargo` directly. |
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

Use `just` as the entry point for all tasks (native cargo recipes; Makefile.toml is historical reference only).

| Command | Purpose | Timeout |
|---------|---------|---------|
| `just check` | Compilation check | <5s |
| `just test` | Full test suite (unit + integration + property) | <30s |
| `just test-unit` | Unit tests only (fast dev loop) | <10s |
| `just test-mutation` | Mutation testing (≥60% score) | <5min |
| `just lint` | Clippy + rustfmt | <60s |
| `just pre-commit` | check → lint → test-unit | <2min |
| `just slo-check` | Performance SLOs validation | - |
| `just audit` | Security vulnerabilities scan | - |
| `just doc` | Build HTML docs into `target/doc/` | - |
| `just test-doc` | Validate all `# Examples` blocks compile/run | - |
| `just test-marketplace` | Marketplace pack tests (no network) | - |
| `just bench` | Run benchmarks | - |
| `just sync` | Full μ₁-μ₅ pipeline with audit | - |
| `just sync-dry` | Preview sync without writing | - |
| `ggen validate <ttl>` | SHACL validation | - |
| `ggen receipt verify <path>` | Verify cryptographic receipt | - |

## 🛡️ Verification & Strict Mode

ggen enforces a multi-surface "Strict Mode" (enabled via `[validation] strict_mode = true` in `ggen.toml`).

### Diagnostic Codes (Law Surfaces)

| Code | Law Surface | Severity | Meaning |
|------|-------------|----------|---------|
| **GGEN-TPL-001** | SPARQL ↔ Tera | ERROR | Template consumes `{{ var }}` that SELECT does not produce. |
| **GGEN-OUT-001** | SPARQL ↔ ggen.toml | ERROR | `output_file` pattern consumes unbound variable. |
| **GGEN-YIELD-001**| ggen.toml ↔ OS | ERROR | `output_file` escapes the project root (Layer Violation). |
| **GGEN-RULE-001** | ggen.toml ↔ OS | ERROR | `{file = ...}` binding points at a missing file. |
| **GGEN-QUERY-002**| SPARQL | WARNING | `SELECT *` used (disables provision checks). |
| **E0011 / E0013** | SPARQL | WARNING* | `CONSTRUCT` / `SELECT` lacks `ORDER BY` (Strict Mode: ERROR). |
| **E0015** | SPARQL | WARNING | Identity `CONSTRUCT` detected (no-op mapping). |

### Cryptographic Receipts

Every `ggen sync` produces a signed BLAKE3 transition receipt at `.ggen/receipts/latest.json`.
- **Verify:** `ggen receipt verify .ggen/receipts/latest.json --public-key .ggen/keys/public.pem`
- **Audit:** Receipts bind all input hashes (ontology, queries, templates, manifest) to all output hashes.

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
just test-unit                              # Verify fails
vim crates/ggen-core/src/feature.rs         # Implement (GREEN)
just test-unit                              # Verify passes
just pre-commit                             # Refactor (maintain GREEN)

# 3. Validation (Definition of Done)
just check && just lint && just test && just slo-check

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
