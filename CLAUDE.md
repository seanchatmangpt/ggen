# ggen - Rust Project Configuration (80/20 Edition)

## 🎯 Core Identity

**ggen**: Language-agnostic, deterministic code generation CLI. Ontologies + RDF → reproducible code projections.

**Tech Stack**: Rust (workspace), SPARC + Chicago TDD + DfLSS
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

---

## 🚨 CRITICAL: THE VITAL FEW (20% that matters 80%)

### 1. CONCURRENT EXECUTION RULE

**Golden Rule**: "1 MESSAGE = ALL RELATED OPERATIONS"

```javascript
// ✅ CORRECT: Single message with ALL operations
[Single Message]:
  Task("Agent 1", "Full instructions...", "coder")
  Task("Agent 2", "Full instructions...", "tester")
  Task("Agent 3", "Full instructions...", "reviewer")

  TodoWrite { todos: [10+ todos in ONE call] }

  Write "file1.rs"
  Write "file2.rs"
  Write "file3.rs"

  Bash "cmd1 && cmd2 && cmd3"
```

**Why**: 2.8-4.4x speed improvement, prevents coordination failures

---

### 2. CARGO MAKE RULE

**NEVER USE DIRECT CARGO COMMANDS**

```bash
# ❌ WRONG
cargo check
cargo test
cargo clippy

# ✅ CORRECT
cargo make check     # <5s timeout
cargo make test      # All tests with timeouts
cargo make lint      # Clippy with timeouts
```

**Why**: Prevents hanging, enforces SLOs, integrated with hooks

---

### 3. ANDON SIGNAL RULE

**Stop the Line When Signals Appear**

| Signal | Trigger | Action |
|--------|---------|--------|
| **RED** | `error[E...]`, `test ... FAILED` | **STOP** - Fix immediately |
| **YELLOW** | `warning:`, clippy | Investigate before release |
| **GREEN** | Clean output | Continue |

**Workflow**: Monitor → Stop → Investigate → Fix → Verify → Cleared ✅

**Why**: Prevents defects from propagating (DfLSS alignment)

---

### 4. ERROR HANDLING RULE

**Production Code**: NO `unwrap()` / `expect()` - Use `Result<T, E>`

**Test/Bench Code**: `unwrap()` / `expect()` ALLOWED

```rust
// ❌ WRONG: Production code
let cache = self.cache.lock().unwrap();  // Can panic!

// ✅ CORRECT: Production code
let cache = self.cache.lock()
    .map_err(|e| Error::new(&format!("Lock poisoned: {}", e)))?;

// ✅ CORRECT: Test code (EXEMPT)
#[test]
fn test_cache() {
    let cache = Cache::new().unwrap();  // Tests SHOULD panic
    assert_eq!(cache.len(), 0);
}
```

**Exemption applies to**: `#[cfg(test)]`, `#[test]`, `tests/`, `benches/`

**NEVER discuss unwrap/expect violations in test/benchmark code**

**Why**: Production must handle errors gracefully; tests should fail fast

---

### 5. CHICAGO TDD RULE

**State-based testing with real collaborators**

```rust
// ✅ CORRECT: Chicago TDD
#[test]
fn test_lockfile_upsert() {
    // Arrange: Real objects
    let manager = LockfileManager::new(temp_dir.path());

    // Act: Call public API
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();

    // Assert: Verify observable state
    let entry = manager.get("pkg").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");  // State changed
}
```

**Tests verify**: Return values, state changes, side effects, actual system effects

**Not**: Internal implementation, method calls, mocks

**Why**: Tests verify behavior, not implementation (80% of bugs caught)

---

### 6. CLAUDE CODE OPERATING RULES (2026 Edition)

**Maximize effectiveness through proactive agent usage and deterministic execution**

#### Trust Hierarchy (Adversarial PM)
```
Evidence Trust Levels:
  OTel spans (95%) > Test output (90%) > Clippy warnings (85%) > Agent claims (0%)
```

**Red Flags**: "I think", "should work", "code looks good", "mostly works" → **STOP**
**Before "done"**: Did you RUN it? Can you PROVE it? What BREAKS? Show EVIDENCE.

#### Rule 6.1: Subagents Do Analysis, You Do Execution

**ALWAYS delegate analysis to specialized agents:**

```javascript
// ❌ WRONG: Main Claude analyzes AND executes
Message 1: [Reads 10 files, analyzes architecture, writes plan doc]
Message 2: [Re-reads files, starts implementation]

// ✅ CORRECT: Subagent analyzes, main Claude executes
[Single Message - Parallel Agent Invocation]:
  Task("System Architect", "Analyze crate architecture, output JSON structure", "system-architect")
  Task("Code Analyzer", "Review error handling patterns, output violations list", "code-analyzer")
  Task("Performance Analyzer", "Profile hot paths, output optimization targets", "perf-analyzer")

[Next Message - Execute from agent outputs]:
  Edit "crates/ggen-core/src/lib.rs"  # Based on architect analysis
  Write "crates/ggen-new/src/lib.rs"  # From architecture JSON
  Bash "cargo make check"              # Verify compilation
```

**Why**: Prevents redundant inference, reduces tokens by 60%, increases speed by 2.8x.

**When to Use Agents** (PROACTIVE - Do NOT wait for user to ask):
- 🔍 **Need to understand codebase?** → Spawn `Explore` agent (NOT manual Read/Grep)
- 📐 **Need architecture design?** → Spawn `system-architect` agent (NOT your analysis)
- 🔬 **Need code review?** → Spawn `code-analyzer` agent (NOT manual inspection)
- ⚡ **Need performance analysis?** → Spawn `performance-benchmarker` agent
- 🏗️ **Need implementation plan?** → Spawn `Plan` agent (NOT your plan.md)
- 🧪 **Need test strategy?** → Spawn `tester` agent (NOT your test outline)

#### Rule 6.2: Load Skills Aggressively

**Skills auto-load by WHEN/WHEN_NOT patterns. Trust them.**

```rust
// ❌ WRONG: Re-explaining ggen constitution every time
"Remember, ggen uses Chicago TDD, cargo make, Result<T,E>..."

// ✅ CORRECT: Constitution skill auto-loads, reference it
"Following constitutional principle III (Chicago TDD)..."
// Skill already loaded via WHEN: ["cargo make", "Chicago TDD"]
```

**If skill doesn't auto-load**: Description is too generic. Fix skill, don't compensate.

#### Rule 6.3: Output Deterministically

**ALL outputs MUST be structured (JSON, YAML, markdown lists). NO PROSE.**

```markdown
# ❌ WRONG: Prose requiring interpretation
"We should probably create a new module for RDF parsing and maybe add some error handling..."

# ✅ CORRECT: Structured, deterministic output
## Tasks
1. [pending] Create crates/ggen-rdf/src/parser.rs
2. [pending] Implement Result<RdfTriple, ParseError>
3. [pending] Add tests/rdf/parser_tests.rs
4. [in_progress] Run cargo make test
```

**Why**: Eliminates ambiguity, enables automation, reduces rework cycles.

#### Rule 6.4: Fail Fast on Ambiguity

**Vague specification? STOP. Ask structured questions using AskUserQuestion tool.**

```rust
// ❌ WRONG: Guessing implementation details
// User: "Add validation"
// You: [Implements email regex without asking which fields]

// ✅ CORRECT: Structured clarification
// User: "Add validation"
// You: [Uses AskUserQuestion]
{
  questions: [
    {
      question: "Which fields require validation?",
      options: ["Email only", "Email + username", "All input fields"]
    },
    {
      question: "What validation library?",
      options: ["validator crate (recommended)", "regex", "custom"]
    }
  ]
}
```

**Why**: Guessing creates rework. Clarification takes 30 seconds, rework takes 30 minutes.

#### Rule 6.5: Batch Operations Aggressively

**Group ALL related operations in ONE message.**

```rust
// ❌ WRONG: Sequential messages
Message 1: Write "src/lib.rs"
Message 2: Write "src/error.rs"
Message 3: Bash "cargo make check"

// ✅ CORRECT: Batched operations
[Single Message]:
  Write "crates/ggen-core/src/lib.rs"
  Write "crates/ggen-core/src/error.rs"
  Write "crates/ggen-core/src/types.rs"
  Bash "cargo make check && cargo make test-unit"
  TodoWrite { todos: [5+ todos in ONE call] }
```

**Why**: 2.8-4.4x speed improvement, atomic transactions, better coordination.

#### Rule 6.6: Context Reuse Over Re-computation

**If analysis exists (skill, agent output, prior message), REUSE it. Do NOT re-analyze.**

```rust
// ❌ WRONG: Re-analyzing every time
Message 50: [Re-reads constitution, re-analyzes architecture, re-plans...]

// ✅ CORRECT: Reference existing analysis
Message 50: "Based on architect agent's JSON (Message 10), implementing..."
// OR: "Following constitutional principle IV (cargo make Protocol)..."
```

**Why**: Re-analysis wastes 500-1000 tokens per cycle. Multiply by 20 messages = 10K-20K wasted.

#### Token Efficiency Targets (Per Major Operation)

| Activity | Target Tokens | Method |
|----------|---------------|--------|
| Skill load | ~100 | Auto-invocation via WHEN patterns |
| Agent analysis | ~500 | Structured JSON/YAML output |
| Main execution | ~1000 | Deterministic Write/Edit/Bash |
| **Total** | **~1600** | One analysis + one execution pass |

**If exceeding 2000 tokens**: You're re-analyzing, re-explaining, or guessing. **STOP**.

#### Claude Self-Awareness (Limitations)

**Understanding your failure modes prevents defects:**

| Failure Mode | Probability | Mitigation |
|--------------|-------------|------------|
| Weak on exact counts | 70% error rate | Use `wc -l`, `grep -c`, test assertions |
| Nested logic >3 levels | 60% error rate | Spawn `code-analyzer` agent |
| "Not sure" statements | 70% uncertainty | STOP, clarify, or delegate |
| Confident tone | Often 60-80% actual | Verify with tests/evidence |
| After 15 messages | Coherence degrades | Summarize or restart |

**Why OTEL/Tests Critical**: You CANNOT validate your own execution. "Completion" ≠ correctness.

#### Pre-Action Checklist (MANDATORY)

Before ANY implementation action, verify:

1. ☑️ **Is there a skill for this?** Load explicitly if not auto-loaded.
2. ☑️ **Need analysis?** Spawn agent. Do NOT analyze yourself.
3. ☑️ **Executing?** Use Write/Edit/Bash directly. No additional planning.
4. ☑️ **Output structured?** Must be JSON/YAML/markdown lists. No prose.
5. ☑️ **Created ambiguity?** If yes, use AskUserQuestion immediately.

**Compliance Check**: If you write >500 tokens of explanation, you violated Rule 6.6 (context reuse).

---

## 📁 File Organization (Never Save to Root)

```
ggen/
├── .specify/                      # RDF-first specification system (source of truth)
│   ├── ontology/                  # Ontology schemas
│   │   └── spec-kit-schema.ttl    # Vocabulary definitions
│   ├── memory/                    # Project memory
│   │   ├── constitution.ttl       # Architectural law (source)
│   │   └── constitution.md        # Generated from .ttl
│   ├── specs/NNN-feature/         # Feature specifications
│   │   ├── *.ttl                  # TTL source files (edit these)
│   │   ├── *.md                   # Generated markdown (never edit)
│   │   └── evidence/              # Test evidence, artifacts
│   └── templates/                 # Templates for generation
│       ├── rdf-helpers/           # TTL templates (.ttl.template)
│       └── spec.tera              # Markdown generation template
├── crates/*/src/                  # Source code (per crate)
├── crates/*/tests/                # Integration tests
├── tests/                         # Workspace tests
├── docs/                          # Documentation (generated or manual)
├── scripts/                       # Build scripts
├── benches/                       # Benchmarks
└── templates/                     # Code generation templates
```

**Never**: Save working files, text/mds, tests to root folder
**Never**: Manually edit generated .md files - edit .ttl source, regenerate markdown

---

## 🦀 Elite Rust Mindset (Type-First Thinking)

### The Questions to Ask:
1. **"What can I express in types?"** (before runtime values)
2. **"Is this abstraction zero-cost?"** (generics yes, trait objects no)
3. **"What are the ownership semantics?"** (explicit is better)
4. **"How can I make misuse impossible?"** (type safety > runtime checks)

### 80/20 Idea Generation:
Always generate 3 ideas:
1. **First**: Solve immediate problem
2. **Second**: Go bigger (80% of problems with 20% effort) **← Sweet spot**
3. **Third**: Maximum value (type-level solutions, compile-time guarantees)

**Quality-First 80/20**: Value = quality + consistency + maintainability (not optional)

---

## 🔧 Essential Commands

**Quick Feedback Loop**:
- `cargo make check` - Compilation (<5s)
- `cargo make test-unit` - Unit tests (<10s)
- `cargo make lint` - Clippy

**Full Validation**:
- `cargo make test` - All tests
- `cargo make pre-commit` - Format + lint + tests
- `cargo make ci` - Full CI pipeline

**Performance**:
- `cargo make slo-check` - Verify SLOs
- `cargo make bench` - Benchmarks

---

## 🚫 Prohibited Patterns (Zero Tolerance)

1. **Direct cargo commands** - ALWAYS use `cargo make`
2. **Unwrap/expect in production** - Use `Result<T, E>`
3. **Ignoring Andon signals** - Stop the line when RED
4. **Skipping timeout checks** - `cargo make timeout-check`
5. **Using --no-verify** - Never bypass git hooks
6. **Meaningless tests** - Must verify observable behavior
7. **Multiple messages** - Batch ALL operations in ONE message
8. **Saving to root** - Use proper subdirectories

---

## 🎯 SLOs (Service Level Objectives)

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs

---

## 🚀 Claude Code Task Tool (Primary Agent Execution)

**Claude Code's Task tool spawns ACTUAL agents that do work:**

```javascript
// ✅ CORRECT: Use Task tool for parallel execution
Task("Rust Coder", "Implement RDF parser with Result<T,E>", "coder")
Task("Test Engineer", "Write Chicago TDD tests with AAA pattern", "tester")
Task("Code Reviewer", "Review for type safety and Andon signals", "reviewer")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Topology setup
- `mcp__claude-flow__agent_spawn` - Agent type definitions
- `mcp__claude-flow__task_orchestrate` - High-level planning

**KEY**: MCP coordinates strategy, Task tool executes with real agents

---

## 📋 Agent Coordination Protocol

**Every agent MUST run hooks:**

```bash
# 1️⃣ BEFORE Work
npx claude-flow@alpha hooks pre-task --description "[task]"

# 2️⃣ DURING Work
cargo make check          # Monitor Andon signals
cargo make test-unit      # Quick feedback
npx claude-flow@alpha hooks post-edit --file "[file]"

# 3️⃣ AFTER Work
cargo make test           # All tests must pass
npx claude-flow@alpha hooks post-task --task-id "[task]"
```

---

## 🚨 Definition of Done (Mandatory Validation)

**BEFORE marking complete:**

```bash
# 1. Check for compiler errors (RED signal)
cargo make check  # Must be clean

# 2. Check for test failures (RED signal)
cargo make test   # Must be 100% pass

# 3. Check for clippy warnings (YELLOW signal)
cargo make lint   # Must be clean

# 4. Verify SLOs
cargo make slo-check  # Must meet targets
```

**ONLY mark complete when ALL signals cleared**

---

## 🔗 Git Hooks System (80/20 Defect Detection)

**Pre-Commit Hook** (<5s, 62% defect detection):
- Cargo check (RED if errors)
- Format check (YELLOW, auto-fix)

**Pre-Push Hook** (<60s, 100% defect detection):
- Cargo check (RED)
- Clippy lint (RED)
- Format check (YELLOW)
- Unit tests (RED)
- Security audit (YELLOW)

**NEVER use `--no-verify`** - Defeats defect prevention

---

## 🎯 Key Associations (Mental Models)

- **Types = invariants = compile-time guarantees**
- **Zero-cost = generics/macros/const generics**
- **Performance = references/stack/minimize allocations**
- **Ownership = explicit = memory safety**
- **APIs = type-safe = ergonomic = composable**
- **Tests = observable outputs = behavior verification**
- **80/20 = second idea = sweet spot = maximum value**
- **Andon Signals = stop = fix = verify**
- **DfLSS = prevent defects AND waste from start**

---

## 📝 Speckit Workflow (RDF-First Specification Development)

**MANDATORY**: NO implementation without spec. RDF ontology is source of truth, markdown is generated.

### Architecture: Ontology → Code (Constitutional Equation)

```
spec.ttl (source) → ggen render → spec.md (derived artifact)
```

**Core Principle**: All specifications are Turtle/RDF ontologies. Markdown files are **generated** from TTL using Tera templates.

### Commands Flow

```bash
/speckit.constitution  # Create/update project principles (generates constitution.ttl → constitution.md)
/speckit.specify       # Define requirements as RDF (creates feature.ttl with user stories, requirements)
/speckit.clarify       # Optional: Structured requirement refinement (updates TTL ontology)
/speckit.plan          # Establish technical architecture (generates plan.ttl → plan.md)
/speckit.tasks         # Generate actionable task breakdown (tasks.ttl → tasks.md)
/speckit.implement     # Execute implementation according to RDF specification
```

### Branch & Evidence

- **Branch naming**: `NNN-feature-name` (e.g., `001-rdf-validation`)
- **Evidence directory**: `.specify/specs/NNN-feature/evidence/`
- **TTL source files** (source of truth): `.specify/specs/NNN-feature/*.ttl`
  - `feature.ttl` - User stories, requirements, success criteria
  - `entities.ttl` - Domain entities and relationships
  - `plan.ttl` - Architecture decisions and technical plan
  - `tasks.ttl` - Task breakdown and dependencies
- **Generated markdown**: `.specify/specs/NNN-feature/{spec,plan,tasks,data-model}.md`

### Critical Paths (RDF-First)

```
.specify/ontology/spec-kit-schema.ttl → Ontology schema (vocabulary definitions)
.specify/memory/constitution.ttl      → Architectural law (source of truth, generates constitution.md)
.specify/specs/NNN-feature/*.ttl      → Feature specifications (TTL source)
.specify/specs/NNN-feature/*.md       → Generated artifacts (derived from TTL via Tera templates)
.specify/templates/rdf-helpers/       → TTL templates (user-story, entity, requirement, success-criterion)
.specify/templates/spec.tera          → Markdown generation template (SPARQL → Markdown)
```

### Workflow Integration (RDF-First)

1. **Before feature**: Run `/speckit.specify "Feature description"` → Creates `NNN-feature/feature.ttl`
2. **During planning**: Use `/speckit.plan` → Updates `plan.ttl`, generates `plan.md`
3. **Before coding**: Run `/speckit.tasks` → Creates `tasks.ttl`, generates `tasks.md`
4. **During implementation**:
   - Read TTL source files for truth
   - Regenerate markdown via `ggen render spec.tera feature.ttl > spec.md`
   - Follow `/speckit.implement` guidance
5. **Throughout**:
   - Evidence in `.specify/specs/NNN-feature/evidence/`
   - **NEVER manually edit .md files** - Edit .ttl source, regenerate markdown
   - Use TTL templates from `.specify/templates/rdf-helpers/`

### Integration with Cargo Make

```bash
# Validate TTL specs exist before implementation
cargo make speckit-check  # Verifies .ttl source files present for current branch

# Run full workflow validation (RDF → Markdown chain)
cargo make speckit-validate  # Checks feature.ttl → spec.md generation chain

# Regenerate all markdown from TTL sources
cargo make speckit-render  # Applies Tera templates to all .ttl files
```

### TTL Editing Workflow

**When adding user stories:**
```bash
# 1. Copy template
cp .specify/templates/rdf-helpers/user-story.ttl.template .specify/specs/NNN-feature/us-001.ttl

# 2. Edit TTL file with user story data
# 3. Regenerate spec.md
ggen render .specify/templates/spec.tera .specify/specs/NNN-feature/feature.ttl > .specify/specs/NNN-feature/spec.md
```

**SHACL validation ensures**:
- Priority must be "P1", "P2", or "P3" (not "HIGH", "LOW")
- All required fields present (title, description, acceptance scenarios)
- Minimum 1 acceptance scenario per user story
- Valid RDF syntax and structure

---

---

## 🏗️ CODEBASE ARCHITECTURE (2025 Edition)

### Project Status
- **Version**: 5.0.2 (Enterprise-grade path protection & FMEA validation)
- **Rust**: 1.91.1 (installed), MSRV: 1.74+ (effectively 1.75+), Edition 2021
- **Repository**: https://github.com/seanchatmangpt/ggen
- **License**: MIT

### Workspace Structure (16 Crates)

#### Core Infrastructure
- **ggen-core** (primary logic): Graph-aware code generation engine, RDF processing, caching, template management
- **ggen-utils** (shared): Error handling, logging, configuration, utilities
- **ggen-domain** (business logic): CLI-agnostic domain layer, concurrent agent architecture
- **ggen-config** (types): Configuration management types, validation
- **ggen-config-clap** (integration): TOML→clap bridge
- **ggen-cli-validation** (security): IO validation, path traversal prevention, input sanitization

#### User-Facing
- **ggen-cli**: Main CLI interface (entry points: main.rs, lib.rs)
- **ggen-ai**: LLM integration layer (OpenAI, Anthropic, Ollama, multi-provider via genai)

#### Advanced Features
- **ggen-marketplace** (20K lines): Package registry, discovery, FMEA-based dependency management, v3→v4 migration
- **ggen-test-audit**: Mutation testing, assertion analysis, test quality metrics
- **ggen-test-opt**: Test optimization tooling, concurrency analysis
- **ggen-e2e**: End-to-end testing with testcontainers (Feature 011)
- **ggen-node**: Node.js/WASM bindings

#### Utilities & Metaprogramming
- **ggen-macros**: Procedural macros (guards, bundles, command discovery)
- **ggen-dod**: Data-Oriented Design utilities

### Technology Stack (Current Versions)

**Async & Concurrency**:
- `tokio 1.47` (full features: macros, net, sync, time, rt-multi-thread)
- `rayon 1.11` (data-parallel iterators)
- `futures 0.3`, `futures-util 0.3`, `async-trait 0.1`
- `lru 0.16` (LRU caching with async support)

**RDF & Ontologies**:
- `oxigraph 0.5.1` (RDF store, SPARQL queries, triple processing)
- `tera 1.20` (template engine for code generation)

**Serialization**:
- `serde 1.0` + `serde_derive`, `serde_json 1.0`, `serde_yaml 0.9`, `toml 0.9`

**Error Handling**:
- `thiserror 2.0` (custom error types with derive)
- `anyhow 1.0` (context-aware error propagation)

**CLI Framework**:
- `clap 4.5` (derive-based argument parsing)
- `clap-noun-verb 5.3.4` + `clap-noun-verb-macros 5.3.4` (auto-command discovery)
- `linkme 0.3` (linker plugin for command registry)

**Validation & Security**:
- `glob 0.3` (path pattern matching for protection)
- `regex 1.12`, `typed-path` (path safety)
- `pqcrypto-mldsa 0.1` (Post-Quantum Cryptography, NIST ML-DSA)
- `sha2 0.10`, `hex 0.4`, `base64 0.22` (cryptographic operations)

**Testing** (Chicago TDD + London TDD):
- `chicago-tdd-tools 1.4.0` (state-based testing, AAA pattern)
- `proptest 1.8` (property-based testing)
- `criterion 0.7` (benchmarking with HTML reports)
- `testcontainers 0.25`, `testcontainers-modules 0.13` (E2E with containers)
- `assert_cmd 2.0`, `assert_fs 1.1` (CLI testing)
- `mockall 0.13`, `mockito 1.7` (London TDD mocking, test-only)
- `insta 1.43` (snapshot testing)

**LLM Integration**:
- `genai 0.4` (multi-provider abstraction: OpenAI, Anthropic, Ollama)
- `reqwest 0.12` (async HTTP)

**Observability**:
- `opentelemetry 0.21` + `opentelemetry-otlp 0.14`, `opentelemetry_sdk 0.21`
- `tracing 0.1`, `tracing-opentelemetry 0.22`, `tracing-subscriber 0.3` (json, env-filter, ansi)
- `log 0.4.28`, `env_logger 0.11`

**UI & Output**:
- `indicatif 0.18` (progress bars)
- `console 0.16`, `colored 3.0` (terminal colors/styling)

**Utilities**:
- `chrono 0.4` (time utilities)
- `uuid 1.18` (v4, serde)
- `tempfile 3.23`, `walkdir 2.5`, `dirs 6.0`
- `semver 1.0`, `num_cpus 1.17`
- `git2 0.20` (vendored libgit2, https)

### Build System (Poka-Yoke via Makefile.toml, 68K+ lines)

**Core Philosophy**: Error-proofing mechanisms from Toyota Production System

**Key Targets**:
```
FAST FEEDBACK LOOP          FULL VALIDATION
├─ cargo make check (5s)    ├─ cargo make test (30s timeout)
├─ cargo make test-unit     ├─ cargo make pre-commit (<60s)
├─ cargo make lint (60s)    └─ cargo make ci (full pipeline)

QUALITY ASSURANCE          PERFORMANCE
├─ cargo make speckit-*     ├─ cargo make slo-check
└─ cargo make timeout-check └─ cargo make bench (14 suites)
```

**Poka-Yoke Mechanisms**:
1. **Timeout Enforcement**: All targets have timeouts; SLOs: check <5s, test <30s, lint <60s
2. **Warnings as Errors**: RUSTFLAGS="-D warnings" enforces zero-warning builds
3. **Quality Gates**: pre-commit depends on [check, lint, test-unit]
4. **Andon Signal Escalation**: Quick timeout → escalation timeout on lock contention
5. **SLO Violation Detection**: Every target documents purpose, when, SLO, examples, recovery

**Lint Configuration**:
```
[workspace.lints.rust]
- warnings = "deny"
- unsafe_code = "deny"
- missing_docs = "warn"

[workspace.lints.clippy]
- all = { level = "deny" }
- pedantic = { level = "deny" }
- nursery = { level = "deny" }
- cargo = { level = "deny" }
- unwrap_used = "deny" (CRITICAL)
- expect_used = "deny" (CRITICAL)
- panic = "deny", todo = "deny", unimplemented = "deny"
- multiple_crate_versions = "allow" (complex dependency tree)
```

### Testing Approach (Chicago TDD + Property-Based)

**Philosophy**: State-based testing with real objects (NOT mocks except in London TDD)

**Organization**:
- **Unit Tests**: Inline #[test] blocks in src/ (chicago-tdd-tools 1.4.0)
- **Integration Tests**: crates/*/tests/ and tests/ (proptest, assert_cmd/assert_fs)
- **E2E Tests**: ggen-e2e/tests/ with testcontainers (Linux + macOS native)
- **Snapshots**: insta 1.43 for golden file comparison
- **Benchmarks**: 14 criterion suites in benches/ with HTML reports

**Benchmark Suites**:
- runtime_overhead, async_runtime_benchmarks, memory_profiling
- quick_runtime_validation, conventions_performance, marketplace_performance
- pipeline_performance, cli_startup_performance, comprehensive_slo_benchmarks
- v2_performance, marketplace_v2_benchmarks, fortune500_performance

**Test Quality Features**:
- Mutation testing via cargo-mutants (ggen-test-audit)
- Assertion analysis & false-positive detection (ggen-test-audit)
- London TDD mocking ONLY in #[cfg(test)] blocks

### Code Organization Patterns

**Error Handling Pattern** (Type-First):
```rust
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Validation failed: {0}")]
    ValidationError(String),
    #[error("RDF error: {0}")]
    RdfError(String),
}
pub type Result<T> = std::result::Result<T, Error>;
```
Production code: NO unwrap/expect. Test code: unwrap/expect ALLOWED.

**Module Organization** (ggen-core example):
```
src/
├── lib.rs (root, public API)
├── cache.rs, template.rs, tera_env.rs (generation)
├── types/ (path_protection, fmea, codeowners, enterprise)
├── poka_yoke/ (path protection, input validation)
├── prevention/ (error prevention, Andon signals)
├── validation/ (SPARQL-based SHACL validation)
├── lifecycle/ (build, test, deploy phases)
├── rdf/ (SPARQL queries, Oxigraph integration)
├── codegen/ (code generation logic)
├── v6/ (V6 architecture: multi-pass generation)
└── ...
```

**Path Protection Pattern**:
- Mechanism: Glob-based path matching (glob crate)
- protected_paths: src/domain/** (NEVER overwritten)
- regenerate_paths: src/generated/** (always regenerated)
- Errors: ProtectedPathViolation, ImplicitProtectionViolation, PathOverlapError

### Active Features (2025)

**Feature 004**: Test Quality Audit & Performance Optimization
- Crates: ggen-test-audit, ggen-test-opt
- Focus: Mutation testing, assertion analysis, concurrent test execution

**Feature 011**: End-to-End Testing with testcontainers
- Crate: ggen-e2e
- Platforms: Linux (containers) + macOS (native)
- Validation: Byte-for-byte output identical

**Feature 999**: Documentation as RDF Ontology
- Location: .specify/specs/999-docs-as-code/
- Type: Specification-driven documentation system

**Marketplace**: Package registry & FMEA-based dependency management
- Crate: ggen-marketplace (20K lines)
- Features: Registry search/install, FMEA analysis, v3→v4 migration

### Documentation & Specification System

**Key Files**:
```
README.md (12K)              ← Quick start, use cases
CLAUDE.md (22K)              ← This file: constitutional law
TESTING.md                   ← Comprehensive testing guide
CONTRIBUTING.md              ← Contribution guidelines
SECURITY.md                  ← Security policies
PERFORMANCE.md               ← Performance targets
docs/ARCHITECTURE.md (11K)   ← System overview
docs/CODING_STANDARDS.md     ← Conventions
docs/CHICAGO_TDD_IMPLEMENTATION.md ← Testing patterns
```

**RDF-First Specification System** (.specify/):
```
.specify/
├── ontology/spec-kit-schema.ttl (vocabulary definitions)
├── memory/constitution.ttl (architectural law, generates .md)
├── specs/004-test-audit/feature.ttl (source of truth)
├── specs/004-test-audit/*.md (generated from .ttl)
├── templates/rdf-helpers/ (TTL templates)
└── templates/spec.tera (markdown generation)
```

**Core Rule**: TTL is source of truth, markdown is generated (NEVER edit .md files)

### Key Statistics

- **Crates**: 16 workspace members
- **Benchmarks**: 14 criterion suites
- **Documentation**: 45+ markdown files
- **Dependencies**: 100+ (see Cargo.toml workspace.dependencies)
- **Recent Commits** (latest 5):
  1. Update documentation (#104)
  2. Implement advanced concurrent agent architecture (#103)
  3. Create mdbook on ggen sync with Alexander patterns (#102)
  4. docs: Fix poka-yoke guide (emphasis ggen.toml-only workflow)
  5. docs: Add poka-yoke quick reference guide for v5.0.2

### SLOs (Service Level Objectives - Verified)

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s (1000+ triples)
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs
- Test execution ≤ 30s (timeout escalates to 120s on contention)

### Entry Points & Critical Files

**Crate Entry Points**:
- CLI: `crates/ggen-cli/src/main.rs` (command entry point)
- Library: `crates/ggen-cli/src/lib.rs` (public API)
- Core: `crates/ggen-core/src/lib.rs` (generation engine)
- Domain: `crates/ggen-domain/src/lib.rs` (business logic)

**Infrastructure Files**:
- `/CLAUDE.md` - Constitutional law (THIS FILE)
- `/Makefile.toml` - Build orchestration (68K+ poka-yoke rules)
- `/Cargo.toml` - Workspace configuration
- `/.specify/` - RDF specification system (source of truth)
- `/.cursorrules` - IDE rules
- `/.pre-commit-config.yaml` - Git hooks protocol

---

## 📝 Remember

**Claude Flow coordinates, Claude Code creates!**

**Stop the line when Andon signals appear!**

**Always use `cargo make` - NEVER direct cargo!**

**TodoWrite always has 10+ todos in ONE call!**

**Tests verify behavior - code doesn't work if tests don't pass!**

**TTL before code - NO implementation without .ttl specifications!**

**Markdown is generated - NEVER edit .md, edit .ttl source!**

**Constitutional equation: spec.md = μ(feature.ttl)**

---

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files.
Never save working files, text/mds and tests to the root folder.
TODO LISTS ARE ALWAYS 10 ITEMS OR MORE. THEY ARE ALWAYS FULLY COMPLETED BEFORE PROGRESSING TO THE NEXT TASK.

## Active Technologies (Current State - 2025)

**Core Stack**:
- Rust 1.91.1 (installed), 1.86.0 (via asdf), MSRV: 1.74+ (effectively 1.75+), edition 2021
- Tokio 1.47 async runtime (multi-threaded, full features)
- Oxigraph 0.5.1 (RDF store + SPARQL)
- Tera 1.20 (template engine)
- Chicago TDD 1.4.0 (state-based testing)

**Testing & Quality**:
- chicago-tdd-tools 1.4.0 (state-based, real objects)
- proptest 1.8 (property-based testing)
- criterion 0.7 (14 benchmark suites with HTML)
- testcontainers 0.25 (E2E with containers)
- mockall 0.13, mockito 1.7 (London TDD, test-only)
- insta 1.43 (snapshot testing)

**Security & Crypto**:
- pqcrypto-mldsa 0.1 (Post-Quantum, NIST ML-DSA)
- sha2 0.10, hex 0.4, base64 0.22

**Observability**:
- OpenTelemetry 0.21 (OTLP export)
- tracing 0.1 + json subscriber
- SLO monitoring via cargo make targets

**LLM Integration**:
- genai 0.4 (multi-provider: OpenAI, Anthropic, Ollama)
- reqwest 0.12 (async HTTP)

**Spec System**:
- RDF ontologies (Turtle .ttl files)
- Tera templates for code generation
- SPARQL queries via Oxigraph

## Recent Development Patterns (2025)

**Commit Themes**:
- Documentation updates (RDF-first specification adoption)
- Architecture improvements (concurrent agent architecture #103)
- Specification system refinement (.specify/ infrastructure)
- Poka-Yoke hardening (path protection, FMEA)
- Clippy lint fixes (automated quality enforcement)
- Multi-language support (Node.js bindings)

**Code Quality Trends**:
1. Type-first design: NewType patterns, SPARQL-based validation
2. Concurrent execution: Tokio async, rayon parallelism
3. Enterprise hardening: Path protection, FMEA, PQC
4. Testing investment: Mutation testing, E2E with containers
5. Specification-driven: RDF before code, generated documentation

**Architectural Trends**:
1. Ontology-driven: RDF specifications as source of truth
2. Concurrent execution: Tokio + rayon + testcontainers parallel
3. Enterprise hardening: Glob-based path protection, FMEA analysis, CODEOWNERS, PQC
4. Observability: OpenTelemetry, structured logging (tracing + json), SLO monitoring
5. Multi-pass generation: V6 architecture with composition/passes pattern
