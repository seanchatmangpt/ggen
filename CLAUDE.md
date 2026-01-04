# ggen - Rust Project Configuration (Bleeding-Edge 2026 Edition)

## 🎯 Core Identity

**ggen**: Language-agnostic, deterministic code generation CLI. Ontologies + RDF → reproducible code projections.

**Tech Stack**: Rust (workspace), SPARC + Chicago TDD + DfLSS
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

---

## 🚨 THREE PARADIGM SHIFTS (The Bleeding Edge - 2026)

This section defines the most impactful changes to ggen workflow. Mastery of these three shifts unlocks 2.8-4.4x speedup and dramatically better outcomes.

### Paradigm Shift 1: Big Bang 80/20 (Specification-First)

**Old way**: Vague requirement → Plan → Code → Test → [Iterate when broken]

**New way**: Specification closure verification → Single-pass construction → Validation receipts

**The principle**: Iteration is a defect signal of incomplete specification, not a normal workflow.

**What changes**:

1. **Specification closure is MANDATORY before implementation**
   - Before coding: run `/speckit-verify`
   - If incomplete: **STOP**, clarify with user, update .ttl
   - Only proceed when closure score = 100%

2. **Iteration means your spec was incomplete**
   - This is NOT normal
   - Fix the spec, don't iterate the code
   - One pass = one implementation

3. **Monoidal composition**: Systems compose without rework
   - Types enforce invariants
   - Code generation is "compilation from specification"
   - No adaptation layers needed

**When you use it**:
- Every non-trivial task starts with `/speckit-verify`
- If spec says "TBD", "maybe", "probably": **NOT closed**
- Incomplete spec → use `bb80-specification-closure` skill

**Benefits**:
- Fewer iterations (single-pass when spec is closed)
- Clearer success criteria upfront
- Fewer surprises during review

**Skill to master**: `bb80-specification-closure` (read it!)

---

### Paradigm Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**Old way (sequential)**: Plan agent (1h) → Code agent (2h) → Test agent (1h) → Reviewer (1h) = **5 hours**

**New way (parallel)**: 10 agents in parallel (2h) + Collision detection (30m) + Convergence (30m) = **3 hours**

**The principle**: Parallel-first is default for non-trivial tasks. Specification closure enables it.

**Six mandatory phases (non-skippable, non-reorderable)**:

```
1. FAN-OUT (Spawn 10+ independent agents)
         ↓
2. INDEPENDENT CONSTRUCTION (All work in parallel, NO coordination)
         ↓
3. COLLISION DETECTION (Analyze overlaps: structural & semantic)
         ↓
4. CONVERGENCE (Apply selection pressure, synthesize best solution)
         ↓
5. REFACTORING & SYNTHESIS (Merge, discard, rewrite as needed)
         ↓
6. CLOSURE (Validate all phases complete or no output)
```

**What makes it work**:

- **Parallelism**: 10 agents simultaneously exploring solution space (2.8-4.4x faster)
- **Diversity**: Multiple perspectives prevent blind spots
- **Collision detection**: When agents converge, you know you're right
- **Selection pressure** (not voting): Best solution wins, not compromise

**Trivial tasks skip EPIC 9**:
- Reading one file
- Running one script
- Displaying help
- (Most other tasks: use EPIC 9)

**When to use**:
```bash
# For any non-trivial task:
/speckit-verify [feature]        # Verify closure first (MANDATORY)
/bb80-parallel "[specification]" # Orchestrate atomic cycle
```

**What happens**:
1. 10 agents spawn with full spec
2. Each works independently (no "waiting for agent X")
3. All produce complete artifacts
4. Collision detection reveals where they converged
5. Convergence synthesizes best parts from all agents
6. You get polished, multi-perspective solution

**Benefits**:
- 2.8-4.4x faster than sequential
- High confidence (collision = multiple agents agreed)
- Better coverage (agents hit different parts)
- No iteration needed if spec is closed

**Skills to master**:
- `bb80-parallel-agents` (when/why to use)
- `bb80-specification-closure` (prerequisite)

**Commands to use**:
- `/speckit-verify` (verify closure)
- `/bb80-parallel` (orchestrate cycle)
- `/collision-detect` (analyze overlaps)
- `/convergence` (synthesize results)

---

### Paradigm Shift 3: Deterministic Validation (Evidence-First)

**Old way**: "I reviewed the code. It looks good. Performance should be fine." (Opinion)

**New way**: "[Receipt] cargo make check ✓ | [Receipt] All 347 tests pass | [Receipt] SLOs met" (Evidence)

**The principle**: Receipts replace review, benchmarks replace narratives, guards replace trust.

**What changes**:

1. **Never narrative review**
   ```
   ❌ "Code looks good"
   ✅ "[Receipt] cargo make lint: ✓ (0 violations)"

   ❌ "Tests should pass"
   ✅ "[Receipt] cargo make test: ✓ (347/347 pass)"

   ❌ "Performance is fine"
   ✅ "[Receipt] cargo make slo-check: ✓ (check 4.2s, test 28s, lint 58s)"
   ```

2. **Evidence is reproducible**
   - Anyone can run `cargo make pre-commit`
   - Same output every time
   - Auditable (timestamps, versions)

3. **Receipts are the new "done"**
   ```
   [Receipt] cargo make check: ✓
   [Receipt] cargo make test: ✓ (347/347)
   [Receipt] cargo make lint: ✓ (0 violations)
   [Receipt] Specification coverage: 95%
   [Receipt] SLO compliance: ✓
   Status: READY FOR DEPLOYMENT
   ```

**When you use it**:
- Before marking work "done": collect receipts
- In communication: reference receipts, not opinions
- Code review: "Receipt-based only, no narrative"

**Receipt categories**:
- Compilation: `cargo make check` pass
- Tests: All tests pass, coverage ≥80%
- Linting: 0 violations (clippy, format, security audit)
- Performance: SLO check pass
- Integration: E2E tests pass
- Security: 0 vulnerabilities

**Benefits**:
- Objective (no opinion involved)
- Reproducible (anyone can verify)
- Auditable (timestamps, full history)
- Measurable (0 violations, not "probably fine")

**Skill to master**: `bb80-deterministic-receipts` (what counts as evidence)

**How to produce receipts**:
```bash
cargo make pre-commit  # Produces timestamped receipts
                        # All validation phases complete
```

---

## 🚨 CRITICAL CONSTITUTIONAL RULES (Supporting Detail)

These rules support the three paradigm shifts. They are still mandatory but operate at a lower level.

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

#### Rule 6.1: Subagents Do Analysis, You Do Execution

**ALWAYS delegate analysis to specialized agents** (EPIC 9 enables this)

#### Rule 6.2: Load Skills Aggressively

**Skills auto-load by context. Trust them.**

#### Rule 6.3: Output Deterministically

**ALL outputs MUST be structured (JSON, YAML, markdown lists). NO PROSE.**

#### Rule 6.4: Fail Fast on Ambiguity

**Vague specification? STOP.** Use `/speckit-verify` or clarify with user.

#### Rule 6.5: Batch Operations Aggressively

**Group ALL related operations in ONE message** (EPIC 9 requires this)

#### Rule 6.6: Context Reuse Over Re-computation

**If analysis exists, REUSE it. Do NOT re-analyze.**

---

## 📁 File Organization (Never Save to Root)

```
ggen/
├── .specify/                      # RDF-first specification system (source of truth)
│   ├── ontology/                  # Ontology schemas
│   ├── memory/                    # Project memory (constitution.ttl)
│   ├── specs/NNN-feature/         # Feature specifications
│   │   ├── *.ttl                  # TTL source files (EDIT THESE)
│   │   ├── *.md                   # Generated (NEVER EDIT)
│   │   └── evidence/              # Test evidence
│   └── templates/                 # Templates for generation
├── crates/*/src/                  # Source code (per crate)
├── crates/*/tests/                # Integration tests
├── tests/                         # Workspace tests
├── docs/                          # Documentation
├── scripts/                       # Build scripts
├── benches/                       # Benchmarks
└── templates/                     # Code generation templates
```

**Rule**: TTL is source of truth, markdown is generated. **NEVER edit .md, edit .ttl source, regenerate.**

---

## 🦀 Elite Rust Mindset (Type-First Thinking)

### The Questions to Ask:
1. **"What can I express in types?"** (before runtime values)
2. **"Is this abstraction zero-cost?"** (generics yes, trait objects no)
3. **"What are the ownership semantics?"** (explicit is better)
4. **"How can I make misuse impossible?"** (type safety > runtime checks)

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

**EPIC 9 Commands**:
- `/speckit-verify [feature]` - Verify closure
- `/bb80-parallel "[spec]"` - Orchestrate atomic cycle
- `/collision-detect` - Analyze overlaps
- `/convergence` - Synthesize result

---

## 🚫 Prohibited Patterns (Zero Tolerance)

1. **Direct cargo commands** - ALWAYS use `cargo make`
2. **Unwrap/expect in production** - Use `Result<T, E>`
3. **Ignoring Andon signals** - Stop the line when RED
4. **Iteration due to vague spec** - Use `/speckit-verify` first
5. **Sequential agent dispatch** - Use EPIC 9 (parallel)
6. **Narrative review** - Use receipts (deterministic)
7. **Saving to root** - Use proper subdirectories

---

## 🎯 SLOs (Service Level Objectives)

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs
- Test execution ≤ 30s (timeout escalates to 120s on contention)

---

## 📝 Speckit Workflow (RDF-First Specification Development)

**MANDATORY**: NO implementation without spec. RDF ontology is source of truth.

### Architecture: Ontology → Code

```
spec.ttl (source) → ggen render → spec.md (derived artifact)
```

### Workflow Integration (RDF-First)

1. **Before feature**: Run `/speckit-verify` → Check closure
2. **During planning**: Invoke `speckit-architect` agent → Architecture decisions in RDF/Turtle
3. **Before EPIC 9**: Verify specification is 100% closed (run `/speckit-verify`)
4. **During implementation**: Read TTL source for truth
5. **Throughout**: Evidence in `.specify/specs/NNN-feature/evidence/`

**NEVER manually edit .md files** - Edit .ttl source, regenerate markdown.

---

## 🔧 Using ggen Internally: Code Generation Instead of Hand-Coding

**CRITICAL INSIGHT**: ggen is not just a build tool—it's your **specification compiler**. Use it to replace hand-coding entirely.

### The Specification-First Approach

Instead of writing code by hand, you **declare** what you want in RDF (Turtle), then ggen **compiles** it into code.

**Pattern:**
```
Write TTL Ontology → ggen sync → Generates Code
  (Specification)              (Implementation)
```

This replaces manual boilerplate with deterministic generation. **Time savings: 60-80% reduction in hand-coding**.

### 1. The ggen sync Command - Your Unified Interface

**Single command**: `ggen sync` orchestrates the entire code generation pipeline:

```bash
# Basic generation from manifest
ggen sync

# Dry-run preview
ggen sync --dry-run

# Watch mode (auto-regenerate on changes)
ggen sync --watch --verbose

# Validate spec without generating
ggen sync --validate-only

# Generate specific rule only
ggen sync --rule=structs

# Output as JSON (for CI/CD)
ggen sync --format json
```

**How it works** (pipeline):
```
Load ggen.toml → Load Ontology (TTL) → Phase 1: Inference (CONSTRUCT queries)
  ↓
Materialize enriched graph → Phase 2: Generation (SELECT queries + Templates)
  ↓
Render Tera templates with SPARQL results → Write files
  ↓
Audit trail + reproducibility metadata
```

### 2. Define Your Domain in RDF/Turtle

Instead of hand-writing hundreds of lines, write a declarative TTL specification:

**Example: CLI Project Specification** (from ggen marketplace)

```turtle
@prefix cnv: <https://ggen.dev/clap-noun-verb#> .

# Project metadata
<http://example.com/calculator> a cnv:CliProject ;
    cnv:projectName "calculator" ;
    cnv:projectVersion "0.1.0" ;
    cnv:projectDescription "Simple arithmetic CLI" .

# Define resource (noun)
<#calc> a cnv:Noun ;
    cnv:nounName "calc" ;
    cnv:hasVerbs <#add>, <#subtract>, <#multiply> .

# Define actions (verbs)
<#add> a cnv:Verb ;
    cnv:verbName "add" ;
    cnv:verbDescription "Add two numbers" ;
    cnv:hasArguments <#leftArg>, <#rightArg> .

# Define parameters (arguments)
<#leftArg> a cnv:Argument ;
    cnv:argumentName "left" ;
    cnv:argumentType "i32" ;
    cnv:isRequired true .

<#rightArg> a cnv:Argument ;
    cnv:argumentName "right" ;
    cnv:argumentType "i32" ;
    cnv:isRequired true .
```

This 25-line TTL spec replaces **500+ lines of hand-written clap derive code**.

### 3. Manifest Configuration (ggen.toml)

Define generation rules and templates:

```toml
[project]
name = "calculator"
version = "0.1.0"

[ontology]
source = "ontology.ttl"       # Your RDF specification
imports = ["common.ttl"]      # Optional imports

[ontology.prefixes]
cnv = "https://ggen.dev/clap-noun-verb#"

# Phase 1: Enrichment via CONSTRUCT queries
[[inference.rules]]
name = "derive-verbs"
description = "Enrich graph with verb handlers"
construct = """
CONSTRUCT {
  ?verb a cnv:HandlerFunction ;
    cnv:handler_name ?name .
}
WHERE {
  ?verb a cnv:Verb ;
    cnv:verbName ?name .
}
"""

# Phase 2: Generation via SELECT + Templates
[[generation.rules]]
name = "generate-cli"
query = { inline = """
PREFIX cnv: <https://ggen.dev/clap-noun-verb#>
SELECT ?projectName ?version ?noun ?verbName
WHERE {
  ?proj a cnv:CliProject ;
    cnv:projectName ?projectName ;
    cnv:projectVersion ?version .
  ?noun cnv:hasVerbs ?verb .
  ?verb cnv:verbName ?verbName .
}
""" }
template = { file = "templates/cli.rs.tmpl" }
output_file = "src/main.rs"
mode = "Overwrite"

# Validation rules
[validation]
validate_syntax = true  # Validate generated Rust syntax
no_unsafe = true        # Reject code containing unsafe blocks
```

**Verification Note**: This documentation has been verified against the actual `GgenManifest` type definitions in `crates/ggen-core/src/manifest/types.rs`. The manifest structure, query source formats, and template source syntax all match the source code specifications. A live `ggen sync` execution has not yet been performed due to build dependencies still compiling, but the API is verified architecturally correct.

### 4. Templates Extract SPARQL Results

Tera templates access SPARQL query results via special context variables:

**Example: CLI Template** (`templates/cli.rs.tmpl`)

```jinja2
---
to: "src/main.rs"
vars:
  - project_name
sparql:
  verbs: |
    PREFIX cnv: <https://ggen.dev/clap-noun-verb#>
    SELECT ?noun ?verbName ?description
    WHERE {
      ?noun a cnv:Noun ; cnv:nounName ?noun_name .
      ?verb a cnv:Verb ;
        cnv:verbName ?verbName ;
        cnv:verbDescription ?description ;
        cnv:inNoun ?noun .
    }
---

// Generated by ggen from ontology
use clap::Parser;

#[derive(Parser)]
#[command(name = "{{ project_name | pascal }}")]
pub enum Cli {
    {% for verb in sparql_results.verbs %}
    /// {{ verb.description }}
    #[command(name = "{{ verb.verbName | lower }}")]
    {{ verb.verbName | pascal }} {
        /// Left operand
        left: i32,
        /// Right operand
        right: i32,
    },
    {% endfor %}
}

impl Cli {
    pub async fn execute(self) -> Result<()> {
        match self {
            {% for verb in sparql_results.verbs %}
            Self::{{ verb.verbName | pascal }} { left, right } => {
                let result = handlers::{{ verb.noun | snake }}::{{ verb.verbName }}(left, right)?;
                println!("{}", result);
            }
            {% endfor %}
        }
        Ok(())
    }
}
```

**Result**: 30-line template generates **complete CLI handler boilerplate** from SPARQL results.

### 5. Real-World Workflow: REST API Generation

**How to generate REST API without hand-coding:**

```bash
# Step 1: Write API specification in TTL
cat > api_spec.ttl << 'EOF'
@prefix api: <http://ggen.io/ontology/api#> .

<http://example.com/api> a api:RestApi ;
    api:basePath "/api" ;
    api:version "1.0.0" .

<#users> a api:Resource ;
    api:resourceName "users" ;
    api:resourcePath "/users" ;
    api:hasEndpoints <#createUser>, <#listUsers> .

<#createUser> a api:Endpoint ;
    api:method "POST" ;
    api:path "/users" ;
    api:requestBody api:CreateUserRequest ;
    api:responseBody api:User .

<#listUsers> a api:Endpoint ;
    api:method "GET" ;
    api:path "/users" ;
    api:responseBody api:UserList .
EOF

# Step 2: Create ggen.toml referencing API template pack
cat > ggen.toml << 'EOF'
[project]
name = "rest-api"

[ontology]
source = "api_spec.ttl"

[[generation.rules]]
name = "handlers"
template = { pack = "rest-api-handlers", version = "1.0" }
EOF

# Step 3: Generate
ggen sync

# Result: Complete Axum handlers, request/response types, error handling
# Generated in: src/handlers/users.rs, src/models.rs, src/error.rs
```

### 6. Incremental Generation (Watch Mode)

**Leverage caching for 2-4x speedup on iteration**:

```bash
# Watch mode monitors TTL changes
ggen sync --watch --verbose

# On each change:
# 1. Detects what changed (manifest/ontology/rules)
# 2. Invalidates only affected generation rules
# 3. Re-runs dependent rules (propagates changes)
# 4. Skips unchanged rules
# Result: 1-2s incremental regeneration vs 5-10s full rebuild
```

**Cache stored in**: `.ggen/cache/` (hashes of manifest, ontology, per-rule state)

### 7. Validation Before Generation (Poka-Yoke)

**Prevent defective specs from generating bad code**:

```bash
# Validate specification against SHACL constraints
ggen sync --validate-only

# This checks:
# - All required properties present (sh:minCount)
# - Values match datatypes (sh:datatype)
# - Values match patterns (sh:pattern - regex)
# - Values in allowed enumeration (sh:in)
# - No circular dependencies in inference rules
# If validation fails: ggen exits with error code, NO code is generated

# Only proceed to generation when validation passes
if ggen sync --validate-only; then
  ggen sync  # Safe to generate
fi
```

### 8. Parallelization: Why ggen Enables EPIC 9

This is the **breakthrough insight** that makes parallel agent orchestration possible:

**The Pattern:**
```
Specification (TTL) is deterministic input
             ↓
10 independent agents can generate FROM THE SAME SPEC
             ↓
All 10 outputs are identical, correct implementations
             ↓
Agents converge on same solution → HIGH CONFIDENCE
```

**Example: 10 agents writing CLI handlers in parallel**

```
Team Specification: api_spec.ttl (single source of truth)
       ↓
Agent 1: ggen sync → handler 1 generated
Agent 2: ggen sync → handler 2 generated
...
Agent 10: ggen sync → handler 10 generated
       ↓
All agents' code is identical (compiled from same spec)
Result: EPIC 9 achieves 2.8-4.4x speedup
```

**Why this works:**
- Specifications are unambiguous (RDF is formal logic)
- Code generation is deterministic (same spec = same code)
- No iteration needed (spec is verified closed before generation)
- Agents can work in true parallelism (no coordination overhead)

### 9. Command Reference

| Command | Purpose | SLO |
|---------|---------|-----|
| `ggen sync` | Full generation pipeline | <5s |
| `ggen sync --dry-run` | Preview without writing | <5s |
| `ggen sync --watch` | Watch TTL files, auto-regenerate | Incremental <2s |
| `ggen sync --validate-only` | SHACL validation gate | <3s |
| `ggen sync --rule NAME` | Run specific rule | <2s |
| `ggen sync --force` | Overwrite without checks | <5s |
| `ggen sync --audit` | Detailed audit trail | <5s |

### 10. Key Design Principles (Why ggen beats hand-coding)

| Aspect | Hand-Coding | ggen Specification |
|--------|-------------|-------------------|
| **Declarative** | ❌ Imperative (how) | ✅ Declarative (what) |
| **Reproducible** | ❌ Manual variations | ✅ Deterministic |
| **Parallelizable** | ❌ Coordination overhead | ✅ Same spec → same output |
| **Composable** | ❌ Each project unique | ✅ Specifications compose |
| **Validatable** | ❌ Test after coding | ✅ Validate before generation |
| **Iterable** | ❌ Iterate code | ✅ Iterate specification |
| **Auditable** | ❌ Narrative reviews | ✅ Deterministic receipts |
| **Time** | ❌ 100% from scratch | ✅ 60-80% faster |

### Summary: When to Use ggen vs Hand-Code

**Use ggen when:**
- Generating multiple similar artifacts (CLIs, APIs, database schemas)
- Working with a specification (domain model, API contract)
- Needing reproducibility across environments/agents
- Parallelizing work across multiple agents
- Generating from domain-specific languages

**Hand-code when:**
- One-off utility, no reuse
- Custom logic not covered by templates
- Exploring/prototyping (spec not yet stable)

**Best practice**: **Specification + ggen for 80% of code, hand-code for custom 20%**

---

## 🏗️ CODEBASE ARCHITECTURE (2025 Edition)

### Project Status
- **Version**: 5.0.2 (Enterprise-grade path protection & FMEA validation)
- **Rust**: 1.91.1 (installed), MSRV: 1.75+, Edition 2021
- **Repository**: https://github.com/seanchatmangpt/ggen
- **License**: MIT

### Workspace Structure (16 Crates)

**Core Infrastructure**:
- **ggen-core**: Graph-aware code generation engine, RDF processing, caching
- **ggen-utils**: Error handling, logging, configuration
- **ggen-domain**: CLI-agnostic domain layer, concurrent agents
- **ggen-config**: Configuration types and validation
- **ggen-cli-validation**: IO validation, path traversal prevention

**User-Facing**:
- **ggen-cli**: Main CLI interface
- **ggen-ai**: LLM integration (OpenAI, Anthropic, Ollama)

**Advanced Features**:
- **ggen-marketplace** (20K lines): Package registry, FMEA dependency management
- **ggen-test-audit**: Mutation testing, assertion analysis
- **ggen-test-opt**: Test optimization
- **ggen-e2e**: End-to-end testing with testcontainers
- **ggen-node**: Node.js/WASM bindings
- **ggen-macros**: Procedural macros
- **ggen-dod**: Data-Oriented Design utilities

### Technology Stack (Current Versions)

**Async & Concurrency**:
- `tokio 1.47`, `rayon 1.11`, `async-trait 0.1`, `lru 0.16`

**RDF & Ontologies**:
- `oxigraph 0.5.1` (RDF store, SPARQL queries)
- `tera 1.20` (template engine)

**Error Handling**:
- `thiserror 2.0`, `anyhow 1.0`

**Testing**:
- `chicago-tdd-tools 1.4.0` (state-based, AAA pattern)
- `proptest 1.8` (property-based)
- `criterion 0.7` (14 benchmark suites)
- `testcontainers 0.25` (E2E)

**Validation & Security**:
- `pqcrypto-mldsa 0.1` (Post-Quantum Cryptography)
- `sha2 0.10`, `hex 0.4`, `base64 0.22`

**Observability**:
- `opentelemetry 0.21` (OTLP export)
- `tracing 0.1` (structured logging)
- SLO monitoring via cargo make targets

### Build System (Poka-Yoke via Makefile.toml)

**Key Targets**:
```
FAST FEEDBACK          FULL VALIDATION       QUALITY ASSURANCE
├─ check (5s)          ├─ test (30s)         ├─ speckit-verify
├─ test-unit (10s)     ├─ pre-commit (60s)   └─ timeout-check
└─ lint (60s)          └─ ci (full pipeline)

PERFORMANCE           EPIC 9 SUPPORT
├─ slo-check          └─ collision-detect
└─ bench (14 suites)
```

**Poka-Yoke Mechanisms**:
1. Timeout enforcement (SLOs: check <5s, test <30s, lint <60s)
2. Warnings as errors (RUSTFLAGS="-D warnings")
3. Quality gates (pre-commit depends on check + lint + test-unit)
4. Andon signal escalation (quick → escalation timeout on contention)
5. SLO violation detection (every target documents SLO)

### Testing Approach (Chicago TDD + Property-Based)

**Philosophy**: State-based testing with real objects (NO mocks except London TDD in tests)

**Organization**:
- **Unit Tests**: Inline #[test] blocks in src/ (chicago-tdd-tools 1.4.0)
- **Integration Tests**: crates/*/tests/ and tests/ (proptest, assert_cmd/assert_fs)
- **E2E Tests**: ggen-e2e/tests/ with testcontainers
- **Snapshots**: insta 1.43 for golden file comparison
- **Benchmarks**: 14 criterion suites with HTML reports

**Test Quality Features**:
- Mutation testing (cargo-mutants, ggen-test-audit)
- Assertion analysis & false-positive detection
- London TDD mocking ONLY in #[cfg(test)] blocks

---

## 📝 Remember

**Specification closure is prerequisite for EPIC 9**

**EPIC 9 is default for non-trivial tasks**

**Deterministic validation replaces human review**

**Big Bang 80/20: Single-pass construction in low-entropy domains**

**Always use `cargo make` - NEVER direct cargo!**

**Collect receipts, not narratives**

**TTL before code - NO implementation without .ttl specifications!**

**Markdown is generated - NEVER edit .md, edit .ttl source!**

---

## Important Reminders

- Do what has been asked; nothing more, nothing less
- NEVER create files unless absolutely necessary
- ALWAYS prefer editing an existing file
- NEVER proactively create documentation files
- Never save working files or tests to root
- TODO LISTS: Always 10+ items in ONE call, fully completed before progressing

---

## Active Technologies (2025-2026)

**Core**: Rust 1.91.1 (MSRV 1.75+, Edition 2021), Tokio 1.47 async runtime, Oxigraph 0.5.1 (RDF + SPARQL), Tera 1.20

**Quality**: chicago-tdd-tools 1.4.0, proptest 1.8, criterion 0.7 (14 suites), testcontainers 0.25, insta 1.43

**Security**: pqcrypto-mldsa 0.1 (Post-Quantum), sha2 0.10, glob-based path protection

**Observability**: OpenTelemetry 0.21, tracing 0.1 (json), SLO monitoring

**Spec System**: RDF ontologies (TTL), SPARQL queries, Tera template generation

---

**Constitutional Equation**: `spec.md = μ(feature.ttl)` | EPIC 9 is default | Receipts replace review
