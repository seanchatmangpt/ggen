<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v5 Strategy: Complete Rewrite](#ggen-v5-strategy-complete-rewrite)
  - [Executive Vision](#executive-vision)
    - [Why Now?](#why-now)
    - [Strategic Principle](#strategic-principle)
  - [V5 Core Architecture](#v5-core-architecture)
    - [Input: RDF Ontology](#input-rdf-ontology)
    - [Processing: Semantic Reasoning](#processing-semantic-reasoning)
    - [Output: Generated Code](#output-generated-code)
  - [What Gets Cut (Ruthless Scope Reduction)](#what-gets-cut-ruthless-scope-reduction)
    - [Out of Scope (v5.0)](#out-of-scope-v50)
    - [In Scope (v5.0 MVP)](#in-scope-v50-mvp)
  - [Technical Stack (v5.0)](#technical-stack-v50)
    - [Language Choice](#language-choice)
    - [Key Dependencies (LOCKED)](#key-dependencies-locked)
    - [Crate Structure](#crate-structure)
  - [V5 MVP Features](#v5-mvp-features)
    - [Phase 0: Foundation (Week 1)](#phase-0-foundation-week-1)
    - [Phase 1: Code Generation (Week 2)](#phase-1-code-generation-week-2)
    - [Phase 2: Polish & Examples (Week 3)](#phase-2-polish--examples-week-3)
    - [Phase 3: v5.0 Release](#phase-3-v50-release)
  - [Data Model: RDF for Code Generation](#data-model-rdf-for-code-generation)
    - [Ontology Structure (Turtle Example)](#ontology-structure-turtle-example)
    - [SPARQL Query for Generation](#sparql-query-for-generation)
  - [CLI Interface (v5.0)](#cli-interface-v50)
    - [Simple Invocation](#simple-invocation)
    - [Exit Codes (Semantic)](#exit-codes-semantic)
  - [Competitive Advantages](#competitive-advantages)
    - [vs Yeoman / Nx / Cargo Generates](#vs-yeoman--nx--cargo-generates)
    - [vs LLM + Prompt Engineering](#vs-llm--prompt-engineering)
    - [vs Existing RDF Tools](#vs-existing-rdf-tools)
  - [What Happens to v4 Features?](#what-happens-to-v4-features)
  - [Success Metrics (v5.0)](#success-metrics-v50)
    - [Performance](#performance)
    - [Quality](#quality)
    - [Usability](#usability)
    - [Adoption](#adoption)
  - [Risk Mitigation](#risk-mitigation)
    - [Risk: "RDF/SPARQL is too complex for users"](#risk-rdfsparql-is-too-complex-for-users)
    - [Risk: "Performance not good enough"](#risk-performance-not-good-enough)
    - [Risk: "Single language (Rust) limits appeal"](#risk-single-language-rust-limits-appeal)
    - [Risk: "Users want to migrate from v4"](#risk-users-want-to-migrate-from-v4)
  - [Implementation Roadmap (8 weeks)](#implementation-roadmap-8-weeks)
    - [Week 1-2: Foundation](#week-1-2-foundation)
    - [Week 3-4: Code Generation](#week-3-4-code-generation)
    - [Week 5: Polish](#week-5-polish)
    - [Week 6: Testing & Benchmarks](#week-6-testing--benchmarks)
    - [Week 7: Release Prep](#week-7-release-prep)
    - [Week 8: Launch](#week-8-launch)
  - [Architecture Principles](#architecture-principles)
    - [1. **Semantic First**](#1-semantic-first)
    - [2. **Determinism Guaranteed**](#2-determinism-guaranteed)
    - [3. **Single Responsibility**](#3-single-responsibility)
    - [4. **Minimal Dependencies**](#4-minimal-dependencies)
    - [5. **Fast Feedback Loop**](#5-fast-feedback-loop)
  - [Migration Path for v4 Users](#migration-path-for-v4-users)
    - [Option 1: Keep v4 (Maintenance Mode)](#option-1-keep-v4-maintenance-mode)
    - [Option 2: Migrate to v5 + Companion Tools](#option-2-migrate-to-v5--companion-tools)
    - [Option 3: Hybrid (Recommended)](#option-3-hybrid-recommended)
  - [File Structure (v5.0)](#file-structure-v50)
  - [Constitutional Alignment for v5](#constitutional-alignment-for-v5)
    - [II. Deterministic RDF Projections ✅](#ii-deterministic-rdf-projections-)
    - [III. Chicago TDD ✅](#iii-chicago-tdd-)
    - [V. Type-First Thinking ✅](#v-type-first-thinking-)
    - [VI. Andon Signal Protocol ✅](#vi-andon-signal-protocol-)
    - [IX. Lean Six Sigma Quality ✅](#ix-lean-six-sigma-quality-)
  - [Decision Points - LOCKED DECISIONS (2024-12-14)](#decision-points---locked-decisions-2024-12-14)
  - [Poka-Yoke Safety for Coding Agents (Critical Feature)](#poka-yoke-safety-for-coding-agents-critical-feature)
    - [Problem: Unsafe Code Generation](#problem-unsafe-code-generation)
    - [Solution: Multi-Layer Poka-Yoke Guards](#solution-multi-layer-poka-yoke-guards)
    - [Safety Invariants (Chicago TDD Enforced)](#safety-invariants-chicago-tdd-enforced)
    - [Testing Poka-Yoke (Chicago TDD)](#testing-poka-yoke-chicago-tdd)
  - [Next Steps](#next-steps)
  - [Success = Staying Focused](#success--staying-focused)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v5 Strategy: Complete Rewrite

**Date**: 2024-12-14
**Version**: v5.0.0 (Breaking from v4)
**Compatibility**: None (clean slate)
**Philosophy**: Semantic precision over generalization

---

## Executive Vision

**ggen v5** is a complete rewrite that strips away everything non-essential and focuses on a single, expertly-executed job:

> **"Generate deterministic, reproducible code from semantic domain models (RDF ontologies) with zero ambiguity."**

### Why Now?
- v4 tried to be "all things" (workflows, projects, marketplace, agents, FMEA, etc)
- Most features can be better solved by specialized tools
- Core strength: RDF-based semantic reasoning for code generation
- Opportunity: Become the best-in-class semantic code generation tool

### Strategic Principle
> **Delegate horizontally, excel vertically**

Delegate: Project scaffolding, agent orchestration, CI/CD, marketplace
Excel: Semantic-driven code generation from ontologies

---

## V5 Core Architecture

### Input: RDF Ontology
```
ontology.rdf (RDF/XML, Turtle, N3, JSONLD)
  ├─ Domain Model (classes, properties, cardinalities)
  ├─ Code Generation Rules (via RDF properties)
  └─ Target Specifications (language, framework, patterns)
```

### Processing: Semantic Reasoning
```
SPARQL Query Engine
  ↓
Extract semantic facts from RDF graph
  ↓
Apply transformation rules
  ↓
Generate code fragments
  ↓
Compose into final project
```

### Output: Generated Code
```
project/
  ├─ src/
  │   ├─ models/          (from RDF classes)
  │   ├─ repositories/    (from RDF properties)
  │   └─ services/        (from RDF rules)
  ├─ tests/               (generated from SHACL constraints)
  ├─ docs/                (from rdfs:comment annotations)
  └─ config/              (framework-specific)
```

---

## What Gets Cut (Ruthless Scope Reduction)

### Out of Scope (v5.0)
1. ❌ **Workflow Management** → Use GitHub Actions / Temporal / Airflow
2. ❌ **Agent Orchestration** → Use Claude API / LangChain / Anthropic SDK
3. ❌ **Project Management** → Use Nx / Lerna / Cargo Workspace
4. ❌ **Marketplace** → Use npm / crates.io / PyPI registries
5. ❌ **FMEA / Quality Gates** → Use pre-commit hooks / linters / formatters
6. ❌ **JTBD Documentation** → Users write their own or use Claude
7. ❌ **Multi-language rendering** → Just Rust for v5, templates for others

### In Scope (v5.0 MVP)
1. ✅ **RDF Ontology Parsing** (Turtle, RDF/XML, JSONLD)
2. ✅ **SPARQL Query Engine** (in-memory, fast)
3. ✅ **Code Generation from Semantic Rules**
4. ✅ **Template Engine** (Handlebars or Tera)
5. ✅ **Single Target Language** (Rust or TypeScript - TBD)
6. ✅ **CLI Interface** (simple, idiomatic)
7. ✅ **Deterministic Output** (same input = identical output)

---

## Technical Stack (v5.0)

### Language Choice
**Rust** (keep existing investment, maintain determinism, performance)

### Key Dependencies (LOCKED)
```toml
# RDF & SPARQL
oxigraph = "0.3"          # Embedded SPARQL engine, all RDF formats, N3 support
oxigraph_model = "0.3"    # Type-safe RDF model

# CLI
clap = "4.x"              # argument parsing
clap_complete = "4.x"     # shell completions

# Templates with safety filters
tera = "1.x"              # Jinja2-like templates
regex = "1.x"             # Pattern matching for safety validation

# Type safety & error handling
thiserror = "1.x"         # Semantic error types (Result<T,E>)
serde = { version = "1.x", features = ["derive"] }
serde_json = "1.x"        # JSON for inter-process, not core RDF

# Poka-yoke safety
parking_lot = "0.12"      # Fast mutex (Poison-free)
rayon = "1.x"             # Parallel processing (data race prevention)

# Performance
criterion = "0.5"         # Benchmarking (compile-time guarantees)

# ZERO overhead:
# - tokio (sync-first, no async by default)
# - sqlx (no database persistence)
# - reqwest (no HTTP by default)
# - tonic (no gRPC)
# - serde_json (only for output, not core)
```

### Crate Structure
```
ggen-v5/
├── Cargo.toml          # Workspace root
├── crates/
│   ├── ggen-core/      # RDF parsing + SPARQL
│   ├── ggen-codegen/   # Code generation engine
│   ├── ggen-templates/ # Built-in templates
│   └── ggen-cli/       # CLI binary
└── examples/
    ├── basic.rdf       # Hello world ontology
    ├── microservice/   # Realistic example
    └── enterprise/     # Complex example
```

---

## V5 MVP Features

### Phase 0: Foundation (Week 1)
- [ ] RDF parser (Turtle format first)
- [ ] In-memory RDF graph
- [ ] SPARQL query executor
- [x] Single unified CLI (`ggen sync` with ggen.toml configuration)

### Phase 1: Code Generation (Week 2)
- [ ] Template engine integration
- [ ] Generate Rust struct definitions from RDF classes
- [ ] Generate trait implementations from RDF rules
- [ ] Generate tests from SHACL constraints

### Phase 2: Polish & Examples (Week 3)
- [ ] CLI ergonomics (help, validation, error messages)
- [ ] 3 example ontologies with full generated projects
- [ ] Documentation: semantic modeling guide
- [ ] Performance benchmarks (target: <100ms for 1000-class ontology)

### Phase 3: v5.0 Release
- [ ] All phases complete
- [ ] Breaking change announcement
- [ ] Migration guide for v4 users (what to use instead)

---

## Data Model: RDF for Code Generation

### Ontology Structure (Turtle Example)
```turtle
@prefix : <http://example.com/myapp#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Domain Classes
:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user in the system" ;
  :codegen-as "struct" ;
  :fields (
    [ :name "id" ; :type "Uuid" ; :required true ]
    [ :name "email" ; :type "String" ; :required true ]
    [ :name "created_at" ; :type "DateTime" ; :required true ]
  ) .

:UserRepository a rdfs:Class ;
  rdfs:label "UserRepository" ;
  :codegen-as "trait" ;
  :methods (
    [ :name "create" ; :params (:User) ; :returns :User ]
    [ :name "find_by_id" ; :params ("Uuid") ; :returns :User ]
  ) .

# Code Generation Rules
:generate-async-trait
  a rdf:Property ;
  rdfs:domain :UserRepository ;
  rdfs:range xsd:boolean ;
  :codegen-hint "impl#[tokio::main]" .
```

### SPARQL Query for Generation
```sparql
PREFIX : <http://example.com/myapp#>

SELECT ?class ?name ?fields
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?name ;
    :fields ?fieldsList .
  OPTIONAL { ?fieldsList :items ?fields }
}
```

---

## CLI Interface (v5.0)

### Simple Invocation
```bash
# Configure once in ggen.toml
cat > ggen.toml <<'EOF'
[project]
name = "my-project"

[generation]
ontology_dir = "ontology/"
templates_dir = "templates/"
output_dir = "src/generated/"
EOF

# Then simply run
ggen sync

# That's it - everything is configured via ggen.toml
```

### Exit Codes (Semantic)
```
0 = Success
1 = Ontology parsing error
2 = Generation constraint violation
3 = Template not found
4 = Output directory error
```

---

## Competitive Advantages

### vs Yeoman / Nx / Cargo Generates
- **Semantic Precision**: RDF reasoning ensures correctness, not regex patterns
- **Language Agnostic**: Same ontology → different code generators (Rust, Python, Go)
- **Auditable**: Every line traceable to ontology decision + template
- **Fast**: <100ms for enterprise-scale models

### vs LLM + Prompt Engineering
- **Deterministic**: Same input = identical output (no hallucinations)
- **Offline**: No API calls, no token costs, works disconnected
- **Reproducible**: Perfect for CI/CD, version control friendly
- **Explainable**: Reasoning is transparent (SPARQL queries, not hidden ML)

### vs Existing RDF Tools
- **Code-First**: Templates are first-class, not an afterthought
- **Rust Native**: Performance + type safety
- **Battery-Included**: Everything needed for code gen in one binary
- **No Dependencies**: Minimal attack surface, easy to audit

---

## What Happens to v4 Features?

| v4 Feature | v5 Solution |
|-----------|------------|
| Workflow commands | Use GitHub Actions + temporal.io |
| Agent orchestration | Claude API + Anthropic SDK directly |
| Project management | Use Nx, Cargo, Poetry native workspaces |
| Marketplace | Publish ontologies to GitHub/registry |
| FMEA validation | Use pre-commit hooks + linters |
| JTBD docs | Generate from rdfs:comment + rdfs:seeAlso |
| Multi-command CLI | Single `ggen sync` command (manifest-driven) |
| RDF store | Use separate triplestore (Blazegraph, Virtuoso) |

---

## Success Metrics (v5.0)

### Performance
- [ ] Parse 1000-class ontology in <100ms
- [ ] Generate 500-line code module in <50ms
- [ ] Binary size <15MB (stripped)
- [ ] Memory usage <50MB for typical workloads

### Quality
- [ ] 100% deterministic (hash identity test)
- [ ] 95%+ test coverage
- [ ] Zero unsafe Rust code (or justified)
- [ ] All dependencies audited for security

### Usability
- [ ] `ggen --help` explains everything
- [ ] 3 complete example projects in repo
- [ ] 5-minute quick start guide
- [ ] "Semantic modeling for code gen" tutorial

### Adoption
- [ ] First stable release (v5.0.0)
- [ ] GitHub discussion on RDF code generation
- [ ] At least 1 external project using ggen v5

---

## Risk Mitigation

### Risk: "RDF/SPARQL is too complex for users"
**Mitigation**: Provide abstraction layer (JSON Schema → RDF compiler)
```bash
ggen scaffold --from-schema my-schema.json > domain.ttl
```

### Risk: "Performance not good enough"
**Mitigation**: Benchmark against oxigraph (embedded triplestore)
- Profile early
- Use in-memory graph, not disk I/O
- Cache SPARQL query plans

### Risk: "Single language (Rust) limits appeal"
**Mitigation**: Design with polyglot in mind
- Separate code gen logic from language-specific templates
- Plan for Python/TypeScript backends in v5.1
- Ontology agnostic to implementation language

### Risk: "Users want to migrate from v4"
**Mitigation**:
- Keep v4 branch available (maintenance mode)
- Write v4 → v5 migration guide
- Don't force migration; let users choose

---

## Implementation Roadmap (8 weeks)

### Week 1-2: Foundation
- RDF parser (Turtle)
- SPARQL engine (basic)
- CLI skeleton

### Week 3-4: Code Generation
- Template engine
- First language (Rust) code gen
- Basic examples

### Week 5: Polish
- Error handling & validation
- Documentation & examples
- Performance optimization

### Week 6: Testing & Benchmarks
- Comprehensive test suite
- Performance benchmarks
- Security audit

### Week 7: Release Prep
- Docs website
- Changelog
- v5.0.0 release candidate

### Week 8: Launch
- Tag v5.0.0
- Announce migration path for v4 users
- Community feedback loop

---

## Architecture Principles

### 1. **Semantic First**
Everything flows from RDF ontology truth
```
RDF Graph → SPARQL Queries → Code Generation
```

### 2. **Determinism Guaranteed**
```
hash(ontology + templates) == hash(output)
```

### 3. **Single Responsibility**
- ggen: Code generation from RDF
- Other tools: Everything else
- Compose via Unix philosophy (pipes, files)

### 4. **Minimal Dependencies**
Each dependency must:
- [ ] Be actively maintained
- [ ] Have <3 transitive dependencies
- [ ] Have security audit trail
- [ ] Be justifiable vs custom code

### 5. **Fast Feedback Loop**
```bash
# <100ms total time
ggen sync
```

---

## Migration Path for v4 Users

### Option 1: Keep v4 (Maintenance Mode)
- v4.x continues to receive security patches
- No new features
- Suitable for existing workflows

### Option 2: Migrate to v5 + Companion Tools
```
Old v4 Workflow          →  New v5 Workflow
ggen sync                →  All configuration from ggen.toml
ggen workflow deploy     →  GitHub Actions / Temporal
ggen marketplace search  →  npm / crates.io search
```

### Option 3: Hybrid (Recommended)
Use v5 for code generation, v4 CLI wrappers for orchestration during transition.

---

## File Structure (v5.0)

```
ggen-v5/
├── GGEN-V5-STRATEGY.md          ← This document
├── Cargo.toml                   ← Workspace
├── crates/
│   ├── ggen-core/
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── rdf/             ← RDF parsing & graph
│   │       ├── sparql/          ← Query engine
│   │       └── lib.rs
│   │
│   ├── ggen-codegen/
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── codegen/         ← Code generation logic
│   │       ├── templates/       ← Built-in templates
│   │       └── lib.rs
│   │
│   ├── ggen-cli/
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── commands/        ← CLI subcommands (generate, validate, query)
│   │       ├── main.rs
│   │       └── lib.rs
│   │
│   └── ggen-templates/          ← Curated templates library
│       ├── Cargo.toml
│       └── templates/
│           ├── rust-service/
│           ├── rust-cli/
│           └── rust-library/
│
├── examples/
│   ├── basic/
│   │   ├── domain.ttl           ← Ontology
│   │   ├── generated/           ← Output
│   │   └── README.md
│   ├── microservice/
│   └── enterprise/
│
├── tests/
│   ├── integration/
│   ├── examples/
│   └── benchmarks/
│
└── docs/
    ├── guide.md                 ← "Semantic Modeling for Code Gen"
    ├── ontology-design.md
    ├── template-authoring.md
    └── migration.md             ← v4 → v5 migration
```

---

## Constitutional Alignment for v5

How v5 stays true to ggen's founding principles:

### II. Deterministic RDF Projections ✅
V5 **doubles down** on this - RDF is core, not peripheral

### III. Chicago TDD ✅
State-based testing of generated code (not just generation logic)

### V. Type-First Thinking ✅
RDF ontology IS a type system (semantic types)

### VI. Andon Signal Protocol ✅
Validation before generation, fail-fast approach

### IX. Lean Six Sigma Quality ✅
Minimal scope, zero waste, focus on vital few

---

## Decision Points - LOCKED DECISIONS (2024-12-14)

**Vision**: Hyperfast SPARQL/N3 code generator with poka-yoke safety for coding agents

1. **Single Target Language (v5.0)**: ✅ **PURE RUST** (hyperfast, deterministic, memory-safe)
   - Zero TypeScript overhead
   - Native performance for agent orchestration
   - Type-level safety prevents unsafe code generation

2. **SPARQL Engine**: ✅ **oxigraph (embedded)** (fast, Rust-native, battle-tested)
   - In-process, no external service latency
   - Full SPARQL 1.1 compliance
   - Deterministic query execution
   - N3 logic support built-in

3. **Template Engine**: ✅ **Tera** (Jinja2-like, full-featured, Rust ecosystem)
   - Production-ready
   - Safety filters for agent code generation
   - Macro support for complex patterns

4. **RDF Formats (v5.0)**: ✅ **All formats supported** (Turtle, RDF/XML, N3, JSONLD, TriG)
   - Comprehensive format support from day 1
   - oxigraph handles all conversions natively
   - Future-proofs against format shifts
   - Enables N3 rule-based code generation patterns

---

## Poka-Yoke Safety for Coding Agents (Critical Feature)

### Problem: Unsafe Code Generation
When agents orchestrate code generation, they need guarantees that output is safe, correct, and traceable.

### Solution: Multi-Layer Poka-Yoke Guards

**Layer 1: Type-Level Safety (Compile-Time)**
```rust
// RDF domain types prevent invalid configurations
pub struct RdfClass {
    iri: ValidIri,          // IRI validated at parse time
    fields: Vec<Field>,     // Field cardinalities checked
    constraints: SHACL,     // Constraints pre-validated
}

// Template rendering enforces type safety
pub struct SafeTemplate {
    filters: SafeFilterSet, // Only whitelisted Tera filters
    macros: ValidatedMacros, // Pre-validated template macros
    escape_mode: EscapeStrategy, // HTML/JSON/Rust escape mode
}

// Code generation guarantees traceable output
pub struct GeneratedCode {
    source: RdfIri,         // Traceable to ontology element
    rule: SparqlQuery,      // Reproducible from SPARQL
    timestamp: Instant,     // Deterministic generation time
}
```

**Layer 2: Runtime Validation (Deterministic)**
```rust
// Pre-generation validation
pub fn validate_ontology(rdf: &RdfGraph) -> Result<ValidOntology, ValidationError> {
    // 1. Check SHACL constraints
    // 2. Verify cardinalities
    // 3. Validate type consistency
    // 4. Test SPARQL queries for termination
    // 5. Ensure no circular dependencies
}

// Safe template rendering with fallback
pub fn render_with_safety(
    template: &SafeTemplate,
    context: &RdfContext,
) -> Result<GeneratedCode, RenderError> {
    // 1. Validate context against template schema
    // 2. Render with timeout (detect infinite loops)
    // 3. Validate output against code constraints
    // 4. Return traceable (source + timestamp)
}
```

**Layer 3: Output Validation (Agent Verification)**
```rust
// Code generation audit trail
pub struct AuditTrail {
    input_ontology_hash: Hash,
    sparql_query: String,
    template_name: String,
    output_code: String,
    timestamp: Instant,
    duration_ms: u64,
    validation_errors: Vec<String>,
}

// Semantic exit codes for agent orchestration
pub enum ExitCode {
    Success = 0,              // Code generated successfully
    OntologyInvalid = 1,      // SHACL or type validation failed
    SparqlError = 2,          // Query didn't terminate or syntax error
    TemplateError = 3,        // Template rendering failed
    OutputInvalid = 4,        // Generated code fails validation
    Timeout = 5,              // Generation exceeded time limit
}
```

**Layer 4: Agent Integration (Orchestration)**
```bash
# Agents receive deterministic feedback
ggen sync  # With [audit] section in ggen.toml

# Audit trail enables verification
{
  "input_hash": "sha256:abc123...",
  "queries_executed": ["Q1", "Q2", "Q3"],
  "templates_rendered": ["struct.tera", "impl.tera"],
  "output_files": ["src/models.rs", "src/lib.rs"],
  "validation_passed": true,
  "exit_code": 0,
  "duration_ms": 47
}

# Agent uses this to decide next action
if audit.validation_passed && audit.exit_code == 0:
  # Safe to commit or continue
elif audit.exit_code in [1, 2, 4]:
  # Ontology/query/output error - fix source
elif audit.exit_code == 5:
  # Timeout - reduce ontology complexity
```

### Safety Invariants (Chicago TDD Enforced)

1. **Determinism**: `hash(ontology + templates + random_seed) == hash(output)`
2. **Traceability**: Every generated line traceable to RDF source + SPARQL query
3. **Termination**: All SPARQL queries guaranteed to terminate within 5s timeout
4. **Isolation**: Generated code cannot escape template sandbox (no shell injection)
5. **Reproducibility**: Two agents running same input produce identical output byte-for-byte

### Testing Poka-Yoke (Chicago TDD)

```rust
#[test]
fn test_unsafe_template_rejected() {
    let unsafe_template = r#"
        {% for i in 1..1000000 %}  // Infinite loop detector
            {{ i }}
        {% endfor %}
    "#;

    assert!(SafeTemplate::from_str(unsafe_template).is_err());
    // Poka-yoke: prevents template from even compiling
}

#[test]
fn test_output_code_validation() {
    let ontology = load_test_ontology("models.ttl");
    let code = generate_code(&ontology, "rust-service").unwrap();

    // Verify output is valid Rust
    assert!(rustfmt::check(&code.content).is_ok());
    // Verify it compiles
    assert!(cargo_check(&code.content).is_ok());
    // Verify no unsafe code
    assert!(!code.content.contains("unsafe"));
}

#[test]
fn test_audit_trail_determinism() {
    let ontology = load_test_ontology("models.ttl");

    let audit1 = generate_with_audit(&ontology, &seed);
    let audit2 = generate_with_audit(&ontology, &seed);

    assert_eq!(
        hash(&audit1.output_code),
        hash(&audit2.output_code),
        "Generated code must be byte-identical"
    );
}
```

---

## Next Steps

1. **Validate Vision** with core team
   - Does "semantic code gen" excite you?
   - Do we commit to "no backwards compat"?

2. **Make Technology Decisions** (above)

3. **Start v5 Branch**
   ```bash
   git checkout -b v5-rewrite
   rm -rf crates/ggen-{domain,marketplace,ai,workflow,fmea}/
   # Fresh start, keep only core
   ```

4. **Week 1 Sprint**
   - RDF parser
   - SPARQL engine proof-of-concept
   - CLI skeleton

5. **Publish Strategy**
   - Announce v5 plans on GitHub discussions
   - Get feedback from existing users
   - Build community enthusiasm for new direction

---

## Success = Staying Focused

> "The most powerful word in software is 'NO'." - Steve Jobs

**v5 wins by refusing to be everything.**

✅ Semantic code generation from RDF
❌ Everything else (use other tools)

This is our competitive moat and our path to excellence.

---

**Document Status**: Strategy Ready for Discussion
**Next Action**: User review + technology decisions

