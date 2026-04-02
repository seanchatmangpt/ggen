<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v3: Self-Hosting Code Generation Through Complete Ontology-Driven Rewrite](#ggen-v3-self-hosting-code-generation-through-complete-ontology-driven-rewrite)
  - [Executive Summary](#executive-summary)
  - [Part 1: Philosophical Foundations](#part-1-philosophical-foundations)
    - [1.1 The Self-Hosting Principle](#11-the-self-hosting-principle)
    - [1.2 The Projection Architecture Framework (PAF)](#12-the-projection-architecture-framework-paf)
    - [1.3 Composition Over Inheritance](#13-composition-over-inheritance)
  - [Part 2: What Changes in v3](#part-2-what-changes-in-v3)
    - [2.1 Architectural Improvements](#21-architectural-improvements)
      - [**Before (v2)**:](#before-v2)
      - [**After (v3)**:](#after-v3)
    - [2.2 Core Ontology Design (ggen_v3_core.ttl)](#22-core-ontology-design-ggen_v3_corettl)
    - [2.3 New Capabilities in v3](#23-new-capabilities-in-v3)
      - [**Self-Documenting Code**](#self-documenting-code)
      - [**Deterministic Versioning**](#deterministic-versioning)
      - [**Safe Evolution**](#safe-evolution)
      - [**Intelligent Regeneration**](#intelligent-regeneration)
  - [Part 3: Implementation Architecture](#part-3-implementation-architecture)
    - [3.1 Three-Phase Rewrite](#31-three-phase-rewrite)
      - [**Phase 1: Foundation (Weeks 1-4)**](#phase-1-foundation-weeks-1-4)
      - [**Phase 2: Projection System (Weeks 5-8)**](#phase-2-projection-system-weeks-5-8)
      - [**Phase 3: Cutover & Validation (Weeks 9-12)**](#phase-3-cutover--validation-weeks-9-12)
    - [3.2 Development Workflow in v3](#32-development-workflow-in-v3)
  - [Part 4: Strategic Advantages](#part-4-strategic-advantages)
    - [4.1 Eliminates Dark Matter in ggen's Own Development](#41-eliminates-dark-matter-in-ggens-own-development)
    - [4.2 Enables Real-Time Feature Parity](#42-enables-real-time-feature-parity)
    - [4.3 Provably Correct Code Generation](#43-provably-correct-code-generation)
  - [Part 5: Phases & Timeline](#part-5-phases--timeline)
    - [Phase 1: Ontology Design (Weeks 1-4)](#phase-1-ontology-design-weeks-1-4)
      - [Week 1: Domain Model](#week-1-domain-model)
      - [Week 2: CLI & Marketplace](#week-2-cli--marketplace)
      - [Week 3-4: Composition & Validation](#week-3-4-composition--validation)
    - [Phase 2: Template & Projection System (Weeks 5-8)](#phase-2-template--projection-system-weeks-5-8)
      - [Week 5-6: Core Templates](#week-5-6-core-templates)
      - [Week 7: CLI & Marketplace Templates](#week-7-cli--marketplace-templates)
      - [Week 8: Documentation & Deployment](#week-8-documentation--deployment)
    - [Phase 3: Cutover & Validation (Weeks 9-12)](#phase-3-cutover--validation-weeks-9-12-1)
      - [Week 9-10: Generation & Comparison](#week-9-10-generation--comparison)
      - [Week 11: Testing & Verification](#week-11-testing--verification)
      - [Week 12: Release & Documentation](#week-12-release--documentation)
  - [Part 6: Success Metrics](#part-6-success-metrics)
    - [6.1 Quantitative Metrics](#61-quantitative-metrics)
    - [6.2 Qualitative Metrics](#62-qualitative-metrics)
  - [Part 7: Risk Mitigation](#part-7-risk-mitigation)
    - [7.1 Major Risks](#71-major-risks)
    - [7.2 Contingency Plans](#72-contingency-plans)
  - [Part 8: Architectural Diagrams](#part-8-architectural-diagrams)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v3: Self-Hosting Code Generation Through Complete Ontology-Driven Rewrite

**Status**: PLANNING
**Version**: 3.0.0-alpha
**Branch**: `claude/plan-ggen-v3-rewrite-01PyJAjvvvwdVWwD6wickodF`
**Paradigm Shift**: ggen eating its own dogfood—ggen v3 is generated from a ggen ontology

---

## Executive Summary

ggen v2 teaches the world to think in ontologies: "Your domain O, projected via SPARQL (μ), becomes code (A = μ(O))."

**ggen v3 applies this to itself**: ggen's own codebase becomes a projection of a comprehensive ggen ontology. The toolchain that generates E-commerce domains, microservices, and academic papers now generates itself.

**Core Transformation**:
- **v2**: "Use ggen to generate your domain"
- **v3**: "ggen is itself generated from ggen"
- **v3 Outcome**: A self-aware, self-improving code generation framework that can regenerate its own codebase deterministically

---

## Part 1: Philosophical Foundations

### 1.1 The Self-Hosting Principle

**Principle**: A code generation tool should be able to generate its own source code.

**Why This Matters**:
1. **Credibility**: If ggen can't generate ggen, claims about code generation are theoretical
2. **Dogfooding**: Every feature used to generate user code is tested on ggen itself
3. **Evolution**: Ontology changes → ggen regenerates itself → new features available immediately
4. **Determinism Proof**: If ggen-generated-ggen byte-matches hand-written ggen, then A = μ(O) is proven

### 1.2 The Projection Architecture Framework (PAF)

**Layered Projections**:
```
O (Ontology) ──────┐
                   ├─ μ_core (Core crate structure)
                   ├─ μ_domain (Domain logic)
                   ├─ μ_cli (CLI commands & arguments)
                   ├─ μ_marketplace (Marketplace system)
                   ├─ μ_ai (AI providers & generators)
                   ├─ μ_guards (Validation framework)
                   ├─ μ_templates (Template system)
                   ├─ μ_tests (Test generation)
                   └─ μ_docs (Documentation generation)
                   ─────────────────────────────────
                   ↓
                   A (ggen source code)
```

Each projection family (μ_*) is driven by specific SPARQL queries on O.

### 1.3 Composition Over Inheritance

**v2 Pattern**: One ontology → Multiple language targets
```
product.ttl → Π_rust (struct) + Π_ts (interface) + Π_py (class)
```

**v3 Pattern**: One ontology → Multiple system concerns
```
ggen.ttl → Π_core (Rust modules) + Π_cli (Commands) + Π_marketplace (Packages)
        → Π_ai (LLM providers) + Π_tests (Test scaffolds) + Π_docs (API refs)
```

The ggen ontology describes:
- **System Structure**: Crates, modules, dependencies, visibility
- **Domain Model**: Core types (Ontology, Project, Template, Package, etc.)
- **API Surface**: CLI commands, marketplace APIs, lifecycle hooks
- **Validation Rules**: Guards, constraints, production readiness criteria
- **Deployment**: Container config, CI/CD, release pipelines

---

## Part 2: What Changes in v3

### 2.1 Architectural Improvements

#### **Before (v2)**:
```
cli/           ← Hand-written command parsing
domain/        ← Hand-written business logic
core/          ← Hand-written RDF/graph logic
marketplace/   ← Hand-written package system
ai/            ← Hand-written LLM clients
utils/         ← Hand-written utilities
```

**Problems**:
- Crate structure is implicit (not in metadata)
- CLI commands defined in code, not in ontology
- Marketplace validation rules hard-coded
- Test patterns vary across crates
- Documentation lags implementation

#### **After (v3)**:
```
ontologies/
└── ggen_v3_core.ttl     ← Single source of truth for ggen's structure
    ├── System structure (crates, modules, visibility)
    ├── Domain types (Ontology, Project, Package, etc.)
    ├── CLI commands (noun-verb definitions)
    ├── Marketplace rules (validation guards, scoring)
    ├── AI providers (OpenAI, Anthropic, etc.)
    ├── Test patterns (unit, integration, E2E)
    └── Deployment targets (Docker, K8s, Cloud)

templates/
├── π_core/              ← Generate Rust module structure
├── π_domain/            ← Generate domain types & traits
├── π_cli/               ← Generate CLI commands
├── π_marketplace/       ← Generate marketplace validation
├── π_ai/                ← Generate LLM provider clients
├── π_tests/             ← Generate test scaffolds
├── π_docs/              ← Generate API documentation
└── π_deployment/        ← Generate deployment configs

crates/
├── ggen-core/           ← Generated from ggen_v3_core.ttl + π_core
├── ggen-domain/         ← Generated from ggen_v3_core.ttl + π_domain
├── ggen-cli/            ← Generated from ggen_v3_core.ttl + π_cli
├── ggen-marketplace/    ← Generated from ggen_v3_core.ttl + π_marketplace
├── ggen-ai/             ← Generated from ggen_v3_core.ttl + π_ai
├── ggen-utils/          ← Generated from ggen_v3_core.ttl
└── ggen-tests/          ← Generated test suite
```

**Benefits**:
- System structure explicit in RDF (queryable, versionable)
- CLI commands defined as ontology entities → auto-completion, docs, help
- Marketplace rules = SPARQL queries (auditable, testable)
- Test patterns generated deterministically
- Documentation always in sync (generated from ontology)
- New features = ontology edits → regenerate → done

### 2.2 Core Ontology Design (ggen_v3_core.ttl)

**Key Entity Classes**:
```
ggen:Crate
├── name: String
├── version: String
├── dependencies: [Crate]
├── modules: [Module]
└── visibility: {public, private}

ggen:Module
├── name: String
├── exports: [Type | Trait | Function]
├── internal: [Type | Function]
└── tests: [Test]

ggen:Type
├── name: String
├── kind: {Struct, Enum, Trait, Union}
├── fields: [Field]
├── constraints: [Constraint]
└── mappings: {Language → LanguageType}

ggen:CliCommand
├── noun: String
├── verb: String
├── args: [Argument]
├── implementation: Crate
└── tests: [IntegrationTest]

ggen:Guard
├── name: String
├── checks: [Check]
├── applies_to: [Package | Crate | Type]
└── scoring: ScoringRule

ggen:Projection (abstract)
├── name: String
├── query: SparqlQuery
├── template: TemplateFile
├── targets: [Crate | Module | File]
└── frequency: {OnChange, OnDemand, Scheduled}
```

### 2.3 New Capabilities in v3

#### **Self-Documenting Code**
```sparql
# Query: Get all public APIs
SELECT ?type ?name ?documentation WHERE {
  ?type ggen:isPublic true .
  ?type ggen:name ?name .
  ?type ggen:documentation ?documentation .
}
```
→ Generated API docs that never lag implementation

#### **Deterministic Versioning**
```
ggen_v3_core.ttl (version 3.0.0)
  → projections applied
  → code generated
  → hash = blake3(output)
  → if hash_v3.0.0 == expected, then v3.0.0 is canonical
```

#### **Safe Evolution**
```
ggen_v3_core.ttl (v3.0.0) → code_v3.0.0
ggen_v3_core.ttl (v3.1.0) → code_v3.1.0
  ├── New CLI command defined → π_cli generates it
  ├── New Guard added → π_marketplace generates validation
  └── Type constraints changed → π_tests generates new property tests
```

#### **Intelligent Regeneration**
```
Delta detection:
  ggen_v3_core.ttl (v3.0.0) vs ggen_v3_core.ttl (v3.1.0)
    → SPARQL diff
    → Only affected modules regenerate
    → Faster CI, smaller diffs
```

---

## Part 3: Implementation Architecture

### 3.1 Three-Phase Rewrite

#### **Phase 1: Foundation (Weeks 1-4)**
- **Deliverable**: ggen_v3_core.ttl (complete ontology)
- **Scope**: Define all current ggen v2 structure in RDF
- **Validation**: Can query SPARQL and see entire v2 codebase represented
- **Risk**: Ontology design is hardest part; must be correct before generating

#### **Phase 2: Projection System (Weeks 5-8)**
- **Deliverable**: Full set of π_* templates + working generator
- **Scope**: Implement π_core, π_domain, π_cli, π_marketplace, π_ai, π_tests
- **Validation**: `ggen project gen ggen-v3 --ontology ggen_v3_core.ttl` produces compilable code
- **Risk**: Templates must handle all edge cases; high complexity

#### **Phase 3: Cutover & Validation (Weeks 9-12)**
- **Deliverable**: v3.0.0-alpha released
- **Scope**: Compare generated v3 with hand-written v2; merge optimizations
- **Validation**: v3 builds, all tests pass, feature parity with v2
- **Risk**: Finding where hand-written v2 was more optimal than generated v3

### 3.2 Development Workflow in v3

**User Workflow**:
```bash
# 1. Clone ggen v3
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# 2. Review ontology (it's source code now)
vim ontologies/ggen_v3_core.ttl

# 3. Add a new CLI command (edit ontology, not CLI code)
# Add new ggen:CliCommand entity to ggen_v3_core.ttl

# 4. Regenerate ggen
ggen project gen . --ontology ontologies/ggen_v3_core.ttl

# 5. Commit both ontology and generated code
git add ontologies/ggen_v3_core.ttl crates/ggen-cli/src/...
git commit -m "feat: add new command via ontology update"

# 6. Run tests (all generated)
cargo test

# 7. Deploy
cargo release
```

**Key Difference from v2**:
- v2: Edit source code directly
- v3: Edit ontology → regenerate → commit both

---

## Part 4: Strategic Advantages

### 4.1 Eliminates Dark Matter in ggen's Own Development

**Current (v2) Dark Matter**:
- 32 CLI commands scattered across `crate/ggen-crates/ggen-cli/src/` (~4000 LOC of boilerplate)
- Marketplace validation rules hard-coded in guards.rs (~2000 LOC)
- Test patterns copied across crates (~8000 LOC of repetition)
- API documentation manually written (~3000 LOC)
- Docker/K8s configs hand-tuned (~1000 LOC)

**Total Dark Matter**: ~18,000 LOC of "invisible scaffolding"

**In v3**:
- 32 CLI commands = ggen:CliCommand entities (32 lines in TTL)
- Marketplace validation = SPARQL queries (50 lines in TTL)
- Test patterns = ggen:TestTemplate entities (20 lines in TTL)
- Documentation = auto-generated from ontology
- Deployment = template projections

**Reduction**: ~95% fewer LOC for structural code

### 4.2 Enables Real-Time Feature Parity

**Feature Request Flow (v2)**:
1. User requests new CLI command
2. Developer writes Rust code
3. Developer writes tests
4. Developer updates documentation
5. Review/merge/deploy cycle
6. Deploy new version

**Feature Request Flow (v3)**:
1. User requests new CLI command
2. Developer adds ggen:CliCommand to ontology
3. Developer runs `ggen project gen`
4. All code, tests, docs auto-generated
5. Developer reviews & tests generated code
6. Merge & deploy same day

### 4.3 Provably Correct Code Generation

**Claim**: Every artifact generated by ggen is correct
**Proof**: ggen itself is generated by ggen
- If ggen-generated-ggen has bugs → those bugs fix ggen's own code
- If A = μ(O) works for ggen, it works for users

---

## Part 5: Phases & Timeline

### Phase 1: Ontology Design (Weeks 1-4)

#### Week 1: Domain Model
```
[ ] ggen:Crate, ggen:Module, ggen:Type, ggen:Field
[ ] ggen:Trait, ggen:Implementation, ggen:Function
[ ] Relationships (module contains type, crate exports module, etc.)
[ ] Query: "Get all public types in ggen-core"
[ ] Query: "Get all types with Field.deprecated = true"
```

#### Week 2: CLI & Marketplace
```
[ ] ggen:CliCommand, ggen:Argument, ggen:Flag
[ ] ggen:Package, ggen:Guard, ggen:ValidationRule
[ ] ggen:LlmProvider, ggen:Model, ggen:Endpoint
[ ] Query: "Get all commands that operate on packages"
[ ] Query: "Get all validation rules for 8020 packages"
```

#### Week 3-4: Composition & Validation
```
[ ] ggen:Projection families (π_core, π_cli, etc.)
[ ] ggen:TestCase, ggen:TestTemplate patterns
[ ] ggen:Constraint, ggen:ScoringRule, ggen:MaturityLevel
[ ] Full ontology review & validation
[ ] SPARQL queries cover >95% of ggen's surface
```

**Deliverable**: `ontologies/ggen_v3_core.ttl` (5,000-7,000 lines TTL)

### Phase 2: Template & Projection System (Weeks 5-8)

#### Week 5-6: Core Templates
```
[ ] π_core: Module scaffolding, crate structure, Cargo.toml generation
[ ] π_domain: Type definitions, trait generation, serialization derives
[ ] π_tests: Unit test scaffolds, integration test patterns, property tests
[ ] Validation: Generated code compiles
```

#### Week 7: CLI & Marketplace Templates
```
[ ] π_cli: Command parsing, argument handling, noun-verb routing
[ ] π_marketplace: Validation guards, scoring logic, receipts
[ ] π_ai: LLM client generation, streaming support
[ ] Integration test: Generate one CLI command end-to-end
```

#### Week 8: Documentation & Deployment
```
[ ] π_docs: API documentation, architecture diagrams, README generation
[ ] π_deployment: Docker templates, K8s manifests, CI/CD workflows
[ ] π_tests: Full test suite generation for each component
[ ] E2E validation: `ggen project gen` produces >95% of ggen codebase
```

**Deliverable**: Full template suite + working generator (8-10 generators)

### Phase 3: Cutover & Validation (Weeks 9-12)

#### Week 9-10: Generation & Comparison
```
[ ] Run full generation: ggen project gen ggen-v3 --ontology ggen_v3_core.ttl
[ ] Diff generated-ggen vs hand-written-ggen v2
[ ] Categorize differences (optimizations, hand-written, missing)
[ ] Integrate hand-written optimizations into templates
```

#### Week 11: Testing & Verification
```
[ ] All tests pass on generated codebase
[ ] Feature parity: All v2 features work in v3
[ ] Performance: v3 ≥ v2 performance
[ ] Documentation: API docs, migration guide, ontology reference
```

#### Week 12: Release & Documentation
```
[ ] v3.0.0-alpha released
[ ] Migration guide for users (v2 → v3)
[ ] Ontology documentation & query examples
[ ] Change detection & incremental regen tutorial
```

---

## Part 6: Success Metrics

### 6.1 Quantitative Metrics

| Metric | v2 | v3 Target | Impact |
|--------|----|-----------:|--------|
| **CLI Command LOC** | ~4,000 | <500 (hand-written + generated) | 87% reduction |
| **Test Pattern Repetition** | ~8,000 | <800 (template) | 90% reduction |
| **Time to Add New Command** | 4-6 hours | 15-20 minutes | 15x faster |
| **Ontology Completeness** | N/A | >95% system coverage | Provable |
| **Deterministic Regeneration** | N/A | 100% byte-identical | Canonicalization |
| **Code Review Burden** | High | Medium (review ontology + templates) | Shifted focus |

### 6.2 Qualitative Metrics

- **Credibility**: ggen is self-hosting; "eat your own dogfood" achieved
- **Clarity**: System structure explicit (queryable, versionable)
- **Evolution**: Features = ontology edits, not code changes
- **Maintenance**: Breaking changes = ontology migration (structured, trackable)
- **Community**: Contributions = ontology improvements, template enhancements

---

## Part 7: Risk Mitigation

### 7.1 Major Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| **Ontology Design Wrong** | Medium | Critical | Week 1-4 design review with community; iterate ontology |
| **Generator Can't Handle Edge Cases** | Medium | High | Start with 80% of codebase; hand-write remaining 20% |
| **Performance Regression** | Low | Medium | Benchmark generated vs hand-written; optimize hot paths |
| **Migration Path Unclear** | Low | Medium | Extensive documentation; provide migration tools |
| **Maintenance Burden Increases** | Low | Medium | Tooling to manage ontology changes; automated validation |

### 7.2 Contingency Plans

**If Ontology Design Stalls** (Week 4+):
- Pause full-system generation
- Generate just CLI commands + marketplace validation (highest ROI)
- Ship v2.9 with partial generation enabled
- Iterate ontology for v3.1

**If Generator Can't Reach 95% Coverage**:
- Define "core" vs "extended" codebase
- Generate 80% deterministically
- Keep remaining 20% hand-written with clear interfaces
- Document boundary between generated/manual

---

## Part 8: Architectural Diagrams

See: [GGEN_V3_ARCHITECTURE.md](./GGEN_V3_ARCHITECTURE.md) for C4 diagrams

---

## Conclusion

ggen v3 is **not just a rewrite**; it's a paradigm shift:
- ggen becomes its own primary use case
- Self-hosting proves the projection model works
- Ontology becomes the source code; code becomes the artifact
- Users see: "ggen built itself with ggen; why can't you?"

**The ultimate dogfood: ggen v3 is generated from ggen.**

---

**Document Version**: 1.0
**Created**: 2025-11-17
**Branch**: `claude/plan-ggen-v3-rewrite-01PyJAjvvvwdVWwD6wickodF`
**Next Step**: Phase 1 - Ontology Design Sprint
