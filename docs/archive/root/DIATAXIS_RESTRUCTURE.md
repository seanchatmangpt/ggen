<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Diataxis Documentation Restructure (Documentation Only)](#diataxis-documentation-restructure-documentation-only)
  - [Current State Analysis](#current-state-analysis)
    - [What ggen docs/ Has Now](#what-ggen-docs-has-now)
    - [What astro docs/ Has](#what-astro-docs-has)
  - [Proposed Additions (80/20 - Documentation Only)](#proposed-additions-8020---documentation-only)
    - [1. Add Root-Level Summary Docs (HIGH VALUE)](#1-add-root-level-summary-docs-high-value)
    - [2. Add thesis/ Directory (HIGH VALUE)](#2-add-thesis-directory-high-value)
    - [3. Keep ALL Existing Structure](#3-keep-all-existing-structure)
  - [Root-Level Documentation Templates](#root-level-documentation-templates)
    - [docs/ARCHITECTURE.md](#docsarchitecturemd)
    - [docs/BEST_PRACTICES.md](#docsbest_practicesmd)
    - [docs/PRD.md](#docsprdmd)
    - [docs/AUTOMATION.md](#docsautomationmd)
  - [CI/CD Pipeline](#cicd-pipeline)
    - [docs/PERFORMANCE.md](#docsperformancemd)
  - [Thesis Directory Content](#thesis-directory-content)
    - [docs/thesis/ontology-driven-development.md](#docsthesisontology-driven-developmentmd)
    - [docs/thesis/deterministic-generation.md](#docsthesisdeterministic-generationmd)
    - [docs/thesis/rdf-as-universal-schema.md](#docsthesisrdf-as-universal-schemamd)
    - [docs/thesis/ai-assisted-codegen.md](#docsthesisai-assisted-codegenmd)
  - [Implementation Plan (Documentation Only)](#implementation-plan-documentation-only)
    - [Phase 1: Root-Level Docs (Week 1)](#phase-1-root-level-docs-week-1)
    - [Phase 2: Thesis Directory (Week 1)](#phase-2-thesis-directory-week-1)
    - [Phase 3: Content Population (Week 2)](#phase-3-content-population-week-2)
    - [Phase 4: Validation (Week 2)](#phase-4-validation-week-2)
  - [Summary of Changes (Documentation Only)](#summary-of-changes-documentation-only)
    - [ADDITIONS](#additions)
    - [NO CHANGES](#no-changes)
  - [Benefits](#benefits)
    - [For New Users](#for-new-users)
    - [For Contributors](#for-contributors)
    - [For Maintainers](#for-maintainers)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Diataxis Documentation Restructure (Documentation Only)

**Scope**: docs/ directory restructure ONLY
**No Changes To**: Rust code, crates, Cargo.toml, scripts
**Reference**: ~/dis/astro/docs/ structure
**Goal**: Enrich ggen documentation with astro's proven patterns

---

## Current State Analysis

### What ggen docs/ Has Now
```
docs/
├── tutorials/
├── how-to/
├── reference/
├── explanations/
├── diataxis/              # Duplicate structure
│   ├── tutorials/
│   ├── how-to/
│   ├── reference/
│   └── explanations/
├── book/                  # mdBook format
├── getting-started/
├── examples/
│   └── diataxis-case-study/
├── contributing/
├── architecture/
├── analysis/
└── [many other dirs]
```

### What astro docs/ Has
```
docs/
├── tutorials/              # Diataxis tutorials
├── how-to/                # Diataxis how-to
├── reference/             # Diataxis reference
├── explanation/           # Diataxis explanations
├── thesis/                # Research-level docs ← KEY ADDITION
├── diataxis/              # Meta documentation about Diataxis
├── ARCHITECTURE.md        # Root-level summary ← KEY ADDITION
├── AUTOMATION.md          # Root-level summary
├── BEST_PRACTICES.md      # Root-level summary
├── PRD.md                 # Product requirements
├── [other summary docs]
└── [other dirs]
```

---

## Proposed Additions (80/20 - Documentation Only)

### 1. Add Root-Level Summary Docs (HIGH VALUE)

```
docs/
├── ARCHITECTURE.md          # NEW - System architecture overview
├── BEST_PRACTICES.md        # NEW - Development best practices
├── AUTOMATION.md            # NEW - CI/CD and automation guide
├── PRD.md                   # NEW - Product requirements
├── TESTING_STRATEGY.md      # NEW - Testing philosophy
└── PERFORMANCE.md           # NEW - Benchmarks and SLOs
```

**Purpose**: Quick executive summaries without diving into subdirectories

### 2. Add thesis/ Directory (HIGH VALUE)

```
docs/thesis/
├── ontology-driven-development.md    # Why RDF + ontologies
├── deterministic-generation.md       # Reproducibility guarantees
├── rdf-as-universal-schema.md        # RDF benefits over JSON/YAML
├── ai-assisted-codegen.md            # AI integration philosophy
└── template-driven-architecture.md   # Tera + RDF integration
```

**Purpose**: Research-level documentation explaining design philosophy

### 3. Keep ALL Existing Structure

```
docs/
├── tutorials/               # KEEP - Main Diataxis tutorials
├── how-to/                  # KEEP - Main Diataxis how-to
├── reference/               # KEEP - Main Diataxis reference
├── explanations/            # KEEP - Main Diataxis explanations
├── diataxis/                # KEEP - Legacy/alternate navigation
├── book/                    # KEEP - mdBook version
├── getting-started/         # KEEP - Quick onboarding
├── examples/                # KEEP - Working examples
├── contributing/            # KEEP - Contributor guides
├── architecture/            # KEEP - Detailed architecture docs
├── analysis/                # KEEP - Historical analysis
└── [all other existing]     # KEEP EVERYTHING
```

---

## Root-Level Documentation Templates

### docs/ARCHITECTURE.md
```markdown
# ggen System Architecture

**Quick Reference**: 2-minute architecture overview

## System Diagram
```
[High-level component diagram]
```

## Core Components
1. **RDF/SPARQL Engine** (Oxigraph) - Knowledge graph processing
2. **Template System** (Tera) - Code generation
3. **AI Integration** (genai) - Multi-provider AI support
4. **CLI Framework** (clap-noun-verb) - Command auto-discovery
5. **Marketplace** - Package management

## Data Flow
RDF Ontology → SPARQL Query → Template Variables → Generated Code

## Key Design Decisions
- Why RDF over JSON: [Link to thesis/rdf-as-universal-schema.md]
- Why Tera: [Link to explanations/]
- Why clap-noun-verb: [Link to architecture/]

See detailed architecture docs in: `docs/architecture/`
```

### docs/BEST_PRACTICES.md
```markdown
# ggen Development Best Practices

**Quick Reference**: Coding standards and conventions

## Code Quality
- **Testing**: Chicago TDD (state-based testing)
- **Error Handling**: No unwrap/expect in production
- **Documentation**: Test-driven docs (all examples validated)

## Documentation Standards
- **Framework**: Diataxis (4 quadrants)
- **Code Examples**: JavaScript + JSDoc + Zod (NOT TypeScript)
- **Validation**: All code examples must pass validation

## Performance
- **Build**: `cargo make` (never direct `cargo`)
- **SLOs**: build <5s, check <2s
- **Benchmarks**: See `docs/PERFORMANCE.md`

See detailed practices in: `docs/contributing/`
```

### docs/PRD.md
```markdown
# ggen Product Requirements Document

**Vision**: Deterministic, ontology-driven code generation

## Target Users
1. Backend developers (Rust, Python, JavaScript)
2. Data engineers (RDF/SPARQL knowledge)
3. AI engineers (LLM-powered generation)

## Core Features (80/20)
1. RDF/SPARQL processing
2. Template-based code generation
3. Multi-language support
4. AI-assisted generation
5. Package marketplace

## Success Metrics
- 1,168+ passing tests
- Production-ready (98/100 score)
- Sub-5s build times

See detailed requirements in: `docs/getting-started/`
```

### docs/AUTOMATION.md
```markdown
# Automation & CI/CD

**Quick Reference**: Validation and automation

## Validation Commands
```bash
# Documentation validation
./scripts/validate-docs/validate-all.sh

# Full validation
./scripts/run-validation-suite.sh

# Pre-commit validation
cargo make pre-commit
```

## CI/CD Pipeline
- Pre-commit: Format + lint check
- Pre-push: Full test suite
- CI: GitHub Actions (lint, test, docs)

See detailed automation in: `docs/contributing/VALIDATION.md`
```

### docs/TESTING_STRATEGY.md
```markdown
# Testing Strategy

**Philosophy**: Chicago School TDD

## Test Categories
1. **Unit Tests** (80% coverage on critical paths)
2. **Integration Tests** (API + database)
3. **Documentation Tests** (All examples validated)
4. **Performance Tests** (SLO verification)

## Coverage Targets
- Core: 80%+
- CLI: 60%+
- Utils: 40%+

See detailed strategy in: `docs/contributing/TESTING.md`
```

### docs/PERFORMANCE.md
```markdown
# Performance & Benchmarks

**SLOs**: Service Level Objectives

## Build Performance
- First build: ≤15s
- Incremental: ≤2s
- Check: ≤2s

## Runtime Performance
- RDF processing: ≤5s for 1k+ triples
- Template rendering: <1ms per template
- CLI startup: ≤50ms

## Current Metrics
- Build: 0.79s (84% under 5s SLO) ✅
- Tests: 1,168 passing ✅

See detailed benchmarks in: `docs/benchmark-results/`
```

---

## Thesis Directory Content

### docs/thesis/ontology-driven-development.md
```markdown
# Thesis: Ontology-Driven Development

## Abstract
Software development can be modeled as projecting knowledge graphs
into executable code across languages.

## The Problem
Traditional code generation:
- Tightly coupled to single language
- No semantic validation
- Brittle to schema changes

## The Solution: RDF + Ontologies
1. **Universal Schema**: RDF represents domain knowledge
2. **Semantic Validation**: SPARQL ensures consistency
3. **Language Agnostic**: Project to any target language

## Evidence
- 1,168+ tests demonstrate reliability
- Multi-language support (Rust, Python, JavaScript)
- Deterministic outputs guarantee reproducibility

## Related Work
- Semantic Web (W3C)
- Model-Driven Architecture (OMG)
- Domain-Specific Languages

See implementation: `docs/explanations/fundamentals/ontology-driven-development.md`
```

### docs/thesis/deterministic-generation.md
```markdown
# Thesis: Deterministic Code Generation

## Abstract
Code generation must be reproducible: same inputs → same outputs.

## The Problem
Non-deterministic generation:
- CI/CD failures from random outputs
- Difficult to debug
- Breaks git diffs

## The Solution: Deterministic Transforms
1. **Pure Functions**: No side effects in templates
2. **Explicit Dependencies**: All inputs declared
3. **Idempotent Operations**: Re-running produces identical results

## Implementation
- Tera templates (deterministic by design)
- RDF graph (immutable)
- SPARQL queries (reproducible)

See implementation: `docs/reference/`
```

### docs/thesis/rdf-as-universal-schema.md
```markdown
# Thesis: RDF as Universal Schema Language

## Abstract
RDF provides better schema foundation than JSON/YAML/XML.

## Advantages
1. **Semantic Validation**: SPARQL enforces constraints
2. **Linked Data**: References resolve globally
3. **Extensibility**: No breaking changes on schema evolution
4. **Reasoning**: Infer new facts from existing

## Comparison

| Feature | JSON Schema | XML Schema | RDF/OWL |
|---------|-------------|------------|---------|
| Semantic | ❌ | ❌ | ✅ |
| Validation | ✅ | ✅ | ✅ |
| Reasoning | ❌ | ❌ | ✅ |
| Linked Data | ❌ | ❌ | ✅ |

See implementation: `docs/explanations/fundamentals/rdf-for-programmers.md`
```

### docs/thesis/ai-assisted-codegen.md
```markdown
# Thesis: AI-Assisted Code Generation

## Abstract
LLMs enhance code generation when combined with formal schemas.

## The Hybrid Approach
1. **RDF defines structure** (deterministic)
2. **AI fills details** (creative)
3. **Templates ensure correctness** (validated)

## Benefits
- Best of both worlds: structure + creativity
- Validate AI outputs against schema
- Graceful degradation (works without AI)

See implementation: `docs/how-to/generation/`
```

---

## Implementation Plan (Documentation Only)

### Phase 1: Root-Level Docs (Week 1)
```bash
# Create 6 root-level summary docs
cd /Users/sac/ggen/docs
touch ARCHITECTURE.md BEST_PRACTICES.md AUTOMATION.md PRD.md TESTING_STRATEGY.md PERFORMANCE.md
```

### Phase 2: Thesis Directory (Week 1)
```bash
# Create thesis directory with 4-5 research docs
mkdir -p thesis
cd thesis
touch ontology-driven-development.md deterministic-generation.md rdf-as-universal-schema.md ai-assisted-codegen.md
```

### Phase 3: Content Population (Week 2)
- Populate each root doc with 200-400 words (quick reference)
- Populate each thesis doc with 800-1200 words (research depth)
- Link to existing detailed docs

### Phase 4: Validation (Week 2)
- Update link checker for new docs
- Verify all cross-references
- Run full validation suite

---

## Summary of Changes (Documentation Only)

### ADDITIONS
- ✅ 6 root-level summary docs
- ✅ thesis/ directory with 4-5 research docs
- ✅ Better navigation from root

### NO CHANGES
- ❌ No Rust code changes
- ❌ No Cargo.toml changes
- ❌ No crates/ changes
- ❌ No scripts/ changes (except docs validation)
- ❌ Keep ALL existing docs/ structure

---

## Benefits

### For New Users
- ✅ Quick overview via root-level docs (2-5 min read)
- ✅ Deep understanding via thesis docs (research level)
- ✅ All existing tutorials/how-tos remain unchanged

### For Contributors
- ✅ Design philosophy explained in thesis/
- ✅ Best practices at docs/ root
- ✅ No confusion about Rust workspace (not changing)

### For Maintainers
- ✅ Easy to point people to root docs
- ✅ Thesis docs explain "why" behind decisions
- ✅ Zero risk (purely additive)

---

**Next Step**: Create Phase 1 content (6 root docs) - ready to proceed?
