<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Documentation Restructure Proposal V2 (80/20 Enrichment)](#ggen-documentation-restructure-proposal-v2-8020-enrichment)
  - [Philosophy Change: ADD Don't Delete](#philosophy-change-add-dont-delete)
  - [Proposed Additions to Current Structure](#proposed-additions-to-current-structure)
    - [1. Add Specs Directory (NEW - Highest Priority)](#1-add-specs-directory-new---highest-priority)
    - [2. Add Root-Level Summary Docs (NEW)](#2-add-root-level-summary-docs-new)
    - [3. Add Thesis Directory (NEW - 80/20 Research)](#3-add-thesis-directory-new---8020-research)
    - [4. Add Infrastructure Directory (NEW)](#4-add-infrastructure-directory-new)
    - [5. Enhance Existing Docs Structure (KEEP + ADD)](#5-enhance-existing-docs-structure-keep--add)
  - [Package.json Enhancements (Inspired by Astro)](#packagejson-enhancements-inspired-by-astro)
  - [Root-Level Documentation (80/20 Critical Docs)](#root-level-documentation-8020-critical-docs)
    - [ARCHITECTURE.md](#architecturemd)
    - [AUTOMATION.md](#automationmd)
    - [BEST_PRACTICES.md](#best_practicesmd)
    - [PRD.md](#prdmd)
  - [Specs Pattern (Complete from Astro)](#specs-pattern-complete-from-astro)
    - [Standard Spec Structure](#standard-spec-structure)
  - [Immediate Actions (80/20)](#immediate-actions-8020)
    - [Phase 1: Add Specs (Highest ROI)](#phase-1-add-specs-highest-roi)
    - [Phase 2: Add Root Docs](#phase-2-add-root-docs)
    - [Phase 3: Add Thesis](#phase-3-add-thesis)
    - [Phase 4: Add Infrastructure](#phase-4-add-infrastructure)
    - [Phase 5: Add package.json](#phase-5-add-packagejson)
  - [Benefits of Enrichment Approach](#benefits-of-enrichment-approach)
    - [For New Contributors](#for-new-contributors)
    - [For Maintainers](#for-maintainers)
    - [For Users](#for-users)
  - [What We Keep (Everything)](#what-we-keep-everything)
  - [What We Add (80/20 High Value)](#what-we-add-8020-high-value)
  - [Implementation Priority (80/20)](#implementation-priority-8020)
    - [Must Have (Week 1)](#must-have-week-1)
    - [Should Have (Week 2)](#should-have-week-2)
    - [Nice to Have (Week 3)](#nice-to-have-week-3)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Documentation Restructure Proposal V2 (80/20 Enrichment)

**Aligned with**: ~/dis/astro reference structure (80/20 comprehensive)
**Status**: Proposal - ADDITIVE not destructive
**Goal**: Enrich existing structure with proven patterns from astro

---

## Philosophy Change: ADD Don't Delete

**Previous approach**: ❌ Consolidate and simplify
**New approach**: ✅ Enrich with 80/20 most valuable patterns

**Key insight from astro**:
- 60 docs in `docs/` (comprehensive documentation)
- 31 specs in `specs/` (feature specifications)
- Root-level summary docs (ARCHITECTURE.md, AUTOMATION.md, etc.)
- Multiple documentation layers serving different purposes

---

## Proposed Additions to Current Structure

### 1. Add Specs Directory (NEW - Highest Priority)

```
specs/
├── 001-rdf-sparql-engine/
│   ├── spec.md              # Full specification
│   ├── plan.md              # Implementation plan
│   ├── research.md          # Research & alternatives
│   ├── data-model.md        # Data structures
│   ├── tasks.md             # Task breakdown
│   ├── quickstart.md        # Quick reference
│   └── checklists/
│       └── requirements.md  # Validation checklist
│
├── 002-template-system/
│   ├── spec.md
│   ├── plan.md
│   ├── data-model.md
│   └── quickstart.md
│
├── 003-ai-integration/
│   ├── spec.md
│   ├── plan.md
│   ├── research.md          # Provider comparison
│   └── quickstart.md
│
├── 004-marketplace-v2/
│   ├── spec.md
│   ├── plan.md
│   ├── tasks.md
│   └── checklists/
│       └── requirements.md
│
├── 005-cli-framework/
│   ├── spec.md
│   ├── plan.md
│   └── research.md          # clap-noun-verb design
│
└── 006-lifecycle-hooks/
    ├── spec.md
    ├── plan.md
    └── data-model.md
```

**Spec Structure (80/20 - Include What Matters)**:
- ✅ spec.md (REQUIRED) - Full specification
- ✅ plan.md (REQUIRED) - Implementation plan
- ✅ data-model.md (for complex features) - Data structures
- ✅ research.md (for design-heavy features) - Alternatives considered
- ✅ tasks.md (for large features) - Task breakdown
- ✅ quickstart.md (for user-facing features) - Quick reference
- ✅ checklists/ (for critical features) - Validation requirements

### 2. Add Root-Level Summary Docs (NEW)

```
docs/
├── ARCHITECTURE.md          # System architecture overview
├── AUTOMATION.md            # Automation & CI/CD
├── BEST_PRACTICES.md        # Development best practices
├── PRD.md                   # Product requirements document
├── MIGRATION_GUIDE.md       # Version migration guides
├── TESTING_STRATEGY.md      # Testing philosophy & coverage
└── PERFORMANCE.md           # Performance benchmarks & SLOs
```

**Purpose**: Quick executive summaries at docs root, detailed content in subdirectories

### 3. Add Thesis Directory (NEW - 80/20 Research)

```
docs/thesis/
├── ontology-driven-development.md    # Core thesis
├── deterministic-generation.md       # Reproducibility thesis
├── rdf-as-universal-schema.md        # RDF benefits thesis
└── ai-assisted-codegen.md            # AI integration thesis
```

**Purpose**: Research-level documentation of core concepts and design philosophy

### 4. Add Infrastructure Directory (NEW)

```
infrastructure/
├── docker/
│   ├── Dockerfile
│   └── docker-compose.yml
├── scripts/
│   ├── dev-setup.sh         # Development environment setup
│   ├── clean.sh             # Clean build artifacts
│   ├── validate-all.sh      # Master validation script
│   └── benchmark.sh         # Performance benchmarking
└── ci/
    └── .github/
        └── workflows/
            ├── ci.yml
            ├── validate-docs.yml
            └── benchmark.yml
```

**Purpose**: Infrastructure automation and deployment scripts

### 5. Enhance Existing Docs Structure (KEEP + ADD)

```
docs/
├── tutorials/               # EXISTING - Keep all
│   ├── 01-quick-start.md
│   ├── 02-first-template.md
│   └── 03-rdf-basics.md
│
├── how-to/                  # EXISTING - Keep all
│   ├── configuration/
│   ├── generation/
│   └── deployment/
│
├── reference/               # EXISTING - Keep all
│   ├── cli/
│   ├── configuration/
│   └── api/
│
├── explanations/            # EXISTING - Keep all
│   ├── fundamentals/
│   └── architecture/
│
├── contributing/            # EXISTING - Keep all
│   ├── GETTING_STARTED.md
│   ├── VALIDATION.md
│   └── TESTING.md
│
├── examples/                # EXISTING - Keep all
│   └── diataxis-case-study/
│
├── diataxis/                # EXISTING - Keep (legacy navigation)
│   ├── tutorials/
│   ├── how-to/
│   ├── reference/
│   └── explanations/
│
├── book/                    # EXISTING - Keep (mdBook format)
│   └── ...
│
├── getting-started/         # EXISTING - Keep (quick onboarding)
│   └── ...
│
├── architecture/            # EXISTING - Keep (detailed specs)
│   └── ...
│
├── analysis/                # EXISTING - Keep (historical analysis)
│   └── ...
│
└── thesis/                  # NEW - Add research docs
    └── ...
```

**Key Point**: KEEP existing structure, ADD new capabilities

---

## Package.json Enhancements (Inspired by Astro)

```json
{
  "name": "ggen-workspace",
  "version": "4.0.0",
  "private": true,
  "scripts": {
    "dev": "cargo run --package ggen-cli-lib --bin ggen --",
    "build": "cargo make build",
    "test": "cargo make test",
    "test:unit": "cargo make test-unit",
    "test:integration": "cargo make test-integration",
    "test:all": "cargo make test",
    "test:coverage": "cargo make coverage",
    "lint": "cargo make lint",
    "lint:fix": "cargo make lint-fix",
    "format": "cargo make format",
    "format:check": "cargo make format-check",
    "typecheck": "cargo make check",
    "validate": "cargo make pre-commit",
    "validate:all": "./scripts/run-validation-suite.sh",
    "validate:docs": "./scripts/validate-docs/validate-all.sh",
    "setup": "./infrastructure/scripts/dev-setup.sh",
    "clean": "./infrastructure/scripts/clean.sh",
    "benchmark": "./infrastructure/scripts/benchmark.sh",
    "docs:serve": "mdbook serve docs/book",
    "docs:build": "mdbook build docs/book",
    "docs:validate": "./scripts/validate-docs/validate-all.sh"
  },
  "engines": {
    "rust": ">=1.74.0",
    "cargo": ">=1.74.0"
  }
}
```

**Purpose**: Unified command interface across Rust and Node ecosystems

---

## Root-Level Documentation (80/20 Critical Docs)

### ARCHITECTURE.md
```markdown
# ggen Architecture

**System**: RDF-based code generation toolkit
**Core**: Oxigraph (RDF) + Tera (Templates) + genai (AI)

## Component Diagram
[Include high-level architecture diagram]

## Key Subsystems
1. RDF/SPARQL Engine (Oxigraph)
2. Template System (Tera)
3. AI Integration (genai)
4. CLI Framework (clap-noun-verb)
5. Marketplace (Package management)

## Data Flow
[Describe: RDF → SPARQL → Template → Code]
```

### AUTOMATION.md
```markdown
# Automation & CI/CD

## Validation Pipeline
- Pre-commit: cargo make pre-commit
- CI: GitHub Actions (lint, test, build)
- Documentation: validate-docs pipeline

## Scripts
- `./infrastructure/scripts/dev-setup.sh`
- `./infrastructure/scripts/validate-all.sh`
- `./infrastructure/scripts/benchmark.sh`

## Hooks
- Pre-commit: Format + lint check
- Pre-push: Full test suite
```

### BEST_PRACTICES.md
```markdown
# Development Best Practices

## Code Quality
- Chicago TDD (state-based testing)
- No unwrap/expect in production code
- Comprehensive error handling with Result<T,E>

## Documentation
- Test-driven documentation (all examples validated)
- Diataxis framework (4 quadrants)
- JavaScript + JSDoc + Zod (NOT TypeScript)

## Performance
- Cargo make for all operations (never direct cargo)
- Andon signals (stop on errors)
- SLO targets (build <5s, check <2s)
```

### PRD.md
```markdown
# Product Requirements Document

## Vision
Deterministic, ontology-driven code generation across languages

## Target Users
1. Backend developers (Rust, Python, JavaScript)
2. Data engineers (RDF/SPARQL knowledge graphs)
3. AI engineers (LLM-powered generation)

## Core Features
[List 20% of features that deliver 80% of value]
```

---

## Specs Pattern (Complete from Astro)

### Standard Spec Structure
```
specs/NNN-feature-name/
├── spec.md              # REQUIRED: Full specification
│   ├── Executive Summary
│   ├── User Scenarios & Testing
│   ├── Technical Design
│   ├── API Surface
│   ├── Data Model
│   ├── Implementation Plan
│   ├── Testing Strategy
│   └── Success Metrics
│
├── plan.md              # REQUIRED: Implementation plan
│   ├── Phase breakdown
│   ├── Task list
│   ├── Dependencies
│   └── Timeline
│
├── research.md          # OPTIONAL: Research & alternatives
│   ├── Options considered
│   ├── Trade-offs
│   └── Decision rationale
│
├── data-model.md        # OPTIONAL: Data structures
│   ├── Rust structs
│   ├── RDF ontology
│   └── Database schema
│
├── tasks.md             # OPTIONAL: Detailed tasks
│   ├── Task breakdown
│   ├── Acceptance criteria
│   └── Progress tracking
│
├── quickstart.md        # OPTIONAL: Quick reference
│   └── Usage examples
│
├── checklists/
│   └── requirements.md  # Validation checklist
│
└── contracts/           # OPTIONAL: API contracts
    └── api-contract.md
```

---

## Immediate Actions (80/20)

### Phase 1: Add Specs (Highest ROI)
```bash
# Create specs for 6 core features
mkdir -p specs/{001-rdf-sparql-engine,002-template-system,003-ai-integration,004-marketplace-v2,005-cli-framework,006-lifecycle-hooks}

# Populate each with spec.md + plan.md minimum
```

### Phase 2: Add Root Docs
```bash
# Create executive summary docs
touch docs/{ARCHITECTURE.md,AUTOMATION.md,BEST_PRACTICES.md,PRD.md,TESTING_STRATEGY.md,PERFORMANCE.md}
```

### Phase 3: Add Thesis
```bash
# Create research docs
mkdir -p docs/thesis
touch docs/thesis/{ontology-driven-development.md,deterministic-generation.md,rdf-as-universal-schema.md}
```

### Phase 4: Add Infrastructure
```bash
# Move/create infrastructure scripts
mkdir -p infrastructure/{docker,scripts,ci}
# Move scripts/validate-docs/ → infrastructure/scripts/
```

### Phase 5: Add package.json
```bash
# Create unified script interface
touch package.json
```

---

## Benefits of Enrichment Approach

### For New Contributors
- ✅ **specs/** gives clear feature documentation
- ✅ **ARCHITECTURE.md** provides system overview
- ✅ **BEST_PRACTICES.md** sets coding standards
- ✅ **package.json** provides familiar commands

### For Maintainers
- ✅ **specs/** tracks feature evolution
- ✅ **thesis/** documents design decisions
- ✅ **Root docs** provide quick reference
- ✅ **infrastructure/** centralizes automation

### For Users
- ✅ **docs/** remains comprehensive
- ✅ **quickstart.md** in each spec
- ✅ **PRD.md** shows product vision
- ✅ **examples/** provides working code

---

## What We Keep (Everything)

- ✅ docs/tutorials/ (all existing)
- ✅ docs/how-to/ (all existing)
- ✅ docs/reference/ (all existing)
- ✅ docs/explanations/ (all existing)
- ✅ docs/diataxis/ (legacy navigation)
- ✅ docs/book/ (mdBook format)
- ✅ docs/getting-started/ (quick onboarding)
- ✅ docs/architecture/ (detailed specs)
- ✅ docs/analysis/ (historical data)
- ✅ docs/contributing/ (contributor guides)
- ✅ docs/examples/ (working examples)

---

## What We Add (80/20 High Value)

- 🆕 specs/ (6 core feature specs)
- 🆕 docs/ARCHITECTURE.md
- 🆕 docs/AUTOMATION.md
- 🆕 docs/BEST_PRACTICES.md
- 🆕 docs/PRD.md
- 🆕 docs/TESTING_STRATEGY.md
- 🆕 docs/PERFORMANCE.md
- 🆕 docs/thesis/ (research docs)
- 🆕 infrastructure/scripts/ (automation)
- 🆕 package.json (unified commands)

---

## Implementation Priority (80/20)

### Must Have (Week 1)
1. ✅ Create `specs/` with 6 core feature specs
2. ✅ Create `docs/ARCHITECTURE.md` (system overview)
3. ✅ Create `docs/BEST_PRACTICES.md` (coding standards)
4. ✅ Create `package.json` (unified commands)

### Should Have (Week 2)
5. ✅ Create `docs/thesis/` (4 research docs)
6. ✅ Create `docs/AUTOMATION.md`
7. ✅ Create `docs/PRD.md`
8. ✅ Move scripts to `infrastructure/`

### Nice to Have (Week 3)
9. ✅ Create `docs/TESTING_STRATEGY.md`
10. ✅ Create `docs/PERFORMANCE.md`
11. ✅ Add contracts/ to specs
12. ✅ Enhance CI workflows

---

## Next Steps

1. **Review this enrichment approach**
2. **Approve Phase 1 (specs + root docs)**
3. **I'll create initial content for 6 specs**
4. **Add ARCHITECTURE.md and BEST_PRACTICES.md**
5. **Add package.json with unified commands**

**Timeline**: ~4 hours to create all Phase 1 content
**Risk**: None (purely additive, no deletions)
