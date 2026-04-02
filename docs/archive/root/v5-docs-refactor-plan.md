<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v5 Documentation Refactor Plan](#ggen-v5-documentation-refactor-plan)
  - [Executive Summary](#executive-summary)
    - [Current State (Technical Debt)](#current-state-technical-debt)
    - [Target State (v5 Aligned)](#target-state-v5-aligned)
    - [Strategic Alignment with v5](#strategic-alignment-with-v5)
  - [Phase 1: Immediate Cleanup (Week 1) - HIGH IMPACT, LOW EFFORT](#phase-1-immediate-cleanup-week-1---high-impact-low-effort)
    - [1.1 Archive v3 Planning Docs (REMOVE 300KB)](#11-archive-v3-planning-docs-remove-300kb)
    - [1.2 Archive Temporal Documentation (REMOVE 20+ FILES)](#12-archive-temporal-documentation-remove-20-files)
    - [1.3 Remove Marketplace v2 Docs (CUT FEATURE)](#13-remove-marketplace-v2-docs-cut-feature)
    - [1.4 Remove Agent/Workflow Docs (CUT FEATURES)](#14-remove-agentworkflow-docs-cut-features)
    - [1.5 Consolidate Diagram Directories](#15-consolidate-diagram-directories)
    - [1.6 Initial Validation](#16-initial-validation)
  - [Phase 2: Structural Consolidation (Week 2-3) - MEDIUM IMPACT, MEDIUM EFFORT](#phase-2-structural-consolidation-week-2-3---medium-impact-medium-effort)
    - [2.1 Choose Canonical Diataxis Structure](#21-choose-canonical-diataxis-structure)
    - [2.2 Merge Duplicate Diataxis Structures](#22-merge-duplicate-diataxis-structures)
    - [2.3 Reorganize Root Directory Files](#23-reorganize-root-directory-files)
    - [2.4 Standardize Naming Conventions](#24-standardize-naming-conventions)
    - [2.5 Create Missing Core Directories](#25-create-missing-core-directories)
  - [Phase 3: Content Alignment with v5 (Week 4-6) - HIGH IMPACT, HIGH EFFORT](#phase-3-content-alignment-with-v5-week-4-6---high-impact-high-effort)
    - [3.1 Rewrite README.md (Single Entry Point)](#31-rewrite-readmemd-single-entry-point)
  - [Why ggen?](#why-ggen)
  - [Documentation](#documentation)
  - [What's New in v5?](#whats-new-in-v5)
  - [Contributing](#contributing)
  - [License](#license)
    - [4.2 Link Validation & Repair](#42-link-validation--repair)
    - [4.3 Create Architecture Decision Records (ADRs)](#43-create-architecture-decision-records-adrs)
    - [4.4 Documentation Standards Document](#44-documentation-standards-document)
    - [4.5 Spell Check & Grammar](#45-spell-check--grammar)
  - [Phase 5: Tooling & Automation (Week 9-10) - SUSTAINABILITY](#phase-5-tooling--automation-week-9-10---sustainability)
    - [5.1 Documentation Generation (mdBook)](#51-documentation-generation-mdbook)
    - [5.2 CI/CD Documentation Checks](#52-cicd-documentation-checks)
    - [5.3 Documentation Metrics Dashboard](#53-documentation-metrics-dashboard)
    - [5.4 Automated Diagram Generation](#54-automated-diagram-generation)
  - [Migration Strategy for Existing Users](#migration-strategy-for-existing-users)
    - [For Users Upgrading from v4](#for-users-upgrading-from-v4)
    - [For Documentation Contributors](#for-documentation-contributors)
  - [Success Metrics & Validation](#success-metrics--validation)
    - [Quantitative Metrics](#quantitative-metrics)
    - [Qualitative Metrics](#qualitative-metrics)
    - [User Testing](#user-testing)
  - [Implementation Roadmap](#implementation-roadmap)
    - [Week 1: Immediate Cleanup (Dec 23-27)](#week-1-immediate-cleanup-dec-23-27)
    - [Week 2-3: Structural Consolidation (Dec 30 - Jan 10)](#week-2-3-structural-consolidation-dec-30---jan-10)
    - [Week 4-6: Content Alignment (Jan 13 - Jan 31)](#week-4-6-content-alignment-jan-13---jan-31)
    - [Week 7-8: Quality & Completeness (Feb 3-14)](#week-7-8-quality--completeness-feb-3-14)
    - [Week 9-10: Tooling & Automation (Feb 17-28)](#week-9-10-tooling--automation-feb-17-28)
    - [Week 11: Final Review & Launch (Mar 3-7)](#week-11-final-review--launch-mar-3-7)
  - [Risk Mitigation](#risk-mitigation)
    - [Risk: "Users confused by missing v4 features"](#risk-users-confused-by-missing-v4-features)
    - [Risk: "Documentation still incomplete at launch"](#risk-documentation-still-incomplete-at-launch)
    - [Risk: "RDF/SPARQL too complex for users"](#risk-rdfsparql-too-complex-for-users)
    - [Risk: "Breaking link cascade from refactor"](#risk-breaking-link-cascade-from-refactor)
  - [Post-Launch: Continuous Improvement](#post-launch-continuous-improvement)
    - [Documentation Feedback Loop](#documentation-feedback-loop)
    - [Quarterly Reviews](#quarterly-reviews)
    - [Version Strategy](#version-strategy)
  - [Deliverables Checklist](#deliverables-checklist)
    - [Phase 1: Cleanup](#phase-1-cleanup)
    - [Phase 2: Consolidation](#phase-2-consolidation)
    - [Phase 3: Content Creation](#phase-3-content-creation)
    - [Phase 4: Quality](#phase-4-quality)
    - [Phase 5: Tooling](#phase-5-tooling)
    - [Final Launch](#final-launch)
  - [Appendix A: File Structure (Target State)](#appendix-a-file-structure-target-state)
  - [Appendix B: Content Inventory (What Stays, What Goes)](#appendix-b-content-inventory-what-stays-what-goes)
    - [KEEP (v5 Aligned)](#keep-v5-aligned)
    - [ARCHIVE (Historical Value)](#archive-historical-value)
    - [REMOVE (Cut Features)](#remove-cut-features)
  - [Document Status](#document-status)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v5 Documentation Refactor Plan

**Date**: 2025-12-18
**Version**: v5.0.0
**Alignment**: GGEN-V5-STRATEGY.md + GGEN-V5-DESIGN.md
**Philosophy**: Semantic precision, user-first, zero waste

---

## Executive Summary

This plan restructures ggen documentation to align with the v5 strategic vision: **"Generate deterministic, reproducible code from semantic domain models (RDF ontologies) with zero ambiguity."**

### Current State (Technical Debt)
- **517 files, ~96,000 lines** of markdown
- **167 files in root** (should be <20)
- **3-4x duplication** of Diataxis structure
- **300KB of unused v3 planning** docs (10 files, never implemented)
- **20+ temporal/week-based** files mixed with permanent docs
- **1234 TODO/FIXME markers** across 264 files
- **20 competing README files** (unclear entry point)

### Target State (v5 Aligned)
- **<150 files** focused on semantic code generation
- **Single canonical Diataxis structure**
- **Zero v3/v4 cruft** (archived or removed)
- **Clear user journey**: Install → Model → Generate → Deploy
- **100% complete docs** (zero TODO markers in published content)
- **Single README.md** as definitive entry point

### Strategic Alignment with v5

| v5 Core Principle | Documentation Impact |
|-------------------|---------------------|
| **RDF-First** | Tutorials focus on ontology modeling, not CLI commands |
| **Semantic Precision** | Reference docs for SPARQL patterns, RDF structure |
| **Deterministic Output** | Explain reproducibility guarantees, audit trails |
| **Ruthless Scope Reduction** | Remove docs for cut features (workflows, agents, marketplace, FMEA) |
| **Single Language (Rust)** | Remove polyglot generation docs (TypeScript, Python) |
| **Poka-Yoke Safety** | Document validation, constraints, error prevention |

---

## Phase 1: Immediate Cleanup (Week 1) - HIGH IMPACT, LOW EFFORT

**Goal**: Remove 70% of documentation debt without touching active content.

### 1.1 Archive v3 Planning Docs (REMOVE 300KB)

**Rationale**: v3 was abandoned planning (Nov 2025), never implemented. v5 is the actual direction.

```bash
mkdir -p docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_ARCHITECTURE_C4.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_PROJECTION_FAMILIES_DETAILED.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_ONTOLOGY_SPEC.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_IMPLEMENTATION_ROADMAP.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_TYPE_SYSTEM_COMPREHENSIVE.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_SECTOR_BUNDLES_CATALOG.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_SECURITY_THREAT_MODEL.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_SPARQL_PATTERNS_AND_OPTIMIZATION.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_VISION.md docs/archive/v3-planning-abandoned-2025-11/
mv docs/GGEN_V3_COMPREHENSIVE_INDEX.md docs/archive/v3-planning-abandoned-2025-11/

# Add README explaining why archived
cat > docs/archive/v3-planning-abandoned-2025-11/README.md << 'EOF'
# v3 Planning Docs (Archived - Never Implemented)

These documents represent comprehensive v3 planning from November 2025 that was never implemented.
ggen v5 represents the actual strategic direction (see GGEN-V5-STRATEGY.md).

**Status**: Historical planning, not reflective of current product.
**Archived**: 2025-12-18
**Total Size**: 300KB+ of planning documentation
EOF
```

**Impact**: -10 files, -300KB, clearer strategic direction

### 1.2 Archive Temporal Documentation (REMOVE 20+ FILES)

**Rationale**: Week-based and phase-based docs are ephemeral evidence, not permanent documentation.

```bash
mkdir -p docs/archive/temporal-evidence/
mv docs/WEEK*.md docs/archive/temporal-evidence/
mv docs/*WEEK*.md docs/archive/temporal-evidence/
mv docs/PHASE*.md docs/archive/temporal-evidence/
mv docs/remediation/WEEK*.md docs/archive/temporal-evidence/
mv docs/metrics/week* docs/archive/temporal-evidence/
mv docs/metrics/daily_reports/ docs/archive/temporal-evidence/

# Consolidate deliverables
mv docs/TEST_IMPROVEMENTS_WEEK1.md docs/archive/temporal-evidence/
mv docs/TESTER_PHASE2_DELIVERABLES.md docs/archive/temporal-evidence/
```

**Impact**: -20 files, clear separation of permanent vs temporary docs

### 1.3 Remove Marketplace v2 Docs (CUT FEATURE)

**Rationale**: v5 strategy explicitly cuts marketplace ("Use npm / crates.io / PyPI registries").

```bash
mkdir -p docs/archive/marketplace-v2-cut-feature/
mv docs/MARKETPLACE_V2_*.md docs/archive/marketplace-v2-cut-feature/
mv docs/marketplace_*.md docs/archive/marketplace-v2-cut-feature/
mv docs/architecture/marketplace-v2-migration/ docs/archive/marketplace-v2-cut-feature/
mv docs/marketplace/ docs/archive/marketplace-v2-cut-feature/

# Keep only ontology-packs (legitimate feature)
# Do NOT move docs/ontology-packs/ (this is RDF content distribution, not marketplace)
```

**Impact**: -47 files, aligns with v5 strategic scope

### 1.4 Remove Agent/Workflow Docs (CUT FEATURES)

**Rationale**: v5 cuts agent orchestration ("Use Claude API / LangChain / Anthropic SDK") and workflows ("Use GitHub Actions / Temporal").

```bash
mkdir -p docs/archive/cut-features-agents-workflows/
mv docs/agent/ docs/archive/cut-features-agents-workflows/
mv docs/swarm/ docs/archive/cut-features-agents-workflows/
mv docs/remediation/AGENT_COORDINATION.md docs/archive/cut-features-agents-workflows/
mv docs/remediation/SWARM_STATUS_REPORT.md docs/archive/cut-features-agents-workflows/
mv docs/HIVE_MIND_INTEGRATION_COMPLETE.md docs/archive/cut-features-agents-workflows/
```

**Impact**: -15 files, clear product focus

### 1.5 Consolidate Diagram Directories

```bash
# Move all diagrams to single canonical location
mv docs/diagrams/* docs/assets/diagrams/
mv docs/*.puml docs/assets/diagrams/
rmdir docs/diagrams/

# Organize by type
mkdir -p docs/assets/diagrams/{architecture,flow,sequence}
```

**Impact**: Single source of truth for visual assets

### 1.6 Initial Validation

**Before merge:**
```bash
# Ensure no broken links after moves
cargo make check-links  # If this exists, otherwise use linkcheck
grep -r "docs/GGEN_V3" . --include="*.md" | wc -l  # Should be 0
grep -r "docs/MARKETPLACE_V2" . --include="*.md" | wc -l  # Should be 0

# Update any references
find docs/ -name "*.md" -exec sed -i 's|docs/GGEN_V3|docs/archive/v3-planning-abandoned-2025-11|g' {} \;
```

**Deliverable**: `-92 files` from docs root, clear strategic alignment

---

## Phase 2: Structural Consolidation (Week 2-3) - MEDIUM IMPACT, MEDIUM EFFORT

**Goal**: Establish single canonical Diataxis structure aligned with v5 product.

### 2.1 Choose Canonical Diataxis Structure

**Decision**: Use root-level structure (simpler, clearer)

```
docs/
├── tutorials/           # CANONICAL - Learning-oriented
├── how-to-guides/       # CANONICAL - Problem-oriented
├── reference/           # CANONICAL - Information-oriented
├── explanations/        # CANONICAL - Understanding-oriented
```

**Rationale**:
- Shorter paths (docs/tutorials/ vs docs/diataxis/tutorials/)
- Aligns with industry standard (mdBook, Docusaurus, etc.)
- Existing `/ontology-packs/` uses this pattern successfully

### 2.2 Merge Duplicate Diataxis Structures

**Step 1: Audit Content Overlap**

```bash
# Compare content between duplicated locations
diff -r docs/tutorials/ docs/diataxis/tutorials/
diff -r docs/how-to-guides/ docs/diataxis/how-to/
diff -r docs/reference/ docs/diataxis/reference/
diff -r docs/explanations/ docs/diataxis/explanations/

# Also check src/ structure
diff -r docs/tutorials/ docs/src/tutorials/
```

**Step 2: Merge Strategy**

For each file in duplicate locations:
1. If identical → Delete duplicate
2. If different → Manual review, merge unique content
3. If TODO-heavy → Move to `/wip/` (work in progress)

**Step 3: Execute Merge**

```bash
# Merge diataxis/ content into root-level
rsync -av --ignore-existing docs/diataxis/tutorials/ docs/tutorials/
rsync -av --ignore-existing docs/diataxis/how-to/ docs/how-to-guides/
rsync -av --ignore-existing docs/diataxis/reference/ docs/reference/
rsync -av --ignore-existing docs/diataxis/explanations/ docs/explanations/

# Merge src/ content (alternate rendering source)
rsync -av --ignore-existing docs/src/tutorials/ docs/tutorials/
rsync -av --ignore-existing docs/src/how-to-guides/ docs/how-to-guides/
rsync -av --ignore-existing docs/src/reference/ docs/reference/
rsync -av --ignore-existing docs/src/explanations/ docs/explanations/

# Move conflicts to review directory
mkdir -p docs/wip/merge-conflicts/
# Manual resolution required

# Archive old structures
mv docs/diataxis/ docs/archive/diataxis-duplicate/
mv docs/src/ docs/archive/src-duplicate/
```

**Impact**: -66 files (eliminate 2/3 of duplication)

### 2.3 Reorganize Root Directory Files

**Current**: 167 files in root (chaos)
**Target**: <20 files in root (navigation)

**Root-Level Files to KEEP:**
```
docs/
├── README.md                    # Single entry point
├── ARCHITECTURE.md              # System overview (rename from GGEN_ARCHITECTURE_OVERVIEW.md)
├── CHANGELOG.md                 # Version history (if docs-specific)
└── CONTRIBUTING.md              # How to contribute to docs
```

**Root-Level Files to MOVE:**

```bash
# Development & Process
mkdir -p docs/development/
mv docs/CHICAGO_TDD_IMPLEMENTATION.md docs/development/
mv docs/DEVELOPMENT_WORKFLOW.md docs/development/
mv docs/SPARC_*.md docs/development/
mv docs/lean_quality/ docs/development/

# Architecture (consolidate)
mv docs/GGEN_ARCHITECTURE_OVERVIEW.md docs/ARCHITECTURE.md
mv docs/ggen-cli-architecture.md docs/architecture/cli-architecture.md
mv docs/architecture/packs/ docs/archive/packs-cut-feature/  # v5 cuts this
mv docs/architecture/rdf-control-plane/ docs/architecture/core/

# Testing & QA
mkdir -p docs/testing/
mv docs/ERROR_MESSAGE_QUALITY_TESTS.md docs/testing/
mv docs/EVOLUTION_OLLAMA_TEST_SUITE.md docs/testing/
mv docs/GRAPH_TESTS_VALIDATION.md docs/testing/
mv docs/TEST_*.md docs/testing/
mv docs/qa/ docs/testing/qa/

# Performance & Benchmarks
mv docs/PERFORMANCE_BENCHMARKING.md docs/performance/
mv docs/BENCHMARKS_*.md docs/performance/
mv docs/benchmarks/ docs/performance/

# Validation & Quality
mv docs/POKA_YOKE_*.md docs/validation/
mv docs/FMEA_*.md docs/validation/
mv docs/validation/COMBINED_CERTIFICATION.md docs/archive/  # v4-specific

# Examples
mv docs/examples/diataxis-case-study/ docs/archive/  # Not RDF examples

# Research & Papers
mkdir -p docs/research/
mv docs/papers/ docs/research/papers/
mv docs/UNIVERSITY_BUSINESS_MODEL.md docs/archive/  # Not core product

# Migration & Troubleshooting
mkdir -p docs/troubleshooting/
mv docs/troubleshooting/ docs/troubleshooting/  # Already exists
mv docs/innovation/TROUBLESHOOTING.md docs/troubleshooting/v4-to-v5.md

# Remediation (archive - historical)
mv docs/remediation/ docs/archive/remediation-historical/

# Innovation (archive - v4 planning)
mv docs/innovation/ docs/archive/innovation-v4/
```

**Impact**: Root directory: 167 files → 15 files

### 2.4 Standardize Naming Conventions

**Established Rules:**

1. **Directories**: `lowercase-with-dashes/`
2. **Regular Files**: `lowercase-with-dashes.md`
3. **Meta Files (Root Only)**: `UPPERCASE.md` (README, ARCHITECTURE, CHANGELOG, CONTRIBUTING)
4. **No Version Markers**: Remove `_v2`, `_V3`, `_CURRENT` suffixes

**Batch Rename Script:**

```bash
#!/bin/bash
# docs/scripts/standardize-names.sh

# Convert UPPERCASE_FILES.md to lowercase-files.md (except meta files)
find docs/ -name "*.md" -type f | while read file; do
    dir=$(dirname "$file")
    base=$(basename "$file")

    # Skip root meta files
    if [[ "$dir" == "docs" && "$base" =~ ^(README|ARCHITECTURE|CHANGELOG|CONTRIBUTING)\.md$ ]]; then
        continue
    fi

    # Convert to lowercase with dashes
    new_base=$(echo "$base" | tr '[:upper:]' '[:lower:]' | tr '_' '-')

    if [[ "$base" != "$new_base" ]]; then
        echo "Renaming: $file -> $dir/$new_base"
        git mv "$file" "$dir/$new_base"
    fi
done
```

**Impact**: Consistent naming, improved discoverability

### 2.5 Create Missing Core Directories

```bash
mkdir -p docs/{getting-started,contributing,troubleshooting}
mkdir -p docs/reference/{api,configuration,templates,sparql}
mkdir -p docs/architecture/{decisions,diagrams,design}
mkdir -p docs/examples/{basic,microservice,enterprise}
```

---

## Phase 3: Content Alignment with v5 (Week 4-6) - HIGH IMPACT, HIGH EFFORT

**Goal**: Rewrite core documentation to match v5 product vision.

### 3.1 Rewrite README.md (Single Entry Point)

**Current**: Machine-parseable sync reference (confusing)
**Target**: User-facing project overview

**New Structure:**

```markdown
# ggen: Semantic Code Generation from RDF Ontologies

> Generate deterministic, reproducible code from domain models with zero ambiguity.

## What is ggen?

ggen v5 is a semantic code generator that transforms RDF ontologies into production-ready Rust code using SPARQL queries and templates.

**In 3 Steps:**
1. **Model** your domain in RDF (Turtle, N3, RDF/XML, JSON-LD)
2. **Query** semantic facts with SPARQL
3. **Generate** deterministic code via Tera templates

## Quick Start

```bash
# Install
cargo install ggen

# Validate ontology
ggen validate domain.ttl

# Generate code
ggen generate --ontology domain.ttl --output ./src
```

## Why ggen?

- **Deterministic**: Same input = identical output (no hallucinations)
- **Semantic Precision**: RDF reasoning ensures correctness
- **Reproducible**: Perfect for CI/CD, version control friendly
- **Fast**: <100ms for enterprise-scale models
- **Safe**: Poka-yoke validation prevents invalid code generation

## Documentation

- **[Getting Started](getting-started/)** - Install and generate your first code
- **[Tutorials](tutorials/)** - Learn RDF modeling and code generation
- **[How-To Guides](how-to-guides/)** - Solve specific problems
- **[Reference](reference/)** - Complete API and CLI documentation
- **[Explanations](explanations/)** - Understand architecture and design

## What's New in v5?

ggen v5 is a complete rewrite focused on semantic code generation excellence:

- ✅ **RDF-First**: Ontologies are source of truth, not CLI flags
- ✅ **Pure Rust**: Hyperfast, deterministic, memory-safe
- ✅ **Full RDF Support**: Turtle, N3, RDF/XML, JSON-LD, TriG
- ✅ **SPARQL 1.1**: Complete query engine via Oxigraph
- ✅ **Poka-Yoke Safety**: Multi-layer validation for agent orchestration

**Removed in v5** (use specialized tools instead):
- ❌ Workflow management → Use GitHub Actions / Temporal
- ❌ Agent orchestration → Use Claude API / LangChain
- ❌ Marketplace → Use crates.io / npm / PyPI
- ❌ Multi-language → v5 generates Rust (Python/TS in v5.1+)

See [v5 Strategy](GGEN-V5-STRATEGY.md) for full rationale.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

[Current License]
```

### 3.2 Create Getting Started Guide

**File**: `docs/getting-started/README.md`

**Content Outline:**
1. **Installation** - Cargo install, binary download, Docker
2. **Your First Generation** - 5-minute tutorial
3. **Understanding RDF** - Gentle introduction (link to explanations)
4. **Understanding ggen.toml** - Project manifest format
5. **Next Steps** - Links to tutorials

**Target Time**: User generates working code in <10 minutes

### 3.3 Rewrite Core Tutorials (v5 Aligned)

**Remove v4 Tutorials:**
- `tutorials/marketplace-quick-start.md` (marketplace cut)
- `tutorials/marketplace-workflow.md` (marketplace cut)
- `tutorials/ai-powered-generation.md` (agents cut)

**Create v5 Tutorials:**

1. **`tutorials/01-rdf-basics.md`**
   - What is RDF? (no prerequisites)
   - Turtle syntax crash course
   - Creating your first ontology
   - Validating with ggen

2. **`tutorials/02-ontology-to-code.md`**
   - Design domain model in RDF
   - Define code generation directives
   - Write ggen.toml manifest
   - Generate Rust structs and traits
   - Understand audit trail

3. **`tutorials/03-sparql-patterns.md`**
   - SPARQL query basics
   - Common code generation patterns
   - Filtering, ordering, grouping
   - Using CONSTRUCT for transformations

4. **`tutorials/04-templates.md`**
   - Tera template syntax
   - Safe filters and macros
   - Context variables from SPARQL
   - Debugging template errors

5. **`tutorials/05-production-deployment.md`**
   - CI/CD integration (GitHub Actions)
   - Reproducibility guarantees
   - Audit trail validation
   - Error handling strategies

### 3.4 Rewrite How-To Guides (Problem-Oriented)

**Remove v4 How-Tos:**
- `how-to-guides/configure-llm.md` (AI cut)
- `how-to-guides/create-templates.md` (marketplace cut)
- `how-to-guides/deploy-production.md` (workflow cut)
- `how-to-guides/cicd-workflows.md` (incomplete, workflow cut)

**Create v5 How-Tos:**

1. **`how-to-guides/install.md`**
   - Cargo install
   - Binary downloads
   - Docker container
   - Building from source

2. **`how-to-guides/validate-ontology.md`**
   - Using `ggen validate`
   - SHACL constraint validation
   - Common validation errors
   - Fixing ontology issues

3. **`how-to-guides/write-sparql-queries.md`**
   - Query ontology classes
   - Extract properties and relationships
   - Handle optional values
   - Optimize query performance

4. **`how-to-guides/customize-templates.md`**
   - Override built-in templates
   - Create custom Tera templates
   - Use template inheritance
   - Test template output

5. **`how-to-guides/debug-generation.md`**
   - Enable verbose logging
   - Inspect SPARQL results
   - Validate generated code
   - Read audit trail JSON

6. **`how-to-guides/integrate-ci-cd.md`**
   - GitHub Actions workflow
   - GitLab CI pipeline
   - Verify determinism in CI
   - Cache generation artifacts

7. **`how-to-guides/migrate-from-v4.md`**
   - What changed in v5
   - Feature mapping (v4 → v5 + companion tools)
   - Migration checklist
   - Example conversions

### 3.5 Rewrite Reference Documentation (Complete & Accurate)

**Create Missing Reference Docs:**

1. **`reference/cli.md`** (expand existing)
   - `ggen generate` - Full options, exit codes, examples
   - `ggen validate` - SHACL validation, error codes
   - `ggen query` - Interactive SPARQL shell
   - `ggen --version` / `--help`
   - Shell completions

2. **`reference/ggen-toml.md`** (extract from GGEN-V5-DESIGN.md)
   - Full schema documentation
   - All sections: `[project]`, `[ontology]`, `[generation]`, `[validation]`, `[options]`
   - Examples: minimal, complete, advanced
   - Environment variable expansion
   - Validation rules

3. **`reference/rdf-ontology-structure.md`**
   - ggen-specific RDF properties (`:codegen-as`, `:fields`, `:methods`, etc.)
   - Supported RDF formats
   - Code generation directives
   - SHACL constraints
   - N3 rules

4. **`reference/sparql-patterns.md`**
   - Common query patterns for code generation
   - Performance optimization
   - Query timeout handling
   - CONSTRUCT vs SELECT

5. **`reference/templates.md`** (expand existing)
   - Tera template reference
   - Context variables from SPARQL
   - Built-in filters and functions
   - Safe vs unsafe filters
   - Template testing

6. **`reference/audit-trail.md`**
   - JSON schema
   - Fields and semantics
   - Exit codes and error types
   - Using for verification

7. **`reference/error-codes.md`**
   - All exit codes (0-5)
   - Error message catalog
   - Troubleshooting steps

### 3.6 Rewrite Explanations (Conceptual Understanding)

**Remove v4 Explanations:**
- `explanations/marketplace.md` (cut feature)
- `explanations/ontology-driven.md` (rewrite for v5)
- `explanations/projections.md` (v4-specific)

**Create v5 Explanations:**

1. **`explanations/why-rdf.md`**
   - Semantic precision vs JSON Schema
   - Reasoning capabilities
   - Ontology reuse and extension
   - Linked data benefits

2. **`explanations/architecture.md`**
   - v5 system architecture
   - RDF → SPARQL → Templates pipeline
   - Oxigraph embedded engine
   - Tera template rendering
   - Poka-yoke safety layers

3. **`explanations/determinism.md`**
   - How reproducibility is guaranteed
   - Hash-based verification
   - Audit trail mechanics
   - Agent safety implications

4. **`explanations/poka-yoke-safety.md`**
   - Multi-layer validation
   - Type-level safety (Rust)
   - Runtime validation (SHACL)
   - Output validation (syntax check)
   - Agent orchestration safety

5. **`explanations/semantic-code-generation.md`**
   - How SPARQL queries extract facts
   - Template context preparation
   - Code generation rules
   - Traceability (output → ontology)

6. **`explanations/performance.md`**
   - Architecture optimizations
   - SPARQL query optimization
   - Template caching
   - Benchmark methodology

7. **`explanations/v5-design-philosophy.md`**
   - "Delegate horizontally, excel vertically"
   - Single responsibility (code generation only)
   - Why features were cut
   - Companion tool recommendations

### 3.7 Create Examples (Code-Based Learning)

**Structure:**

```
docs/examples/
├── 01-basic-struct/
│   ├── README.md
│   ├── domain.ttl           # Ontology
│   ├── ggen.toml            # Manifest
│   ├── generated/           # Output
│   └── expected/            # Golden files
├── 02-repository-pattern/
│   ├── README.md
│   ├── domain.ttl
│   ├── ggen.toml
│   ├── generated/
│   └── tests/               # Generated tests
├── 03-microservice/
│   ├── README.md
│   ├── domain.ttl
│   ├── ggen.toml
│   ├── generated/
│   └── Cargo.toml           # Full Rust project
└── 04-enterprise-system/
    ├── README.md
    ├── domain/
    │   ├── user.ttl
    │   ├── product.ttl
    │   └── order.ttl
    ├── ggen.toml
    └── generated/
```

**Each example includes:**
- Complete RDF ontology
- ggen.toml manifest
- Generated code
- README with step-by-step walkthrough
- Golden files for testing

---

## Phase 4: Quality & Completeness (Week 7-8) - SUSTAINED EFFORT

**Goal**: Zero TODO markers, 100% link validity, professional polish.

### 4.1 Resolve 1234 TODO/FIXME/WIP Markers

**Strategy**: Categorize and triage

**Category 1: High-Impact Incomplete Tutorials (Priority 1)**
- `tutorials/01-first-todo-app.md` (85 TODOs) → **Rewrite or remove**
- `how-to/setup-electric-sync.md` (37 TODOs) → **Remove** (not v5 feature)
- `how-to/build-forms-zod.md` (40 TODOs) → **Remove** (not v5 feature)

**Category 2: Reference Docs with TODOs (Priority 2)**
- `architecture/rdf-control-plane/queries/sparql-library.rq` (30 TODOs) → **Complete or archive**
- Review all reference docs, complete missing sections

**Category 3: Archive Incomplete Docs (Priority 3)**
- Move any doc with >10 TODOs to `/wip/` or archive
- Create issue tracker for future completion

**Validation:**
```bash
# Ensure zero TODOs in published docs
grep -r "TODO\|FIXME\|WIP" docs/{tutorials,how-to-guides,reference,explanations} && exit 1
```

### 4.2 Link Validation & Repair

```bash
# Install link checker
cargo install lychee

# Check all markdown links
lychee docs/**/*.md --exclude "archive|wip"

# Fix broken internal links
# Update references to moved/renamed files
```

### 4.3 Create Architecture Decision Records (ADRs)

**File**: `docs/architecture/decisions/README.md`

**ADRs to Create:**

1. **ADR-001: Why RDF over JSON Schema** (`001-rdf-over-json-schema.md`)
2. **ADR-002: Why Oxigraph over Apache Jena** (`002-oxigraph-over-jena.md`)
3. **ADR-003: Why Tera over Handlebars** (`003-tera-template-engine.md`)
4. **ADR-004: CLI Design (Noun-Verb Pattern)** (`004-clap-noun-verb-cli.md`)
5. **ADR-005: Rust-Only for v5.0** (`005-rust-only-v5.md`)
6. **ADR-006: Cutting Marketplace Feature** (`006-cut-marketplace.md`)
7. **ADR-007: Cutting Agent Orchestration** (`007-cut-agent-orchestration.md`)
8. **ADR-008: ggen.toml Manifest Design** (`008-ggen-toml-manifest.md`)

**Template:**
```markdown
# ADR-XXX: [Title]

**Date**: YYYY-MM-DD
**Status**: Accepted | Deprecated | Superseded
**Decision Makers**: [Team/Individual]

## Context

What is the issue we're facing?

## Decision

What did we decide?

## Rationale

Why did we decide this?

## Consequences

What are the positive and negative outcomes?

## Alternatives Considered

What other options did we evaluate?
```

### 4.4 Documentation Standards Document

**File**: `docs/CONTRIBUTING.md`

**Content:**

```markdown
# Contributing to ggen Documentation

## Documentation Principles

1. **Diataxis Framework**: All docs fit into one of four categories
2. **User-First**: Write for users, not developers
3. **Code Examples**: Every concept has a working example
4. **Completeness**: No TODO/FIXME in published docs
5. **Accuracy**: Test all code examples

## Structure

- `tutorials/` - Learning-oriented, step-by-step
- `how-to-guides/` - Problem-oriented, task-based
- `reference/` - Information-oriented, complete
- `explanations/` - Understanding-oriented, conceptual

## Writing Style

- Use active voice
- Keep sentences short (<25 words)
- Use code blocks with syntax highlighting
- Provide runnable examples
- Link to related documentation

## Naming Conventions

- Directories: `lowercase-with-dashes/`
- Files: `lowercase-with-dashes.md`
- Meta files (root): `UPPERCASE.md`

## Before Submitting

- [ ] All links work (run `lychee docs/`)
- [ ] Code examples run successfully
- [ ] No TODO/FIXME markers
- [ ] Spell check passed
- [ ] Follows Diataxis category guidelines
```

### 4.5 Spell Check & Grammar

```bash
# Install spell checker
cargo install typos-cli

# Check documentation
typos docs/

# Create custom dictionary for technical terms
cat > .typos.toml << 'EOF'
[default.extend-words]
RDF = "RDF"
SPARQL = "SPARQL"
Oxigraph = "Oxigraph"
Tera = "Tera"
ggen = "ggen"
EOF
```

---

## Phase 5: Tooling & Automation (Week 9-10) - SUSTAINABILITY

**Goal**: Automate documentation quality checks, generation, deployment.

### 5.1 Documentation Generation (mdBook)

**Install mdBook:**
```bash
cargo install mdbook mdbook-toc mdbook-mermaid
```

**Create book.toml:**
```toml
[book]
title = "ggen Documentation"
authors = ["ggen contributors"]
language = "en"
description = "Semantic code generation from RDF ontologies"
src = "docs"

[build]
build-dir = "target/doc-book"

[preprocessor.toc]
command = "mdbook-toc"

[preprocessor.mermaid]
command = "mdbook-mermaid"

[output.html]
default-theme = "rust"
git-repository-url = "https://github.com/seanchatmangpt/ggen"
edit-url-template = "https://github.com/seanchatmangpt/ggen/edit/main/{path}"
```

**Create SUMMARY.md:**
```markdown
# Summary

[Introduction](README.md)

# Getting Started
- [Installation](getting-started/installation.md)
- [Quick Start](getting-started/quickstart.md)
- [First Project](getting-started/first-project.md)

# Tutorials
- [RDF Basics](tutorials/01-rdf-basics.md)
- [Ontology to Code](tutorials/02-ontology-to-code.md)
- [SPARQL Patterns](tutorials/03-sparql-patterns.md)
- [Templates](tutorials/04-templates.md)
- [Production Deployment](tutorials/05-production-deployment.md)

# How-To Guides
...

# Reference
...

# Explanations
...
```

### 5.2 CI/CD Documentation Checks

**File**: `.github/workflows/docs-quality.yml`

```yaml
name: Documentation Quality

on:
  pull_request:
    paths:
      - 'docs/**'
      - 'book.toml'
  push:
    branches: [main]

jobs:
  quality-checks:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Check for TODOs in published docs
        run: |
          if grep -r "TODO\|FIXME\|WIP" docs/{tutorials,how-to-guides,reference,explanations}; then
            echo "Error: Found TODO markers in published documentation"
            exit 1
          fi

      - name: Validate links
        uses: lycheeverse/lychee-action@v1
        with:
          args: --exclude "archive|wip" docs/**/*.md
          fail: true

      - name: Spell check
        uses: crate-ci/typos@v1.16.0

      - name: Build documentation
        run: |
          cargo install mdbook mdbook-toc mdbook-mermaid
          mdbook build

      - name: Test code examples
        run: |
          # Extract and test code blocks from markdown
          cargo make test-doc-examples
```

### 5.3 Documentation Metrics Dashboard

**File**: `docs/scripts/metrics.sh`

```bash
#!/bin/bash
# Generate documentation health metrics

echo "# Documentation Metrics"
echo ""
echo "Generated: $(date)"
echo ""

echo "## File Counts"
echo "- Tutorials: $(find docs/tutorials -name '*.md' | wc -l)"
echo "- How-To Guides: $(find docs/how-to-guides -name '*.md' | wc -l)"
echo "- Reference: $(find docs/reference -name '*.md' | wc -l)"
echo "- Explanations: $(find docs/explanations -name '*.md' | wc -l)"
echo "- Total: $(find docs -name '*.md' | wc -l)"
echo ""

echo "## Quality Indicators"
echo "- TODO markers: $(grep -r 'TODO\|FIXME\|WIP' docs/**/*.md | wc -l)"
echo "- Broken links: $(lychee docs/**/*.md --exclude archive | grep 'ERROR' | wc -l)"
echo "- Spell errors: $(typos docs/ | wc -l)"
echo ""

echo "## Size Metrics"
echo "- Total lines: $(cat docs/**/*.md | wc -l)"
echo "- Average file size: $(du -sh docs/ | cut -f1)"
```

### 5.4 Automated Diagram Generation

For PlantUML diagrams:

```bash
# Install PlantUML
sudo apt-get install plantuml

# Generate all diagrams
find docs/assets/diagrams -name "*.puml" | while read file; do
    plantuml -tpng "$file"
done
```

---

## Migration Strategy for Existing Users

### For Users Upgrading from v4

**File**: `docs/how-to-guides/migrate-from-v4.md`

**Key Messages:**

1. **v5 is a complete rewrite** - No backwards compatibility
2. **Feature mapping**:
   - Code generation → Still core feature
   - Marketplace → Use crates.io / npm
   - Workflows → Use GitHub Actions
   - Agents → Use Claude API directly
3. **Migration path**:
   - Option 1: Stay on v4 (maintenance mode, security patches)
   - Option 2: Migrate to v5 + companion tools (recommended)
   - Option 3: Hybrid (use v4 wrappers temporarily)

### For Documentation Contributors

**File**: `docs/CONTRIBUTING.md`

**Migration Checklist:**

- [ ] Familiarize with new Diataxis structure
- [ ] Review ADRs for v5 decisions
- [ ] Use new naming conventions
- [ ] Test examples before submitting
- [ ] Run quality checks locally

---

## Success Metrics & Validation

### Quantitative Metrics

| Metric | Current | Target | Validation |
|--------|---------|--------|------------|
| Total files | 517 | <150 | `find docs -name '*.md' \| wc -l` |
| Root directory files | 167 | <20 | `ls docs/*.md \| wc -l` |
| TODO markers (published) | 1234 | 0 | `grep -r TODO docs/{tutorials,how-to,reference,explanations}` |
| Duplicate structures | 3 | 1 | Manual verification |
| Broken links | Unknown | 0 | `lychee docs/` |
| README files | 20 | 5 | `find docs -name README.md \| wc -l` |
| Spell errors | Unknown | <10 | `typos docs/` |

### Qualitative Metrics

- [ ] **Clarity**: New user can install and generate code in <15 minutes
- [ ] **Completeness**: All core features documented
- [ ] **Consistency**: Follows Diataxis framework
- [ ] **Accuracy**: All code examples work
- [ ] **Discoverability**: Easy to find relevant documentation

### User Testing

**Before v5.0 release:**

1. **Internal dogfooding**: ggen team generates code using only docs
2. **External beta**: 3-5 users try quick start guide
3. **Feedback incorporation**: Address gaps and confusion
4. **Final review**: Core team approval

---

## Implementation Roadmap

### Week 1: Immediate Cleanup (Dec 23-27)
- [ ] Archive v3 planning docs (-10 files, -300KB)
- [ ] Archive temporal documentation (-20 files)
- [ ] Remove marketplace docs (-47 files)
- [ ] Remove agent/workflow docs (-15 files)
- [ ] Consolidate diagrams
- [ ] Validation: Root directory <100 files

### Week 2-3: Structural Consolidation (Dec 30 - Jan 10)
- [ ] Choose canonical Diataxis structure
- [ ] Merge duplicate Diataxis locations (-66 files)
- [ ] Reorganize root directory files (167 → 20)
- [ ] Standardize naming conventions
- [ ] Create missing core directories
- [ ] Validation: Single source of truth

### Week 4-6: Content Alignment (Jan 13 - Jan 31)
- [ ] Rewrite README.md (single entry point)
- [ ] Create Getting Started guide
- [ ] Rewrite tutorials (5 new v5 tutorials)
- [ ] Rewrite how-to guides (7 problem-oriented guides)
- [ ] Rewrite reference docs (7 complete references)
- [ ] Rewrite explanations (7 conceptual docs)
- [ ] Create examples (4 working projects)
- [ ] Validation: Content matches v5 product

### Week 7-8: Quality & Completeness (Feb 3-14)
- [ ] Resolve 1234 TODO markers (0 in published docs)
- [ ] Link validation and repair
- [ ] Create 8 ADRs
- [ ] Write documentation standards (CONTRIBUTING.md)
- [ ] Spell check and grammar review
- [ ] Validation: Professional polish

### Week 9-10: Tooling & Automation (Feb 17-28)
- [ ] Setup mdBook
- [ ] Create CI/CD docs quality checks
- [ ] Create documentation metrics dashboard
- [ ] Setup automated diagram generation
- [ ] Validation: Sustainable process

### Week 11: Final Review & Launch (Mar 3-7)
- [ ] User testing (internal + external beta)
- [ ] Address feedback
- [ ] Core team final review
- [ ] Publish docs website
- [ ] Announce v5 documentation
- [ ] Validation: User success stories

---

## Risk Mitigation

### Risk: "Users confused by missing v4 features"

**Mitigation:**
- Clear "What's New in v5" section in README
- Dedicated migration guide
- Feature mapping table (v4 → v5 + companion tools)
- Keep v4 docs accessible (separate branch/archive)

### Risk: "Documentation still incomplete at launch"

**Mitigation:**
- Phased release: Mark incomplete sections with "Coming Soon" banners
- Prioritize critical path docs (Getting Started, Tutorials 1-2)
- Use GitHub issues for remaining TODOs
- Community contributions encouraged

### Risk: "RDF/SPARQL too complex for users"

**Mitigation:**
- "RDF for Beginners" tutorial (no prerequisites)
- Gentle learning curve (basic → advanced)
- Lots of working examples
- Link to external RDF resources

### Risk: "Breaking link cascade from refactor"

**Mitigation:**
- Use link checker in CI/CD
- Incremental refactor with validation
- Update references immediately after moves
- Redirect plan for common old URLs

---

## Post-Launch: Continuous Improvement

### Documentation Feedback Loop

1. **GitHub Discussions**: "Documentation Feedback" category
2. **Confusion Heatmap**: Track which pages users struggle with
3. **Search Analytics**: What are users looking for?
4. **Community Contributions**: Accept PRs for clarity improvements

### Quarterly Reviews

Every 3 months:
- Review documentation metrics
- Update for new features
- Refresh examples
- Re-validate links and code

### Version Strategy

- **Main branch**: Current stable (v5.x)
- **Docs for older versions**: Archive under `/docs/v4/`, `/docs/v3/` if needed
- **Versioned website**: mdBook supports versioning

---

## Deliverables Checklist

### Phase 1: Cleanup
- [ ] Archive directory created with README
- [ ] v3 planning docs archived
- [ ] Temporal docs archived
- [ ] Marketplace docs archived
- [ ] Agent/workflow docs archived
- [ ] Diagrams consolidated
- [ ] Validation report: file count reduction

### Phase 2: Consolidation
- [ ] Canonical Diataxis structure established
- [ ] Duplicate structures merged
- [ ] Root directory organized (<20 files)
- [ ] Naming conventions standardized
- [ ] Directory structure documented

### Phase 3: Content Creation
- [ ] README.md rewritten
- [ ] Getting Started guide created
- [ ] 5 tutorials written and tested
- [ ] 7 how-to guides written and tested
- [ ] 7 reference docs completed
- [ ] 7 explanations written
- [ ] 4 working examples created

### Phase 4: Quality
- [ ] Zero TODO markers in published docs
- [ ] All links validated
- [ ] 8 ADRs created
- [ ] CONTRIBUTING.md written
- [ ] Spell check passed

### Phase 5: Tooling
- [ ] mdBook configured and working
- [ ] CI/CD quality checks running
- [ ] Documentation metrics dashboard
- [ ] Automated diagram generation

### Final Launch
- [ ] User testing completed
- [ ] Feedback incorporated
- [ ] Core team approval
- [ ] Docs website live
- [ ] Announcement published

---

## Appendix A: File Structure (Target State)

```
docs/
├── README.md                          # Single entry point
├── ARCHITECTURE.md                    # System overview
├── CONTRIBUTING.md                    # Contribution guidelines
│
├── getting-started/
│   ├── README.md
│   ├── installation.md
│   ├── quickstart.md
│   └── first-project.md
│
├── tutorials/                         # CANONICAL
│   ├── README.md
│   ├── 01-rdf-basics.md
│   ├── 02-ontology-to-code.md
│   ├── 03-sparql-patterns.md
│   ├── 04-templates.md
│   └── 05-production-deployment.md
│
├── how-to-guides/                     # CANONICAL
│   ├── README.md
│   ├── install.md
│   ├── validate-ontology.md
│   ├── write-sparql-queries.md
│   ├── customize-templates.md
│   ├── debug-generation.md
│   ├── integrate-ci-cd.md
│   └── migrate-from-v4.md
│
├── reference/                         # CANONICAL
│   ├── README.md
│   ├── cli.md
│   ├── ggen-toml.md
│   ├── rdf-ontology-structure.md
│   ├── sparql-patterns.md
│   ├── templates.md
│   ├── audit-trail.md
│   └── error-codes.md
│
├── explanations/                      # CANONICAL
│   ├── README.md
│   ├── why-rdf.md
│   ├── architecture.md
│   ├── determinism.md
│   ├── poka-yoke-safety.md
│   ├── semantic-code-generation.md
│   ├── performance.md
│   └── v5-design-philosophy.md
│
├── examples/
│   ├── 01-basic-struct/
│   ├── 02-repository-pattern/
│   ├── 03-microservice/
│   └── 04-enterprise-system/
│
├── architecture/
│   ├── README.md
│   ├── decisions/                     # ADRs
│   │   ├── README.md
│   │   ├── 001-rdf-over-json-schema.md
│   │   ├── 002-oxigraph-over-jena.md
│   │   ├── 003-tera-template-engine.md
│   │   └── ...
│   ├── design/
│   └── diagrams/
│
├── development/
│   ├── chicago-tdd.md
│   ├── sparc-methodology.md
│   └── lean-quality.md
│
├── testing/
│   ├── test-strategy.md
│   └── qa-framework.md
│
├── performance/
│   ├── benchmarks.md
│   └── optimization.md
│
├── troubleshooting/
│   ├── common-errors.md
│   ├── debugging.md
│   └── faq.md
│
├── ontology-packs/                    # Keep as-is (well-organized)
│   ├── tutorials/
│   ├── how-to/
│   ├── reference/
│   └── explanations/
│
├── assets/
│   └── diagrams/
│       ├── architecture/
│       ├── flow/
│       └── sequence/
│
├── scripts/
│   ├── standardize-names.sh
│   ├── metrics.sh
│   └── link-check.sh
│
└── archive/
    ├── v3-planning-abandoned-2025-11/
    ├── temporal-evidence/
    ├── marketplace-v2-cut-feature/
    ├── cut-features-agents-workflows/
    ├── diataxis-duplicate/
    └── src-duplicate/
```

**Total Files**: ~140 (vs 517 current)
**Root Files**: ~15 (vs 167 current)
**Clear Navigation**: Single README, canonical categories
**Zero Duplication**: One source of truth for each category

---

## Appendix B: Content Inventory (What Stays, What Goes)

### KEEP (v5 Aligned)

**Architecture:**
- Core RDF/SPARQL architecture docs
- Poka-yoke safety design
- Performance benchmarks

**Development:**
- Chicago TDD implementation
- SPARC methodology
- Lean quality principles

**Reference:**
- CLI reference (updated for v5)
- RDF/SPARQL reference
- Template reference (Tera)
- Configuration reference (ggen.toml)

**Ontology Packs:**
- Entire directory (legitimate v5 feature)

### ARCHIVE (Historical Value)

**v3 Planning:**
- All GGEN_V3_*.md files (10 files)
- Never implemented, historical planning

**Temporal Evidence:**
- All WEEK*.md files (20+ files)
- Phase deliverables
- Metrics/daily reports

**Migration Docs:**
- Marketplace v2 migration (superseded)
- v2 → v3 migration (never happened)

### REMOVE (Cut Features)

**Marketplace:**
- All MARKETPLACE_V2_*.md files (47 files)
- Marketplace architecture
- Marketplace API docs

**Agent Orchestration:**
- docs/agent/ (11 files)
- docs/swarm/ (2 files)
- Agent coordination guides

**Workflows:**
- Workflow management docs
- JTBD documentation framework

**Incomplete Work:**
- Tutorials with 85+ TODOs
- How-tos with 37-40 TODOs
- Any WIP with no completion plan

---

## Document Status

**Version**: 1.0
**Date**: 2025-12-18
**Status**: Planning - Ready for Implementation
**Next Action**: Core team review + Phase 1 execution approval
**Estimated Completion**: March 7, 2026 (11 weeks)

---

**Key Success Indicator**: New user can install ggen, model a domain in RDF, and generate working Rust code in <15 minutes using only the documentation.
