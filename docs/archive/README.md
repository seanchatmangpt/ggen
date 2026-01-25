<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation Archive](#documentation-archive)
  - [Organization](#organization)
  - [What's Here](#whats-here)
    - [Analysis Documents (`analysis/`)](#analysis-documents-analysis)
    - [Why Archived?](#why-archived)
    - [Finding What You Need](#finding-what-you-need)
  - [Key Documents by Purpose](#key-documents-by-purpose)
    - [If You Want to Understand...](#if-you-want-to-understand)
  - [File Listing](#file-listing)
    - [Phase & Implementation (8 files)](#phase--implementation-8-files)
    - [Testing & Quality (9 files)](#testing--quality-9-files)
    - [CI/CD & Infrastructure (4 files)](#cicd--infrastructure-4-files)
    - [Audits & Reports (5 files)](#audits--reports-5-files)
    - [System Design (6 files)](#system-design-6-files)
    - [Doctest & Data (5 files)](#doctest--data-5-files)
    - [Planning & Process (8 files)](#planning--process-8-files)
  - [How to Search the Archive](#how-to-search-the-archive)
    - [Using grep (find mentions of a topic)](#using-grep-find-mentions-of-a-topic)
    - [Using find (locate specific files)](#using-find-locate-specific-files)
    - [Search GitHub](#search-github)
  - [Contributing](#contributing)
  - [File Cleanup Schedule](#file-cleanup-schedule)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation Archive

This directory contains historical analysis documents, detailed implementation reports, and technical planning documents.

## Organization

```
archive/
├── analysis/              # Technical analysis and planning documents
│   ├── README.md
│   ├── AUTONOMOUS_ONTOLOGY_SYSTEM.md
│   ├── GGEN_8020_ROADMAP.md
│   └── ... (40+ more analysis docs)
└── README.md              # This file
```

## What's Here

### Analysis Documents (`analysis/`)

These files contain:
- **Implementation planning** - Phase implementations (Phase 3, 4, 4D, etc.)
- **Testing strategies** - Test suite design, coverage analysis, expert testing patterns
- **CI/CD design** - GitHub Actions workflows, bulletproof architecture
- **Audit reports** - Code quality, security, compliance audits
- **Research documents** - Dark matter measurement, evidence graph schemas
- **Progress reports** - Completion summaries, integration status, delivery reports
- **Conversion records** - Doctest conversions, data motion validation
- **Integration guides** - Sector bundles, collaboration patterns, marketplace guide

### Why Archived?

These documents are:
- **Historical** - Captured the development process at specific points in time
- **Detailed** - Contain extensive technical planning and reasoning
- **Reference** - Useful for understanding "why" certain decisions were made
- **Large** - 40+ files, mostly superseded by current documentation

### Finding What You Need

**Most users should:**
1. Start with `/README.md` in the repository root
2. Navigate to `/docs/` for current documentation
3. Check the main documentation sections:
   - Tutorials (how to get started)
   - How-to guides (accomplish specific tasks)
   - Reference (detailed technical info)
   - Explanations (understand concepts)

**If you need historical context:**
- Browse `archive/analysis/` for detailed planning documents
- Search for specific topics using `grep` or `find`
- Check git history for evolution of decisions

## Key Documents by Purpose

### If You Want to Understand...

**System Architecture:**
- `/docs/explanations/architecture.md` - Current architecture
- `archive/analysis/AUTONOMOUS_ONTOLOGY_SYSTEM.md` - Detailed design

**Development Phases:**
- `archive/analysis/GGEN_8020_ROADMAP.md` - Overall roadmap
- `archive/analysis/PHASE_3_IMPLEMENTATION_SUMMARY.md` - Phase 3 details
- `archive/analysis/PHASE_4_COMPLETION_SUMMARY.md` - Phase 4 completion
- `archive/analysis/PHASES_4_6_ADVANCED_RUST.md` - Phases 4-6

**Testing Strategy:**
- `/docs/TESTING.md` - Current testing guidelines
- `archive/analysis/COMPREHENSIVE_TEST_SUITE_SUMMARY.md` - Test suite design
- `archive/analysis/EXPERT_TESTING_PATTERNS.md` - Testing patterns

**CI/CD Implementation:**
- `/docs/how-to-guides/cicd-workflows.md` - How to use CI/CD
- `archive/analysis/BULLETPROOF_CI_CD_ARCHITECTURE.md` - Architecture design
- `archive/analysis/CI_CD_IMPLEMENTATION_GUIDE.md` - Implementation guide

**Quality & Audits:**
- `archive/analysis/CODEBASE_AUDIT_REPORT.md` - Code quality audit
- `archive/analysis/AUDIT_REPORT.md` - Security audit
- `archive/analysis/TEST_COVERAGE_ANALYSIS_COMPREHENSIVE.md` - Coverage analysis

**Marketplace System:**
- `/docs/explanations/marketplace.md` - Current marketplace
- `archive/analysis/MARKETPLACE_COMPREHENSIVE_GUIDE.md` - Detailed guide
- `archive/analysis/SECTOR_BUNDLES_INTEGRATION_GUIDE.md` - Bundles design

**Ontology System:**
- `/docs/explanations/ontology-driven.md` - Ontology principles
- `archive/analysis/AUTONOMOUS_ONTOLOGY_SYSTEM.md` - System design
- `archive/analysis/EVIDENCE_GRAPH_SCHEMA.md` - Schema definition

## File Listing

### Phase & Implementation (8 files)
- PHASE_3_IMPLEMENTATION_SUMMARY.md
- PHASE_4_COMPLETION_SUMMARY.md
- PHASE_4D_COMPILATION_REPORT.md
- PHASE_4_6_COMPLETE_IMPLEMENTATION.md
- PHASES_4_6_ADVANCED_RUST.md
- FINAL_DELIVERY_SUMMARY.md
- INTEGRATION_STATUS_REPORT.md
- GGEN_8020_ROADMAP.md

### Testing & Quality (9 files)
- COMPREHENSIVE_TEST_SUITE_SUMMARY.md
- EXPERT_TESTING_PATTERNS.md
- EXPERT_TESTING_IMPLEMENTATION_STATUS.md
- TEST_FRAMEWORK_SUMMARY.md
- TEST_COVERAGE_ANALYSIS_COMPREHENSIVE.md
- CRITICAL_UNTESTED_PATHS.md
- TESTING_GAPS_ANALYSIS.md
- TESTING_ANALYSIS_SUMMARY.md
- TESTING_IMPLEMENTATION_GUIDE.md

### CI/CD & Infrastructure (4 files)
- BULLETPROOF_CI_CD_ARCHITECTURE.md
- BULLETPROOF_CI_CD_SUMMARY.md
- CI_CD_IMPLEMENTATION_GUIDE.md
- DATA_MOTION_FIXES_SUMMARY.md

### Audits & Reports (5 files)
- CODEBASE_AUDIT_REPORT.md
- AUDIT_REPORT.md
- MARKDOWN_ANALYSIS_REPORT.md
- AI_INTEGRATION_REPORT.md
- AI_GENERATORS_GRANITE4_TEST_REPORT.md

### System Design (6 files)
- AUTONOMOUS_ONTOLOGY_SYSTEM.md
- EVIDENCE_GRAPH_SCHEMA.md
- DARK_MATTER_MEASUREMENT.md
- COMPLETE_8020_SYSTEM_SUMMARY.md
- SECTOR_BUNDLES_INTEGRATION_GUIDE.md
- MARKETPLACE_COMPREHENSIVE_GUIDE.md

### Doctest & Data (5 files)
- DOCTEST_CONVERSION_SUMMARY.md
- DOCTEST_CONVERSION_ANALYSIS.md
- DOCTEST_CONVERSION_CHECKLIST.md
- DATA_MOTION_VALIDATION.md
- FMEA_DOCTEST_CONVERSION.md

### Planning & Process (8 files)
- CREATE_PR_INSTRUCTIONS.md
- COLLABORATION_GUIDE.md
- ACADEMIC_PAPER_SYSTEM_INDEX.md
- ANALYSIS_INDEX.md
- MURA_INVENTORY.md
- DIATAXIS_INTEGRATION_SUMMARY.md
- FMEA_CHANGES_REVIEW.md
- PR_DESCRIPTION.md

## How to Search the Archive

### Using grep (find mentions of a topic)

```bash
# Find documents mentioning "marketplace"
grep -r "marketplace" docs/archive/

# Find documents mentioning "SPARQL"
grep -r "SPARQL" docs/archive/

# Find documents mentioning "testing"
grep -r "testing" docs/archive/
```

### Using find (locate specific files)

```bash
# List all files in archive
find docs/archive/ -type f -name "*.md"

# Find files matching pattern
find docs/archive/ -type f -name "*PHASE*"

# Find files mentioning a date
find docs/archive/ -type f -mtime -30  # Last 30 days
```

### Search GitHub

Many of these documents are also in git history:
```bash
git log --all --full-history --source --remotes --oneline -- 'GGEN_8020_ROADMAP.md'
```

## Contributing

If you're working on improvements to ggen:

1. **Current documentation:** Check `/docs/` first
2. **Historical context:** Browse `archive/` for rationale
3. **New findings:** Update current docs, archive old docs when superseded
4. **Analysis:** Add to `archive/analysis/` with descriptive title

## File Cleanup Schedule

Archive files are kept indefinitely for historical reference but:
- **Regularly reviewed** for accuracy
- **Linked from** current docs where relevant
- **Superseded by** newer documentation in `/docs/`
- **Never deleted** to preserve development history

---

**Last Updated:** November 17, 2025
**Archive Reorganization:** Moved analysis docs from repository root to reduce clutter
**Maintained By:** ggen Documentation Team
