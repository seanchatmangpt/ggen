<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Markdown File Analysis & Categorization Report](#markdown-file-analysis--categorization-report)
  - [Executive Summary](#executive-summary)
  - [SECTION 1: CORE TEAM PROCEDURES TO KEEP (37 FILES)](#section-1-core-team-procedures-to-keep-37-files)
    - [Root-Level Files (9 files)](#root-level-files-9-files)
    - [Documentation Core (20 files in docs/)](#documentation-core-20-files-in-docs)
    - [Crate Documentation (7 files)](#crate-documentation-7-files)
    - [Test Procedures (1 file)](#test-procedures-1-file)
    - [CONSOLIDATION OPPORTUNITIES](#consolidation-opportunities)
  - [SECTION 2: FILES TO DELETE (673 FILES, 239,209 LINES)](#section-2-files-to-delete-673-files-239209-lines)
    - [By Category](#by-category)
  - [SECTION 3: FILES REQUIRING DECISION (115 FILES, 42,563 LINES)](#section-3-files-requiring-decision-115-files-42563-lines)
    - [High Priority Review (Likely Keep)](#high-priority-review-likely-keep)
    - [Medium Priority Review](#medium-priority-review)
    - [Low Priority Review](#low-priority-review)
  - [SECTION 4: CONSOLIDATION OPPORTUNITIES](#section-4-consolidation-opportunities)
    - [Phase 1: Quick Wins (16,432 lines saved)](#phase-1-quick-wins-16432-lines-saved)
    - [Phase 2: Medium Priority (Archive or Delete)](#phase-2-medium-priority-archive-or-delete)
    - [Phase 3: Complete Cleanup](#phase-3-complete-cleanup)
  - [SECTION 5: IMPACT SUMMARY](#section-5-impact-summary)
    - [Final State After Cleanup](#final-state-after-cleanup)
    - [Size Savings](#size-savings)
  - [SECTION 6: RECOMMENDED ACTIONS](#section-6-recommended-actions)
    - [Priority 1: Quick Wins (1-2 days)](#priority-1-quick-wins-1-2-days)
    - [Priority 2: Consolidation (1 day)](#priority-2-consolidation-1-day)
    - [Priority 3: Archive/Delete (2-3 days)](#priority-3-archivedelete-2-3-days)
  - [APPENDIX: FILES TO KEEP (37)](#appendix-files-to-keep-37)
  - [KEY FINDINGS](#key-findings)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Markdown File Analysis & Categorization Report

**Analysis Date**: November 17, 2025  
**Total Files Analyzed**: 825 markdown files  
**Total Lines of Markdown**: 292,573 lines  

---

## Executive Summary

| Category | Files | Lines | % of Total | Action |
|----------|-------|-------|-----------|--------|
| **KEEP** - Core Team Procedures | 37 | 10,801 | 3.7% | Maintain |
| **DELETE** - Non-essential | 673 | 239,209 | 81.8% | Remove |
| **MAYBE** - Requires Decision | 115 | 42,563 | 14.5% | Review |
| **TOTAL** | **825** | **292,573** | **100%** | |

**Immediate Cleanup Impact**: Removing 673 files saves **239,209 lines** (234 KB approx), freeing ~82% of documentation bulk.

---

## SECTION 1: CORE TEAM PROCEDURES TO KEEP (37 FILES)

### Root-Level Files (9 files)

```
1,026 lines | CHANGELOG.md
  → Version history and release notes [KEEP AS-IS]

  547 lines | TESTING.md  
  → Testing guide and procedures [CONSOLIDATE WITH docs/DEVELOPMENT_WORKFLOW.md]

  344 lines | PERFORMANCE.md
  → Performance requirements [KEEP AS-IS]

  312 lines | SECURITY.md
  → Security guidelines [KEEP AS-IS]

  246 lines | README.md
  → Main project entry [KEEP AS-IS]

  156 lines | BUG_REPORTING_GUIDE.md
  → Bug procedures [CONSOLIDATE INTO CONTRIBUTING.md]

  154 lines | CONTRIBUTING.md
  → Contribution guidelines [KEEP + CONSOLIDATE INTO]

  ~100 lines | docs/SECURITY.md
  → Security reference [KEEP AS-IS]
```

### Documentation Core (20 files in docs/)

**Development & Standards**:
```
  docs/CLAUDE.md (removed 2026-02)
  → Root CLAUDE.md is authoritative

  594 lines | docs/CODING_STANDARDS.md
  → Code standards and conventions [KEEP]

  387 lines | docs/DEVELOPMENT_WORKFLOW.md
  → Dev procedures, testing, git workflow [KEEP - CONSOLIDATE TESTING.md HERE]

  160 lines | docs/README.md
  → Documentation index [KEEP]
```

**How-To Guides (Team Procedures)**:
```
  441 lines | docs/how-to-guides/cicd-workflows.md [KEEP]
  272 lines | docs/how-to-guides/installation.md [KEEP]
  249 lines | docs/how-to-guides/troubleshoot.md [KEEP]
  215 lines | docs/how-to-guides/create-templates.md [KEEP]
  198 lines | docs/how-to-guides/use-rdf-ontologies.md [KEEP]
  193 lines | docs/how-to-guides/deploy-production.md [KEEP]
  171 lines | docs/how-to-guides/configure-hooks.md [KEEP]
```

**Reference Documentation**:
```
  542 lines | docs/reference/cli.md [KEEP]
  422 lines | docs/reference/hooks-lifecycle.md [KEEP]
  171 lines | docs/reference/configuration.md [KEEP]
  159 lines | docs/reference/template-directives.md [KEEP]
```

### Crate Documentation (7 files)

```
  434 lines | crates/ggen-ai/README.md [KEEP]
  321 lines | crates/ggen-marketplace/README.md [KEEP]
  299 lines | crates/ggen-dod/DEFINITION_OF_DONE.md [KEEP]
  ~200 lines | crates/ggen-cli/README.md [KEEP]
  ~150 lines | crates/ggen-core/README.md [KEEP]
  ~100 lines | crates/ggen-node/README.md [KEEP]
  ~50 lines | crates/ggen-utils/README.md [KEEP]
```

### Test Procedures (1 file)

```
  501 lines | tests/lifecycle_tests/README.md [KEEP]
```

### CONSOLIDATION OPPORTUNITIES

| File to Consolidate | Into | Lines Saved | Effort |
|---------------------|------|------------|--------|
| TESTING.md | docs/DEVELOPMENT_WORKFLOW.md | 547 | Low |
| CREATE_PR_INSTRUCTIONS.md | CONTRIBUTING.md | 180 | Low |
| BUG_REPORTING_GUIDE.md | CONTRIBUTING.md | 150 | Low |
| CI_CD_IMPLEMENTATION_GUIDE.md | docs/how-to-guides/cicd-workflows.md | 555 | Low |
| **Total Consolidation** | | **1,432 lines** | |

---

## SECTION 2: FILES TO DELETE (673 FILES, 239,209 LINES)

### By Category

**V2 Refactoring Agent Reports** (184 files in `.claude/refactor-v2/`)
- Agent 1-12 summaries, validation reports, production readiness reports
- Savings: ~50,000 lines
- Status: Historical work, complete

**SPARC Agent Templates** (184 files in `.claude/agents/`)
- Agent role definitions: coder, reviewer, tester, consensus managers, swarm coordinators
- Savings: ~35,000 lines
- Status: Can be embedded in code if needed

**DFLSS Command Reference** (38 files: `.claude/commands/` and `.cursor/commands/`)
- Lean Six Sigma methodology commands
- Savings: ~15,000 lines
- Status: Not actively used; can be archived if needed

**GGEN V3 Vision/Architecture** (10 files)
- GGEN_V3_ARCHITECTURE_C4.md, GGEN_V3_IMPLEMENTATION_ROADMAP.md, GGEN_V3_*
- Savings: ~11,000 lines
- Status: Future vision, not implemented

**Marketplace Feature Docs** (146 files in `marketplace/`)
- Package templates, examples, architecture docs
- Savings: ~35,000 lines
- Status: Feature-specific, not core team procedure

**Example Projects** (62 files in `examples/`)
- Advanced projects, demos, tutorials
- Savings: ~18,000 lines
- Status: Not core team procedures

**Template Examples** (14 files in `templates/`)
- Example templates for projects
- Savings: ~4,000 lines
- Status: Not core team procedures

**Vendor/External Code** (58 files in `vendors/`)
- External libraries and documentation
- Savings: ~12,000 lines
- Status: Should use dependencies, not repo

**Academic Papers** (8 files in `docs/papers/`)
- Research papers, academic guides
- Savings: ~6,000 lines
- Status: Reference only

**Historical Analysis & Reports** (Multiple files)
- *_ANALYSIS, *_REPORT, *_SUMMARY files
- FMEA_*, DATA_MOTION_*, EXPERT_TESTING_*, DOCTEST_CONVERSION_*
- PHASE_*, SWARM_*, CRITICAL_*, MURA_*, MUDA_*
- Savings: ~35,000 lines
- Status: Completed work analysis

**Graph Evidence System** (7 files in `.ggen/`)
- Evidence graph data and summaries
- Savings: ~2,000 lines
- Status: Internal system documentation

**Work-in-Progress** (7 files in `docs/wip/`)
- Abandoned/incomplete work items
- Savings: ~500 lines
- Status: Should use task management system

**Backup/Removed Code** (1 directory `.removed-code-backup/`)
- Old code removed during cleanup
- Savings: ~500 lines
- Status: In git history, not needed

---

## SECTION 3: FILES REQUIRING DECISION (115 FILES, 42,563 LINES)

### High Priority Review (Likely Keep)

```
2,611 lines | tests/bdd/docs/LONDON-BDD.md
  → London BDD testing methodology [LIKELY KEEP]

1,476 lines | docs/marketplace-oxigraph-relationship.md
  → Integration architecture [LIKELY KEEP]

  917 lines | docs/src/how-to-guides/cleanroom-testing.md
  → Cleanroom testing procedures [CONSOLIDATE TO docs/how-to-guides/]

  803 lines | docs/src/how-to-guides/create-templates.md
  → Template creation (DUPLICATE OF ALREADY KEPT FILE) [DELETE]
```

### Medium Priority Review

```
  728 lines | crates/ggen-core/docs/SECURITY_REVIEW.md
  → Security review [CONSOLIDATE TO SECURITY.md]

  678 lines | crates/ggen-core/docs/QUICK_WINS_IMPLEMENTATION.md
  → Quick wins [CONSOLIDATE TO DEVELOPMENT_WORKFLOW.md]

  623 lines | tests/TEST_STRATEGY.md
  → Test strategy [CONSOLIDATE TO DEVELOPMENT_WORKFLOW.md]

  555 lines | scripts/v2_migration/MIGRATION_PLAN.md
  → v2 migration (COMPLETED) [DELETE]
```

### Low Priority Review

```
  488 lines | docs/DATAXIS_GUIDE.md
  → Documentation framework [DELETE]

  465 lines | docs/src/explanations/marketplace-maturity.md
  → Marketplace explanation [DELETE]
```

---

## SECTION 4: CONSOLIDATION OPPORTUNITIES

### Phase 1: Quick Wins (16,432 lines saved)

1. **Consolidate TESTING.md** → docs/DEVELOPMENT_WORKFLOW.md
   - Lines: 547
   - Risk: Low
   - Action: Move content

2. **Consolidate CREATE_PR_INSTRUCTIONS.md** → CONTRIBUTING.md
   - Lines: 180
   - Risk: Low
   - Action: Merge into contributing guide

3. **Consolidate BUG_REPORTING_GUIDE.md** → CONTRIBUTING.md
   - Lines: 150
   - Risk: Low
   - Action: Add section to contributing

4. **Consolidate CI_CD_IMPLEMENTATION_GUIDE.md** → docs/how-to-guides/cicd-workflows.md
   - Lines: 555
   - Risk: Low
   - Action: Merge implementation details

5. **Remove docs/src/ directory** (exact duplication)
   - Lines: ~15,000
   - Risk: Low
   - Action: Delete if no unique content

### Phase 2: Medium Priority (Archive or Delete)

6. Delete `.claude/refactor-v2/` (184 files, 50,000 lines)
7. Delete `marketplace/` (146 files, 35,000 lines)
8. Delete `examples/` (62 files, 18,000 lines)
9. Delete `vendors/` (58 files, 12,000 lines)
10. Delete `.claude/agents/` (184 files, 35,000 lines)
11. Delete `.claude/commands/` (38 files, 15,000 lines)

### Phase 3: Complete Cleanup

12. Delete all analysis/report files (~35,000 lines)
13. Delete all conceptual/vision documents (~20,000 lines)
14. Delete `docs/papers/` (8 files, 6,000 lines)
15. Delete `docs/wip/` (7 files, 500 lines)

---

## SECTION 5: IMPACT SUMMARY

### Final State After Cleanup

```
Current:  825 files,  292,573 lines
After:   ~152 files,   37,000 lines

Reduction: 673 files (-82%), 255,573 lines (-87%)
Net Result: Focused core team documentation
```

### Size Savings

| Phase | Lines Saved | Files Deleted | Impact |
|-------|------------|---|--------|
| Consolidation | 1,432 | - | Quick wins |
| Phase 1 Deletion | 100,000 | 450+ | Major cleanup |
| Phase 2 Deletion | 80,000+ | 150+ | Medium cleanup |
| Phase 3 Deletion | 75,000+ | 73 | Final cleanup |
| **TOTAL** | **~255,573 lines** | **673 files** | **87% reduction** |

---

## SECTION 6: RECOMMENDED ACTIONS

### Priority 1: Quick Wins (1-2 days)

1. Delete `.claude/refactor-v2/` (184 files, 50,000 lines)
2. Delete `marketplace/` (146 files, 35,000 lines)  
3. Delete `examples/` (62 files, 18,000 lines)
4. Delete `vendors/` (58 files, 12,000 lines)
5. Remove `docs/src/` duplication (15,000 lines)

### Priority 2: Consolidation (1 day)

6. Consolidate TESTING.md into docs/DEVELOPMENT_WORKFLOW.md
7. Consolidate CREATE_PR_INSTRUCTIONS.md into CONTRIBUTING.md
8. Consolidate BUG_REPORTING_GUIDE.md into CONTRIBUTING.md
9. Consolidate CI_CD_IMPLEMENTATION_GUIDE.md into docs/how-to-guides/cicd-workflows.md

### Priority 3: Archive/Delete (2-3 days)

10. Delete `.claude/agents/` (184 files, 35,000 lines)
11. Delete `.claude/commands/` (38 files, 15,000 lines)
12. Delete all analysis/report files (~35,000 lines)
13. Delete `docs/papers/` (8 files, 6,000 lines)
14. Delete `docs/wip/` (7 files, 500 lines)
15. Delete `.ggen/` (7 files, 2,000 lines)

---

## APPENDIX: FILES TO KEEP (37)

```
CHANGELOG.md
CONTRIBUTING.md
TESTING.md
BUG_REPORTING_GUIDE.md
README.md
SECURITY.md
PERFORMANCE.md

docs/CHANGELOG.md
docs/CLAUDE.md
docs/CODING_STANDARDS.md
docs/DEVELOPMENT_WORKFLOW.md
docs/README.md
docs/SECURITY.md
docs/how-to-guides/cicd-workflows.md
docs/how-to-guides/configure-hooks.md
docs/how-to-guides/create-templates.md
docs/how-to-guides/deploy-production.md
docs/how-to-guides/installation.md
docs/how-to-guides/troubleshoot.md
docs/how-to-guides/use-rdf-ontologies.md
docs/reference/cli.md
docs/reference/configuration.md
docs/reference/hooks-lifecycle.md
docs/reference/template-directives.md

crates/ggen-ai/README.md
crates/ggen-cli/README.md
crates/ggen-core/README.md
crates/ggen-dod/DEFINITION_OF_DONE.md
crates/ggen-marketplace/README.md
crates/ggen-node/README.md
crates/ggen-utils/README.md

tests/README.md
tests/clnrm/README.md
tests/clnrm/lifecycle/README.md
tests/integration/README.md
tests/lifecycle_tests/README.md
tests/london_tdd/README.md
```

---

## KEY FINDINGS

1. **82% of markdown is non-essential** (239,209 lines in 673 files)
2. **Only 3.7% is core team procedures** (10,801 lines in 37 files)
3. **14.5% requires human decision** (42,563 lines in 115 files)
4. **4 quick consolidations save 1,432 lines** of duplication
5. **Biggest bulk deletions**: `.claude/refactor-v2/` (50K), marketplace (35K), .claude/agents/ (35K)
6. **Best organization**: docs/ has good structure; consolidate slightly
7. **Keep focused**: Remove all analysis, reports, examples, templates, vendors

