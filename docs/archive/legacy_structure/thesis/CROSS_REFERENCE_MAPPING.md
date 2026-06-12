<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Cross-Reference Mapping and Update Strategy for ggen PhD Thesis](#cross-reference-mapping-and-update-strategy-for-ggen-phd-thesis)
  - [Document Status](#document-status)
  - [Executive Summary](#executive-summary)
  - [Current Chapter Structure (After Phase 1 Integration)](#current-chapter-structure-after-phase-1-integration)
  - [Section 1: LaTeX Comments That Need Updating](#section-1-latex-comments-that-need-updating)
    - [Updates Required:](#updates-required)
  - [Section 2: Inline Text References (ALREADY CORRECT!)](#section-2-inline-text-references-already-correct)
  - [Section 3: Chapter Labels (Semantic, Not Numerical)](#section-3-chapter-labels-semantic-not-numerical)
  - [Section 4: Cross-Reference Analysis](#section-4-cross-reference-analysis)
    - [Finding 1: No \ref{} Commands in Main Thesis](#finding-1-no-%5Cref-commands-in-main-thesis)
    - [Finding 2: Separate Chapter File Has Invalid References](#finding-2-separate-chapter-file-has-invalid-references)
  - [Section 5: Missing Chapter Cross-Reference Labels](#section-5-missing-chapter-cross-reference-labels)
  - [Section 6: Table of Contents](#section-6-table-of-contents)
  - [Section 7: Bibliography References](#section-7-bibliography-references)
  - [Section 8: Specific Text Replacements Needed](#section-8-specific-text-replacements-needed)
    - [Edit Operations for thesis.tex:](#edit-operations-for-thesistex)
  - [Section 9: Validation Checklist](#section-9-validation-checklist)
  - [Section 10: Special Cases and Considerations](#section-10-special-cases-and-considerations)
    - [Appendices](#appendices)
    - [Section and Subsection Labels](#section-and-subsection-labels)
  - [Section 11: Phase 2 Integration Preparation](#section-11-phase-2-integration-preparation)
  - [Section 12: Automated Update Script (Optional)](#section-12-automated-update-script-optional)
  - [Section 13: Summary Statistics](#section-13-summary-statistics)
  - [Section 14: Old vs. New Numbering Quick Reference](#section-14-old-vs-new-numbering-quick-reference)
  - [Recommendations](#recommendations)
    - [Immediate Actions (This Phase):](#immediate-actions-this-phase)
    - [Phase 2 Actions (Later):](#phase-2-actions-later)
    - [Best Practices:](#best-practices)
  - [Contact Information](#contact-information)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Cross-Reference Mapping and Update Strategy for ggen PhD Thesis

## Document Status
**Generated:** 2026-01-06
**Purpose:** Comprehensive mapping for updating chapter cross-references after Phase 1 integration
**Current State:** Analysis of /home/user/ggen/thesis.tex (1627 lines)

---

## Executive Summary

**GOOD NEWS:** The thesis is in excellent shape for cross-reference updates!

- **No \ref{ch:XXX} commands found** - No LaTeX cross-references need updating
- **Semantic labels stable** - Labels like `\label{ch:introduction}` don't change with renumbering
- **Inline text ALREADY CORRECT** - Thesis structure section (lines 323-334) already describes new numbering
- **Only LaTeX comments need updates** - 9 comments showing old chapter numbers

---

## Current Chapter Structure (After Phase 1 Integration)

| New # | Title | Label | Status |
|-------|-------|-------|--------|
| 1 | Introduction and RDF Ontology Foundations | `ch:introduction` | ✅ Correct |
| 2 | Related Work | `ch:related-work` | ✅ NEW from Agents |
| 3 | Formal Semantics and Theoretical Foundations | `ch:formal-semantics` | ✅ NEW from Agents |
| 4 | SPARQL Query Language and Ontology Querying | `ch:sparql` | ✅ Renumbered (was Ch 2) |
| 5 | Template-Based Code Generation Architecture | `ch:templates` | ⚠️ Renumbered (was Ch 3) |
| 6 | OpenAPI Specification Generation | `ch:openapi` | ⚠️ Renumbered (was Ch 4) |
| 7 | JavaScript/TypeScript Code Generation | `ch:javascript` | ⚠️ Renumbered (was Ch 5) |
| 8 | Zod Validation Schemas and Type Safety | `ch:zod` | ⚠️ Renumbered (was Ch 6) |
| 9 | Type Guards and Runtime Validation | `ch:type-guards` | ⚠️ Renumbered (was Ch 7) |
| 10 | Integration Patterns and Best Practices | `ch:integration` | ⚠️ Renumbered (was Ch 8) |
| 11 | Evaluation & Methodology | (TBD) | 🆕 NEW from Phase 2 |
| 12 | Case Studies | `ch:case-study` | ⚠️ Renumbered (was Ch 9) |
| 13 | Conclusions and Future Work | `ch:conclusions` | ⚠️ Renumbered (was Ch 10) |

---

## Section 1: LaTeX Comments That Need Updating

**Location: thesis.tex**

These are comments only (no functional impact) but should be corrected for accuracy:

### Updates Required:

| Line | Current Comment | Should Be | Status |
|------|----------------|-----------|--------|
| 197 | `% Chapter 1: Introduction` | ✅ CORRECT | No change |
| 337 | `% Chapter 2: Related Work` | ✅ CORRECT | No change |
| 453 | `% Chapter 3: Formal Semantics` | ✅ CORRECT | No change |
| 609 | `% Chapter 4: SPARQL (renumbered from Chapter 2)` | ✅ CORRECT | No change |
| 724 | `% Chapter 3: Template Architecture` | ❌ `% Chapter 5: Template Architecture` | **UPDATE** |
| 807 | `% Chapter 4: OpenAPI Generation` | ❌ `% Chapter 6: OpenAPI Generation` | **UPDATE** |
| 872 | `% Chapter 5: JavaScript/TypeScript Generation` | ❌ `% Chapter 7: JavaScript/TypeScript Generation` | **UPDATE** |
| 923 | `% Chapter 6: Zod Validation` | ❌ `% Chapter 8: Zod Validation` | **UPDATE** |
| 1005 | `% Chapter 7: Type Guards` | ❌ `% Chapter 9: Type Guards` | **UPDATE** |
| 1073 | `% Chapter 8: Integration Patterns` | ❌ `% Chapter 10: Integration Patterns` | **UPDATE** |
| 1192 | `% Chapter 9: Case Study` | ❌ `% Chapter 12: Case Study` | **UPDATE** |
| 1368 | `% Chapter 10: Conclusions` | ❌ `% Chapter 13: Conclusions` | **UPDATE** |

**Total Updates:** 8 comment lines

---

## Section 2: Inline Text References (ALREADY CORRECT!)

**Location: Lines 323-334 in thesis.tex**

These describe the chapter structure and are **ALREADY CORRECT** - no updates needed:

```latex
\begin{description}
    \item[Chapter 2] surveys related work across semantic web, code generation, API specifications, type systems, and multi-artifact consistency
    \item[Chapter 3] presents formal semantics and theoretical foundations, including determinism and type soundness theorems
    \item[Chapter 4] covers SPARQL fundamentals and how graph pattern matching enables data extraction for code generation
    \item[Chapter 5] presents the template-based code generation architecture
    \item[Chapter 6] demonstrates OpenAPI specification generation from ontologies
    \item[Chapter 7] discusses JavaScript/TypeScript code generation
    \item[Chapter 8] covers Zod validation schemas and runtime type safety
    \item[Chapter 9] presents type guards and runtime validation mechanisms
    \item[Chapter 10] provides integration patterns across full-stack architectures
    \item[Chapter 11] presents comprehensive evaluation methodology with research questions and hypotheses
    \item[Chapter 12] presents detailed case studies including Blog API, E-commerce Platform, and Microservices Architecture
    \item[Chapter 13] discusses conclusions, expanded limitations discussion, and future research directions
\end{description}
```

**Status:** ✅ No changes required

---

## Section 3: Chapter Labels (Semantic, Not Numerical)

**All chapter labels use semantic names, not numbers, so they are STABLE:**

| Label | Chapter | Line | Status |
|-------|---------|------|--------|
| `\label{ch:introduction}` | Ch 1 | 199 | ✅ No change |
| `\label{ch:related-work}` | Ch 2 | 339 | ✅ No change |
| `\label{ch:formal-semantics}` | Ch 3 | 455 | ✅ No change |
| `\label{ch:sparql}` | Ch 4 | 611 | ✅ No change |
| `\label{ch:templates}` | Ch 5 | 726 | ✅ No change |
| `\label{ch:openapi}` | Ch 6 | 809 | ✅ No change |
| `\label{ch:javascript}` | Ch 7 | 874 | ✅ No change |
| `\label{ch:zod}` | Ch 8 | 925 | ✅ No change |
| `\label{ch:type-guards}` | Ch 9 | 1007 | ✅ No change |
| `\label{ch:integration}` | Ch 10 | 1075 | ✅ No change |
| `\label{ch:case-study}` | Ch 12 | 1194 | ✅ No change |
| `\label{ch:conclusions}` | Ch 13 | 1370 | ✅ No change |

**Status:** ✅ No updates needed - semantic labels are renumbering-proof!

---

## Section 4: Cross-Reference Analysis

### Finding 1: No \ref{} Commands in Main Thesis

**Search Pattern:** `\ref{`
**Results:** NONE found in thesis.tex
**Implication:** ✅ No cross-references to update

### Finding 2: Separate Chapter File Has Invalid References

**File:** /home/user/ggen/docs/thesis/chapter-02-sparql-query-language.tex
**Lines 989-990:**

```latex
from OpenAPI specification extraction (Chapter~\ref{ch:openapi-generation})
to Rust trait implementation (Chapter~\ref{ch:rust-generation})
and validation infrastructure (Chapter~\ref{ch:validation}).
```

**Problem:** These labels don't exist in thesis.tex:
- `ch:openapi-generation` ❌ (should be `ch:openapi`)
- `ch:rust-generation` ❌ (doesn't exist - thesis has no Rust generation chapter)
- `ch:validation` ❌ (should be `ch:zod` or `ch:type-guards`)

**Action Required:** Fix these references if this chapter is integrated, or remove the file if it's superseded by the integrated content in thesis.tex.

---

## Section 5: Missing Chapter Cross-Reference Labels

Based on the intended structure, these labels should exist but don't:

| Intended Chapter | Missing Label | Current Status |
|-----------------|---------------|----------------|
| Ch 11: Evaluation & Methodology | `\label{ch:evaluation}` | 🆕 Needs creation |
| Ch 12: Case Studies (expanded) | `\label{ch:case-studies}` | Current has `ch:case-study` (singular) |
| Ch 13: Conclusions (expanded) | Already exists as `ch:conclusions` | ✅ OK |

**Note:** Chapter 11 will be added in Phase 2, Chapter 12 will be expanded with new case studies.

---

## Section 6: Table of Contents

**Status:** ✅ Auto-generated by LaTeX `\tableofcontents` command
**Action Required:** None - LaTeX will automatically update when thesis is recompiled

---

## Section 7: Bibliography References

**Search Pattern:** `\bibitem{`, `\cite{`
**Status:** ✅ No changes needed - bibliography is author-based, not chapter-based
**Note:** All citations use semantic keys like `\cite{w3c:rdf11:2014}`, not chapter numbers

---

## Section 8: Specific Text Replacements Needed

### Edit Operations for thesis.tex:

```latex
# Edit 1: Line 724
OLD: % Chapter 3: Template Architecture
NEW: % Chapter 5: Template Architecture

# Edit 2: Line 807
OLD: % Chapter 4: OpenAPI Generation
NEW: % Chapter 6: OpenAPI Generation

# Edit 3: Line 872
OLD: % Chapter 5: JavaScript/TypeScript Generation
NEW: % Chapter 7: JavaScript/TypeScript Generation

# Edit 4: Line 923
OLD: % Chapter 6: Zod Validation
NEW: % Chapter 8: Zod Validation

# Edit 5: Line 1005
OLD: % Chapter 7: Type Guards
NEW: % Chapter 9: Type Guards

# Edit 6: Line 1073
OLD: % Chapter 8: Integration Patterns
NEW: % Chapter 10: Integration Patterns

# Edit 7: Line 1192
OLD: % Chapter 9: Case Study
NEW: % Chapter 12: Case Study

# Edit 8: Line 1368
OLD: % Chapter 10: Conclusions
NEW: % Chapter 13: Conclusions
```

---

## Section 9: Validation Checklist

After applying updates, verify:

- [ ] All LaTeX comments show correct chapter numbers
- [ ] Thesis structure section (lines 323-334) remains correct
- [ ] All `\label{ch:XXX}` commands intact
- [ ] No broken `\ref{}` commands (grep for `\ref{ch:`)
- [ ] LaTeX compiles without warnings: `pdflatex thesis.tex`
- [ ] Table of contents shows correct chapter numbers
- [ ] Bibliography compiles: `bibtex thesis && pdflatex thesis.tex && pdflatex thesis.tex`

---

## Section 10: Special Cases and Considerations

### Appendices

**Current Structure:**
- Appendix A: SPARQL Query Reference (label: `app:sparql-reference`)
- Appendix B: Template Reference (label: `app:template-reference`)
- Appendix C: RDF Schema Reference (label: `app:rdf-schema`)

**Status:** ✅ Appendices are lettered (A, B, C), not numbered - no changes needed

### Section and Subsection Labels

**Pattern:** All section labels use semantic names:
- `\label{sec:problem-statement}`
- `\label{sec:sparql-fundamentals}`
- `\label{subsec:triple-model}`

**Status:** ✅ No numerical labels - all stable

---

## Section 11: Phase 2 Integration Preparation

When adding Chapter 11 (Evaluation & Methodology):

1. **Insert after line 1191** (before current Case Study chapter)
2. **Add label:** `\label{ch:evaluation}`
3. **Update thesis structure description** in Chapter 1 (lines 323-334) if content changes
4. **Update comment:** Add `% Chapter 11: Evaluation & Methodology`
5. **Verify numbering:** Ensure Case Study becomes Ch 12, Conclusions becomes Ch 13

When expanding Chapter 12 (Case Studies):

1. **Consider label change:** `ch:case-study` → `ch:case-studies` (plural)
2. **Update references:** Any `\ref{ch:case-study}` → `\ref{ch:case-studies}`
3. **Update structure description** if case study count changes

---

## Section 12: Automated Update Script (Optional)

```bash
#!/bin/bash
# Update chapter number comments in thesis.tex

sed -i 's/% Chapter 3: Template Architecture/% Chapter 5: Template Architecture/' thesis.tex
sed -i 's/% Chapter 4: OpenAPI Generation/% Chapter 6: OpenAPI Generation/' thesis.tex
sed -i 's/% Chapter 5: JavaScript\/TypeScript Generation/% Chapter 7: JavaScript\/TypeScript Generation/' thesis.tex
sed -i 's/% Chapter 6: Zod Validation/% Chapter 8: Zod Validation/' thesis.tex
sed -i 's/% Chapter 7: Type Guards/% Chapter 9: Type Guards/' thesis.tex
sed -i 's/% Chapter 8: Integration Patterns/% Chapter 10: Integration Patterns/' thesis.tex
sed -i 's/% Chapter 9: Case Study/% Chapter 12: Case Study/' thesis.tex
sed -i 's/% Chapter 10: Conclusions/% Chapter 13: Conclusions/' thesis.tex

echo "✅ Chapter comments updated"
echo "⚠️  Manual verification required - review changes before committing"
```

---

## Section 13: Summary Statistics

| Category | Count | Status |
|----------|-------|--------|
| Total chapters | 13 | ✅ Defined |
| LaTeX comment updates | 8 | ⚠️ Required |
| Inline text updates | 0 | ✅ Already correct |
| Label updates | 0 | ✅ Semantic labels stable |
| Cross-reference updates | 0 | ✅ None found |
| New chapters (Phase 2) | 1 | 🆕 Pending |
| Expanded chapters (Phase 2) | 2 | 🆕 Pending |

---

## Section 14: Old vs. New Numbering Quick Reference

| Old # | New # | Chapter Title | Label |
|-------|-------|---------------|-------|
| 1 | 1 | Introduction | `ch:introduction` |
| - | 2 | Related Work (NEW) | `ch:related-work` |
| - | 3 | Formal Semantics (NEW) | `ch:formal-semantics` |
| 2 | 4 | SPARQL | `ch:sparql` |
| 3 | 5 | Templates | `ch:templates` |
| 4 | 6 | OpenAPI | `ch:openapi` |
| 5 | 7 | JavaScript/TypeScript | `ch:javascript` |
| 6 | 8 | Zod | `ch:zod` |
| 7 | 9 | Type Guards | `ch:type-guards` |
| 8 | 10 | Integration | `ch:integration` |
| - | 11 | Evaluation (NEW - Phase 2) | TBD |
| 9 | 12 | Case Studies | `ch:case-study` |
| 10 | 13 | Conclusions | `ch:conclusions` |

---

## Recommendations

### Immediate Actions (This Phase):
1. ✅ Update 8 LaTeX comments showing old chapter numbers
2. ✅ Verify thesis compiles with `pdflatex thesis.tex`
3. ✅ Review generated TOC for correct numbering

### Phase 2 Actions (Later):
1. 🆕 Add Chapter 11: Evaluation & Methodology
2. 🆕 Expand Chapter 12: Case Studies with additional case studies
3. 🆕 Expand Chapter 13: Conclusions with enhanced future work
4. ✅ Update thesis structure description in Chapter 1 if content changes significantly

### Best Practices:
- ✅ **Continue using semantic labels** (not `ch:2`, use `ch:sparql`)
- ✅ **Test compilation after each change** with `pdflatex && bibtex && pdflatex && pdflatex`
- ✅ **Maintain this mapping document** as chapters evolve
- ✅ **Use git commits** to track changes: `git add thesis.tex && git commit -m "docs: Update chapter numbering comments"`

---

## Contact Information

**Document Maintainer:** ggen PhD Thesis Integration Agent
**Last Updated:** 2026-01-06
**Version:** 1.0
**Related Documents:**
- `/home/user/ggen/thesis.tex` (main thesis)
- `/home/user/ggen/docs/thesis/chapter-02-sparql-query-language.tex` (standalone chapter - needs review)
- `/home/user/ggen/docs/thesis/GLOSSARY_LATEX.tex` (glossary)

---

**END OF MAPPING DOCUMENT**
