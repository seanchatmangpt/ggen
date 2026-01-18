# Implementation Plan: Grand Unified KGC Thesis

**Branch**: `012-grand-unified-kgc-thesis` | **Date**: 2025-12-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/012-grand-unified-kgc-thesis/spec.md`

## Summary

Generate a complete PhD thesis document on "Grand Unified Theory of Full-Stack Knowledge Graph Completeness" from RDF ontology using ggen. The thesis proves that deterministic code generation from semantic ontologies eliminates specification-implementation drift across the technology stack, integrating ggen, @unrdf/hooks, @unrdf/kgc-4d, NextJS, TanStack, and Electric SQL.

**Technical Approach**: Extend existing `examples/thesis-gen` patterns with 15+ generation rules, 17 entity classes, 30+ SPARQL queries, and 14 Tera templates to produce publication-ready LaTeX with amsthm theorems, algorithm2e pseudocode, and biblatex bibliography.

## Technical Context

**Language/Version**: Rust 1.75+ (ggen v5), LaTeX (pdflatex + biber)
**Primary Dependencies**: ggen-core, Oxigraph (RDF), Tera (templates), memoir (LaTeX), amsthm, biblatex, algorithm2e
**Storage**: File system (RDF ontology .ttl files, generated .tex files)
**Testing**: `cargo make test` (ggen), `pdflatex` compilation verification
**Target Platform**: macOS/Linux CLI, PDF output
**Project Type**: Single project - thesis content generation
**Performance Goals**: <5s generation for 10K triples, first-compile success rate >99%
**Constraints**: <200MB memory, deterministic output (byte-identical on regeneration)
**Scale/Scope**: 100-200 page thesis, 5000-10000 RDF triples, 15 LaTeX files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: Uses existing ggen-core crate infrastructure. No new crates needed - content generation via templates.
- [x] **II. Deterministic RDF Projections**: All SPARQL queries use ORDER BY for deterministic results. Templates have zero hardcoded strings. Regeneration produces identical output.
- [x] **III. Chicago TDD**: Tests verify observable LaTeX output against expected structure. Real Oxigraph collaborator. 80%+ coverage via generation rule tests.
- [x] **IV. cargo make Protocol**: All commands via `cargo make check`, `cargo make test`, `ggen sync`. SLOs: <5s generation, <15s compilation.
- [x] **V. Type-First Thinking**: RDF schema defines all entity types. SPARQL variables typed via XSD. Tera templates type-safe.
- [x] **VI. Andon Signal Protocol**: RED: pdflatex errors, undefined refs. YELLOW: LaTeX warnings. GREEN: clean compilation. Stop on RED.
- [x] **VII. Error Handling**: ggen uses `Result<T, E>`. Template rendering errors propagate. No unwrap in production code.
- [x] **VIII. Concurrent Execution**: Batch all file writes in single ggen sync. Files organized in specs/012-*/output/ (not root).
- [x] **IX. Lean Six Sigma Quality**: Pre-commit hooks run ggen check. Full validation via `cargo make ci`. Zero tolerance for compilation errors.

**Quality Gates Pass?**: [x] YES

## Project Structure

### Documentation (this feature)

```text
specs/012-grand-unified-kgc-thesis/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 research output
├── data-model.md        # Phase 1 entity definitions
├── quickstart.md        # Phase 1 usage guide
├── contracts/           # Phase 1 API contracts
│   └── thesis-generation.yaml
├── checklists/
│   └── requirements.md  # Spec validation checklist
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (this feature)

```text
specs/012-grand-unified-kgc-thesis/
├── ggen.toml                    # Generation manifest (15 rules)
├── ontology/
│   ├── thesis-schema.ttl        # Class/property definitions
│   └── kgc-unified-content.ttl  # Thesis content (7 chapters, 10+ theorems, 20+ equations)
├── templates/
│   ├── thesis-main.tera         # Main document scaffold
│   ├── front-matter.tera        # Title, abstract, dedication
│   ├── chapter.tera             # Chapter content with sections
│   ├── theorem.tera             # Theorems, lemmas, definitions
│   ├── equation.tera            # Mathematical equations
│   ├── algorithm.tera           # Pseudocode blocks
│   ├── figure.tera              # Figures with captions
│   ├── table.tera               # Tabular data
│   ├── bibliography.tera        # BibTeX entries
│   ├── appendix.tera            # Appendices
│   ├── code-listing.tera        # Code examples
│   ├── subsection.tera          # Nested content
│   ├── preamble.tera            # LaTeX configuration
│   └── chapter-index.tera       # Navigation
└── output/                      # Generated LaTeX files
    ├── thesis.tex
    ├── front-matter.tex
    ├── chapters/
    │   └── all-chapters.tex
    ├── theorems.tex
    ├── equations.tex
    ├── algorithms.tex
    ├── figures.tex
    ├── tables.tex
    ├── references.bib
    ├── appendices.tex
    ├── code-listings.tex
    ├── subsections.tex
    └── preamble.tex
```

**Structure Decision**: Single project structure using existing ggen infrastructure. All thesis-specific content in `specs/012-grand-unified-kgc-thesis/` following speckit conventions. No new Rust crates required - extends existing thesis-gen patterns.

## Complexity Tracking

> No violations - all constitution principles satisfied.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |

## Architecture Decisions

### ADR-001: Extend thesis-gen Patterns
**Decision**: Reuse proven query/template patterns from `examples/thesis-gen`
**Rationale**: 15+ working generation rules, 1646-line content ontology as reference
**Alternatives Rejected**: Custom thesis format (no proven patterns)

### ADR-002: memoir Document Class
**Decision**: Use memoir LaTeX class with biblatex/biber
**Rationale**: Incorporates 30+ packages, 99%+ first-compile success, active maintenance
**Alternatives Rejected**: book (less flexible), report (limited customization)

### ADR-003: Zero Hardcoding Principle
**Decision**: All content from SPARQL queries, templates are structure only
**Rationale**: Enables deterministic regeneration, single source of truth in ontology
**Alternatives Rejected**: Hybrid templates with some hardcoded content (breaks reproducibility)

### ADR-004: amsthm + thmtools for Theorems
**Decision**: Use amsthm with thmtools wrapper for theorem environments
**Rationale**: AMS-LaTeX standard, unified customization interface, proven stability
**Alternatives Rejected**: ntheorem (less widely adopted)

### ADR-005: algorithm2e for Pseudocode
**Decision**: Use algorithm2e package for algorithm typesetting
**Rationale**: Comprehensive features, active maintenance, clean syntax
**Alternatives Rejected**: algorithmicx (more verbose), algorithmic (deprecated)

### ADR-006: Layered Ontology Structure
**Decision**: Separate schema (thesis-schema.ttl) from content (kgc-unified-content.ttl)
**Rationale**: Schema reusable, content specific to this thesis
**Alternatives Rejected**: Single monolithic ontology (harder to maintain)

### ADR-007: cleveref Last Load Order
**Decision**: Load cleveref AFTER hyperref (critical for cross-reference resolution)
**Rationale**: Known LaTeX package conflict - cleveref must be absolute last
**Alternatives Rejected**: Different package ordering (causes "??" undefined refs)

## Implementation Phases

### Phase 1: Ontology & Templates (Days 1-3)
- Create thesis-schema.ttl with 17 entity classes
- Create kgc-unified-content.ttl with 7 chapters, thesis content
- Create 14 Tera templates
- Create ggen.toml manifest with 15 generation rules

### Phase 2: Content Authoring (Days 4-7)
- Author 7 chapters in RDF (Introduction, Foundations, Info Theory, KGC-4D, Case Study, @unrdf, Conclusions)
- Define 10+ theorems with formal proofs
- Define 20+ equations with LaTeX notation
- Define 5+ algorithms with pseudocode
- Add 30+ bibliography references

### Phase 3: Generation & Validation (Days 8-10)
- Run `ggen sync` to generate LaTeX
- Compile with pdflatex/biber
- Verify cross-references resolve
- Verify bibliography renders
- Validate 100+ page output

## Success Metrics

| Metric | Target | Verification |
|--------|--------|--------------|
| Generation time | <5s | `time ggen sync` |
| First compile errors | 0 | `pdflatex thesis.tex` |
| Undefined references | 0 | grep for "??" in PDF |
| Page count | 100+ | PDF properties |
| Theorem count | 10+ | grep for `\begin{theorem}` |
| Equation count | 20+ | grep for `\begin{equation}` |
| Bibliography entries | 30+ | wc -l references.bib |
| Regeneration identical | Yes | diff after re-run |

## Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| ggen | v5 | Code generation engine |
| Oxigraph | latest | RDF processing |
| Tera | latest | Template rendering |
| pdflatex | TeX Live 2024 | LaTeX compilation |
| biber | 2.19+ | Bibliography processing |
| memoir | CTAN | Document class |
| amsmath | CTAN | Mathematics |
| amsthm | CTAN | Theorem environments |
| algorithm2e | CTAN | Pseudocode |
| biblatex | CTAN | Bibliography |
| hyperref | CTAN | Cross-references |
| cleveref | CTAN | Smart references |

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Complex LaTeX cross-refs | HIGH | Use consistent labelId pattern, test compilation after each chapter |
| Large ontology performance | MEDIUM | Benchmark with 5K+ triples, optimize SPARQL if needed |
| Bibliography format errors | MEDIUM | Validate each Reference entity against BibTeX schema |
| Hyperdimensional math notation | LOW | Define standard notation in preamble, use consistent symbols |

---

**Plan Status**: COMPLETE
**Ready for**: `/speckit.tasks` to generate implementation tasks
