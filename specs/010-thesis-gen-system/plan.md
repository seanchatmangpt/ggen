# Implementation Plan: Reusable Ontology-Driven PhD Thesis Generation System

**Branch**: `010-thesis-gen-system` | **Date**: 2025-12-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/010-thesis-gen-system/spec.md`

## Summary

Build a reusable thesis generation system where `ggen sync` transforms RDF ontologies into complete 50+ page PhD theses. ALL text content (title, chapters, theorems, equations, figures, tables, bibliography) comes from SPARQL queries—templates contain ONLY structural LaTeX markup and Tera variable placeholders with zero hardcoded strings.

The system leverages ggen's existing RDF processing pipeline (Oxigraph), SPARQL execution, and Tera template rendering to produce LaTeX output that compiles to publication-ready PDFs.

## Technical Context

**Language/Version**: Rust 1.75+ (ggen existing codebase) + LaTeX (pdflatex for output)
**Primary Dependencies**: Oxigraph (RDF), Tera (templates), existing ggen-core/ggen-domain crates
**Storage**: File system (TTL ontology files, Tera templates, generated LaTeX output)
**Testing**: Chicago TDD with `cargo make test`, LaTeX compilation validation
**Target Platform**: CLI (macOS/Linux), LaTeX compilation to PDF
**Project Type**: Single project (ggen workspace extension + thesis example)
**Performance Goals**: <5s generation for 50-page thesis ontology, <100MB memory
**Constraints**: Zero hardcoded strings in templates, deterministic output, reusable across topics
**Scale/Scope**: 7+ chapters, 30+ sections, 50+ pages, 30+ references per thesis

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: Feature is an example/template project using existing ggen crates (no new crate needed—uses ggen-core, ggen-domain). Self-contained thesis example directory.
- [x] **II. Deterministic RDF Projections**: ggen sync already guarantees determinism. Same ontology + templates → identical LaTeX output. Checksums verifiable.
- [x] **III. Chicago TDD**: Tests will verify: (1) SPARQL queries return expected data, (2) templates render without hardcoded strings, (3) LaTeX compiles successfully, (4) PDF page count meets target.
- [x] **IV. cargo make Protocol**: All validation uses `cargo make` targets. LaTeX compilation scripted in Makefile.toml.
- [x] **V. Type-First Thinking**: Ontology schema defines types. SPARQL queries are typed. Template variables have default handling.
- [x] **VI. Andon Signal Protocol**: ggen sync errors = RED, LaTeX warnings = YELLOW, successful PDF = GREEN.
- [x] **VII. Error Handling**: ggen sync uses Result<T,E> throughout. Template missing variables handled gracefully with defaults.
- [x] **VIII. Concurrent Execution**: All file generation batched in single ggen sync. Templates/ontology organized in proper directories.
- [x] **IX. Lean Six Sigma Quality**: Pre-commit hooks, template validation, schema conformance checks.

**Quality Gates Pass?**: [x] YES / [ ] NO

## Project Structure

### Documentation (this feature)

```text
specs/010-thesis-gen-system/
├── plan.md              # This file
├── research.md          # Phase 0: Template patterns, LaTeX best practices
├── data-model.md        # Phase 1: Ontology schema for thesis entities
├── quickstart.md        # Phase 1: How to create a thesis with ggen
├── contracts/           # Phase 1: Ontology schema (SHACL shapes)
│   └── thesis-schema.ttl
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
examples/thesis-gen/
├── ggen.toml                    # Manifest with 15+ generation rules
├── ontology/
│   ├── thesis-schema.ttl        # Class/property definitions (reusable)
│   └── thesis-content.ttl       # Instance data (topic-specific)
├── templates/
│   ├── thesis-main.tera         # Main document structure
│   ├── front-matter.tera        # Title, abstract, acknowledgments
│   ├── chapter.tera             # Chapter with sections
│   ├── section.tera             # Section with paragraphs
│   ├── theorem.tera             # Theorem/lemma/proof environments
│   ├── equation.tera            # Math equation blocks
│   ├── algorithm.tera           # Pseudocode blocks
│   ├── figure.tera              # Figure environments
│   ├── table.tera               # Table environments
│   ├── bibliography.tera        # BibTeX generation
│   └── appendix.tera            # Appendix sections
├── output/                      # Generated LaTeX files
│   ├── thesis.tex
│   ├── chapters/
│   ├── references.bib
│   └── figures/
└── Makefile                     # pdflatex compilation

tests/thesis-gen/
├── test_ontology_schema.rs      # Schema validation tests
├── test_template_variables.rs   # No hardcoded strings verification
├── test_latex_compilation.rs    # LaTeX builds successfully
└── test_page_count.rs           # 50+ pages verification
```

**Structure Decision**: Example project in `examples/thesis-gen/` demonstrating ggen's thesis generation capability. Uses existing ggen crates—no new workspace crates needed. Tests in `tests/thesis-gen/` for integration testing.

## Complexity Tracking

> No violations—feature uses existing ggen architecture without modifications.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |

## Phase Completion Status

### Phase 0: Research ✅ COMPLETE

- [x] Template design patterns (zero hardcoding via Tera variables)
- [x] LaTeX environment modeling (environments as RDF data)
- [x] Page count calculation (content density requirements)
- [x] Cross-reference strategy (labelId properties)
- [x] BibTeX field mapping

**Output**: `research.md`

### Phase 1: Design & Contracts ✅ COMPLETE

- [x] Entity definitions (Thesis, Chapter, Section, Theorem, Equation, etc.)
- [x] Property specifications (40+ properties defined)
- [x] Ontology schema (SHACL-ready TTL file)
- [x] Quickstart guide for users
- [x] Agent context updated

**Outputs**:
- `data-model.md` - Complete entity-relationship model
- `contracts/thesis-schema.ttl` - Reusable ontology schema (450+ lines)
- `quickstart.md` - User guide for thesis generation

### Phase 2: Task Generation 🔜 PENDING

Run `/speckit.tasks` to generate actionable task breakdown.

## Constitution Re-Check (Post-Design)

*Re-verified after Phase 1 design completion:*

- [x] **I. Crate-First**: Example project uses existing crates ✓
- [x] **II. Deterministic**: Schema + content separation ensures reproducibility ✓
- [x] **III. Chicago TDD**: Test strategy defined in project structure ✓
- [x] **IV. cargo make**: Makefile.toml integration planned ✓
- [x] **V. Type-First**: Ontology schema is type system ✓
- [x] **VI. Andon Signals**: Error handling documented ✓
- [x] **VII. Error Handling**: ggen sync uses Result<T,E> ✓
- [x] **VIII. Concurrent**: Single ggen sync batches all generation ✓
- [x] **IX. Quality**: Schema validation + template checks ✓

**Quality Gates Pass?**: [x] YES - Ready for `/speckit.tasks`
