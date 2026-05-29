# Tasks: Reusable Ontology-Driven PhD Thesis Generation System

**Input**: Design documents from `/specs/010-thesis-gen-system/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/thesis-schema.ttl

**Tests**: Not explicitly requested - validation via ggen sync and LaTeX compilation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- All paths relative to `examples/thesis-gen/`

---

## Phase 1: Setup (Project Initialization)

**Purpose**: Create example project structure and copy schema

- [X] T001 Create directory structure: `examples/thesis-gen/{ontology,templates,output,figures}/`
- [X] T002 [P] Copy schema from `specs/010-thesis-gen-system/contracts/thesis-schema.ttl` to `examples/thesis-gen/ontology/thesis-schema.ttl`
- [X] T003 [P] Create `.gitignore` in `examples/thesis-gen/` (ignore output/*.aux, *.log, *.bbl, *.blg)
- [X] T004 [P] Create `examples/thesis-gen/Makefile` with pdflatex/bibtex compilation targets

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure required by ALL user stories

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Core Manifest (ggen.toml)

- [X] T005 Create `examples/thesis-gen/ggen.toml` with project metadata and ontology prefixes
- [X] T006 Add ontology source configuration to `ggen.toml` pointing to `ontology/thesis-schema.ttl` and `ontology/thesis-content.ttl`

### LaTeX Package Configuration (ontology-driven)

- [X] T007 [P] Define LaTeX package requirements in ontology: `ontology/thesis-schema.ttl` (add `thesis:RequiredPackage` instances)
- [X] T008 [P] Define document class and geometry settings in ontology (12pt, a4paper, 1in margins)

### Core Ontology Content Structure

- [X] T009 Create `examples/thesis-gen/ontology/thesis-content.ttl` with thesis metadata (title, author, institution, date)
- [X] T010 Add abstract, dedication, acknowledgments to thesis-content.ttl (200-500 words each)

**Checkpoint**: Foundation ready - ontology structure established, ggen.toml configured

---

## Phase 3: User Story 1 - Generate Complete Thesis (Priority: P1) 🎯 MVP

**Goal**: Generate 50+ page PDF with ALL content from ontology, zero hardcoded strings in templates

**Independent Test**: Run `ggen sync && cd output && make pdf` and verify 50+ page PDF with content from ontology

### Templates for Basic Document Structure

- [X] T011 [P] [US1] Create `templates/thesis-main.tera` - main document with `\documentclass`, packages, `\begin{document}`, includes (NO hardcoded strings except LaTeX commands sourced from ontology)
- [X] T012 [P] [US1] Create `templates/front-matter.tera` - title page, abstract, dedication, acknowledgments (all from SPARQL variables)
- [X] T013 [P] [US1] Create `templates/toc.tera` - table of contents, list of figures, list of tables

### Chapter and Section Templates

- [X] T014 [P] [US1] Create `templates/chapter.tera` - chapter structure with `\chapter{}` from ontology variables
- [X] T015 [P] [US1] Create `templates/section.tera` - section structure with `\section{}` from ontology variables
- [X] T016 [P] [US1] Create `templates/subsection.tera` - subsection structure from ontology variables

### Thesis Content (7 Chapters, 30+ Sections)

- [X] T017 [US1] Add Chapter 1 (Introduction) to `ontology/thesis-content.ttl` with 4 sections (~2000 words)
- [X] T018 [P] [US1] Add Chapter 2 (Theoretical Foundations) with 5 sections (~2500 words)
- [X] T019 [P] [US1] Add Chapter 3 (ggen Architecture) with 5 sections (~2500 words)
- [X] T020 [P] [US1] Add Chapter 4 (ASTRO Case Study) with 5 sections (~2500 words)
- [X] T021 [P] [US1] Add Chapter 5 (Figex Case Study) with 5 sections (~2500 words)
- [X] T022 [P] [US1] Add Chapter 6 (Methodology Synthesis) with 4 sections (~2000 words)
- [X] T023 [P] [US1] Add Chapter 7 (Conclusion) with 3 sections (~1500 words)

### Generation Rules

- [X] T024 [US1] Add generation rule for `thesis-main` in `ggen.toml` (SELECT thesis metadata)
- [X] T025 [P] [US1] Add generation rule for `front-matter` in `ggen.toml`
- [X] T026 [P] [US1] Add generation rule for `toc` in `ggen.toml`
- [X] T027 [P] [US1] Add generation rule for `chapters` in `ggen.toml` (SELECT all chapters ORDER BY orderIndex)
- [X] T028 [P] [US1] Add generation rule for `sections` in `ggen.toml` (SELECT all sections with chapter relation)

### Validation

- [X] T029 [US1] Run `ggen sync` in `examples/thesis-gen/` - verify no template errors
- [X] T030 [US1] Run `pdflatex` and `bibtex` in `output/` - verify compilation succeeds
- [X] T031 [US1] Verify generated PDF is minimum 50 pages

**Checkpoint**: User Story 1 complete - basic thesis generates with all content from ontology

---

## Phase 4: User Story 2 - Template Reusability (Priority: P1)

**Goal**: Same templates work with different thesis content ontology

**Independent Test**: Create alternate `thesis-content-alt.ttl`, run `ggen sync`, verify valid thesis output

### Reusability Verification

- [X] T032 [US2] Create `examples/thesis-gen/ontology/thesis-content-alt.ttl` with different topic (AI Ethics in Healthcare)
- [X] T033 [P] [US2] Add 7 chapters with different titles/content to `thesis-content-alt.ttl`
- [X] T034 [P] [US2] Add thesis metadata (different author, title, institution) to `thesis-content-alt.ttl`
- [X] T035 [US2] Update `ggen.toml` to use `thesis-content-alt.ttl` (or create `ggen-alt.toml`)
- [X] T036 [US2] Run `ggen sync` with alternate ontology - verify same templates render successfully
- [X] T037 [US2] Compile alternate thesis PDF - verify different content appears

### Zero Hardcoding Verification

- [X] T038 [US2] Audit all templates: grep for hardcoded English strings (should find NONE except LaTeX commands)
- [X] T039 [US2] Document template reusability in `examples/thesis-gen/README.md`

**Checkpoint**: User Stories 1 AND 2 complete - templates proven reusable across topics

---

## Phase 5: User Story 3 - Academic Document Elements (Priority: P2)

**Goal**: Theorems, equations, algorithms, figures, tables render with proper LaTeX environments

**Independent Test**: Define each element type in ontology, verify proper LaTeX environment in output

### Theorem/Lemma/Proof Templates

- [X] T040 [P] [US3] Create `templates/theorem.tera` - theorem/lemma/corollary/definition environments from ontology `thesis:LatexEnvironment`
- [X] T041 [P] [US3] Create `templates/proof.tera` - proof environment from ontology
- [X] T042 [US3] Add 10 theorems with proofs to `ontology/thesis-content.ttl` (3 lemmas, 5 theorems, 2 corollaries)
- [X] T043 [US3] Add generation rule for `theorems` in `ggen.toml`

### Equation Templates

- [X] T044 [P] [US3] Create `templates/equation.tera` - equation environment with `\label{}` from ontology
- [X] T045 [US3] Add 20 equations to `ontology/thesis-content.ttl` (entropy, drift metric, generation function, etc.)
- [X] T046 [US3] Add generation rule for `equations` in `ggen.toml`

### Algorithm Templates

- [X] T047 [P] [US3] Create `templates/algorithm.tera` - algorithm environment with input/output/steps from ontology
- [X] T048 [US3] Add 5 algorithms to `ontology/thesis-content.ttl` (ggen sync pipeline, SPARQL execution, template rendering, etc.)
- [X] T049 [US3] Add generation rule for `algorithms` in `ggen.toml`

### Figure Templates

- [X] T050 [P] [US3] Create `templates/figure.tera` - figure environment with `\includegraphics`, caption, label from ontology
- [X] T051 [P] [US3] Add 10 figure placeholder images to `examples/thesis-gen/figures/` (architecture diagrams, flowcharts)
- [X] T052 [US3] Add 10 figure definitions to `ontology/thesis-content.ttl` with paths, captions, labels
- [X] T053 [US3] Add generation rule for `figures` in `ggen.toml`

### Table Templates

- [X] T054 [P] [US3] Create `templates/table.tera` - booktabs table with headers and rows from ontology
- [X] T055 [US3] Add 5 tables to `ontology/thesis-content.ttl` (results, comparisons, metrics)
- [X] T056 [US3] Add generation rule for `tables` in `ggen.toml`

### Validation

- [X] T057 [US3] Run `ggen sync` - verify theorem/equation/algorithm/figure/table files generate
- [X] T058 [US3] Compile PDF - verify all environments render correctly with proper numbering

**Checkpoint**: User Story 3 complete - all academic elements render with proper LaTeX environments

---

## Phase 6: User Story 4 - Bibliography from Ontology (Priority: P2)

**Goal**: Generate valid BibTeX file from ontology-defined references

**Independent Test**: Define 30 references in ontology, run `ggen sync`, verify `references.bib` compiles with bibtex

### Bibliography Template

- [X] T059 [P] [US4] Create `templates/bibliography.tera` - BibTeX entry generation from ontology Reference entities
- [X] T060 [P] [US4] Create `templates/back-matter.tera` - `\bibliographystyle{plainnat}` and `\bibliography{references}` from ontology

### Reference Content

- [X] T061 [US4] Add 30 references to `ontology/thesis-content.ttl` (mix of @article, @inproceedings, @book, @phdthesis, @misc)
- [X] T062 [P] [US4] Add citations (`\cite{}`) to chapter content in `ontology/thesis-content.ttl`

### Generation Rules

- [X] T063 [US4] Add generation rule for `bibliography` in `ggen.toml` outputting `references.bib`
- [X] T064 [US4] Add generation rule for `back-matter` in `ggen.toml`

### Validation

- [X] T065 [US4] Run `ggen sync` - verify `output/references.bib` generates
- [X] T066 [US4] Run `bibtex thesis` - verify no undefined citations
- [X] T067 [US4] Verify all 30 references appear in bibliography section of PDF

**Checkpoint**: User Story 4 complete - bibliography generates from ontology, all citations resolve

---

## Phase 7: User Story 5 - Appendices and Code Listings (Priority: P3)

**Goal**: Generate appendices with code listings from ontology

**Independent Test**: Define appendix content in ontology, verify proper lettered numbering in PDF

### Appendix Templates

- [X] T068 [P] [US5] Create `templates/appendix.tera` - appendix environment with letter and content from ontology
- [X] T069 [P] [US5] Create `templates/code-listing.tera` - lstlisting environment with language, caption from ontology

### Appendix Content

- [X] T070 [US5] Add Appendix A (Full Ontology Schema) to `ontology/thesis-content.ttl`
- [X] T071 [P] [US5] Add Appendix B (Sample Templates) with code listings
- [X] T072 [P] [US5] Add Appendix C (SPARQL Query Examples) with code listings

### Generation Rules

- [X] T073 [US5] Add generation rule for `appendices` in `ggen.toml`
- [X] T074 [US5] Add generation rule for `code-listings` in `ggen.toml`

### Validation

- [X] T075 [US5] Run `ggen sync` - verify appendix files generate
- [X] T076 [US5] Compile PDF - verify Appendix A, B, C appear with proper lettering after main content

**Checkpoint**: User Story 5 complete - appendices with code listings render correctly

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final integration, documentation, quality assurance

### Cross-Reference Verification

- [X] T077 [P] Verify all `\ref{}` cross-references resolve (figures, tables, theorems, equations)
- [X] T078 [P] Verify all `\cite{}` citations resolve (bibtex runs clean)
- [X] T079 Verify table of contents shows all chapters with correct page numbers

### Documentation

- [X] T080 [P] Create `examples/thesis-gen/README.md` with usage instructions
- [X] T081 [P] Update `specs/010-thesis-gen-system/quickstart.md` with final paths and examples
- [X] T082 [P] Add example output screenshots to `examples/thesis-gen/docs/`

### Quality Assurance

- [X] T083 Final `ggen sync` - verify <5s generation time
- [X] T084 Final PDF compilation - verify zero errors, <5 warnings
- [X] T085 Verify final PDF page count is 50+ pages
- [X] T086 Template audit: confirm 100% of visible text from ontology (zero hardcoded strings)

### Cleanup

- [X] T087 [P] Remove any test/debug content from ontology files
- [X] T088 [P] Organize output directory structure (thesis.tex, chapters/, references.bib)
- [X] T089 Final commit with all generated examples

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - Core MVP
- **User Story 2 (Phase 4)**: Depends on US1 completion (reuses templates)
- **User Stories 3-5 (Phases 5-7)**: Depend on Foundational, can run in parallel with each other
- **Polish (Phase 8)**: Depends on all user stories completing

### User Story Dependencies

```
Setup (Phase 1)
    │
    ▼
Foundational (Phase 2) ─────────────────┐
    │                                    │
    ▼                                    │
US1: Basic Thesis (P1) 🎯 MVP           │
    │                                    │
    ▼                                    │
US2: Reusability (P1)                   │
                                         │
┌────────────────────────────────────────┘
│
├──▶ US3: Academic Elements (P2) ──┐
├──▶ US4: Bibliography (P2) ───────┼──▶ Polish (Phase 8)
└──▶ US5: Appendices (P3) ─────────┘
```

### Parallel Opportunities

**Within Phase 1 (Setup)**:
- T002, T003, T004 can run in parallel

**Within Phase 2 (Foundational)**:
- T007, T008 can run in parallel

**Within Phase 3 (US1)**:
- T011-T016 (templates) can run in parallel
- T018-T023 (chapter content) can run in parallel
- T025-T028 (generation rules) can run in parallel

**Within Phase 5 (US3)**:
- T040-T041, T044, T047, T050, T054 (templates) can run in parallel
- T051 (figures) can run in parallel with template creation

**Across User Stories (after Foundational)**:
- US3, US4, US5 can run in parallel (different templates/content)

---

## Parallel Example: User Story 1 Templates

```bash
# Launch all templates in parallel (different files, no dependencies):
Task: "Create templates/thesis-main.tera"
Task: "Create templates/front-matter.tera"
Task: "Create templates/toc.tera"
Task: "Create templates/chapter.tera"
Task: "Create templates/section.tera"
Task: "Create templates/subsection.tera"

# Launch all chapter content in parallel:
Task: "Add Chapter 2 to ontology/thesis-content.ttl"
Task: "Add Chapter 3 to ontology/thesis-content.ttl"
Task: "Add Chapter 4 to ontology/thesis-content.ttl"
Task: "Add Chapter 5 to ontology/thesis-content.ttl"
Task: "Add Chapter 6 to ontology/thesis-content.ttl"
Task: "Add Chapter 7 to ontology/thesis-content.ttl"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 Only)

1. Complete Phase 1: Setup (4 tasks)
2. Complete Phase 2: Foundational (6 tasks)
3. Complete Phase 3: User Story 1 (21 tasks)
4. **STOP and VALIDATE**: Run `ggen sync`, compile PDF, verify 50+ pages
5. Complete Phase 4: User Story 2 (8 tasks)
6. **STOP and VALIDATE**: Verify template reusability with alternate content
7. Deploy/demo MVP with core thesis generation working

### Full Implementation

1. Complete MVP (Phases 1-4)
2. Complete Phase 5: User Story 3 - Academic Elements (19 tasks)
3. Complete Phase 6: User Story 4 - Bibliography (9 tasks)
4. Complete Phase 7: User Story 5 - Appendices (9 tasks)
5. Complete Phase 8: Polish (13 tasks)
6. **FINAL VALIDATION**: 50+ page PDF, zero hardcoded strings, all cross-references resolve

### Task Summary

| Phase | Description | Task Count |
|-------|-------------|------------|
| 1 | Setup | 4 |
| 2 | Foundational | 6 |
| 3 | US1: Basic Thesis (P1) | 21 |
| 4 | US2: Reusability (P1) | 8 |
| 5 | US3: Academic Elements (P2) | 19 |
| 6 | US4: Bibliography (P2) | 9 |
| 7 | US5: Appendices (P3) | 9 |
| 8 | Polish | 13 |
| **Total** | | **89** |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story is independently testable after completion
- Commit after each task or logical group
- Stop at any checkpoint to validate progress
- All visible text in PDF MUST come from ontology SPARQL queries
- Templates contain ONLY Tera variables and LaTeX structural commands
