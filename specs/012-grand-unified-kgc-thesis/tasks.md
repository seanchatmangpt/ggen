# Tasks: Grand Unified KGC Thesis

**Input**: Design documents from `/specs/012-grand-unified-kgc-thesis/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/thesis-generation.yaml

**Tests**: Not explicitly requested - implementation tasks only.

**Organization**: Tasks grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US6)
- Include exact file paths in descriptions

## Path Conventions

All paths relative to `specs/012-grand-unified-kgc-thesis/`:
- `ontology/` - RDF ontology files
- `templates/` - Tera template files
- `output/` - Generated LaTeX files

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [ ] T001 Create feature directory structure (ontology/, templates/, output/, evidence/)
- [ ] T002 Create ggen.toml manifest with 15 generation rules in specs/012-grand-unified-kgc-thesis/ggen.toml
- [ ] T003 Create thesis-schema.ttl with 17 entity classes (Thesis, Chapter, Section, Theorem, Equation, Algorithm, Figure, Table, Reference, Appendix, etc.) in ontology/thesis-schema.ttl
- [ ] T004 [P] Create preamble.tera template with LaTeX package loading order (memoir → amsmath → amsthm → algorithm2e → biblatex → hyperref → cleveref) in templates/preamble.tera
- [ ] T005 [P] Create output/.gitkeep to ensure output directory exists

**Checkpoint**: Directory structure and manifest ready

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core templates that ALL user stories depend on

**CRITICAL**: No user story work can begin until this phase is complete

### Core Templates (Parallelizable)

- [ ] T006 [P] Create thesis-main.tera template with document structure and \input directives in templates/thesis-main.tera
- [ ] T007 [P] Create front-matter.tera template for title page, abstract, dedication, acknowledgments in templates/front-matter.tera
- [ ] T008 [P] Create chapter.tera template with chapter/section/subsection iteration in templates/chapter.tera
- [ ] T009 [P] Create theorem.tera template for theorem/lemma/definition/proposition/corollary environments in templates/theorem.tera
- [ ] T010 [P] Create equation.tera template for numbered equations with descriptions in templates/equation.tera
- [ ] T011 [P] Create algorithm.tera template for algorithm2e pseudocode blocks in templates/algorithm.tera
- [ ] T012 [P] Create figure.tera template for figures with captions and labels in templates/figure.tera
- [ ] T013 [P] Create table.tera template for tables with headers and rows in templates/table.tera
- [ ] T014 [P] Create bibliography.tera template for BibTeX entries in templates/bibliography.tera
- [ ] T015 [P] Create appendix.tera template for appendices with code listings in templates/appendix.tera
- [ ] T016 [P] Create code-listing.tera template for syntax-highlighted code in templates/code-listing.tera
- [ ] T017 [P] Create subsection.tera template for nested content in templates/subsection.tera
- [ ] T018 [P] Create chapter-index.tera template for navigation in templates/chapter-index.tera

### Base Ontology

- [ ] T019 Create kgc-unified-content.ttl with thesis root entity (title, author, institution, date, abstract) in ontology/kgc-unified-content.ttl
- [ ] T020 Verify ggen sync runs without errors on base ontology

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Generate Complete PhD Thesis Document (Priority: P1)

**Goal**: Generate publication-ready PhD thesis in LaTeX format from RDF ontology

**Independent Test**: Run `ggen sync` on thesis ontology and compile resulting LaTeX to PDF

### Implementation for User Story 1

- [ ] T021 [US1] Add thesis metadata (subtitle, department, dedication, acknowledgments) to thesis root entity in ontology/kgc-unified-content.ttl
- [ ] T022 [P] [US1] Create Chapter 1 entity (Introduction) with orderIndex=1, title="Introduction", labelId="ch:intro" in ontology/kgc-unified-content.ttl
- [ ] T023 [P] [US1] Create Chapter 2 entity (Theoretical Foundations) with orderIndex=2, title, labelId="ch:foundations" in ontology/kgc-unified-content.ttl
- [ ] T024 [P] [US1] Create Chapter 3 entity (Hyperdimensional Information Theory) with orderIndex=3, labelId="ch:info-theory" in ontology/kgc-unified-content.ttl
- [ ] T025 [P] [US1] Create Chapter 4 entity (KGC-4D Temporal Framework) with orderIndex=4, labelId="ch:kgc-4d" in ontology/kgc-unified-content.ttl
- [ ] T026 [P] [US1] Create Chapter 5 entity (TanStack Case Study) with orderIndex=5, labelId="ch:case-study" in ontology/kgc-unified-content.ttl
- [ ] T027 [P] [US1] Create Chapter 6 entity (@unrdf Integration) with orderIndex=6, labelId="ch:unrdf" in ontology/kgc-unified-content.ttl
- [ ] T028 [P] [US1] Create Chapter 7 entity (Conclusions and Future Work) with orderIndex=7, labelId="ch:conclusions" in ontology/kgc-unified-content.ttl
- [ ] T029 [US1] Add Section entities for Chapter 1 (Motivation, Research Questions, Contributions, Thesis Organization) in ontology/kgc-unified-content.ttl
- [ ] T030 [US1] Run ggen sync to generate initial LaTeX files in output/
- [ ] T031 [US1] Compile generated LaTeX with pdflatex to verify structure in output/thesis.tex
- [ ] T032 [US1] Verify table of contents, list of figures, list of tables generate correctly

**Checkpoint**: User Story 1 complete - thesis structure compiles to PDF

---

## Phase 4: User Story 2 - Prove Knowledge Graph Completeness Theorem (Priority: P1)

**Goal**: Formal proof that KGC guarantees full-stack consistency

**Independent Test**: Mathematician can read Chapter 2 and verify each proof step

### Implementation for User Story 2

- [ ] T033 [US2] Create Definition entity for Knowledge Graph Completeness with formal mathematical notation in ontology/kgc-unified-content.ttl
- [ ] T034 [P] [US2] Create Lemma entity for Semantic Preservation with statement and proof in ontology/kgc-unified-content.ttl
- [ ] T035 [P] [US2] Create Lemma entity for Type Consistency with statement and proof in ontology/kgc-unified-content.ttl
- [ ] T036 [US2] Create Theorem entity for Zero-Drift Theorem with statement referencing Definition and Lemmas in ontology/kgc-unified-content.ttl
- [ ] T037 [US2] Add proof content to Zero-Drift Theorem with numbered steps referencing prior lemmas in ontology/kgc-unified-content.ttl
- [ ] T038 [P] [US2] Create Equation entity for semantic entailment formula (eq:entailment) in ontology/kgc-unified-content.ttl
- [ ] T039 [P] [US2] Create Equation entity for type preservation formula (eq:type-preservation) in ontology/kgc-unified-content.ttl
- [ ] T040 [P] [US2] Create Equation entity for drift measure formula (eq:drift) in ontology/kgc-unified-content.ttl
- [ ] T041 [US2] Add Section entities for Chapter 2 (KGC Definition, Semantic Preservation, Type Consistency, Zero-Drift Theorem) in ontology/kgc-unified-content.ttl
- [ ] T042 [US2] Add labelId properties to all definitions, lemmas, theorems for cross-referencing in ontology/kgc-unified-content.ttl
- [ ] T043 [US2] Run ggen sync and verify theorems render with proofs in output/theorems.tex
- [ ] T044 [US2] Compile LaTeX and verify cross-references resolve (no "??" warnings)

**Checkpoint**: User Story 2 complete - formal proofs compile with cross-references

---

## Phase 5: User Story 3 - Demonstrate 4D Temporal Event Sourcing (Priority: P2)

**Goal**: Document @unrdf/kgc-4d temporal semantics for event sourcing

**Independent Test**: Chapter 4 concepts verifiable against @unrdf/kgc-4d npm package

### Implementation for User Story 3

- [ ] T045 [US3] Add Section entities for Chapter 4 (Temporal Model, GitBackbone, Event Sourcing Calculus, Time-Travel Queries) in ontology/kgc-unified-content.ttl
- [ ] T046 [P] [US3] Create Definition entity for 4D Temporal Coordinate System in ontology/kgc-unified-content.ttl
- [ ] T047 [P] [US3] Create Definition entity for LogicalTime with nanosecond precision in ontology/kgc-unified-content.ttl
- [ ] T048 [P] [US3] Create Definition entity for Event types (CreateEvent, UpdateEvent, DeleteEvent) in ontology/kgc-unified-content.ttl
- [ ] T049 [US3] Create Algorithm entity for reconstructState with input, output, and ordered steps in ontology/kgc-unified-content.ttl
- [ ] T050 [US3] Create Algorithm entity for freezeUniverse in ontology/kgc-unified-content.ttl
- [ ] T051 [P] [US3] Create Theorem entity for Causal Consistency with proof in ontology/kgc-unified-content.ttl
- [ ] T052 [P] [US3] Create Theorem entity for Deterministic Reconstruction with proof in ontology/kgc-unified-content.ttl
- [ ] T053 [P] [US3] Create Theorem entity for Event Immutability with proof in ontology/kgc-unified-content.ttl
- [ ] T054 [P] [US3] Create Theorem entity for Temporal Monotonicity with proof in ontology/kgc-unified-content.ttl
- [ ] T055 [US3] Create Equation entity for state transition function delta in ontology/kgc-unified-content.ttl
- [ ] T056 [US3] Create Equation entity for state reconstruction function rho in ontology/kgc-unified-content.ttl
- [ ] T057 [US3] Run ggen sync and verify algorithms render in output/algorithms.tex
- [ ] T058 [US3] Compile LaTeX and verify algorithm pseudocode formatting

**Checkpoint**: User Story 3 complete - temporal semantics documented with algorithms

---

## Phase 6: User Story 4 - Explain Hyperdimensional Information Theory (Priority: P2)

**Goal**: Document hyperdimensional vector space embeddings and information-theoretic bounds

**Independent Test**: Reader with information theory background can verify entropy calculations

### Implementation for User Story 4

- [ ] T059 [US4] Add Section entities for Chapter 3 (Shannon Entropy, Mutual Information, HD Embeddings, Semantic Fidelity) in ontology/kgc-unified-content.ttl
- [ ] T060 [P] [US4] Create Equation entity for Shannon entropy H(O) = -sum p(t) log p(t) in ontology/kgc-unified-content.ttl
- [ ] T061 [P] [US4] Create Equation entity for mutual information I(O;C) = H(O) - H(O|C) in ontology/kgc-unified-content.ttl
- [ ] T062 [P] [US4] Create Equation entity for semantic fidelity Phi = I(O;C)/H(O) in ontology/kgc-unified-content.ttl
- [ ] T063 [P] [US4] Create Equation entity for HD encoding encode(s,p,o) = S tensor P tensor O in ontology/kgc-unified-content.ttl
- [ ] T064 [P] [US4] Create Equation entity for rate-distortion R(D) bound in ontology/kgc-unified-content.ttl
- [ ] T065 [P] [US4] Create Equation entity for graph bundling O_hv in ontology/kgc-unified-content.ttl
- [ ] T066 [US4] Create Definition entity for Hyperdimensional Computing with dimensions d in [1000, 10000] in ontology/kgc-unified-content.ttl
- [ ] T067 [US4] Create Theorem entity for Semantic Fidelity Bound with proof in ontology/kgc-unified-content.ttl
- [ ] T068 [US4] Create Theorem entity for Multi-Target Superadditivity with proof in ontology/kgc-unified-content.ttl
- [ ] T069 [US4] Create Lemma entity for Information Preservation during generation in ontology/kgc-unified-content.ttl
- [ ] T070 [US4] Run ggen sync and verify equations render in output/equations.tex
- [ ] T071 [US4] Compile LaTeX and verify mathematical notation renders correctly

**Checkpoint**: User Story 4 complete - information theory with 15+ equations

---

## Phase 7: User Story 5 - Validate Full-Stack Integration Case Study (Priority: P3)

**Goal**: Document case study with TanStack DB + NextJS + Electric SQL

**Independent Test**: Case study steps can be replicated with provided ontology

### Implementation for User Story 5

- [ ] T072 [US5] Add Section entities for Chapter 5 (Architecture, Generated Artifacts, Metrics, Lessons Learned) in ontology/kgc-unified-content.ttl
- [ ] T073 [US5] Create Figure entity for ggen architecture diagram in ontology/kgc-unified-content.ttl
- [ ] T074 [US5] Create Figure entity for pnpm monorepo structure in ontology/kgc-unified-content.ttl
- [ ] T075 [US5] Create Table entity for generated artifacts line counts (schemas.js, hooks.js, mutations.js, etc.) in ontology/kgc-unified-content.ttl
- [ ] T076 [US5] Create Table entity for consistency metrics comparison (73% reduction) in ontology/kgc-unified-content.ttl
- [ ] T077 [US5] Add prose content describing TanStack DB integration in Chapter 5 sections in ontology/kgc-unified-content.ttl
- [ ] T078 [US5] Run ggen sync and verify figures/tables render in output/figures.tex and output/tables.tex
- [ ] T079 [US5] Compile LaTeX and verify case study chapter renders correctly

**Checkpoint**: User Story 5 complete - case study with metrics validated

---

## Phase 8: User Story 6 - Document @unrdf/hooks Policy Framework (Priority: P3)

**Goal**: Document Knowledge Hooks for policy enforcement

**Independent Test**: Hook patterns verifiable against @unrdf/hooks npm package

### Implementation for User Story 6

- [ ] T080 [US6] Add Section entities for Chapter 6 (Hook Architecture, CRUD Hooks, SHACL Integration, Examples) in ontology/kgc-unified-content.ttl
- [ ] T081 [P] [US6] Create Table entity for SHACL constraint types (minCount, maxCount, datatype, pattern, class, etc.) in ontology/kgc-unified-content.ttl
- [ ] T082 [P] [US6] Create CodeListing entity for defineHook example in TypeScript in ontology/kgc-unified-content.ttl
- [ ] T083 [P] [US6] Create CodeListing entity for executeHook example in ontology/kgc-unified-content.ttl
- [ ] T084 [P] [US6] Create CodeListing entity for KnowledgeHookManager example in ontology/kgc-unified-content.ttl
- [ ] T085 [US6] Create Figure entity for hook execution pipeline diagram in ontology/kgc-unified-content.ttl
- [ ] T086 [US6] Add prose content describing pre/post hooks for CRUD operations in ontology/kgc-unified-content.ttl
- [ ] T087 [US6] Run ggen sync and verify code listings render in output/code-listings.tex
- [ ] T088 [US6] Compile LaTeX and verify Chapter 6 renders correctly

**Checkpoint**: User Story 6 complete - @unrdf/hooks documented with examples

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Bibliography, appendices, final validation

### Bibliography (30+ References)

- [ ] T089 [P] Create Reference entities for RDF/SPARQL/SHACL W3C specifications (5+ refs) in ontology/kgc-unified-content.ttl
- [ ] T090 [P] Create Reference entities for information theory sources (Shannon, Cover & Thomas, etc. - 5+ refs) in ontology/kgc-unified-content.ttl
- [ ] T091 [P] Create Reference entities for semantic web papers (Berners-Lee, etc. - 5+ refs) in ontology/kgc-unified-content.ttl
- [ ] T092 [P] Create Reference entities for hyperdimensional computing papers (5+ refs) in ontology/kgc-unified-content.ttl
- [ ] T093 [P] Create Reference entities for event sourcing and @unrdf packages (5+ refs) in ontology/kgc-unified-content.ttl
- [ ] T094 [P] Create Reference entities for TanStack, Electric SQL, Rust tooling (5+ refs) in ontology/kgc-unified-content.ttl

### Appendices

- [ ] T095 [P] Create Appendix A entity with extended mathematical proofs in ontology/kgc-unified-content.ttl
- [ ] T096 [P] Create Appendix B entity with ggen configuration reference in ontology/kgc-unified-content.ttl
- [ ] T097 [P] Create Appendix C entity with generated code examples from case study in ontology/kgc-unified-content.ttl
- [ ] T098 [P] Create Appendix D entity with ontology schema definitions in ontology/kgc-unified-content.ttl

### Final Validation

- [ ] T099 Run ggen sync to generate complete thesis in output/
- [ ] T100 Run pdflatex thesis.tex (pass 1) in output/
- [ ] T101 Run biber thesis to process bibliography in output/
- [ ] T102 Run pdflatex thesis.tex (pass 2 and 3) to resolve cross-references in output/
- [ ] T103 Verify PDF has 100+ pages, all cross-references resolve
- [ ] T104 Verify bibliography has 30+ entries rendered correctly
- [ ] T105 Run ggen sync again and verify byte-identical output (determinism test)
- [ ] T106 Create evidence/completion.md documenting all success criteria met

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 - BLOCKS all user stories
- **User Stories (Phases 3-8)**: All depend on Phase 2 completion
  - US1 (P1) and US2 (P1) can proceed in parallel
  - US3 (P2) and US4 (P2) can proceed in parallel
  - US5 (P3) and US6 (P3) can proceed in parallel
- **Polish (Phase 9)**: Depends on Phases 3-8 completion

### User Story Dependencies

| Story | Depends On | Can Parallelize With |
|-------|------------|---------------------|
| US1 | Phase 2 | US2 |
| US2 | Phase 2 | US1 |
| US3 | Phase 2 | US4 |
| US4 | Phase 2 | US3 |
| US5 | Phase 2 | US6 |
| US6 | Phase 2 | US5 |

### Within Each User Story

- Sections before theorems/equations
- Theorems before proofs
- All content before ggen sync verification
- Generation before LaTeX compilation

### Parallel Opportunities

**Phase 2** (14 parallel templates):
```
T006-T018 can all run in parallel (different template files)
```

**Phase 3** (7 parallel chapters):
```
T022-T028 can all run in parallel (different chapter entities)
```

**Phase 4** (5 parallel equations):
```
T034+T035 (lemmas), T038+T039+T040 (equations)
```

**Phase 5-6** (4 parallel theorems each):
```
T051+T052+T053+T054 (temporal theorems)
T060+T061+T062+T063+T064+T065 (info theory equations)
```

**Phase 9** (6 parallel reference batches + 4 parallel appendices):
```
T089-T094 (bibliography), T095-T098 (appendices)
```

---

## Parallel Example: Maximum Concurrency

```bash
# Phase 2 - Launch 13 template agents in parallel:
Task: "Create thesis-main.tera" [templates/thesis-main.tera]
Task: "Create front-matter.tera" [templates/front-matter.tera]
Task: "Create chapter.tera" [templates/chapter.tera]
Task: "Create theorem.tera" [templates/theorem.tera]
Task: "Create equation.tera" [templates/equation.tera]
Task: "Create algorithm.tera" [templates/algorithm.tera]
Task: "Create figure.tera" [templates/figure.tera]
Task: "Create table.tera" [templates/table.tera]
Task: "Create bibliography.tera" [templates/bibliography.tera]
Task: "Create appendix.tera" [templates/appendix.tera]
Task: "Create code-listing.tera" [templates/code-listing.tera]
Task: "Create subsection.tera" [templates/subsection.tera]
Task: "Create chapter-index.tera" [templates/chapter-index.tera]

# Phase 3 - Launch 7 chapter agents in parallel:
Task: "Create Chapter 1 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 2 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 3 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 4 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 5 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 6 entity" [ontology/kgc-unified-content.ttl]
Task: "Create Chapter 7 entity" [ontology/kgc-unified-content.ttl]
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Compile thesis.tex to PDF
5. Deploy/demo if ready

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 + US2 → Test independently → Core thesis with proofs
3. Add US3 + US4 → Test independently → Theoretical framework complete
4. Add US5 + US6 → Test independently → Case studies and integration
5. Polish → Final validation → Complete thesis

### Parallel Team Strategy

With 5+ developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Thesis structure)
   - Developer B: User Story 2 (KGC theorem)
   - Developer C: User Story 3 (KGC-4D)
   - Developer D: User Story 4 (Info theory)
   - Developer E: User Story 5+6 (Case studies)
3. All reconvene for Polish phase

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story is independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All file paths relative to `specs/012-grand-unified-kgc-thesis/`
