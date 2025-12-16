# Feature Specification: Reusable Ontology-Driven PhD Thesis Generation System

**Feature Branch**: `010-thesis-gen-system`
**Created**: 2025-12-16
**Status**: Draft
**Input**: User description: "Reusable ontology-driven PhD thesis generation system producing 50+ page PDF with zero hardcoded content"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Generate Complete Thesis from Ontology (Priority: P1)

As a researcher, I want to generate a complete 50+ page PhD thesis PDF by running a single `ggen sync` command, where ALL content (title, abstract, chapters, sections, theorems, equations, figures, tables, references) comes from the RDF ontology with zero hardcoded text in templates.

**Why this priority**: This is the core value proposition - deterministic, reproducible thesis generation from a single source of truth.

**Independent Test**: Run `ggen sync` followed by `pdflatex` compilation and verify the resulting PDF has 50+ pages with all content sourced from ontology.

**Acceptance Scenarios**:

1. **Given** a thesis ontology with 7+ chapters defined, **When** I run `ggen sync`, **Then** the system generates all LaTeX files with content extracted from RDF triples
2. **Given** templates with only variable placeholders (no hardcoded strings), **When** I run `ggen sync`, **Then** every piece of text in the output comes from ontology queries
3. **Given** generated LaTeX files, **When** I compile with `pdflatex`, **Then** the resulting PDF is 50+ pages with proper academic formatting

---

### User Story 2 - Reuse Templates Across Different Theses (Priority: P1)

As a researcher, I want to reuse the same template set for different thesis topics by only changing the ontology content, not the templates.

**Why this priority**: Reusability is the defining characteristic that separates this from a one-off solution.

**Independent Test**: Create a second thesis ontology with different topic/content, run `ggen sync`, verify templates work without modification.

**Acceptance Scenarios**:

1. **Given** a template set designed for thesis generation, **When** I swap the ontology to a different research topic, **Then** the system generates a valid thesis with the new content
2. **Given** any thesis ontology conforming to the schema, **When** I run `ggen sync`, **Then** all templates render successfully without errors

---

### User Story 3 - Generate Academic Document Elements (Priority: P2)

As a researcher, I want to define theorems, lemmas, proofs, equations, algorithms, figures, and tables in my ontology and have them render with proper LaTeX academic formatting.

**Why this priority**: Academic documents require specialized environments; without them, the output is not suitable for publication.

**Independent Test**: Define each document element type in ontology, run `ggen sync`, verify LaTeX output uses appropriate environments (theorem, algorithm, figure, table).

**Acceptance Scenarios**:

1. **Given** a theorem with statement and proof in ontology, **When** I run `ggen sync`, **Then** the output contains `\begin{theorem}...\end{theorem}` and `\begin{proof}...\end{proof}` environments
2. **Given** an algorithm with pseudocode steps, **When** I run `ggen sync`, **Then** the output contains `\begin{algorithm}...\end{algorithm}` with algorithmic environment
3. **Given** figures with captions and labels, **When** I run `ggen sync`, **Then** proper figure environments with `\includegraphics` are generated
4. **Given** tables with data rows, **When** I run `ggen sync`, **Then** proper table environments with booktabs formatting are generated

---

### User Story 4 - Manage Bibliography from Ontology (Priority: P2)

As a researcher, I want to define all references in the ontology and have BibTeX entries generated automatically.

**Why this priority**: Academic theses require proper citation management; manual BibTeX defeats the single-source-of-truth principle.

**Independent Test**: Define 10+ references in ontology, run `ggen sync`, verify generated `.bib` file compiles with bibtex.

**Acceptance Scenarios**:

1. **Given** references defined with author, title, year, journal/conference, **When** I run `ggen sync`, **Then** a valid BibTeX file is generated
2. **Given** citations embedded in chapter content, **When** I compile the thesis, **Then** all citations resolve correctly

---

### User Story 5 - Generate Appendices and Supplementary Materials (Priority: P3)

As a researcher, I want to include appendices with code listings, data tables, and supplementary proofs defined in the ontology.

**Why this priority**: Appendices are valuable but not essential for core thesis structure.

**Independent Test**: Define appendix content in ontology, verify it renders after main chapters.

**Acceptance Scenarios**:

1. **Given** appendix sections in ontology, **When** I run `ggen sync`, **Then** appendices appear after main content with proper numbering (A, B, C)
2. **Given** code listings in appendix, **When** compiled, **Then** code is syntax-highlighted with lstlistings

---

### Edge Cases

- What happens when an ontology has no chapters defined? System generates a minimal thesis structure with error warnings.
- What happens when a required property (like chapter title) is missing? Template fails gracefully with specific error message identifying the missing property.
- What happens when SPARQL query returns empty results? Template generates empty section with TODO marker or skips section entirely based on configuration.
- What happens when a figure references an image file that doesn't exist? LaTeX compilation fails but ggen sync succeeds with warning.
- What happens when special characters appear in ontology content? LaTeX escaping is handled automatically by templates.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST generate ALL text content from SPARQL queries against the RDF ontology (zero hardcoded strings in templates)
- **FR-002**: System MUST produce LaTeX output that compiles to a minimum of 50 pages with standard academic formatting
- **FR-003**: System MUST support thesis document classes with chapter, section, subsection hierarchies
- **FR-004**: System MUST render theorems, lemmas, corollaries, propositions, and proofs using appropriate LaTeX environments
- **FR-005**: System MUST render mathematical equations with proper LaTeX math formatting
- **FR-006**: System MUST render algorithms with pseudocode using algorithmic environments
- **FR-007**: System MUST generate figure environments from ontology-defined figure metadata
- **FR-008**: System MUST generate table environments from ontology-defined table data
- **FR-009**: System MUST generate a valid BibTeX file from ontology-defined references
- **FR-010**: System MUST support appendix generation with proper lettered numbering
- **FR-011**: System MUST generate table of contents, list of figures, and list of tables
- **FR-012**: System MUST generate front matter (title page, abstract, acknowledgments, dedication)
- **FR-013**: System MUST generate back matter (bibliography, appendices, index)
- **FR-014**: System MUST escape LaTeX special characters in ontology content automatically
- **FR-015**: Templates MUST be reusable across different thesis topics without modification
- **FR-016**: System MUST support cross-references between chapters, sections, theorems, figures, and tables
- **FR-017**: System MUST generate unique labels for all cross-referenceable elements
- **FR-018**: System MUST preserve ordering defined in ontology (chapter order, section order, reference order)

### Key Entities

- **Thesis**: Root document with title, subtitle, author, institution, date, abstract, dedication, acknowledgments
- **Chapter**: Major division with number, title, abstract, list of sections
- **Section**: Subdivision with number, title, content paragraphs
- **Subsection**: Further subdivision with content
- **Theorem**: Mathematical statement with label, statement text, proof text, theorem type (theorem/lemma/corollary/proposition)
- **Equation**: Mathematical expression with label, LaTeX markup, description
- **Algorithm**: Pseudocode with label, title, input/output descriptions, steps
- **Figure**: Visual element with label, caption, image path, positioning
- **Table**: Data grid with label, caption, column headers, row data
- **Reference**: Citation with BibTeX type, authors, title, year, venue, DOI/URL
- **Appendix**: Supplementary section with letter, title, content
- **CodeListing**: Source code with language, caption, code content

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Generated PDF is minimum 50 pages with 12pt font, 1-inch margins
- **SC-002**: 100% of visible text in the PDF originates from ontology SPARQL queries (zero hardcoded template strings)
- **SC-003**: Templates are reusable: same templates generate valid output for any thesis topic with conforming ontology
- **SC-004**: LaTeX compilation succeeds with zero errors and fewer than 5 warnings on first run
- **SC-005**: All cross-references (citations, figure refs, theorem refs) resolve correctly
- **SC-006**: Generation time is under 5 seconds for a 50-page thesis ontology
- **SC-007**: Ontology modification requires only `ggen sync` to regenerate (no template changes)
- **SC-008**: Generated thesis includes minimum: 7 chapters, 30 sections, 10 theorems, 20 equations, 10 figures, 5 tables, 30 references
- **SC-009**: Bibliography renders correctly with author-year citation style
- **SC-010**: Table of contents accurately reflects all chapters and sections with page numbers

## Assumptions

- Target format is LaTeX with pdflatex compilation (not XeLaTeX or LuaLaTeX)
- Image files for figures are provided externally (ontology references paths, doesn't embed images)
- Standard academic paper formatting (1-inch margins, 12pt serif font, double-spaced body text)
- BibTeX for bibliography management (not biblatex)
- Single-column layout for main content
- Numbered chapter style (not starred chapters)
- American English spelling conventions in generated content schema

## Out of Scope

- Interactive PDF features (hyperlinks within document are in scope, but forms/multimedia are not)
- Multiple output formats (only LaTeX/PDF; HTML, EPUB, Word are not included)
- Real-time collaborative editing of ontology
- Version control integration beyond what ggen already provides
- Citation management UI (ontology editing is done directly in TTL files)
- Automatic image generation or diagram creation (images must be pre-created)

## Clarifications

### Session 2025-12-16

- Q: How should the system validate that templates contain no hardcoded text (FR-001, SC-002)? → A: Manual review during development + planned LLM scanner using Ollama with JSON scoring system

## Validation Mechanism (Zero-Hardcoding Compliance)

### Phase 1: Manual Review (Immediate)
- Developer manually inspects each `.tera` template file
- Checklist verification: no literal strings outside `{{ variable }}` blocks
- Exception: Tera control flow syntax (`{% for %}`, `{% if %}`, `{# comments #}`) is allowed
- Exception: LaTeX structural commands that are NOT content (e.g., `\centering`, `\hline`) are allowed ONLY when they cannot vary by thesis topic

### Phase 2: LLM Scanner (Planned)
- **Tool**: Ollama via ggen-ai crate (GenAiClient with `qwen3:8b` model)
- **Input**: Each `.tera` template file content
- **Output**: JSON scoring object with structure:
  ```json
  {
    "file": "template-name.tera",
    "score": 0.0-1.0,
    "violations": [
      {
        "line": 15,
        "text": "hardcoded string found",
        "severity": "error|warning",
        "suggestion": "move to ontology as thesis:propertyName"
      }
    ],
    "compliant": true|false
  }
  ```
- **Threshold**: Score >= 0.95 required for compliance (allows minor structural LaTeX)
- **Integration**: `ggen validate --templates` command or pre-commit hook
