# Data Model: Grand Unified KGC Thesis

**Feature Branch**: `012-grand-unified-kgc-thesis`
**Generated**: 2025-12-16
**Base IRI**: `https://ggen.io/thesis/kgc-unified/`
**Schema IRI**: `https://ggen.io/ontology/thesis#`

## Entity Overview

```
Thesis (1)
├── Chapter (7) [orderIndex]
│   ├── Section (N) [orderIndex]
│   │   ├── Subsection (N) [orderIndex]
│   │   ├── Theorem (N) [orderIndex, theoremType]
│   │   ├── Equation (N) [orderIndex]
│   │   ├── Algorithm (N) [orderIndex]
│   │   │   └── AlgorithmStep (N) [stepIndex]
│   │   ├── Figure (N) [orderIndex]
│   │   └── Table (N) [orderIndex]
│   │       └── TableRow (N) [rowIndex]
├── Reference (30+) [citeKey]
└── Appendix (N) [letter]
    └── CodeListing (N)
```

---

## Entity Definitions

### 1. Thesis (Root Entity)

**Description**: Root document entity representing the complete PhD thesis.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | IRI | Yes | Unique identifier |
| `title` | string | Yes | Thesis title |
| `subtitle` | string | No | Optional subtitle |
| `author` | string | Yes | Author name |
| `institution` | string | Yes | University name |
| `department` | string | No | Department name |
| `date` | string | Yes | Submission date |
| `abstract` | text | Yes | Thesis abstract (500-1000 words) |
| `dedication` | text | No | Dedication page |
| `acknowledgments` | text | No | Acknowledgments section |

**Relationships**:
- `hasChapter` → Chapter (1..N)
- `hasReference` → Reference (1..N)
- `hasAppendix` → Appendix (0..N)

**Validation Rules**:
- Exactly one Thesis instance
- Abstract required, 500-1000 words
- At least 7 chapters required

---

### 2. Chapter

**Description**: Major structural unit of the thesis.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Chapter sequence (1-7) |
| `title` | string | Yes | Chapter title |
| `labelId` | string | Yes | LaTeX label (e.g., `ch:intro`) |
| `abstract` | text | No | Chapter abstract/overview |

**Relationships**:
- `hasSection` → Section (1..N)

**Validation Rules**:
- orderIndex must be unique within thesis
- labelId must follow pattern `ch:[a-z-]+`

**Chapters for GUT Thesis**:
1. Introduction
2. Theoretical Foundations (Zero-Drift Theorem)
3. Hyperdimensional Information Theory
4. KGC-4D Temporal Framework
5. TanStack/Electric SQL Case Study
6. @unrdf Integration Patterns
7. Conclusions and Future Work

---

### 3. Section

**Description**: Content unit within a chapter.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Section sequence within chapter |
| `title` | string | Yes | Section title |
| `labelId` | string | Yes | LaTeX label (e.g., `sec:problem`) |
| `content` | text | Yes | Section prose content |

**Relationships**:
- `hasSubsection` → Subsection (0..N)
- `hasTheorem` → Theorem (0..N)
- `hasEquation` → Equation (0..N)
- `hasAlgorithm` → Algorithm (0..N)
- `hasFigure` → Figure (0..N)
- `hasTable` → Table (0..N)

**Validation Rules**:
- orderIndex unique within parent chapter
- labelId must follow pattern `sec:[a-z-]+`
- Content may contain LaTeX markup and `\ref{}` references

---

### 4. Subsection

**Description**: Nested content unit within a section.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Subsection sequence |
| `title` | string | Yes | Subsection title |
| `labelId` | string | Yes | LaTeX label |
| `content` | text | Yes | Subsection prose |

**Validation Rules**:
- orderIndex unique within parent section
- labelId must follow pattern `subsec:[a-z-]+`

---

### 5. Theorem

**Description**: Formal mathematical statement with optional proof.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Theorem sequence in section |
| `theoremType` | enum | Yes | Type: theorem, lemma, corollary, definition, proposition |
| `theoremName` | string | No | Optional named theorem (e.g., "Zero-Drift") |
| `statement` | text | Yes | Formal statement (LaTeX math) |
| `proof` | text | No | Proof content |
| `labelId` | string | Yes | LaTeX label (e.g., `thm:zero-drift`) |

**Theorem Types**:
| Type | LaTeX Environment | Style |
|------|-------------------|-------|
| theorem | `\begin{theorem}` | plain (italic body) |
| lemma | `\begin{lemma}` | plain |
| corollary | `\begin{corollary}` | plain |
| definition | `\begin{definition}` | definition (roman body) |
| proposition | `\begin{proposition}` | plain |

**Validation Rules**:
- theoremType must be one of enum values
- statement must contain valid LaTeX
- If proof exists, must be complete

**Key Theorems for GUT Thesis**:
- Definition: Knowledge Graph Completeness
- Theorem: Zero-Drift Theorem
- Lemma: Semantic Preservation Lemma
- Theorem: Deterministic Reconstruction
- Theorem: Causal Consistency
- Theorem: Event Immutability
- Theorem: Semantic Fidelity Bound
- Proposition: Multi-Target Superadditivity

---

### 6. Equation

**Description**: Mathematical formula with LaTeX representation.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Equation sequence |
| `latex` | string | Yes | LaTeX math content |
| `description` | text | No | Explanation of equation |
| `labelId` | string | Yes | LaTeX label (e.g., `eq:entropy`) |

**Validation Rules**:
- latex must be valid LaTeX math mode content
- labelId must follow pattern `eq:[a-z-]+`

**Key Equations for GUT Thesis** (20+ required):
1. Shannon Entropy: `H(O) = -\sum p(t) \log_2 p(t)`
2. Mutual Information: `I(O;C) = H(O) - H(O|C)`
3. Semantic Fidelity: `\Phi = I(O;C)/H(O)`
4. HD Encoding: `encode(s,p,o) = S \otimes P \otimes O`
5. Rate-Distortion: `R(D) = \min I(O;C) \text{ s.t. } E[d] \leq D`
6. State Transition: `\delta(G, CreateEvent(\tau)) = G \cup \{\tau\}`
7. State Reconstruction: `\rho(t) = fold(\delta, \emptyset, \{e | e.t \leq t\})`
8-20. Additional equations for proofs...

---

### 7. Algorithm

**Description**: Pseudocode block with ordered steps.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Algorithm sequence |
| `title` | string | Yes | Algorithm name |
| `input` | string | Yes | Input parameters |
| `output` | string | Yes | Output specification |
| `caption` | string | No | Figure caption |
| `labelId` | string | Yes | LaTeX label (e.g., `alg:reconstruct`) |

**Relationships**:
- `hasStep` → AlgorithmStep (1..N)

**AlgorithmStep Fields**:
| Field | Type | Required |
|-------|------|----------|
| `stepIndex` | integer | Yes |
| `content` | string | Yes |

**Key Algorithms for GUT Thesis** (5+ required):
1. `reconstructState(t)` - State reconstruction
2. `freezeUniverse()` - Snapshot creation
3. `generateCode(ontology, template)` - Code generation
4. `calculateFidelity(O, C)` - Semantic fidelity measurement
5. `executeHookChain(hooks, quad)` - Hook orchestration

---

### 8. Figure

**Description**: Visual element with external image reference.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Figure sequence |
| `caption` | string | Yes | Figure caption |
| `imagePath` | string | Yes | Path to image file |
| `width` | string | No | Width (e.g., `0.8\textwidth`) |
| `position` | string | No | LaTeX position (h, t, b, p) |
| `labelId` | string | Yes | LaTeX label (e.g., `fig:arch`) |

**Validation Rules**:
- imagePath must reference existing file
- width should be LaTeX dimension

**Key Figures for GUT Thesis**:
1. ggen Architecture Diagram
2. RDF-to-Code Generation Pipeline
3. 4D Temporal Coordinate System
4. KGC-4D Event Sourcing Flow
5. Hyperdimensional Embedding Space
6. TanStack/Electric SQL Integration
7. Hook Execution Pipeline
8. Full-Stack Consistency Diagram

---

### 9. Table

**Description**: Tabular data with headers and rows.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `orderIndex` | integer | Yes | Table sequence |
| `caption` | string | Yes | Table caption |
| `labelId` | string | Yes | LaTeX label (e.g., `tab:metrics`) |

**Relationships**:
- `hasHeader` → header strings with `headerIndex`
- `hasRow` → TableRow (1..N)

**TableRow Fields**:
| Field | Type | Required |
|-------|------|----------|
| `rowIndex` | integer | Yes |
| `hasCell` → cell strings with `cellIndex`

**Key Tables for GUT Thesis**:
1. Semantic Fidelity Comparison (Rust vs TS vs Python)
2. Generation Time Benchmarks
3. Code Consistency Metrics (73% reduction)
4. Theorem Type Summary
5. Hook Performance Metrics

---

### 10. Reference

**Description**: Bibliography entry in BibTeX format.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `citeKey` | string | Yes | Citation key (e.g., `shannon1948`) |
| `bibType` | enum | Yes | Entry type |
| `author` | string | Yes | Author(s) |
| `title` | string | Yes | Work title |
| `year` | integer | Yes | Publication year |
| `journal` | string | No | Journal name (for article) |
| `booktitle` | string | No | Conference name (for inproceedings) |
| `publisher` | string | No | Publisher (for book) |
| `school` | string | No | University (for phdthesis) |
| `volume` | string | No | Volume number |
| `pages` | string | No | Page range |
| `doi` | string | No | Digital Object Identifier |
| `url` | string | No | URL |
| `howpublished` | string | No | Access method (for misc) |

**BibTeX Types**:
- `article`: Journal articles
- `book`: Books
- `inproceedings`: Conference papers
- `phdthesis`: PhD dissertations
- `misc`: Other (W3C specs, etc.)

**Validation Rules**:
- citeKey must be unique
- Required fields vary by bibType

**Key References for GUT Thesis** (30+ required):
- Shannon (1948): Information theory
- Berners-Lee (1998): Semantic web vision
- W3C RDF/SPARQL specifications
- Cover & Thomas (2006): Information theory textbook
- ACM surveys on hyperdimensional computing
- @unrdf package documentation
- TanStack/Electric SQL papers

---

### 11. Appendix

**Description**: Supplementary section with extended content.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `letter` | string | Yes | Appendix letter (A, B, C...) |
| `title` | string | Yes | Appendix title |
| `labelId` | string | Yes | LaTeX label |
| `content` | text | Yes | Appendix prose |

**Relationships**:
- `hasCodeListing` → CodeListing (0..N)

**Key Appendices for GUT Thesis**:
- A: Extended Mathematical Proofs
- B: ggen Configuration Reference
- C: Generated Code Examples
- D: Ontology Schema Definitions

---

### 12. CodeListing

**Description**: Source code example in appendix.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `language` | string | Yes | Programming language |
| `caption` | string | Yes | Listing caption |
| `code` | text | Yes | Source code content |
| `labelId` | string | Yes | LaTeX label |

**Supported Languages**:
- `rust`: Rust code
- `typescript`: TypeScript/JavaScript
- `python`: Python
- `sparql`: SPARQL queries
- `turtle`: RDF Turtle syntax
- `latex`: LaTeX examples
- `bash`: Shell commands

---

## State Transitions

### Thesis Lifecycle
```
Draft → Under Review → Revision → Final → Published
```

### Generation Lifecycle
```
Ontology Authored → SPARQL Queried → Templates Rendered → LaTeX Compiled → PDF Generated
```

---

## Validation Summary

| Entity | Min Count | Max Count | Required Fields |
|--------|-----------|-----------|-----------------|
| Thesis | 1 | 1 | title, author, institution, date, abstract |
| Chapter | 7 | 10 | orderIndex, title, labelId |
| Section | 1/ch | N | orderIndex, title, labelId, content |
| Theorem | 10 total | N | orderIndex, theoremType, statement, labelId |
| Equation | 20 total | N | orderIndex, latex, labelId |
| Algorithm | 5 total | N | orderIndex, title, input, output, labelId |
| Figure | 5 total | N | orderIndex, caption, imagePath, labelId |
| Table | 5 total | N | orderIndex, caption, labelId |
| Reference | 30 | N | citeKey, bibType, author, title, year |
| Appendix | 1 | 10 | letter, title, labelId, content |
