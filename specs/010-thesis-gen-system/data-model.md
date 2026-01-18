# Data Model: Thesis Generation Ontology Schema

**Feature**: 010-thesis-gen-system
**Date**: 2025-12-16
**Version**: 1.0.0

## Entity Relationship Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              THESIS                                      │
│  title, subtitle, author, institution, date, abstract,                  │
│  dedication, acknowledgments                                            │
└────────────────────────────────────┬────────────────────────────────────┘
                                     │ hasChapter (1..*)
                                     ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                              CHAPTER                                     │
│  orderIndex, title, abstract, labelId                                   │
└────────────────────────────────────┬────────────────────────────────────┘
                                     │ hasSection (1..*)
                                     ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                              SECTION                                     │
│  orderIndex, title, content, labelId                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                         hasSubsection (0..*)                            │
│                         hasTheorem (0..*)                               │
│                         hasEquation (0..*)                              │
│                         hasAlgorithm (0..*)                             │
│                         hasFigure (0..*)                                │
│                         hasTable (0..*)                                 │
│                         cites (0..*)                                    │
└─────────────────────────────────────────────────────────────────────────┘

┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   THEOREM    │  │   EQUATION   │  │  ALGORITHM   │  │    FIGURE    │
│  statement   │  │    latex     │  │    title     │  │   caption    │
│    proof     │  │ description  │  │    input     │  │  imagePath   │
│  theoremType │  │   labelId    │  │   output     │  │   labelId    │
│   labelId    │  └──────────────┘  │    steps     │  │  position    │
└──────────────┘                    │   labelId    │  └──────────────┘
                                    └──────────────┘

┌──────────────┐  ┌──────────────┐  ┌──────────────────────────────────┐
│    TABLE     │  │   APPENDIX   │  │           REFERENCE              │
│   caption    │  │   letter     │  │  bibType, citeKey, author,       │
│   headers    │  │   title      │  │  title, year, journal, booktitle,│
│    rows      │  │  content     │  │  publisher, school, doi, url     │
│   labelId    │  │  labelId     │  └──────────────────────────────────┘
└──────────────┘  └──────────────┘

┌──────────────────────────────────────────────────────────────────────────┐
│                         LATEX_ENVIRONMENT                                │
│  envName, envBegin, envEnd, envLabel, requiresProof                     │
│  (Stores LaTeX syntax as data for zero-hardcoding)                      │
└──────────────────────────────────────────────────────────────────────────┘
```

## Entity Definitions

### Thesis (Root Entity)

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `title` | string | YES | Main thesis title |
| `subtitle` | string | NO | Secondary title line |
| `author` | string | YES | Author name |
| `institution` | string | YES | University/organization |
| `department` | string | NO | Department name |
| `date` | string | YES | Submission date |
| `abstract` | string | YES | 200-500 word abstract |
| `dedication` | string | NO | Dedication text |
| `acknowledgments` | string | NO | Acknowledgments text |
| `hasChapter` | Chapter[] | YES | Ordered list of chapters |
| `hasReference` | Reference[] | YES | Bibliography entries |
| `hasAppendix` | Appendix[] | NO | Appendix sections |

### Chapter

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Sort order (1, 2, 3...) |
| `title` | string | YES | Chapter title |
| `abstract` | string | NO | Chapter introduction/summary |
| `labelId` | string | YES | LaTeX label (e.g., "ch:intro") |
| `hasSection` | Section[] | YES | Ordered sections |

### Section

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Sort order within chapter |
| `title` | string | YES | Section title |
| `content` | string | YES | Paragraph content (supports \\cite{}) |
| `labelId` | string | YES | LaTeX label (e.g., "sec:background") |
| `hasSubsection` | Subsection[] | NO | Nested subsections |
| `hasTheorem` | Theorem[] | NO | Theorems in this section |
| `hasEquation` | Equation[] | NO | Equations in this section |
| `hasAlgorithm` | Algorithm[] | NO | Algorithms in this section |
| `hasFigure` | Figure[] | NO | Figures in this section |
| `hasTable` | Table[] | NO | Tables in this section |
| `cites` | Reference[] | NO | Citations used |

### Subsection

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Sort order within section |
| `title` | string | YES | Subsection title |
| `content` | string | YES | Paragraph content |
| `labelId` | string | YES | LaTeX label |

### Theorem

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Global theorem number |
| `theoremType` | enum | YES | theorem/lemma/corollary/proposition/definition |
| `name` | string | NO | Named theorem (e.g., "Completeness") |
| `statement` | string | YES | Theorem statement text |
| `proof` | string | NO | Proof text (not for definitions) |
| `labelId` | string | YES | LaTeX label (e.g., "thm:zero-drift") |

### Equation

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Equation number |
| `latex` | string | YES | LaTeX math markup |
| `description` | string | YES | Explanation of equation |
| `labelId` | string | YES | LaTeX label (e.g., "eq:entropy") |

### Algorithm

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Algorithm number |
| `title` | string | YES | Algorithm name |
| `input` | string | YES | Input description |
| `output` | string | YES | Output description |
| `steps` | string[] | YES | Ordered pseudocode steps |
| `labelId` | string | YES | LaTeX label (e.g., "alg:sync") |

### Figure

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Figure number |
| `caption` | string | YES | Figure caption |
| `imagePath` | string | YES | Path to image file |
| `width` | string | NO | Width (e.g., "0.8\\textwidth") |
| `position` | string | NO | Placement hint (h, t, b, p) |
| `labelId` | string | YES | LaTeX label (e.g., "fig:arch") |

### Table

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `orderIndex` | integer | YES | Table number |
| `caption` | string | YES | Table caption |
| `headers` | string[] | YES | Column headers |
| `rows` | TableRow[] | YES | Data rows |
| `labelId` | string | YES | LaTeX label (e.g., "tab:results") |

### TableRow

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `cells` | string[] | YES | Cell values matching header count |

### Reference (BibTeX Entry)

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `citeKey` | string | YES | Citation key (e.g., "chatman2025") |
| `bibType` | enum | YES | article/inproceedings/book/phdthesis/misc |
| `author` | string | YES | Author(s) |
| `title` | string | YES | Work title |
| `year` | string | YES | Publication year |
| `journal` | string | COND | Journal name (for article) |
| `booktitle` | string | COND | Conference name (for inproceedings) |
| `publisher` | string | COND | Publisher (for book) |
| `school` | string | COND | University (for phdthesis) |
| `volume` | string | NO | Volume number |
| `pages` | string | NO | Page range |
| `doi` | string | NO | DOI identifier |
| `url` | string | NO | URL if applicable |

### Appendix

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `letter` | string | YES | Appendix letter (A, B, C...) |
| `title` | string | YES | Appendix title |
| `content` | string | YES | Appendix content |
| `labelId` | string | YES | LaTeX label (e.g., "app:code") |
| `hasCodeListing` | CodeListing[] | NO | Code listings |

### CodeListing

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `language` | string | YES | Programming language |
| `caption` | string | YES | Listing caption |
| `code` | string | YES | Source code content |
| `labelId` | string | YES | LaTeX label |

### LatexEnvironment (Meta-entity)

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `envName` | string | YES | Environment identifier |
| `envBegin` | string | YES | `\begin{...}` command |
| `envEnd` | string | YES | `\end{...}` command |
| `envLabel` | string | YES | Display label ("Theorem", "Algorithm") |
| `requiresProof` | boolean | NO | Whether proof environment follows |

## Validation Rules

### Cardinality Constraints

1. Thesis MUST have at least 7 chapters
2. Each chapter MUST have at least 2 sections
3. Thesis MUST have at least 30 references
4. Each chapter SHOULD have at least one theorem, equation, or figure

### Referential Integrity

1. All `cites` properties MUST reference existing Reference entities
2. All `\ref{}` in content MUST have corresponding `labelId`
3. All `imagePath` values MUST exist at generation time (warning, not error)

### Content Constraints

1. `content` fields MUST NOT be empty strings
2. `abstract` MUST be 200-500 words
3. `orderIndex` MUST be unique within parent scope
4. `labelId` MUST be unique across entire thesis
5. `citeKey` MUST be unique and valid BibTeX identifier

### State Transitions

Not applicable—thesis is a static document model.

## Ontology Prefix Definitions

```turtle
@prefix thesis: <https://ggen.io/ontology/thesis#> .
@prefix content: <https://ggen.io/thesis/content#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
```

## Sample Instance Data

```turtle
content:MainThesis a thesis:Thesis ;
    thesis:title "Ontology-Driven Code Generation" ;
    thesis:subtitle "A Unified Framework via RDF Knowledge Graphs" ;
    thesis:author "Sean Chatman" ;
    thesis:institution "ggen.io Research" ;
    thesis:date "December 2025" ;
    thesis:hasChapter content:Chapter1, content:Chapter2 ;
    thesis:hasReference content:RefBernersLee2001 .

content:Chapter1 a thesis:Chapter ;
    thesis:orderIndex 1 ;
    thesis:title "Introduction" ;
    thesis:labelId "ch:intro" ;
    thesis:hasSection content:Section1_1 .

content:Section1_1 a thesis:Section ;
    thesis:orderIndex 1 ;
    thesis:title "The Code Generation Problem" ;
    thesis:labelId "sec:problem" ;
    thesis:content "Software development creates specification-implementation drift..." ;
    thesis:cites content:RefBernersLee2001 .
```
