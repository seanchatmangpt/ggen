# PhD Thesis Diagrams - Ontology-Driven Code Generation

This directory contains 5 professional LaTeX TikZ diagrams for the PhD thesis on ontology-driven code generation using the ggen framework.

## Diagrams

### 1. System Architecture Overview (`01_system_architecture.tex`)
- **Lines:** ~50
- **Description:** Five-layer architecture showing the complete ggen framework
- **Layers:** Input → Processing → Template → Generation → Output
- **Components:** RDF ontology, Oxigraph, SPARQL, Tera templates, code generators
- **Reference:** `\ref{fig:system-architecture}`

### 2. RDF Ontology Structure (`02_rdf_ontology.tex`)
- **Lines:** ~40
- **Description:** Semantic model for API contract generation
- **Components:** Entity, Service, Endpoint, Parameter, Response, Error classes
- **Features:** Relationships, constraints, example instances, legend
- **Reference:** `\ref{fig:rdf-ontology}`

### 3. Generation Pipeline Flow (`03_generation_pipeline.tex`)
- **Lines:** ~45
- **Description:** Two-phase code generation process
- **Phase 1:** SPARQL query execution and pattern matching
- **Phase 2:** Template rendering and validation
- **Features:** Feedback loops, error handling, validation checks
- **Reference:** `\ref{fig:generation-pipeline}`

### 4. Type Guard Composition (`04_type_guard_composition.tex`)
- **Lines:** ~35
- **Description:** TypeScript type narrowing and composition
- **Components:** Base types, union types, discriminated unions, type guards
- **Features:** Guard composition with && and || operators
- **Reference:** `\ref{fig:type-guard-composition}`

### 5. Multi-Artifact Consistency (`05_multi_artifact_consistency.tex`)
- **Lines:** ~40
- **Description:** Single source of truth and artifact synchronization
- **Components:** RDF ontology, generated artifacts, versioning, validation
- **Features:** Update flow, consistency checks, guarantees
- **Reference:** `\ref{fig:multi-artifact-consistency}`

## Compilation

### Individual Diagrams
Each diagram file is self-contained and wrapped in a `\begin{figure}...\end{figure}` environment. Include in your thesis:

```latex
\input{diagrams/01_system_architecture.tex}
```

### All Diagrams Preview
Compile `all_diagrams.tex` to preview all diagrams:

```bash
pdflatex all_diagrams.tex
```

### Required Packages
Ensure your thesis preamble includes:

```latex
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows, positioning, shapes.multipart, shapes.symbols}
```

## Design Features

- **Color Scheme:** PDF-friendly blues, greens, purples, yellows
- **Typography:** Readable font sizes (small, footnotesize, tiny as appropriate)
- **Shapes:** Rectangles, cylinders, ellipses, diamonds for visual variety
- **Arrows:** Different styles for data flow, inheritance, validation, composition
- **Legends:** Included where helpful for interpretation
- **Labels:** All figures have captions and labels for cross-referencing

## Quality Standards

- **Academic Quality:** Suitable for PhD thesis submission
- **Professional Layout:** Consistent styling across all diagrams
- **Clean Code:** Well-commented TikZ with clear node definitions
- **Compilable:** Each diagram compiles independently
- **Scalable:** Vector graphics (PDF output, not rasterized)
- **Size:** 2-3 inches tall, suitable for standard thesis page layout

## Usage in Thesis

Reference diagrams in text using:

```latex
Figure~\ref{fig:system-architecture} illustrates the layered architecture...
As shown in Figure~\ref{fig:rdf-ontology}, the semantic model...
The generation pipeline (Figure~\ref{fig:generation-pipeline}) operates in two phases...
Type guards compose to enable narrowing (Figure~\ref{fig:type-guard-composition})...
Consistency is maintained through validation (Figure~\ref{fig:multi-artifact-consistency})...
```

## File Organization

```
diagrams/
├── 01_system_architecture.tex      # 5-layer architecture
├── 02_rdf_ontology.tex             # Semantic model
├── 03_generation_pipeline.tex       # 2-phase generation
├── 04_type_guard_composition.tex    # Type narrowing
├── 05_multi_artifact_consistency.tex # Synchronization
├── all_diagrams.tex                 # Preview all diagrams
└── README.md                        # This file
```

## Credits

Generated for the ggen framework PhD thesis on ontology-driven code generation.
Date: 2026-01-06
