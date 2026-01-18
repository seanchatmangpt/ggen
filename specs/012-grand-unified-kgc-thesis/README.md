# Grand Unified KGC Thesis Generator

**Ontology-driven academic thesis generation using ggen v5**

This project demonstrates ggen's capability to generate complete LaTeX thesis documents from RDF ontologies, showcasing:
- 15 coordinated generation rules
- Type-safe RDF schema (17 entity classes)
- Deterministic chapter/section/theorem/equation generation
- BibTeX bibliography management
- Algorithm pseudocode generation
- Figure and table rendering

## Quickstart

### Prerequisites

- Rust 1.75+ (existing ggen toolchain)
- ggen CLI v5.0.0+
- LaTeX distribution (for compiling generated `.tex` files)

### Step 1: Populate Content Ontology

Create `ontology/kgc-unified-content.ttl` with your thesis content:

```turtle
@prefix : <https://ggen.io/thesis/kgc-unified/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:thesis-001 a :Thesis ;
    :title "Grand Unified Theory of Knowledge Graph Construction" ;
    :author "John Doe" ;
    :date "2025-12-16"^^xsd:date ;
    :abstract "This thesis presents..." ;
    :dedication "To my advisors..." ;
    :acknowledgments "I would like to thank..." ;
    :documentClass "book" ;
    :packages "amsmath,graphicx,algorithm2e" ;
    :customCommands "\\newcommand{\\kgc}{KGC}" .

:chapter-1 a :Chapter ;
    :chapterNumber 1 ;
    :title "Introduction" ;
    :content "Knowledge graphs have emerged..." .

:chapter-2 a :Chapter ;
    :chapterNumber 2 ;
    :title "Related Work" ;
    :content "Prior research in KGC..." .

:theorem-1 a :Theorem ;
    :theoremNumber 1 ;
    :statement "For any knowledge graph G..." ;
    :proof "By induction on the graph size..." ;
    :belongsToChapter :chapter-1 .

:equation-1 a :Equation ;
    :equationNumber 1 ;
    :latex "E = mc^2" ;
    :description "Einstein's mass-energy equivalence" .

:algorithm-1 a :Algorithm ;
    :algorithmNumber 1 ;
    :title "Knowledge Graph Embedding" ;
    :description "Compute entity embeddings from triples" .

:algorithm-step-1 a :AlgorithmStep ;
    :belongsToAlgorithm :algorithm-1 ;
    :stepNumber 1 ;
    :description "Initialize embedding matrix E ← random()" .

:figure-1 a :Figure ;
    :figureNumber 1 ;
    :caption "KGC Pipeline Architecture" ;
    :path "figures/kgc-pipeline.pdf" .

:table-1 a :Table ;
    :tableNumber 1 ;
    :caption "Dataset Statistics" ;
    :headers "Dataset,Entities,Relations,Triples" .

:table-row-1 a :TableRow ;
    :belongsToTable :table-1 ;
    :cells "FB15k,14951,1345,592213" .

:reference-1 a :Reference ;
    :citeKey "bordes2013translating" ;
    :author "Bordes, Antoine and Usunier, Nicolas" ;
    :title "Translating Embeddings for Modeling Multi-relational Data" ;
    :year 2013 ;
    :journal "NeurIPS" ;
    :pages "2787--2795" .

:appendix-a a :Appendix ;
    :letter "A" ;
    :title "Proof of Theorem 2" ;
    :content "Full proof details..." .

:code-1 a :CodeListing ;
    :listingNumber 1 ;
    :language "python" ;
    :caption "TransE Implementation" ;
    :source "def transe_loss(h, r, t):\\n    return torch.norm(h + r - t)" .
```

### Step 2: Create Handlebars Templates

Create templates in `templates/` directory:

**templates/thesis-main.tex.hbs**:
```latex
\documentclass{{documentClass}}
{{{packages}}}

{{{customCommands}}}

\title{ {{title}} }
\author{ {{author}} }
\date{ {{date}} }

\begin{document}
\maketitle

\begin{abstract}
{{abstract}}
\end{abstract}

\include{front-matter}

{{#each chapters}}
\include{chapters/chapter-{{chapterNumber}}}
{{/each}}

\bibliographystyle{plain}
\bibliography{bibliography}

{{#each appendices}}
\include{appendix-{{letter}}}
{{/each}}

\end{document}
```

**templates/chapter.tex.hbs**:
```latex
\chapter{ {{title}} }
{{content}}

{{#if theorems}}
\section{Theorems}
{{#each theorems}}
\begin{theorem}
{{statement}}
\end{theorem}
\begin{proof}
{{proof}}
\end{proof}
{{/each}}
{{/if}}
```

**templates/equation.tex.hbs**:
```latex
\begin{equation}
{{latex}}
\end{equation}
% {{description}}
```

**templates/algorithm.tex.hbs**:
```latex
\begin{algorithm}
\caption{ {{title}} }
{{description}}
{{#each steps}}
\State {{description}}
{{/each}}
\end{algorithm}
```

**templates/bibliography.bib.hbs**:
```bibtex
@article{ {{citeKey}},
  author = { {{author}} },
  title = { {{title}} },
  year = { {{year}} },
  journal = { {{journal}} },
  volume = { {{volume}} },
  pages = { {{pages}} }
}
```

### Step 3: Validate Configuration

```bash
cd /Users/sac/ggen/specs/012-grand-unified-kgc-thesis
cargo make check  # Verify ggen.toml syntax
```

### Step 4: Generate Thesis

```bash
# Generate all components
ggen sync

# Output structure:
# output/
# ├── thesis-main.tex          (main document)
# ├── front-matter.tex         (dedication, acknowledgments)
# ├── preamble.tex             (LaTeX packages/commands)
# ├── chapter-index.tex        (table of contents)
# ├── bibliography.bib         (BibTeX references)
# ├── chapters/
# │   ├── chapter-1.tex        (Introduction)
# │   ├── chapter-2.tex        (Related Work)
# │   ├── theorems.tex         (all theorems)
# │   ├── equations.tex        (all equations)
# │   ├── algorithms.tex       (all algorithms)
# │   ├── algorithm-steps.tex  (algorithm steps)
# │   ├── figures.tex          (figure includes)
# │   ├── tables.tex           (table environments)
# │   ├── code-listings.tex    (source code)
# │   └── subsections-*.tex    (per-chapter subsections)
# └── appendix-A.tex           (appendices)
```

### Step 5: Compile LaTeX

```bash
cd output
pdflatex thesis-main.tex
bibtex thesis-main
pdflatex thesis-main.tex
pdflatex thesis-main.tex  # Second pass for cross-references
```

## Project Structure

```
012-grand-unified-kgc-thesis/
├── ggen.toml                     # 15 generation rules
├── ontology/
│   ├── thesis-schema.ttl         # 17 entity classes (schema)
│   └── kgc-unified-content.ttl   # Thesis content (RDF data)
├── templates/
│   ├── thesis-main.tex.hbs       # Main document template
│   ├── front-matter.tex.hbs      # Front matter
│   ├── chapter.tex.hbs           # Chapter template
│   ├── theorem.tex.hbs           # Theorem environment
│   ├── equation.tex.hbs          # Equation rendering
│   ├── algorithm.tex.hbs         # Algorithm pseudocode
│   ├── figure.tex.hbs            # Figure includes
│   ├── table.tex.hbs             # Table environments
│   ├── bibliography.bib.hbs      # BibTeX format
│   ├── appendix.tex.hbs          # Appendix template
│   ├── code-listing.tex.hbs      # Source code listings
│   └── ...                       # Additional templates
└── output/                       # Generated .tex files (gitignored)
```

## Schema Entities

The `thesis-schema.ttl` defines 17 RDF classes:

1. **Thesis** - Top-level document
2. **Chapter** - Numbered chapters
3. **Section** - Chapter sections
4. **Subsection** - Section subdivisions
5. **Theorem** - Mathematical theorems with proofs
6. **Equation** - LaTeX equations
7. **Algorithm** - Pseudocode algorithms
8. **AlgorithmStep** - Individual algorithm steps
9. **Figure** - Diagrams and images
10. **Table** - Data tables
11. **TableRow** - Table row data
12. **Reference** - BibTeX citations
13. **Appendix** - Appendix sections
14. **CodeListing** - Source code with syntax highlighting

## Generation Rules

The 15 rules in `ggen.toml` ensure:
- **Deterministic output** - Same ontology → same LaTeX
- **Type safety** - SPARQL queries enforce schema
- **Modularity** - Each rule generates isolated components
- **Composability** - Generated files include each other

## Advanced Features

### Incremental Updates

Modify RDF triples and regenerate:
```bash
# Edit ontology/kgc-unified-content.ttl
# Add new chapter
:chapter-3 a :Chapter ;
    :chapterNumber 3 ;
    :title "Methodology" ;
    :content "Our approach consists of..." .

# Regenerate
ggen sync  # Only chapter-3.tex is updated
```

### Cross-References

Use LaTeX labels in content:
```turtle
:chapter-1 a :Chapter ;
    :content "\\label{ch:intro}This chapter introduces..." .

:chapter-2 a :Chapter ;
    :content "As discussed in Chapter~\\ref{ch:intro}..." .
```

### Multi-Language Support

Generate non-English theses:
```turtle
:thesis-001 a :Thesis ;
    :title "Théorie Unifiée de la Construction de Graphes de Connaissances"@fr ;
    :documentClass "book" ;
    :packages "\\usepackage[french]{babel}" .
```

## Navigation

- **Project Root**: `/Users/sac/ggen`
- **Spec Directory**: `/Users/sac/ggen/specs/012-grand-unified-kgc-thesis`
- **Schema**: `ontology/thesis-schema.ttl`
- **Content**: `ontology/kgc-unified-content.ttl` (create this)
- **Templates**: `templates/*.hbs` (create these)
- **Output**: `output/` (generated, gitignored)

## Troubleshotics

**Issue**: `ggen sync` fails with "query error"
- **Solution**: Validate SPARQL queries in `ggen.toml` against `thesis-schema.ttl`

**Issue**: Missing LaTeX packages
- **Solution**: Install full TeX Live distribution: `sudo apt-get install texlive-full`

**Issue**: Generated `.tex` has syntax errors
- **Solution**: Check Handlebars template syntax in `templates/` directory

**Issue**: Bibliography not rendering
- **Solution**: Run `bibtex` between `pdflatex` invocations (see Step 5)

## Related Specifications

- **Feature Spec**: `.specify/specs/012-grand-unified-kgc-thesis/spec.md`
- **Data Model**: `.specify/specs/012-grand-unified-kgc-thesis/data-model.md`
- **Implementation Plan**: `.specify/specs/012-grand-unified-kgc-thesis/plan.md`
- **Tasks**: `.specify/specs/012-grand-unified-kgc-thesis/tasks.md`

## License

This example is part of the ggen project (MIT License).
