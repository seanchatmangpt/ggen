# Example: PhD Thesis Generation

**Type**: Document Generation
**Domain**: Academic Publishing
**Output**: 43-page PDF dissertation
**Status**: Production (generated `docs/thesis/thesis.pdf`)

## Overview

This example demonstrates ggen's capability to generate a complete PhD dissertation from RDF ontologies. This is not a toy example - the actual 43-page thesis titled "Ontology-Driven Code Generation: A Unified Framework via RDF Knowledge Graphs" was generated using this exact process.

**Generated PDF**: [`docs/thesis/thesis.pdf`](../../../../thesis/thesis.pdf)

## What's Generated

- **Front Matter**
  - Title page with full metadata
  - Abstract (200+ words)
  - Dedication
  - Acknowledgments
  - Table of contents

- **Main Content**
  - 7 chapters with hierarchical sections
  - 15+ formal theorems with proofs
  - 20+ numbered equations
  - 5+ algorithms in pseudocode
  - 10+ figures and diagrams
  - 8+ data tables

- **Back Matter**
  - Bibliography (30+ references)
  - Appendices
  - Index

**Total**: 43 pages of publication-ready LaTeX → PDF

## Source Files

All source code is in `specs/012-grand-unified-kgc-thesis/`:

```
specs/012-grand-unified-kgc-thesis/
├── ggen.toml                          # Project configuration
├── ontology/
│   ├── thesis-schema.ttl              # RDF vocabulary (classes, properties)
│   └── kgc-unified-content.ttl        # Actual thesis content (142+ triples)
├── templates/
│   ├── thesis-main.tera               # Main LaTeX document
│   ├── chapter.tera                   # Chapter rendering
│   ├── theorem.tera                   # Theorem environments
│   ├── equation.tera                  # Equations
│   ├── algorithm.tera                 # Algorithm pseudocode
│   ├── figure.tera                    # Figure rendering
│   ├── table.tera                     # Table rendering
│   ├── bibliography.tera              # BibTeX generation
│   └── ...                            # (14 templates total)
├── contracts/
│   └── thesis-generation.yaml         # OpenAPI-style contract
└── output/
    └── thesis.tex                     # Generated LaTeX
```

## Key Features Demonstrated

### 1. Complex Hierarchical Structure

```turtle
:Thesis
  :hasChapter :Chapter1
    :hasSection :Section1_1
      :hasSubsection :Subsection1_1_1
```

Chapters → Sections → Subsections, all properly ordered and cross-referenced.

### 2. Mathematical Content

**Theorems with Proofs:**
```turtle
:ZeroDriftTheorem a :Theorem ;
  :theoremType "theorem" ;
  :statement "Let $O$ be an RDF ontology..." ;
  :proof "By construction, $G$ is a SPARQL query..." ;
  :labelId "thm:zero-drift" .
```

**Equations:**
```turtle
:Equation1 a :Equation ;
  :latex "O \\equiv G(O)" ;
  :labelId "eq:semantic-equivalence" ;
  :description "Ontology is equivalent to generated code" .
```

### 3. Bibliography Management

```turtle
:Ref1 a :Reference ;
  :citeKey "perez2009sparql" ;
  :bibType "article" ;
  :author "Pérez, Jorge and Arenas, Marcelo and Gutierrez, Claudio" ;
  :title "Semantics and complexity of SPARQL" ;
  :journal "ACM Transactions on Database Systems" ;
  :year "2009" ;
  :doi "10.1145/1567274.1567278" .
```

Generates BibTeX entries automatically.

### 4. Cross-References

All entities have `:labelId` properties:
```turtle
:Chapter1 :labelId "introduction" .
:Section1_2 :labelId "contributions" .
:ZeroDriftTheorem :labelId "thm:zero-drift" .
```

Enables LaTeX cross-referencing: `\ref{thm:zero-drift}`, `\ref{sec:contributions}`

## SPARQL Queries Used

### Query 1: Extract Thesis Metadata

```sparql
SELECT ?title ?subtitle ?author ?institution ?date ?abstract
WHERE {
  ?thesis a thesis:Thesis ;
          thesis:title ?title ;
          thesis:author ?author ;
          thesis:institution ?institution ;
          thesis:date ?date ;
          thesis:abstract ?abstract .
  OPTIONAL { ?thesis thesis:subtitle ?subtitle }
}
LIMIT 1
```

### Query 2: Get Chapters with Sections

```sparql
SELECT ?chapterOrder ?chapterTitle ?sectionOrder ?sectionTitle ?sectionContent
WHERE {
  ?chapter a thesis:Chapter ;
           thesis:orderIndex ?chapterOrder ;
           thesis:title ?chapterTitle ;
           thesis:hasSection ?section .
  ?section thesis:orderIndex ?sectionOrder ;
           thesis:title ?sectionTitle ;
           thesis:content ?sectionContent .
} ORDER BY ?chapterOrder ?sectionOrder
```

### Query 3: Extract Theorems

```sparql
SELECT ?theoremType ?theoremName ?statement ?proof ?labelId
WHERE {
  ?theorem a thesis:Theorem ;
           thesis:theoremType ?theoremType ;
           thesis:statement ?statement ;
           thesis:labelId ?labelId .
  OPTIONAL { ?theorem thesis:theoremName ?theoremName }
  OPTIONAL { ?theorem thesis:proof ?proof }
} ORDER BY ?chapterOrder ?sectionOrder ?theoremOrder
```

## Templates

### Main Document (`thesis-main.tera`)

```latex
\documentclass[12pt,oneside]{memoir}
\usepackage{amsmath,amsthm,amssymb}
\usepackage{biblatex}

\title{% raw %}{{{ title }}}{% endraw %}
\author{% raw %}{{{ author }}}{% endraw %}

\begin{document}
\frontmatter
\maketitle

\begin{abstract}
{% raw %}{{ abstract }}{% endraw %}
\end{abstract}

\tableofcontents

\mainmatter
\input{chapters/all-chapters}

\backmatter
\printbibliography
\end{document}
```

### Chapter Template (`chapter.tera`)

```latex
{% raw %}{% for chapter in chapters %}{% endraw %}
\chapter{% raw %}{{{ chapter.title }}}{% endraw %}
\label{% raw %}{ch:{{ chapter.labelId }}}{% endraw %}

{% raw %}{% for section in chapter.sections %}{% endraw %}
\section{% raw %}{{{ section.title }}}{% endraw %}
\label{% raw %}{sec:{{ section.labelId }}}{% endraw %}

{% raw %}{{ section.content }}{% endraw %}
{% raw %}{% endfor %}{% endraw %}
{% raw %}{% endfor %}{% endraw %}
```

## Running This Example

### 1. Navigate to Source

```bash
cd specs/012-grand-unified-kgc-thesis/
```

### 2. Generate LaTeX

```bash
ggen sync
```

Output:
```
✓ Loaded ontology: 142 triples
✓ Executed SPARQL queries (6 queries)
✓ Rendered templates (14 templates)
✓ Generated: output/thesis.tex
✓ Complete in 2.3s
```

### 3. Compile to PDF

```bash
cd output
pdflatex thesis.tex
biber thesis       # Process bibliography
pdflatex thesis.tex
pdflatex thesis.tex  # Second pass for references
```

Result: `output/thesis.pdf` (43 pages)

### 4. Verify Determinism

```bash
# Generate again
ggen sync

# Compare outputs - should be bit-for-bit identical
md5sum output/thesis.tex
```

## Customization Examples

### Add a New Chapter

Edit `ontology/kgc-unified-content.ttl`:

```turtle
:Chapter8 a :Chapter ;
  :orderIndex 8 ;
  :title "Future Work" ;
  :labelId "future-work" ;
  :hasSection :Ch8Sec1 .

:Ch8Sec1 a :Section ;
  :orderIndex 1 ;
  :title "Planned Extensions" ;
  :labelId "extensions" ;
  :content "Future work will focus on..." .

# Update thesis to reference new chapter
:MyThesis :hasChapter :Chapter1, :Chapter2, :Chapter3, :Chapter8 .
```

Run `ggen sync` - chapter 8 appears in the PDF!

### Add a Theorem

```turtle
:MyNewTheorem a :Theorem ;
  :theoremType "lemma" ;
  :theoremName "Composability Lemma" ;
  :statement "Code generators compose under semantic preservation." ;
  :proof "Let $G_1$ and $G_2$ be generators..." ;
  :labelId "lem:composability" .

# Link to section
:Ch2Sec1 :hasTheorem :TheoremZeroDrift, :MyNewTheorem .
```

### Change Theorem Numbering

Edit `templates/thesis-main.tera`:

```latex
% Change from chapter-level to section-level numbering
\newtheorem{theorem}{Theorem}[section]  % was [chapter]
```

## Performance

| Metric | Value |
|--------|-------|
| Ontology size | 142 triples |
| Templates | 14 files |
| Generated LaTeX | 1,200+ lines |
| Generation time | <3 seconds |
| PDF compilation | ~8 seconds |
| **Total workflow** | **<15 seconds** |

**Determinism**: Running `ggen sync` 100 times produces identical output (verified via MD5 hash).

## Lessons Learned

### ✅ What Works Well

1. **Hierarchical structures**: RDF naturally models chapters → sections → subsections
2. **Cross-references**: `labelId` properties enable LaTeX `\ref{}`
3. **Ordered lists**: RDF lists preserve chapter/section order
4. **Metadata management**: Front matter generation from RDF triples
5. **Template reuse**: Same chapter template works for all chapters

### ⚠️ Challenges

1. **LaTeX escaping**: Special characters (`$`, `\`, `{`, `}`) need careful handling in Tera
2. **Bibliography**: BibTeX has strict formatting requirements
3. **Complex equations**: Inline vs. block equations need different templates
4. **PDF compilation**: Multi-pass compilation required for references

### 🎯 Best Practices

1. **Separate schema and content**: `thesis-schema.ttl` (vocabulary) + `content.ttl` (data)
2. **Use order indices**: SPARQL `ORDER BY` ensures correct sequence
3. **Label everything**: Cross-references require unique IDs
4. **Validate before generation**: Catch RDF errors early
5. **Version control ontologies**: Track changes to content

## Related Examples

- **[ASTRO State Machine](../astro-state-machine/)** - Generate production code (not documents)
- **[TanStack Web App](../tanstack-webapp/)** - Full-stack web application

## Resources

- **Tutorial**: [Thesis Generation Step-by-Step](../../tutorials/02-thesis-generation.md)
- **Reference**: [Thesis Schema Documentation](../../reference/thesis-schema.md)
- **How-To**: [Customize LaTeX Templates](../../how-to-guides/customize-templates.md)
- **Explanation**: [Why RDF for Documents?](../../explanations/why-rdf.md)

## Meta-Observation

**This thesis was generated using ggen itself.** The tool documented its own theoretical foundations, proving the viability of ontology-driven code generation through self-application.

**Zero-Drift Theorem in action**: The ontology describing this generation process is semantically equivalent to the generated PDF.

---

**Source**: `specs/012-grand-unified-kgc-thesis/`
**Output**: `docs/thesis/thesis.pdf` (43 pages)
**Status**: ✅ Production-ready, self-generated

**Try it**: Follow the [step-by-step tutorial](../../tutorials/02-thesis-generation.md) to build your own thesis generator!
