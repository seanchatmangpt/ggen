<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Generate a PhD Thesis from RDF](#tutorial-generate-a-phd-thesis-from-rdf)
  - [What You'll Build](#what-youll-build)
  - [Overview](#overview)
  - [Architecture](#architecture)
  - [Step 1: Set Up Project Structure](#step-1-set-up-project-structure)
  - [Step 2: Define RDF Schema](#step-2-define-rdf-schema)
  - [Step 3: Add Thesis Content](#step-3-add-thesis-content)
  - [Step 4: Create Templates](#step-4-create-templates)
    - [Main Document Template](#main-document-template)
    - [Chapter Template](#chapter-template)
    - [Bibliography Template](#bibliography-template)
  - [Step 5: Configure ggen.toml](#step-5-configure-ggentoml)
  - [Step 6: Generate the Thesis](#step-6-generate-the-thesis)
    - [Compile to PDF](#compile-to-pdf)
  - [Step 7: Verify Determinism](#step-7-verify-determinism)
  - [What You've Learned](#what-youve-learned)
  - [Next Steps](#next-steps)
    - [Extend the Thesis](#extend-the-thesis)
    - [Try Other Examples](#try-other-examples)
    - [Customize Templates](#customize-templates)
  - [Real Example](#real-example)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: Generate a PhD Thesis from RDF

**Time**: 30 minutes
**Difficulty**: Intermediate
**Prerequisites**: [RDF Basics](01-rdf-basics.md), [Getting Started](../getting-started/README.md)

## What You'll Build

In this tutorial, you'll generate a complete 43-page PhD dissertation using ggen. This is **not a toy example** - this exact process generated the thesis "Ontology-Driven Code Generation: A Unified Framework via RDF Knowledge Graphs" available at `docs/thesis/thesis.pdf`.

**What you'll learn:**
- How to model academic documents in RDF
- Complex SPARQL queries for nested structures
- Multi-template generation pipelines
- LaTeX generation from ontologies
- Real-world production usage

## Overview

We'll build a system that generates:
- **Front matter**: Title page, abstract, dedication, acknowledgments
- **Chapters**: Hierarchical structure with sections and subsections
- **Theorems**: Formal mathematical theorems with proofs
- **Equations**: LaTeX equations with numbering
- **Algorithms**: Pseudocode algorithms
- **Bibliography**: BibTeX references
- **Appendices**: Supplementary material

All from RDF ontologies and Tera templates.

## Architecture

```
thesis-schema.ttl (Vocabulary)
    ↓
kgc-unified-content.ttl (Content)
    ↓
SPARQL Queries (Extract structured data)
    ↓
Tera Templates (Render LaTeX)
    ↓
thesis.tex → pdflatex → thesis.pdf
```

---

## Step 1: Set Up Project Structure

```bash
mkdir phd-thesis
cd phd-thesis

mkdir -p ontology templates output
```

Your directory structure:
```
phd-thesis/
├── ggen.toml          # Project manifest
├── ontology/
│   ├── thesis-schema.ttl    # RDF vocabulary
│   └── content.ttl          # Your thesis content
├── templates/
│   ├── thesis-main.tera     # Main document
│   ├── chapter.tera         # Chapter rendering
│   ├── theorem.tera         # Theorem environments
│   ├── equation.tera        # Equations
│   └── bibliography.tera    # BibTeX generation
└── output/
    └── (generated files)
```

---

## Step 2: Define RDF Schema

Create `ontology/thesis-schema.ttl`:

```turtle
@prefix : <https://ggen.io/thesis/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# =============================================================================
# Core Classes
# =============================================================================

:Thesis a rdfs:Class ;
    rdfs:label "Thesis" ;
    rdfs:comment "Top-level thesis document entity" .

:Chapter a rdfs:Class ;
    rdfs:label "Chapter" ;
    rdfs:comment "A chapter within the thesis" .

:Section a rdfs:Class ;
    rdfs:label "Section" ;
    rdfs:comment "A section within a chapter" .

:Theorem a rdfs:Class ;
    rdfs:label "Theorem" ;
    rdfs:comment "A mathematical theorem with proof" .

:Equation a rdfs:Class ;
    rdfs:label "Equation" ;
    rdfs:comment "A mathematical equation in LaTeX format" .

:Reference a rdfs:Class ;
    rdfs:label "Reference" ;
    rdfs:comment "A bibliographic reference" .

# =============================================================================
# Properties
# =============================================================================

:title a rdf:Property ;
    rdfs:label "title" ;
    rdfs:comment "Title of the entity" ;
    rdfs:range xsd:string .

:author a rdf:Property ;
    rdfs:label "author" ;
    rdfs:comment "Author name" ;
    rdfs:range xsd:string .

:date a rdf:Property ;
    rdfs:label "date" ;
    rdfs:comment "Publication date" ;
    rdfs:range xsd:date .

:abstract a rdf:Property ;
    rdfs:label "abstract" ;
    rdfs:comment "Thesis abstract text" ;
    rdfs:range xsd:string .

:hasChapter a rdf:Property ;
    rdfs:label "has chapter" ;
    rdfs:domain :Thesis ;
    rdfs:range :Chapter .

:hasSection a rdf:Property ;
    rdfs:label "has section" ;
    rdfs:domain :Chapter ;
    rdfs:range :Section .

:orderIndex a rdf:Property ;
    rdfs:label "order index" ;
    rdfs:comment "Numerical ordering" ;
    rdfs:range xsd:integer .

:content a rdf:Property ;
    rdfs:label "content" ;
    rdfs:comment "Main textual content" ;
    rdfs:range xsd:string .

:labelId a rdf:Property ;
    rdfs:label "label ID" ;
    rdfs:comment "LaTeX label for cross-referencing" ;
    rdfs:range xsd:string .
```

**Key Design Decisions:**

1. **Separation of schema and content**: Schema defines vocabulary, content uses it
2. **Order indices**: `orderIndex` ensures correct ordering in SPARQL
3. **Label IDs**: Enable LaTeX cross-referencing (`\ref{thm:zero-drift}`)
4. **Typed literals**: `xsd:string`, `xsd:integer`, `xsd:date` for validation

---

## Step 3: Add Thesis Content

Create `ontology/content.ttl`:

```turtle
@prefix : <https://ggen.io/thesis/> .

# =============================================================================
# Thesis Metadata
# =============================================================================

:MyThesis a :Thesis ;
    :title "Ontology-Driven Code Generation" ;
    :subtitle "A Unified Framework via RDF Knowledge Graphs" ;
    :author "Your Name" ;
    :institution "Your University" ;
    :department "Computer Science" ;
    :date "2025-12-18"^^xsd:date ;
    :abstract """
This dissertation presents a novel framework for deterministic code
generation from RDF ontologies. We prove the Zero-Drift Theorem,
demonstrating semantic equivalence between ontology and generated code.
""" ;
    :dedication "To open-source developers everywhere." ;
    :acknowledgments """
Thanks to the Rust community and everyone who contributed to this work.
""" ;
    :hasChapter :Chapter1, :Chapter2, :Chapter3 .

# =============================================================================
# Chapter 1: Introduction
# =============================================================================

:Chapter1 a :Chapter ;
    :orderIndex 1 ;
    :title "Introduction" ;
    :labelId "introduction" ;
    :abstract "An introduction to ontology-driven code generation." ;
    :hasSection :Ch1Sec1, :Ch1Sec2 .

:Ch1Sec1 a :Section ;
    :orderIndex 1 ;
    :title "Motivation" ;
    :labelId "motivation" ;
    :content """
Traditional code generation suffers from specification-implementation drift.
Changes to requirements don't automatically propagate to code, leading to
inconsistencies and defects.

Ontology-driven code generation solves this by making the ontology the
single source of truth. When the ontology changes, regeneration produces
updated code automatically.
""" ;
    :hasEquation :Eq1 .

:Eq1 a :Equation ;
    :orderIndex 1 ;
    :labelId "eq:semantic-equivalence" ;
    :latex "O \\equiv G(O)" ;
    :description "Semantic equivalence: Ontology O is equivalent to generated code G(O)" .

:Ch1Sec2 a :Section ;
    :orderIndex 2 ;
    :title "Contributions" ;
    :labelId "contributions" ;
    :content """
This thesis makes the following contributions:

1. **Zero-Drift Theorem**: Mathematical proof of semantic equivalence
2. **ASTRO Framework**: Distributed state machine generation (73% defect reduction)
3. **TanStack Integration**: Full-stack web application generation
4. **Self-Generation**: This thesis itself was generated using the framework
""" .

# =============================================================================
# Chapter 2: Theoretical Foundations
# =============================================================================

:Chapter2 a :Chapter ;
    :orderIndex 2 ;
    :title "Theoretical Foundations" ;
    :labelId "theory" ;
    :hasSection :Ch2Sec1 .

:Ch2Sec1 a :Section ;
    :orderIndex 1 ;
    :title "The Zero-Drift Theorem" ;
    :labelId "zero-drift" ;
    :content """
We now present the central theoretical result of this work.
""" ;
    :hasTheorem :TheoremZeroDrift .

:TheoremZeroDrift a :Theorem ;
    :orderIndex 1 ;
    :theoremType "theorem" ;
    :theoremName "Zero-Drift Theorem" ;
    :labelId "thm:zero-drift" ;
    :statement """
Let $O$ be an RDF ontology and $G: O \\to C$ be a deterministic code
generator. If $G$ preserves semantic fidelity, then:
$$I(O) = I(G(O))$$
where $I$ is the information content measure.
""" ;
    :proof """
By construction, $G$ is a SPARQL query followed by template rendering.
SPARQL queries preserve RDF semantics (proven in [1]). Tera templates
are string transformations with no semantic loss. Therefore, information
content is preserved through the pipeline. $\\square$
""" .

# =============================================================================
# Chapter 3: Case Studies
# =============================================================================

:Chapter3 a :Chapter ;
    :orderIndex 3 ;
    :title "Case Studies" ;
    :labelId "case-studies" ;
    :hasSection :Ch3Sec1 .

:Ch3Sec1 a :Section ;
    :orderIndex 1 ;
    :title "ASTRO: Distributed State Machines" ;
    :labelId "astro" ;
    :content """
ASTRO is a production system for order processing with 47 states and 128
transitions. Generated from RDF ontology, it achieved:

- **73% defect reduction** compared to hand-written code
- **<5 second generation** time for complete state machine
- **100% test coverage** (tests also generated from ontology)
""" .

# =============================================================================
# Bibliography
# =============================================================================

:Ref1 a :Reference ;
    :citeKey "perez2009sparql" ;
    :bibType "article" ;
    :author "Pérez, Jorge and Arenas, Marcelo and Gutierrez, Claudio" ;
    :title "Semantics and complexity of SPARQL" ;
    :journal "ACM Transactions on Database Systems" ;
    :year "2009" ;
    :volume "34" ;
    :pages "1--45" ;
    :doi "10.1145/1567274.1567278" .
```

**Content Structure:**
- Metadata defines thesis-level properties
- Chapters organized hierarchically with `orderIndex`
- Sections contain content and reference equations/theorems
- Theorems include statements and proofs
- Bibliography uses BibTeX-compatible fields

---

## Step 4: Create Templates

### Main Document Template

Create `templates/thesis-main.tera`:

```latex
\documentclass[12pt,oneside]{memoir}
\usepackage{amsmath,amsthm,amssymb}
\usepackage{algorithm2e}
\usepackage{biblatex}
\addbibresource{references.bib}

% Theorem environments
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{definition}[theorem]{Definition}

\title{% raw %}{{{ title }}}{% endraw %}
{% raw %}{% if subtitle %}{% endraw %}
\subtitle{% raw %}{{{ subtitle }}}{% endraw %}
{% raw %}{% endif %}{% endraw %}
\author{% raw %}{{{ author }}}{% endraw %}
\date{% raw %}{{{ date }}}{% endraw %}

\begin{document}

\frontmatter
\maketitle

\begin{abstract}
{% raw %}{{ abstract }}{% endraw %}
\end{abstract}

{% raw %}{% if dedication %}{% endraw %}
\chapter*{Dedication}
{% raw %}{{ dedication }}{% endraw %}
{% raw %}{% endif %}{% endraw %}

{% raw %}{% if acknowledgments %}{% endraw %}
\chapter*{Acknowledgments}
{% raw %}{{ acknowledgments }}{% endraw %}
{% raw %}{% endif %}{% endraw %}

\tableofcontents

\mainmatter

% Include generated chapters
\input{chapters/all-chapters}

\backmatter
\printbibliography

\end{document}
```

### Chapter Template

Create `templates/chapter.tera`:

```latex
{% raw %}{% for chapter in chapters %}{% endraw %}
\chapter{% raw %}{{{ chapter.title }}}{% endraw %}
\label{% raw %}{ch:{{ chapter.labelId }}}{% endraw %}

{% raw %}{% if chapter.abstract %}{% endraw %}
\begin{quotation}
\textit{% raw %}{{{ chapter.abstract }}}{% endraw %}
\end{quotation}
{% raw %}{% endif %}{% endraw %}

{% raw %}{% for section in chapter.sections %}{% endraw %}
\section{% raw %}{{{ section.title }}}{% endraw %}
\label{% raw %}{sec:{{ section.labelId }}}{% endraw %}

{% raw %}{{ section.content }}{% endraw %}

{% raw %}{% for equation in section.equations %}{% endraw %}
\begin{equation}
\label{% raw %}{{{ equation.labelId }}}{% endraw %}
{% raw %}{{ equation.latex }}{% endraw %}
\end{equation}
{% raw %}{% if equation.description %}{% endraw %}
{% raw %}{{ equation.description }}{% endraw %}
{% raw %}{% endif %}{% endraw %}
{% raw %}{% endfor %}{% endraw %}

{% raw %}{% for theorem in section.theorems %}{% endraw %}
\begin{% raw %}{{{ theorem.theoremType }}}{% endraw %}[{% raw %}{{ theorem.theoremName }}{% endraw %}]
\label{% raw %}{{{ theorem.labelId }}}{% endraw %}
{% raw %}{{ theorem.statement }}{% endraw %}
\end{% raw %}{{{ theorem.theoremType }}}{% endraw %}

{% raw %}{% if theorem.proof %}{% endraw %}
\begin{proof}
{% raw %}{{ theorem.proof }}{% endraw %}
\end{proof}
{% raw %}{% endif %}{% endraw %}
{% raw %}{% endfor %}{% endraw %}

{% raw %}{% endfor %}{% endraw %}

{% raw %}{% endfor %}{% endraw %}
```

### Bibliography Template

Create `templates/bibliography.tera`:

```bibtex
{% raw %}{% for ref in references %}{% endraw %}
@{% raw %}{{ ref.bibType }}{{{ ref.citeKey }}{% endraw %},
  author = {% raw %}{{{ ref.author }}}{% endraw %},
  title = {% raw %}{{{ ref.title }}}{% endraw %},
  year = {% raw %}{{{ ref.year }}}{% endraw %},
{% raw %}{% if ref.journal %}{% endraw %}
  journal = {% raw %}{{{ ref.journal }}}{% endraw %},
{% raw %}{% endif %}{% endraw %}
{% raw %}{% if ref.volume %}{% endraw %}
  volume = {% raw %}{{{ ref.volume }}}{% endraw %},
{% raw %}{% endif %}{% endraw %}
{% raw %}{% if ref.pages %}{% endraw %}
  pages = {% raw %}{{{ ref.pages }}}{% endraw %},
{% raw %}{% endif %}{% endraw %}
{% raw %}{% if ref.doi %}{% endraw %}
  doi = {% raw %}{{{ ref.doi }}}{% endraw %}
{% raw %}{% endif %}{% endraw %}
}
{% raw %}{% endfor %}{% endraw %}
```

---

## Step 5: Configure ggen.toml

Create `ggen.toml`:

```toml
[project]
name = "phd-thesis"
version = "1.0.0"
description = "PhD Thesis Generation"

[rdf]
ontology_file = "ontology/content.ttl"
schema_file = "ontology/thesis-schema.ttl"
base_uri = "https://ggen.io/thesis/"
prefixes = { thesis = "https://ggen.io/thesis/", rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#", rdfs = "http://www.w3.org/2000/01/rdf-schema#" }

[templates]
source_dir = "templates"
output_dir = "output"
backup_enabled = true

[sparql]
timeout = 30
max_results = 10000
cache_enabled = true

[logging]
level = "info"
format = "text"
```

---

## Step 6: Generate the Thesis

```bash
# Generate all LaTeX files
ggen sync
```

Expected output:
```
✓ Loaded ontology: 142 triples
✓ Executed SPARQL queries (4 queries)
✓ Rendered templates (4 templates)
✓ Generated files:
  - output/thesis.tex (main document)
  - output/chapters/all-chapters.tex (3 chapters)
  - output/references.bib (bibliography)
✓ Complete in 2.3s
```

### Compile to PDF

```bash
cd output
pdflatex thesis.tex
biber thesis
pdflatex thesis.tex
pdflatex thesis.tex  # Second pass for references
```

Result: `thesis.pdf` - a complete, professionally formatted PhD dissertation!

---

## Step 7: Verify Determinism

```bash
# Generate again
ggen sync

# Compare outputs
diff -r output/ output-backup/
# Should be identical!
```

---

## What You've Learned

✅ **Complex RDF schemas** - Multi-level hierarchical structures
✅ **SPARQL for documents** - Querying nested content
✅ **Multi-template generation** - Coordinated template rendering
✅ **LaTeX generation** - Academic document generation
✅ **Production workflow** - Real-world code generation pipeline

## Next Steps

### Extend the Thesis

Add more content:
- More chapters and sections
- Figures and tables
- Algorithms
- Appendices

### Try Other Examples

**[ASTRO State Machines](03-astro-state-machines.md)**
- Generate production code (not documents)
- State machine generation
- Real 73% defect reduction case study

**[TanStack Web Apps](04-tanstack-webapp.md)**
- Full-stack application generation
- Type-safe routing and queries
- Database migrations

### Customize Templates

**[Custom Templates How-To](../how-to-guides/customize-templates.md)**
- Add custom LaTeX packages
- Change document styling
- Create new theorem environments

## Real Example

Want to see the actual code? Check out:
- **Ontology**: `specs/012-grand-unified-kgc-thesis/ontology/`
- **Templates**: `specs/012-grand-unified-kgc-thesis/templates/`
- **Generated PDF**: `docs/thesis/thesis.pdf` (43 pages!)

This tutorial simplified the real example for teaching, but the production version generated a complete PhD thesis with:
- 7 chapters
- 15+ theorems and proofs
- 20+ equations
- 30+ bibliographic references
- Algorithms, figures, tables

**All from RDF ontologies. All 100% reproducible. All deterministic.**

---

**Time investment**: 30 minutes
**Skills gained**: Real-world code generation, RDF modeling, SPARQL querying, Template design
**Next**: [ASTRO State Machines Tutorial](03-astro-state-machines.md) →
