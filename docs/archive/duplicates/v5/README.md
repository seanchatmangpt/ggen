<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen: Ontology-Driven Code Generation](#ggen-ontology-driven-code-generation)
  - [What is ggen?](#what-is-ggen)
  - [Quick Start](#quick-start)
  - [Why ggen?](#why-ggen)
    - [Zero-Drift Theorem](#zero-drift-theorem)
    - [Production-Proven](#production-proven)
    - [Deterministic & Fast](#deterministic--fast)
    - [Multi-Format Support](#multi-format-support)
  - [Documentation](#documentation)
    - [🚀 Getting Started](#-getting-started)
    - [📚 Tutorials (Learning-Oriented)](#-tutorials-learning-oriented)
    - [🔧 How-To Guides (Problem-Oriented)](#-how-to-guides-problem-oriented)
    - [📖 Reference (Information-Oriented)](#-reference-information-oriented)
    - [💡 Explanations (Understanding-Oriented)](#-explanations-understanding-oriented)
    - [📦 Examples](#-examples)
  - [Common Questions](#common-questions)
    - ["I just want to generate code quickly"](#i-just-want-to-generate-code-quickly)
    - ["I don't know RDF"](#i-dont-know-rdf)
    - ["How do I customize the generated code?"](#how-do-i-customize-the-generated-code)
    - ["What can ggen generate?"](#what-can-ggen-generate)
    - ["Is this production-ready?"](#is-this-production-ready)
  - [Performance](#performance)
  - [Architecture](#architecture)
  - [How It Works: Example](#how-it-works-example)
  - [Contributing](#contributing)
  - [Support](#support)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen: Ontology-Driven Code Generation

> Transform RDF knowledge graphs into production code with zero ambiguity and 100% reproducibility.

## What is ggen?

**ggen** is a deterministic code generator that transforms RDF ontologies into any target artifact using SPARQL queries and Tera templates.

**In 3 Steps:**
1. **Model** your domain in RDF (Turtle, N3, RDF/XML, JSON-LD)
2. **Query** semantic facts with SPARQL
3. **Generate** deterministic code via templates

**Real-World Proof**: This documentation describes a working system that has generated:
- **ASTRO**: Distributed state machines with 47 states, 128 transitions (73% defect reduction in production)
- **TanStack Applications**: Full-stack web apps with type-safe routing, data fetching, and reactive sync
- **PhD Thesis**: 43-page dissertation with theorems, equations, and bibliography (self-generated!)

## Quick Start

```bash
# Install
cargo install ggen

# Create ggen.toml manifest
cat > ggen.toml << 'EOF'
[project]
name = "my-project"

[rdf]
ontology_file = "domain.ttl"

[templates]
source_dir = "templates"
output_dir = "output"
EOF

# Run generation
ggen sync
```

**New to ggen?** → Start with [Getting Started](getting-started/README.md) (15 minutes)

## Why ggen?

### Zero-Drift Theorem
**Mathematical guarantee**: Ontology ≡ Generated Code (provably equivalent representations).

No specification-implementation drift. Ever.

### Production-Proven
| Case Study | Domain | Generated Artifacts | Results |
|------------|--------|---------------------|---------|
| **ASTRO** | Distributed systems | State machines, transitions, guards | 73% defect reduction |
| **TanStack** | Web applications | Routes, queries, schemas, migrations | Type-safe full-stack |
| **Thesis** | Academic publishing | LaTeX chapters, theorems, bibliography | 43-page PhD dissertation |

### Deterministic & Fast
- **Same input = identical output** (bit-for-bit reproducible)
- **<5 second generation** for enterprise-scale ontologies (1000+ triples)
- **No hallucinations** (semantic queries, not LLM generation)

### Multi-Format Support
Generate **anything** from RDF:
- **Code**: Rust, TypeScript, Python, SQL
- **Documents**: LaTeX, Markdown, HTML
- **Data**: JSON, YAML, TOML
- **Infrastructure**: Kubernetes manifests, Terraform configs

Templates are in **your control** - ggen provides the engine.

## Documentation

### 🚀 Getting Started
New to ggen? Start here:
- **[Getting Started](getting-started/README.md)** - Zero to code generation in 15 minutes

### 📚 Tutorials (Learning-Oriented)
Step-by-step guides that teach you how to use ggen:
1. **[RDF Basics](tutorials/01-rdf-basics.md)** - Learn RDF modeling (no prerequisites)
2. **[Generate a Thesis](tutorials/02-thesis-generation.md)** - Generate LaTeX documents from RDF
3. **[ASTRO State Machines](tutorials/03-astro-state-machines.md)** - Generate distributed systems
4. **[TanStack Web Apps](tutorials/04-tanstack-webapp.md)** - Full-stack web application generation
5. **[Custom Templates](tutorials/05-custom-templates.md)** - Build your own code generators

### 🔧 How-To Guides (Problem-Oriented)
Solve specific problems:
- **[Write SPARQL Queries](how-to-guides/write-sparql-queries.md)** - Query patterns
- **[Customize Templates](how-to-guides/customize-templates.md)** - Override defaults
- **[Debug Generation](how-to-guides/debug-generation.md)** - Troubleshooting
- **[Integrate CI/CD](how-to-guides/integrate-ci-cd.md)** - GitHub Actions
- **[Validate Ontologies](how-to-guides/validate-ontologies.md)** - SHACL constraints

### 📖 Reference (Information-Oriented)
Complete technical documentation:
- **[CLI Commands](reference/cli.md)** - `ggen sync`, `ggen validate`, exit codes
- **[ggen.toml](reference/ggen-toml.md)** - Project manifest format
- **[RDF Ontology Structure](reference/rdf-ontology-structure.md)** - Schema patterns
- **[SPARQL Patterns](reference/sparql-patterns.md)** - Query reference
- **[Tera Templates](reference/tera-templates.md)** - Template syntax and filters
- **[Thesis Schema](reference/thesis-schema.md)** - Academic document ontology

### 💡 Explanations (Understanding-Oriented)
Conceptual background:
- **[Why RDF?](explanations/why-rdf.md)** - Semantic precision over JSON
- **[Zero-Drift Theorem](explanations/zero-drift-theorem.md)** - Mathematical foundations
- **[Determinism](explanations/determinism.md)** - Reproducibility guarantees
- **[Architecture](explanations/architecture.md)** - System design
- **[Semantic Fidelity](explanations/semantic-fidelity.md)** - Information preservation

### 📦 Examples
Working code examples from real projects:
- **[Thesis Generation](examples/thesis-generation/)** - 43-page PhD dissertation
- **[ASTRO State Machine](examples/astro-state-machine/)** - Distributed order processing
- **[TanStack Web App](examples/tanstack-webapp/)** - Modern full-stack application

## Common Questions

### "I just want to generate code quickly"
→ [Getting Started Guide](getting-started/README.md) (15 minutes to first generation)

### "I don't know RDF"
→ [RDF Basics Tutorial](tutorials/01-rdf-basics.md) (15 minutes, no prerequisites)

### "How do I customize the generated code?"
→ [Customize Templates How-To](how-to-guides/customize-templates.md)

### "What can ggen generate?"
Anything you can template:
- **Source code** (Rust, TypeScript, Python, etc.)
- **Documents** (LaTeX, Markdown, HTML)
- **Configuration** (JSON, YAML, TOML)
- **Infrastructure** (Kubernetes, Terraform)

Real examples: PhD thesis, distributed state machines, web applications.

### "Is this production-ready?"
Yes. Proven in production with:
- **ASTRO** - 73% defect reduction in distributed order processing
- **TanStack apps** - Type-safe full-stack web applications
- **Academic publishing** - Generated its own 43-page PhD thesis

## Performance

ggen is designed for speed:

| Operation | Performance |
|-----------|-------------|
| Full thesis generation (43 pages) | <5 seconds |
| ASTRO state machine (47 states, 128 transitions) | <3 seconds |
| Enterprise-scale ontology (1000+ triples) | <5 seconds |
| Incremental regeneration | <1 second |

## Architecture

ggen uses a clean 3-stage pipeline:

```
RDF Ontology (Turtle/N3/JSON-LD)
    ↓
SPARQL Query (Extract structured data)
    ↓
Tera Template (Render output)
    ↓
Generated Artifact (Code/Docs/Config)
```

**Key Technologies**:
- **Oxigraph**: SPARQL 1.1 query engine
- **Tera**: Jinja2-like template engine
- **Rust**: Memory-safe, deterministic core

## How It Works: Example

**Problem**: Generate a PhD thesis from structured content.

**1. Define Ontology** (`thesis-schema.ttl`):
```turtle
:Thesis a rdfs:Class ;
    rdfs:label "Thesis" .

:Chapter a rdfs:Class ;
    rdfs:label "Chapter" .

:hasChapter a rdf:Property ;
    rdfs:domain :Thesis ;
    rdfs:range :Chapter .
```

**2. Add Content** (`my-thesis.ttl`):
```turtle
:MyThesis a :Thesis ;
    :title "Ontology-Driven Code Generation" ;
    :author "Sean Chatman" ;
    :hasChapter :Chapter1 .

:Chapter1 a :Chapter ;
    :chapterNumber 1 ;
    :title "Introduction" ;
    :content "Semantic code generation transforms..." .
```

**3. Query with SPARQL**:
```sparql
SELECT ?title ?author ?chapterNum ?chapterTitle
WHERE {
    ?thesis a :Thesis ;
            :title ?title ;
            :author ?author ;
            :hasChapter ?chapter .
    ?chapter :chapterNumber ?chapterNum ;
             :title ?chapterTitle .
} ORDER BY ?chapterNum
```

**4. Template with Tera** (`thesis.tera`):
```latex
\documentclass{memoir}
\title{% raw %}{{ title }}{% endraw %}
\author{% raw %}{{ author }}{% endraw %}
\begin{document}
{% raw %}{% for chapter in chapters %}{% endraw %}
\chapter{% raw %}{{{ chapter.title }}}{% endraw %}
{% raw %}{{ chapter.content }}{% endraw %}
{% raw %}{% endfor %}{% endraw %}
\end{document}
```

**5. Generate** (`ggen sync`):
```bash
$ ggen sync
✓ Loaded ontology (142 triples)
✓ Executed SPARQL queries
✓ Rendered templates
✓ Generated: thesis.tex (43 pages)
Complete in 2.3s
```

**Result**: Production-ready LaTeX, bit-for-bit reproducible.

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## Support

- **Documentation Issues**: [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- **Questions**: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- **Bug Reports**: [Issue Template](https://github.com/seanchatmangpt/ggen/issues/new)

## License

MIT License - See [LICENSE](../../LICENSE) for details.

---

**Next Steps:**
1. **[Getting Started (15 min)](getting-started/README.md)** - Your first code generation
2. **[RDF Basics Tutorial](tutorials/01-rdf-basics.md)** - Learn RDF modeling
3. **[Thesis Generation Tutorial](tutorials/02-thesis-generation.md)** - Generate a complete document

**Real Examples:**
- **[Thesis Generation](examples/thesis-generation/)** - See the actual ontology and templates that generated a 43-page PhD dissertation
- **[ASTRO State Machine](examples/astro-state-machine/)** - Production distributed system with 73% defect reduction
