<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen - Ontology-Driven Code Generation](#ggen---ontology-driven-code-generation)
  - [What is ggen?](#what-is-ggen)
    - [Why RDF Ontologies?](#why-rdf-ontologies)
    - [Perfect For](#perfect-for)
  - [Quick Start (5 Minutes)](#quick-start-5-minutes)
    - [Installation](#installation)
    - [Your First ggen Project (5 minutes)](#your-first-ggen-project-5-minutes)
  - [Documentation](#documentation)
    - [🎓 **I want to learn ggen**](#-i-want-to-learn-ggen)
    - [🔍 **I need to solve a problem**](#-i-need-to-solve-a-problem)
    - [📚 **I need reference information**](#-i-need-reference-information)
    - [💡 **I want to understand concepts**](#-i-want-to-understand-concepts)
    - [🏗️ **I want working examples**](#-i-want-working-examples)
    - [📋 **Full Documentation Index**](#-full-documentation-index)
  - [Core Concepts](#core-concepts)
    - [1. Ontologies (RDF)](#1-ontologies-rdf)
    - [2. SPARQL Queries](#2-sparql-queries)
    - [3. Tera Templates](#3-tera-templates)
    - [4. Generation Rules](#4-generation-rules)
  - [Philosophy](#philosophy)
    - [1. Specification-First (Big Bang 80/20)](#1-specification-first-big-bang-8020)
    - [2. Deterministic Validation](#2-deterministic-validation)
    - [3. RDF-First](#3-rdf-first)
  - [Common Patterns](#common-patterns)
    - [REST API Generation](#rest-api-generation)
    - [Multi-Language Support](#multi-language-support)
    - [Database Schema Generation](#database-schema-generation)
  - [Status](#status)
  - [Contributing](#contributing)
  - [Resources](#resources)
  - [Project Constitution](#project-constitution)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen - Ontology-Driven Code Generation

[![Crates.io](https://img.shields.io/crates/v/ggen.svg)](https://crates.io/crates/ggen)
[![Documentation](https://docs.rs/ggen/badge.svg)](https://docs.rs/ggen)
[![License](https://img.shields.io/crates/l/ggen.svg)](LICENSE)
[![Build Status](https://github.com/seanchatmangpt/ggen/workflows/CI/badge.svg)](https://github.com/seanchatmangpt/ggen/actions)

**Transform RDF ontologies into reproducible code through SPARQL queries and Tera templates.**

---

## What is ggen?

ggen is a **deterministic code generator** that bridges semantic web technologies (RDF, SPARQL, OWL) with modern programming languages. Define your domain model once as an **RDF ontology**, and ggen generates type-safe code across multiple languages.

### Why RDF Ontologies?

- **Single Source of Truth**: Define your data model once, generate everywhere
- **Semantic Validation**: Use OWL constraints and SHACL shapes to catch errors at generation time
- **Intelligent Inference**: SPARQL CONSTRUCT queries materialize implicit relationships
- **Deterministic**: Same ontology + templates = identical output every time
- **Language-Agnostic**: Generate Rust, TypeScript, Python, Go, and more from one source

### Perfect For

- **API Development**: Generate client libraries and servers from API specifications
- **Data Modeling**: Keep microservices synchronized across your architecture
- **Multi-Language Projects**: Sync Rust backends with TypeScript frontends
- **Domain-Driven Design**: Generate code from domain ontologies
- **Academic & Financial**: Research projects requiring semantic validation

---

## Quick Start (5 Minutes)

### Installation

**macOS/Linux (Fastest)**:
```bash
brew install seanchatmangpt/ggen/ggen
ggen --version  # Should show: ggen 5.0.2+
```

**Any Platform (Docker)**:
```bash
docker pull seanchatman/ggen:5.0.2
docker run --rm -v $(pwd):/workspace seanchatman/ggen:5.0.2 sync
```

**From Source (Rust)**:
```bash
cargo install ggen-cli
```

### Your First ggen Project (5 minutes)

**Step 1: Create a minimal ontology** (`schema/Person.ttl`):

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A person in the system" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string ;
    rdfs:label "Full name" .

ex:email a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string ;
    rdfs:label "Email address" .
```

**Step 2: Create configuration** (`ggen.toml`):

```toml
[project]
name = "my-first-app"
version = "0.1.0"

[ontology]
source = "schema/"

[generation]
output_dir = "src/generated"
```

**Step 3: Add a Tera template** (`templates/struct.tera`):

```jinja2
{%- for class in classes %}
#[derive(Debug, Clone)]
pub struct {{ class.name }} {
    {%- for prop in class.properties %}
    pub {{ prop.name }}: String,
    {%- endfor %}
}
{%- endfor %}
```

**Step 4: Generate code**:

```bash
ggen sync
```

Output: `src/generated/struct.rs` with:
```rust
#[derive(Debug, Clone)]
pub struct Person {
    pub name: String,
    pub email: String,
}
```

---

## Documentation

Choose your learning path:

### 🎓 **I want to learn ggen**
Start with [**Tutorials**](docs/GENERATED_TUTORIALS.md) - hands-on, step-by-step projects

### 🔍 **I need to solve a problem**
Check [**How-To Guides**](docs/GENERATED_HOWTO_GUIDES.md) - specific solutions to common tasks

### 📚 **I need reference information**
See [**Reference Docs**](docs/GENERATED_REFERENCE.md) - CLI, ggen.toml, SPARQL, templates

### 💡 **I want to understand concepts**
Read [**Explanations**](docs/GENERATED_EXPLANATIONS.md) - philosophical background and architecture

### 🏗️ **I want working examples**
Explore [**Example Projects**](docs/GENERATED_EXAMPLES.md) - REST APIs, databases, microservices

### 📋 **Full Documentation Index**
[See INDEX.md](docs/INDEX.md) - master listing of all documentation

---

## Core Concepts

### 1. Ontologies (RDF)
Define your domain model in Turtle syntax - classes, properties, relationships, constraints.

### 2. SPARQL Queries
Query the ontology to extract data, run inference (CONSTRUCT), and prepare data for generation.

### 3. Tera Templates
Render code in any language using the Tera template engine with full programming capabilities.

### 4. Generation Rules
Configure which queries feed into which templates, with validation and transformation rules.

---

## Philosophy

ggen follows three paradigm shifts:

### 1. Specification-First (Big Bang 80/20)
- ✅ Define specification in RDF (source of truth)
- ✅ Verify specification closure before coding
- ✅ Generate code from complete specification
- ❌ Never: vague requirements → plan → code → iterate

### 2. Deterministic Validation
- ✅ Same ontology + templates = identical output
- ✅ Reproducible builds, version-able specifications
- ✅ Evidence-based validation (SHACL, ggen validation)
- ❌ Never: subjective code review, narrative validation

### 3. RDF-First
- ✅ Edit `.ttl` files (the source)
- ✅ Generate `.md` documentation from RDF
- ✅ Use ggen to generate ggen documentation
- ❌ Never: edit generated markdown directly

---

## Common Patterns

### REST API Generation
```bash
# 1. Define API spec in RDF
# 2. SPARQL query to extract endpoints
# 3. Template renders Axum/Rocket code
ggen sync
```

### Multi-Language Support
```bash
# Same ontology, different templates
# rust/ → Rust code
# typescript/ → TypeScript code
# python/ → Python code
ggen sync
```

### Database Schema Generation
```bash
# RDF model → SPARQL inference → PostgreSQL DDL
# Includes: tables, indexes, relationships, migrations
ggen sync
```

---

## Status

**Version**: 5.0.2
**Crates**: 17 active (ggen-core, ggen-cli, ggen-ai, ggen-marketplace, ggen-test-audit, etc.)
**Stability**: Production-ready
**License**: Apache 2.0 OR MIT

---

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Development Setup**:
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make check      # Verify setup
cargo make test       # Run tests
cargo make lint       # Check style
```

---

## Resources

- **GitHub Issues**: [Report bugs or request features](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [Ask questions and discuss ideas](https://github.com/seanchatmangpt/ggen/discussions)
- **Security**: [Responsible disclosure](SECURITY.md)
- **Changelog**: [Version history](CHANGELOG.md)

---

## Project Constitution

This project follows strict operational principles. See [CLAUDE.md](CLAUDE.md) for:
- Constitutional rules (cargo make only, RDF-first, Chicago TDD)
- Andon signals (RED = stop, YELLOW = investigate, GREEN = continue)
- Quality gates and validation requirements
- Development philosophy and standards

---

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.

---

**Ready to get started?** → [Quick Start Tutorial](docs/tutorials/01-quickstart.md)
