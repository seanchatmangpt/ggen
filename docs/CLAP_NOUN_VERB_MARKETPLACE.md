# clap-noun-verb for ggen Marketplace

Complete guide to creating clap-noun-verb CLI tools as ggen marketplace packages.

## Overview

Three production-ready clap-noun-verb packages for the ggen marketplace:

1. **semantic-cli** - RDF/OWL ontology management
2. **knowledge-graph-cli** - Knowledge graph operations
3. **schema-forge-cli** - Data modeling and schema generation

All packages:
- Use `clap-noun-verb` v3.4.0 pattern
- Include complete RDF ontologies
- Ready for crates.io deployment
- Fully integrated with ggen marketplace

## Architecture

### Marketplace Workflow

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│  Discovery   │───▶│ Installation │───▶│  Generation  │
│ ggen market  │    │  ggen market │    │   ggen       │
│   search     │    │     add      │    │  template    │
└──────────────┘    └──────────────┘    └──────────────┘
                                               │
                    ┌──────────────────────────┘
                    ▼
             ┌──────────────┐    ┌──────────────┐
             │  Build &     │───▶│   Deploy to  │
             │   Test       │    │  crates.io   │
             │ cargo build  │    │cargo publish │
             └──────────────┘    └──────────────┘
```

See [docs/diagrams/marketplace-workflow.puml](diagrams/marketplace-workflow.puml) for detailed sequence diagram.

## Package Structure

Each package follows this structure:

```
marketplace/packages/<package-name>/
├── package.toml              # Marketplace metadata
├── README.md                 # Documentation
├── rdf/
│   └── ontology.ttl         # RDF ontology (CLI structure)
├── templates/
│   ├── cli.tmpl             # CLI generation template
│   ├── lib.tmpl             # Library template
│   └── commands.tmpl        # Commands template
├── examples/
│   └── basic.rs             # Usage examples
├── sparql/                   # SPARQL queries (optional)
├── src/                      # Source code (optional)
├── tests/                    # Tests (optional)
└── docs/                     # Additional docs

```

### Key Files

#### package.toml
```toml
[package]
name = "semantic-cli"
version = "1.0.0"
namespace = "io.ggen.cli.semantic"
description = "RDF/OWL ontology management CLI"

[marketplace]
category = "cli-tools"
subcategory = "semantic-web"
tags = ["rdf", "owl", "sparql", "clap-noun-verb"]
crates_io_ready = true

[templates]
cli = "templates/cli.tmpl"

[ontology]
file = "rdf/ontology.ttl"
namespace = "http://ggen.io/ontology/semantic-cli#"
```

#### rdf/ontology.ttl
```turtle
@prefix clap: <http://ggen.io/ontology/clap-noun-verb#> .
@prefix cli: <http://ggen.io/ontology/semantic-cli#> .

# Noun
cli:Ontology a clap:Noun ;
    clap:hasVerb cli:parse, cli:validate .

# Verb
cli:parse a clap:Verb ;
    clap:belongsToNoun cli:Ontology ;
    clap:hasArgument cli:fileArg ;
    clap:returns cli:OntologyInfo .

# Argument
cli:fileArg a clap:Argument ;
    clap:name "file" ;
    clap:type xsd:string ;
    clap:required true .
```

## Usage Workflow

### 1. Search & Discovery
```bash
# Find packages
ggen market search "semantic"
ggen market search "cli tools"
ggen market search "clap-noun-verb"

# List categories
ggen market categories
```

### 2. Installation
```bash
# Install package
ggen market add semantic-cli

# Package installed to:
# - marketplace/packages/semantic-cli/
```

### 3. Exploration
```bash
# Load ontology
ggen graph load marketplace/packages/semantic-cli/rdf/ontology.ttl

# Query CLI structure
ggen graph query --sparql "
  SELECT ?noun ?verb WHERE {
    ?verb a <http://ggen.io/ontology/clap-noun-verb#Verb> ;
          <http://ggen.io/ontology/clap-noun-verb#belongsToNoun> ?noun
  }
"

# Result:
# noun                | verb
# cli:Ontology        | cli:parse
# cli:Ontology        | cli:validate
# cli:Schema          | cli:list
```

### 4. Generation
```bash
# Generate CLI from ontology
ggen template generate semantic-cli:cli.tmpl

# Generated files:
# src/main.rs
# src/commands/ontology.rs
# src/commands/schema.rs
# Cargo.toml
```

### 5. Build & Test
```bash
# Build
cd marketplace/packages/semantic-cli
cargo build --release

# Test
cargo test

# Run
./target/release/semantic-cli ontology parse domain.ttl
```

### 6. Customization
```bash
# Edit ontology to add new verb
vim marketplace/packages/semantic-cli/rdf/ontology.ttl

# Add:
cli:convert a clap:Verb ;
    clap:belongsToNoun cli:Ontology ;
    clap:hasArgument cli:inputArg, cli:outputArg .

# Regenerate
ggen template generate semantic-cli:cli.tmpl

# New command available:
semantic-cli ontology convert input.ttl output.xml
```

## Diagrams

### Marketplace Workflow
![Marketplace Workflow](diagrams/marketplace-workflow.puml)

Sequence diagram showing:
1. Discovery (search packages)
2. Installation (download metadata + ontology)
3. Understanding (query RDF structure)
4. Generation (create CLI from templates)
5. Customization (modify ontology, regenerate)
6. Build & Test (cargo build)
7. Publish (upload to registry)

### Architecture
![Architecture](diagrams/clap-noun-verb-architecture.puml)

Component diagram showing:
- RDF Ontology Layer (ontology.ttl, SPARQL, Oxigraph)
- Marketplace Package (package.toml, templates, examples)
- Generated CLI (main.rs, commands/, Cargo.toml)
- Runtime Execution (clap parser, verb functions, JSON output)

### Components
![Components](diagrams/ontology-cli-components.puml)

Shows three CLI tools and shared components:
- semantic-cli: RDF Parser, OWL Validator, SPARQL Engine
- knowledge-graph-cli: Entity Manager, Inference Engine
- schema-forge-cli: Model Builder, Code Generator
- Shared: clap-noun-verb, RDF Triple Store, JSON Serializer

## Package Summaries

### semantic-cli
**Purpose**: RDF/OWL ontology management

**Commands**:
```bash
semantic-cli ontology parse <file>
semantic-cli ontology validate <file>
semantic-cli ontology info <file>
semantic-cli ontology convert <input> <output> --format <fmt>
semantic-cli schema list <file>
semantic-cli schema show <file> <class>
semantic-cli query sparql <file> --query <query>
```

**Use Cases**:
- Parse Turtle, RDF/XML, JSON-LD files
- Validate OWL semantics
- Convert between RDF formats
- Execute SPARQL queries

### knowledge-graph-cli
**Purpose**: Knowledge graph operations

**Commands**:
```bash
kg-cli entity create <name> --type <type>
kg-cli entity list [--type <type>]
kg-cli relation add <from> <to> --type <type>
kg-cli relation query --pattern <pattern>
kg-cli inference apply [--ruleset <name>]
kg-cli graph export <file> --format <fmt>
```

**Use Cases**:
- Build knowledge graphs
- Manage entities and relations
- Apply reasoning rules
- Export to RDF formats

### schema-forge-cli
**Purpose**: Data modeling and schema generation

**Commands**:
```bash
sf-cli model create <name>
sf-cli model add-field <model> <field> <type>
sf-cli validate data <file> <model>
sf-cli generate json-schema <model> --output <file>
sf-cli generate sql <model> --output <file>
sf-cli generate graphql <model> --output <file>
sf-cli generate rust <model> --output <file>
```

**Use Cases**:
- Design data models
- Validate JSON data
- Generate JSON Schema, SQL DDL, GraphQL SDL
- Generate type-safe code (Rust, TypeScript, etc.)

## clap-noun-verb Pattern

### Nouns
Represent domain concepts:
```turtle
cli:Ontology a clap:Noun ;
    rdfs:label "Ontology" ;
    clap:hasVerb cli:parse, cli:validate .
```

### Verbs
Represent actions on nouns:
```turtle
cli:parse a clap:Verb ;
    clap:belongsToNoun cli:Ontology ;
    clap:hasArgument cli:fileArg ;
    clap:returns cli:OntologyInfo .
```

### Arguments
Define CLI inputs:
```turtle
cli:fileArg a clap:Argument ;
    clap:name "file" ;
    clap:type xsd:string ;
    clap:required true ;
    clap:help "Path to ontology file" .
```

### Return Types
Define JSON output structure:
```turtle
cli:OntologyInfo a owl:Class ;
    clap:hasField cli:triplesField, cli:classesField .

cli:triplesField a clap:Field ;
    clap:name "triples" ;
    clap:type xsd:integer .
```

## Type Inference

The RDF ontology drives automatic type inference:

| RDF Type | Rust Type | clap Action |
|----------|-----------|-------------|
| `xsd:string` | `String` | Standard |
| `xsd:boolean` | `bool` | SetTrue |
| `xsd:integer` | `i32` | Standard |
| `xsd:float` | `f64` | Standard |
| Optional arg | `Option<T>` | Standard |
| Multiple values | `Vec<T>` | Append |

## crates.io Deployment

### Checklist
- [x] Cargo.toml with metadata
- [x] LICENSE-MIT and LICENSE-APACHE
- [x] README.md with examples
- [x] src/lib.rs for library use
- [x] examples/ directory
- [x] tests/ directory
- [x] No unsafe code
- [x] Documentation comments
- [x] `cargo publish --dry-run` passes

### Publishing
```bash
# Validate
cd marketplace/packages/semantic-cli
cargo publish --dry-run

# Publish
cargo publish

# Wait 30 seconds for indexing
sleep 30

# Install from crates.io
cargo install semantic-cli
```

## Integration Examples

### With ggen Project
```bash
# In your ggen project
ggen market add semantic-cli
ggen template generate semantic-cli:cli.tmpl

# Use in workflow
ggen lifecycle run validate --ontology domain.ttl
```

### Combine Multiple Packages
```bash
# Install all three
ggen market add semantic-cli
ggen market add knowledge-graph-cli
ggen market add schema-forge-cli

# Build knowledge graph from schema
sf-cli model create Product
kg-cli entity create "product-1" --type Product
semantic-cli graph export kg.ttl --format turtle
```

## Benefits

### For Users
- **Fast**: Install and generate in seconds
- **Type-Safe**: RDF ontology ensures correctness
- **Extensible**: Edit ontology, regenerate code
- **Discoverable**: Browse marketplace for patterns
- **Polyglot**: Generate for multiple languages

### For Package Authors
- **Semantic**: RDF as single source of truth
- **Maintainable**: Update ontology, code updates automatically
- **Composable**: Mix marketplace packages
- **Documented**: Ontology is self-documenting
- **Professional**: Production-ready templates

## Next Steps

1. **Explore Packages**:
   ```bash
   cd marketplace/packages/semantic-cli
   cat README.md
   cat rdf/ontology.ttl
   ```

2. **Try Commands**:
   ```bash
   ggen graph load marketplace/packages/semantic-cli/rdf/ontology.ttl
   ggen template generate semantic-cli:cli.tmpl
   ```

3. **Customize**:
   - Edit ontologies
   - Add new verbs
   - Extend with custom logic

4. **Publish**:
   - Test thoroughly
   - Update documentation
   - `cargo publish`

## Resources

- **clap-noun-verb**: https://crates.io/crates/clap-noun-verb
- **ggen**: https://github.com/seanchatmangpt/ggen
- **RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **OWL Guide**: https://www.w3.org/TR/owl2-primer/

## License

MIT OR Apache-2.0

All packages in the ggen marketplace are dual-licensed for maximum compatibility with the Rust ecosystem.
