# semantic-cli

[![Crates.io](https://img.shields.io/crates/v/semantic-cli)](https://crates.io/crates/semantic-cli)
[![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue)](LICENSE)
[![ggen Marketplace](https://img.shields.io/badge/ggen-marketplace-success)](https://github.com/seanchatmangpt/ggen)

RDF/OWL ontology management CLI built with `clap-noun-verb` pattern. Part of the ggen marketplace ecosystem.

## Features

- **RDF Parsing**: Parse Turtle, RDF/XML, N-Triples, JSON-LD
- **OWL Validation**: Validate ontology syntax and semantics
- **SPARQL Queries**: Execute SPARQL SELECT, ASK, CONSTRUCT queries
- **Format Conversion**: Convert between RDF formats
- **Schema Inspection**: List and inspect classes and properties
- **JSON Output**: All commands output structured JSON

## Installation

### From ggen Marketplace (Recommended)
```bash
ggen market install semantic-cli
ggen template generate semantic-cli:cli.tmpl
cargo build --release
```

### From crates.io
```bash
cargo install semantic-cli
```

### From Source
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen/marketplace/packages/semantic-cli
cargo install --path .
```

## Quick Start

```bash
# Parse an ontology file
semantic-cli ontology parse domain.ttl

# Validate ontology
semantic-cli ontology validate --strict domain.ttl

# Show metadata
semantic-cli ontology info domain.ttl

# Convert between formats
semantic-cli ontology convert domain.ttl output.xml --format xml

# List schema elements
semantic-cli schema list domain.ttl
semantic-cli schema list domain.ttl --type class

# Show class details
semantic-cli schema show domain.ttl "Person"

# Execute SPARQL query
semantic-cli query sparql domain.ttl --query "SELECT ?s WHERE { ?s a owl:Class }"

# SPARQL ASK query
semantic-cli query ask domain.ttl --query "ASK { ?s a owl:Class }"
```

## Command Structure

All commands follow the **clap-noun-verb pattern**:

```
semantic-cli <NOUN> <VERB> [OPTIONS] [ARGS]
```

### Ontology Noun
- `parse <file>` - Parse RDF/OWL file
- `validate <file> [--strict]` - Validate ontology
- `info <file>` - Show metadata
- `convert <input> <output> --format <fmt>` - Convert formats

### Schema Noun
- `list <file> [--type <type>]` - List classes/properties
- `show <file> <name>` - Show class/property details
- `export <file> --format <fmt>` - Export schema

### Query Noun
- `sparql <file> --query <query>` - Execute SPARQL query
- `ask <file> --query <query>` - Execute ASK query
- `construct <file> --query <query>` - Execute CONSTRUCT query

## Examples

### Parse Ontology
```bash
$ semantic-cli ontology parse examples/foaf.ttl
{
  "triples": 127,
  "classes": 14,
  "properties": 58,
  "individuals": 3
}
```

### Validate with Strict Mode
```bash
$ semantic-cli ontology validate --strict examples/invalid.ttl
{
  "valid": false,
  "errors": [
    "Undefined class: foaf:InvalidClass",
    "Property domain mismatch: foaf:name"
  ],
  "warnings": [
    "Missing rdfs:label for class foaf:Person"
  ]
}
```

### List Classes
```bash
$ semantic-cli schema list examples/foaf.ttl --type class
{
  "classes": [
    "foaf:Person",
    "foaf:Organization",
    "foaf:Document",
    "foaf:Image"
  ],
  "count": 4
}
```

### SPARQL Query
```bash
$ semantic-cli query sparql examples/foaf.ttl \
  --query "SELECT ?person ?name WHERE { ?person a foaf:Person ; foaf:name ?name }"
{
  "bindings": [
    {"person": "http://example.org/alice", "name": "Alice"},
    {"person": "http://example.org/bob", "name": "Bob"}
  ],
  "count": 2
}
```

## ggen Marketplace Integration

### Generate from RDF Ontology
```bash
# Load the semantic-cli ontology
ggen graph load marketplace/packages/semantic-cli/rdf/ontology.ttl

# Query the structure
ggen graph query --sparql "
  SELECT ?noun ?verb WHERE {
    ?verb a <http://ggen.io/ontology/clap-noun-verb#Verb> ;
          <http://ggen.io/ontology/clap-noun-verb#belongsToNoun> ?noun
  }
"

# Generate CLI code
ggen template generate semantic-cli:cli.tmpl
```

### Customize & Regenerate
```bash
# Edit rdf/ontology.ttl to add new verbs
# Example: Add ontology merge verb

@prefix cli: <http://ggen.io/ontology/semantic-cli#> .

cli:merge a clap:Verb ;
    rdfs:label "merge" ;
    clap:belongsToNoun cli:Ontology ;
    clap:hasArgument cli:filesArg ;
    clap:returns cli:MergeResult .

# Regenerate to add new command
ggen template generate semantic-cli:cli.tmpl

# New command available:
semantic-cli ontology merge file1.ttl file2.ttl --output merged.ttl
```

## Architecture

Built using:
- **clap-noun-verb** v3.4.0 - Declarative CLI patterns
- **rio** - RDF I/O parsing
- **oxigraph** - SPARQL query engine
- **serde** - JSON serialization

All commands:
1. Parse arguments using clap-noun-verb
2. Process using RDF/SPARQL libraries
3. Return structured JSON

## Development

```bash
# Install from marketplace
ggen market install semantic-cli

# Build
cargo build

# Test
cargo test

# Run examples
cargo run --example basic

# Lint
cargo clippy

# Format
cargo fmt
```

## RDF Ontology

The CLI structure is defined in `rdf/ontology.ttl` using:
- Nouns: `cli:Ontology`, `cli:Schema`, `cli:Query`
- Verbs: `cli:parse`, `cli:validate`, `cli:list`, etc.
- Arguments: Defined with types and constraints
- Return types: JSON-serializable structures

See [rdf/ontology.ttl](rdf/ontology.ttl) for the complete semantic model.

## Contributing

```bash
# Clone ggen
git clone https://github.com/seanchatmangpt/ggen

# Navigate to package
cd ggen/marketplace/packages/semantic-cli

# Make changes

# Test
cargo test

# Submit PR
```

## License

MIT OR Apache-2.0

## Links

- **ggen**: https://github.com/seanchatmangpt/ggen
- **clap-noun-verb**: https://crates.io/crates/clap-noun-verb
- **Marketplace**: `ggen market search semantic`
