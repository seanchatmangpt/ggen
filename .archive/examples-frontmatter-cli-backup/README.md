# Frontmatter CLI

A standalone CLI tool for generating frontmatter as JSON then converting to YAML for ggen templates.

## Features

- **Generate frontmatter** from template descriptions
- **Convert JSON to YAML** format
- **Support for RDF ontologies** and SPARQL queries
- **Multiple template types** (user, api, query)
- **Standalone operation** - no dependencies on ggen packages

## Installation

```bash
cd examples/frontmatter-cli
cargo build --release
```

## Usage

### Generate Frontmatter

```bash
# Basic generation
cargo run -- generate --description "User management system"

# With RDF and SPARQL
cargo run -- generate --description "User authentication system" --yaml --rdf --sparql --output user-auth.yaml

# Different template types
cargo run -- generate --description "API controller" --template-type api --yaml
cargo run -- generate --description "SPARQL query" --template-type query --yaml
```

### Convert JSON to YAML

```bash
cargo run -- convert --input frontmatter.json --output frontmatter.yaml
```

### Show Examples

```bash
cargo run -- example --template-type user
cargo run -- example --template-type api
cargo run -- example --template-type query
```

## Template Types

### User Template
```yaml
to: src/models/{{name}}.rs
vars:
- name: string
- email: string
- role: string
determinism: true
```

### API Template
```yaml
to: src/controllers/{{resource}}_controller.rs
vars:
- resource: string
- actions: array
determinism: true
```

### Query Template
```yaml
to: queries/{{query_name}}.sparql
vars:
- query_name: string
- domain: string
determinism: true
```

## RDF Integration

When using `--rdf`, the CLI generates RDF ontologies:

```turtle
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" .

ex:name a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "name" .
```

## SPARQL Queries

When using `--sparql`, the CLI generates SPARQL queries:

```sparql
SELECT ?name ?email ?role WHERE {
    ?user a ex:User ;
      ex:name ?name ;
      ex:email ?email ;
      ex:role ?role .
}
```

## Integration with ggen

Generated frontmatter can be used directly with ggen:

```bash
# Generate frontmatter
cargo run -- generate --description "User system" --yaml --output user.tmpl

# Use with ggen
ggen project gen user.tmpl --var name=User --out src/
```

## Examples

### Complete User Management System

```bash
cargo run -- generate \
  --description "Complete user management system with authentication" \
  --template-type user \
  --yaml \
  --rdf \
  --sparql \
  --output user-management.tmpl
```

### API Controller with CRUD Operations

```bash
cargo run -- generate \
  --description "REST API controller with CRUD operations" \
  --template-type api \
  --yaml \
  --rdf \
  --sparql \
  --output api-controller.tmpl
```

### SPARQL Query Generator

```bash
cargo run -- generate \
  --description "SPARQL query generator for knowledge graphs" \
  --template-type query \
  --yaml \
  --rdf \
  --sparql \
  --output sparql-generator.tmpl
```

## Output Formats

### JSON Format
```json
{
  "to": "src/models/{{name}}.rs",
  "vars": [
    {"name": "string"},
    {"email": "string"},
    {"role": "string"}
  ],
  "rdf": "@prefix ex: <http://example.org/> .",
  "sparql": "SELECT ?name WHERE { ?user ex:name ?name }",
  "determinism": true
}
```

### YAML Format
```yaml
to: src/models/{{name}}.rs
vars:
- name: string
- email: string
- role: string
rdf: |-
  @prefix ex: <http://example.org/> .
sparql: SELECT ?name WHERE { ?user ex:name ?name }
determinism: true
```

## Dependencies

- `clap` - Command-line argument parsing
- `serde_json` - JSON serialization/deserialization
- `serde_yaml` - YAML serialization/deserialization
- `serde` - Serialization framework

## License

This project follows the same license as the main ggen project.

