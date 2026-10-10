# knowledge-graph-cli

[![Crates.io](https://img.shields.io/crates/v/knowledge-graph-cli)](https://crates.io/crates/knowledge-graph-cli)
[![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue)](LICENSE)
[![ggen Marketplace](https://img.shields.io/badge/ggen-marketplace-success)](https://github.com/seanchatmangpt/ggen)

Knowledge graph operations CLI built with `clap-noun-verb` pattern. Manage entities, relations, and inference rules with ontology-driven commands.

## Features

- **Entity Management**: Create, list, update, delete entities with properties
- **Relation Management**: Add and query relationships between entities
- **Inference Engine**: Apply reasoning rules and validate consistency
- **Graph Operations**: Load, export, visualize knowledge graphs
- **JSON Output**: All commands return structured JSON
- **Persistence**: Optional persistent storage with Sled

## Installation

### From ggen Marketplace
```bash
ggen market install knowledge-graph-cli
ggen template generate knowledge-graph-cli:cli.tmpl
cargo build --release
```

### From crates.io
```bash
cargo install knowledge-graph-cli
```

## Quick Start

```bash
# Create entities
kg-cli entity create "Alice" --type Person --properties '{"age": 30, "city": "NYC"}'
kg-cli entity create "Bob" --type Person --properties '{"age": 25, "city": "SF"}'
kg-cli entity create "Acme Corp" --type Organization

# Add relations
kg-cli relation add alice bob --type knows
kg-cli relation add alice acme-corp --type worksFor

# Query
kg-cli entity list --type Person
kg-cli relation query --pattern "* worksFor *"

# Inference
kg-cli inference rules
kg-cli inference apply
kg-cli inference validate
```

## Command Reference

### Entity Noun
```bash
# Create entity
kg-cli entity create <NAME> --type <TYPE> [--properties <JSON>]

# List entities
kg-cli entity list [--type <TYPE>] [--limit <N>]

# Show details
kg-cli entity show <ID>

# Update properties
kg-cli entity update <ID> --properties <JSON>

# Delete
kg-cli entity delete <ID>
```

### Relation Noun
```bash
# Add relation
kg-cli relation add <FROM> <TO> --type <TYPE> [--properties <JSON>]

# List all relations
kg-cli relation list [--type <TYPE>]

# Query pattern
kg-cli relation query --pattern "<PATTERN>"

# Remove relation
kg-cli relation remove <RELATION_ID>
```

### Inference Noun
```bash
# List inference rules
kg-cli inference rules

# Apply reasoning
kg-cli inference apply [--ruleset <NAME>]

# Validate consistency
kg-cli inference validate
```

### Graph Noun
```bash
# Load from file
kg-cli graph load <FILE> [--format turtle]

# Export to file
kg-cli graph export <FILE> [--format json-ld]

# Show statistics
kg-cli graph stats

# Generate visualization
kg-cli graph visualize --output graph.svg [--format svg]
```

## Examples

### Build a Social Network Graph
```bash
# Create people
kg-cli entity create "Alice" --type Person --properties '{"age": 30}'
kg-cli entity create "Bob" --type Person --properties '{"age": 25}'
kg-cli entity create "Carol" --type Person --properties '{"age": 28}'

# Create relationships
kg-cli relation add alice bob --type knows
kg-cli relation add bob carol --type knows
kg-cli relation add alice carol --type knows

# Apply transitive inference (friend-of-friend)
kg-cli inference apply --ruleset social

# Query the network
kg-cli relation query --pattern "* knows *"
```

### Organization Hierarchy
```bash
# Create org structure
kg-cli entity create "Acme Corp" --type Organization
kg-cli entity create "Engineering" --type Department
kg-cli entity create "Sales" --type Department

# Structure
kg-cli relation add engineering acme-corp --type partOf
kg-cli relation add sales acme-corp --type partOf

# People
kg-cli entity create "Alice" --type Person
kg-cli relation add alice engineering --type memberOf

# Infer org relationships
kg-cli inference apply --ruleset organizational
```

### Export to RDF
```bash
# Export entire graph
kg-cli graph export knowledge.ttl --format turtle

# Load into other tools
semantic-cli ontology parse knowledge.ttl
ggen graph load knowledge.ttl
```

## Inference Rules

Built-in rule sets:

- **transitive**: Transitive property inference
- **symmetric**: Symmetric property inference
- **inverse**: Inverse property inference
- **hierarchical**: Class hierarchy reasoning
- **social**: Social network patterns
- **organizational**: Organizational structure

## ggen Integration

### Generate from Ontology
```bash
# The CLI structure is defined in RDF
ggen graph load marketplace/packages/knowledge-graph-cli/rdf/ontology.ttl

# Query available commands
ggen graph query --sparql "
  SELECT ?noun ?verb WHERE {
    ?verb a <http://ggen.io/ontology/clap-noun-verb#Verb> ;
          <http://ggen.io/ontology/clap-noun-verb#belongsToNoun> ?noun
  }
"

# Regenerate CLI
ggen template generate knowledge-graph-cli:cli.tmpl
```

### Extend with Custom Verbs
Edit `rdf/ontology.ttl`:
```turtle
kg:import a clap:Verb ;
    rdfs:label "import" ;
    clap:belongsToNoun kg:Entity ;
    clap:hasArgument kg:fileArg, kg:formatArg ;
    clap:returns kg:ImportResult .
```

Regenerate:
```bash
ggen template generate knowledge-graph-cli:cli.tmpl
# New command: kg-cli entity import data.csv --format csv
```

## Architecture

- **Entities**: In-memory graph with petgraph
- **Relations**: Directed, typed edges
- **Inference**: Rule-based reasoning engine
- **Persistence**: Optional Sled key-value store
- **Output**: JSON via serde

## Development

```bash
# Install from marketplace
ggen market install knowledge-graph-cli

# Build
cargo build

# Test
cargo test

# Run
cargo run -- entity create Test --type Demo
```

## License

MIT OR Apache-2.0
