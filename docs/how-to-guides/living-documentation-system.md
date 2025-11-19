# How to Use the Living Documentation System

This guide explains how to use ggen's Living Documentation Ecosystem to create documentation that evolves with your codebase through semantic understanding and automated narrative generation.

## Overview

The Living Documentation System provides:

1. **Semantic Ontology** - RDF knowledge graph of code structure
2. **Automated Narratives** - Generated documentation from code analysis
3. **Interactive Interface** - Web-based documentation explorer
4. **NLU Bidirectional Sync** - Natural language documentation updates

## Setup

### 1. Configure the System

Create a configuration file `.ggen/living-docs/config.toml`:

```toml
[ontology]
store_path = ".ggen/living-docs/ontology"
base_uri = "http://ggen.dev/ontology/code#"
enable_cache = true

[narrative]
style = "technical"  # or "conversational", "tutorial", "reference"
include_examples = true
include_diagrams = true
output_dir = "docs/generated"

[interface]
bind_address = "127.0.0.1"
port = 8080
enable_websocket = true
enable_live_reload = true
```

### 2. Install Git Hooks

Enable automatic documentation evolution:

```bash
ggen docs hooks install
```

This installs:
- **pre-commit**: Extract ontology and validate docs
- **post-commit**: Generate narratives
- **pre-push**: Final validation

## Basic Workflow

### 1. Extract Code Ontology

Extract semantic representation from your codebase:

```bash
ggen docs extract ./src
```

This creates an RDF knowledge graph with:
- All code entities (functions, structs, traits, etc.)
- Relationships (calls, implements, uses, etc.)
- Documentation and metadata
- File locations and line numbers

### 2. Generate Narratives

Create human-readable documentation:

```bash
ggen docs narrate
```

Generates:
- Function documentation
- Module overviews
- API references
- Architecture diagrams
- Dependency visualizations

Output is written to `docs/generated/`.

### 3. Start Interactive Server

Launch the web interface:

```bash
ggen docs serve --port 8080
```

Then open http://localhost:8080 to:
- Browse code visually
- Query with natural language
- See real-time updates
- Explore dependency graphs

### 4. Validate Documentation

Check documentation completeness:

```bash
ggen docs validate
```

Output:
```
Documentation Validation Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total Entities: 1,234
Documented: 987
Coverage: 80.0%

Undocumented Entities:
  - execute_sparql (src/graph/mod.rs:42)
  - render_template (src/template/mod.rs:156)

Missing Examples:
  - CodeEntity::new
  - GraphStorage::query
```

## Advanced Usage

### Natural Language Updates

Update documentation using plain English:

```bash
ggen docs sync "update execute_sparql documentation: Executes SPARQL queries with intelligent caching for improved performance"
```

This:
1. Parses the natural language input
2. Extracts semantic updates
3. Updates the RDF ontology
4. Regenerates affected narratives

### Natural Language Queries

Ask questions about your codebase:

```bash
ggen docs query "show all async functions"
ggen docs query "find functions that call execute_sparql"
ggen docs query "list modules with undocumented exports"
```

### SPARQL Queries

For advanced queries, use SPARQL directly:

```bash
ggen docs query --sparql "
  PREFIX code: <http://ggen.dev/ontology/code#>

  SELECT ?function ?doc
  WHERE {
    ?function rdf:type code:Function .
    ?function code:async 'true' .
    ?function code:documentation ?doc .
  }
"
```

### Custom Narratives

Create custom narrative templates:

1. Create template file `.ggen/living-docs/templates/my_template.tera`:

```jinja
# {{ name }}

{% if documentation %}
## Description
{{ documentation }}
{% endif %}

## Details
- **Type**: {{ kind }}
- **File**: {{ file_path }}
- **Visibility**: {{ visibility }}
```

2. Generate using the template:

```bash
ggen docs narrate --template my_template
```

## Integration with CI/CD

### GitHub Actions

Add to `.github/workflows/docs.yml`:

```yaml
name: Living Documentation

on:
  push:
    branches: [main]
  pull_request:

jobs:
  docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Extract Ontology
        run: ggen docs extract ./src

      - name: Validate Documentation
        run: ggen docs validate --strict

      - name: Generate Narratives
        run: ggen docs narrate

      - name: Deploy to GitHub Pages
        if: github.ref == 'refs/heads/main'
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs/generated
```

### Pre-commit Hook

`.git/hooks/pre-commit`:

```bash
#!/usr/bin/env bash
set -e

# Extract and validate
ggen docs extract ./src
ggen docs validate

if [ $? -ne 0 ]; then
    echo "❌ Documentation validation failed"
    exit 1
fi

echo "✅ Documentation validated"
```

## Common Patterns

### Document a New Feature

1. Write your code with doc comments:

```rust
/// Executes SPARQL query with caching
///
/// # Arguments
/// * `query` - The SPARQL query string
///
/// # Returns
/// Query results as JSON
///
/// # Example
/// ```rust
/// let results = execute_sparql("SELECT * WHERE { ?s ?p ?o }")?;
/// ```
pub async fn execute_sparql(query: &str) -> Result<Value> {
    // implementation
}
```

2. Run extraction:

```bash
ggen docs extract ./src
```

3. Generate narratives:

```bash
ggen docs narrate
```

4. Verify in interactive interface:

```bash
ggen docs serve
# Open http://localhost:8080
```

### Track API Changes

When making breaking changes:

1. Extract ontology before changes:

```bash
ggen docs extract ./src --output before.rdf
```

2. Make your changes

3. Extract ontology after changes:

```bash
ggen docs extract ./src --output after.rdf
```

4. Generate diff:

```bash
ggen docs diff before.rdf after.rdf > api-changes.md
```

### Create Migration Guides

```bash
ggen docs narrate --template migration_guide \
  --from-version 2.0.0 \
  --to-version 3.0.0
```

## Troubleshooting

### Documentation Coverage is Low

Check undocumented entities:

```bash
ggen docs validate --verbose
```

Add doc comments to critical entities:

```rust
/// Brief description
///
/// Detailed explanation
pub struct MyStruct {
    // fields
}
```

### Server Not Starting

Check port availability:

```bash
lsof -i :8080
```

Use a different port:

```bash
ggen docs serve --port 8081
```

### Ontology Extraction Fails

Enable debug logging:

```bash
RUST_LOG=debug ggen docs extract ./src
```

Check for syntax errors in source files.

### Narratives Not Generating

Verify ontology was extracted:

```bash
ls -la .ggen/living-docs/ontology/
```

Check template syntax:

```bash
ggen docs narrate --dry-run
```

## Best Practices

### 1. Write Good Doc Comments

```rust
/// Calculate the fibonacci number at position n
///
/// Uses dynamic programming for O(n) time complexity.
///
/// # Arguments
/// * `n` - The position in the fibonacci sequence
///
/// # Returns
/// The fibonacci number at position n
///
/// # Example
/// ```rust
/// assert_eq!(fibonacci(10), 55);
/// ```
///
/// # Panics
/// Panics if n > 186 (overflow for u64)
pub fn fibonacci(n: usize) -> u64 {
    // implementation
}
```

### 2. Regular Validation

Add to your CI pipeline:

```bash
ggen docs validate --strict --fail-on-undocumented
```

### 3. Keep Templates Simple

Use clear, maintainable templates:

```jinja
# {{ name }}

{{ documentation }}

## Usage

```rust
{{ example }}
```
```

### 4. Leverage Natural Language

Instead of manual edits:

```bash
ggen docs sync "GraphStorage implements caching for query results"
ggen docs sync "update fibonacci documentation: Uses memoization"
```

## Next Steps

- Read the [Living Documentation Architecture](../architecture/LIVING_DOCUMENTATION_SYSTEM.md)
- Explore the [API Reference](../reference/living-docs-api.md)
- Check out [examples](../../examples/living-docs/)
- Join the community on [Discord](https://discord.gg/ggen)

## See Also

- [Code Generation Guide](./code-generation.md)
- [Template Customization](./template-customization.md)
- [RDF Ontologies](../explanations/rdf-ontologies.md)
