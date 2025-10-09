# Projections

Targets are just templates. rgen supports both marketplace rpacks and local templates for generating code projections.

## Marketplace Rpack Examples

### CLI Subcommands

```bash
# Install multi-language CLI rpacks
rgen add io.ggen.rust.cli-subcommand
rgen add io.ggen.python.cli-subcommand
rgen add io.ggen.bash.cli-subcommand

# Generate across languages
rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=status
rgen gen io.ggen.python.cli-subcommand:cli/subcommand/python.tmpl name=status
rgen gen io.ggen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=status
```

### API Endpoints

```bash
# Install API rpacks
rgen add io.ggen.rust.api-endpoint
rgen add io.ggen.python.api-endpoint
rgen add io.ggen.typescript.api-endpoint

# Generate REST endpoints
rgen gen io.ggen.rust.api-endpoint:api/endpoint/rust.tmpl name=users
rgen gen io.ggen.python.api-endpoint:api/endpoint/python.tmpl name=users
rgen gen io.ggen.typescript.api-endpoint:api/endpoint/typescript.tmpl name=users
```

### SQL DDL from Ontology

```bash
# Install SQL generation rpacks
rgen add io.ggen.sql.schema
rgen add io.ggen.sql.migrations

# Generate database schema
rgen gen io.ggen.sql.schema:schema/postgres.tmpl name=users
rgen gen io.ggen.sql.migrations:migration/postgres.tmpl name=add_users_table
```

### Edge Function Stubs

```bash
# Install edge function rpacks
rgen add io.ggen.vercel.edge-function
rgen add io.ggen.cloudflare.worker

# Generate edge functions
rgen gen io.ggen.vercel.edge-function:edge/function.ts.tmpl name=api
rgen gen io.ggen.cloudflare.worker:worker/index.js.tmpl name=api
```

### Documentation from Graph Annotations

```bash
# Install documentation rpacks
rgen add io.ggen.docs.api-reference
rgen add io.ggen.docs.user-guide

# Generate documentation
rgen gen io.ggen.docs.api-reference:docs/api.md.tmpl name=users
rgen gen io.ggen.docs.user-guide:docs/guide.md.tmpl name=getting-started
```

## Local Template Examples

### Custom Projections

```bash
# Create custom template
mkdir -p templates/custom/projection
cat > templates/custom/projection/rust.tmpl << 'EOF'
---
to: src/{{ name }}.rs
vars:
  name: example
---
pub struct {{ name | pascal }} {
    // Custom projection logic
}
EOF

# Generate custom projection
rgen gen custom projection --vars name=user
```

## Discovering Projection Rpacks

### By Category

```bash
# Browse categories
rgen categories

# Search by projection type
rgen search cli subcommand
rgen search api endpoint
rgen search sql schema
rgen search docs reference
```

### By Language

```bash
# Search by language
rgen search rust
rgen search python
rgen search typescript
rgen search go
```

### By Framework

```bash
# Search by framework
rgen search clap
rgen search fastapi
rgen search express
rgen search gin
```

## Cross-Language Projections

Marketplace rpacks enable consistent projections across multiple languages:

### Same RDF Ontology
All language-specific rpacks use the same semantic model:
- Consistent variable binding
- Identical SPARQL queries
- Unified frontmatter structure

### Deterministic Output
Version locking ensures reproducible results:
- Same rpack versions → identical outputs
- Cross-language consistency
- Deterministic generation

## Projection Patterns

### Standard Pattern
1. **Describe object in RDF** - Define semantic model
2. **Bind vars via SPARQL** - Extract variables from graph
3. **Render per target** - Generate language-specific code

### Marketplace Pattern
1. **Search for rpacks** - Find language-specific templates
2. **Install dependencies** - Get rpacks and dependencies
3. **Generate consistently** - Use same RDF across languages

### Local Pattern
1. **Create templates** - Build custom projection logic
2. **Define RDF model** - Specify semantic structure
3. **Generate outputs** - Render to target formats

## Best Practices

### Rpack Selection
- Choose rpacks with active maintenance
- Verify compatibility with your rgen version
- Check dependency requirements
- Review example outputs

### Version Management
- Pin versions for production use
- Test updates before applying
- Use semantic versioning
- Maintain lockfile consistency

### Custom Projections
- Start with marketplace rpacks
- Extend with local templates
- Share via rpack publishing
- Document projection patterns
