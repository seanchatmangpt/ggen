# Projections

Targets are just templates. ggen supports both marketplace gpacks and local templates for generating code projections.

## Marketplace Gpack Examples

### CLI Subcommands

```bash
# Install multi-language CLI gpacks
ggen add io.ggen.rust.cli-subcommand
ggen add io.ggen.python.cli-subcommand
ggen add io.ggen.bash.cli-subcommand

# Generate across languages
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=status
ggen gen io.ggen.python.cli-subcommand:cli/subcommand/python.tmpl name=status
ggen gen io.ggen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=status
```

### API Endpoints

```bash
# Install API gpacks
ggen add io.ggen.rust.api-endpoint
ggen add io.ggen.python.api-endpoint
ggen add io.ggen.typescript.api-endpoint

# Generate REST endpoints
ggen gen io.ggen.rust.api-endpoint:api/endpoint/rust.tmpl name=users
ggen gen io.ggen.python.api-endpoint:api/endpoint/python.tmpl name=users
ggen gen io.ggen.typescript.api-endpoint:api/endpoint/typescript.tmpl name=users
```

### SQL DDL from Ontology

```bash
# Install SQL generation gpacks
ggen add io.ggen.sql.schema
ggen add io.ggen.sql.migrations

# Generate database schema
ggen gen io.ggen.sql.schema:schema/postgres.tmpl name=users
ggen gen io.ggen.sql.migrations:migration/postgres.tmpl name=add_users_table
```

### Edge Function Stubs

```bash
# Install edge function gpacks
ggen add io.ggen.vercel.edge-function
ggen add io.ggen.cloudflare.worker

# Generate edge functions
ggen gen io.ggen.vercel.edge-function:edge/function.ts.tmpl name=api
ggen gen io.ggen.cloudflare.worker:worker/index.js.tmpl name=api
```

### Documentation from Graph Annotations

```bash
# Install documentation gpacks
ggen add io.ggen.docs.api-reference
ggen add io.ggen.docs.user-guide

# Generate documentation
ggen gen io.ggen.docs.api-reference:docs/api.md.tmpl name=users
ggen gen io.ggen.docs.user-guide:docs/guide.md.tmpl name=getting-started
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
ggen gen custom projection --vars name=user
```

## Discovering Projection Gpacks

### By Category

```bash
# Browse categories
ggen categories

# Search by projection type
ggen search cli subcommand
ggen search api endpoint
ggen search sql schema
ggen search docs reference
```

### By Language

```bash
# Search by language
ggen search rust
ggen search python
ggen search typescript
ggen search go
```

### By Framework

```bash
# Search by framework
ggen search clap
ggen search fastapi
ggen search express
ggen search gin
```

## Cross-Language Projections

Marketplace gpacks enable consistent projections across multiple languages:

### Same RDF Ontology
All language-specific gpacks use the same semantic model:
- Consistent variable binding
- Identical SPARQL queries
- Unified frontmatter structure

### Deterministic Output
Version locking ensures reproducible results:
- Same gpack versions → identical outputs
- Cross-language consistency
- Deterministic generation

## Projection Patterns

### Standard Pattern
1. **Describe object in RDF** - Define semantic model
2. **Bind vars via SPARQL** - Extract variables from graph
3. **Render per target** - Generate language-specific code

### Marketplace Pattern
1. **Search for gpacks** - Find language-specific templates
2. **Install dependencies** - Get gpacks and dependencies
3. **Generate consistently** - Use same RDF across languages

### Local Pattern
1. **Create templates** - Build custom projection logic
2. **Define RDF model** - Specify semantic structure
3. **Generate outputs** - Render to target formats

## Best Practices

### Gpack Selection
- Choose gpacks with active maintenance
- Verify compatibility with your ggen version
- Check dependency requirements
- Review example outputs

### Version Management
- Pin versions for production use
- Test updates before applying
- Use semantic versioning
- Maintain lockfile consistency

### Custom Projections
- Start with marketplace gpacks
- Extend with local templates
- Share via gpack publishing
- Document projection patterns
