# Test Pack Fixtures

This directory contains realistic test packs for the ggen pack system tests.

## Available Test Packs

### 1. web-api-pack
- **ID**: `test.web-api`
- **Version**: 1.0.0
- **Description**: REST API template pack with RDF metadata
- **Templates**: API handler template with Tera variables
- **RDF**: API ontology defining REST concepts
- **SPARQL**: Query to find API endpoints
- **SHACL**: Validation shape for endpoints

### 2. cli-tool-pack
- **ID**: `test.cli-tool`
- **Version**: 2.0.0
- **Description**: CLI application template pack
- **Dependencies**: Depends on web-api-pack (^1.0)
- **Templates**: CLI main template with clap integration
- **RDF**: CLI tool ontology
- **SPARQL**: Query to list CLI commands

### 3. database-pack
- **ID**: `test.database`
- **Version**: 1.5.0
- **Description**: Database schema and migration templates
- **Templates**: SQL migration template
- **RDF**: Database schema ontology
- **SPARQL**: Query to find database tables

## Directory Structure

```
packs/
├── web-api-pack/
│   ├── gpack.toml
│   ├── templates/
│   │   ├── api-handler.tmpl
│   │   └── api/
│   │       ├── graphs/
│   │       │   ├── api-ontology.ttl
│   │       │   └── shapes/
│   │       │       └── endpoint-shape.shacl.ttl
│   │       └── queries/
│   │           └── find-endpoints.sparql
│   └── ...
├── cli-tool-pack/
│   └── ...
└── database-pack/
    └── ...
```

## Usage in Tests

These fixtures are used by:
- Unit tests: Validate manifest parsing and file discovery
- Integration tests: Test complete workflows
- Performance tests: Benchmark pack operations
- Edge case tests: Test error handling

## Adding New Test Packs

When adding a new test pack:
1. Create directory with `gpack.toml`
2. Add at least one template with frontmatter
3. Add RDF ontology file (`.ttl`)
4. Add SPARQL query file (`.sparql`)
5. Optionally add SHACL shapes
6. Update this README
7. Add tests that use the new pack
