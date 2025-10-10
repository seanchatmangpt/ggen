# Troubleshooting

## General Issues

- **Missing output**: check `to:` and matrix query.
- **Unbound var**: pass `--vars` or add `sparql.vars`.
- **SHACL failure**: fix data to satisfy shape.
- **Nondeterminism**: ensure matrix query has `ORDER BY` and seed is fixed.
- **No writes**: same `K`; use `--dry-run` to inspect.

## Marketplace Issues

### Gpack Not Found

```bash
# Error: gpack 'io.ggen.rust.cli-subcommand' not found
ggen add io.ggen.rust.cli-subcommand

# Check if gpack exists
ggen search rust cli

# Verify correct gpack ID
ggen show io.ggen.rust.cli-subcommand
```

### Version Conflicts

```bash
# Error: version conflict for io.ggen.rust.cli-subcommand
# Check installed versions
ggen packs

# Remove conflicting version
ggen remove io.ggen.rust.cli-subcommand

# Install specific version
ggen add io.ggen.rust.cli-subcommand@0.2.1
```

### Dependency Resolution Failures

```bash
# Error: dependency resolution failed
# Check gpack dependencies
ggen show io.ggen.rust.cli-subcommand

# Install missing dependencies
ggen add io.ggen.macros.std

# Update all gpacks
ggen update
```

### Template Not Found in Gpack

```bash
# Error: template 'cli/subcommand/rust.tmpl' not found in gpack
# List available templates
ggen show io.ggen.rust.cli-subcommand

# Use correct template path
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
```

### Cache Corruption

```bash
# Error: corrupted gpack cache
# Clear cache
rm -rf .ggen/gpacks/

# Reinstall gpacks
ggen add io.ggen.rust.cli-subcommand
```

### Network/Registry Connectivity

```bash
# Error: failed to connect to registry
# Check network connectivity
ping registry.ggen.io

# Verify registry URL
ggen search --help

# Try with verbose output
ggen search rust cli --verbose
```

## Gpack Validation and Linting Errors

### Invalid Gpack Manifest

```bash
# Error: invalid ggen.toml manifest
# Check manifest syntax
ggen pack lint

# Validate against schema
ggen validate io.ggen.rust.cli-subcommand
```

### Template Schema Validation

```bash
# Error: template schema validation failed
# Lint template
ggen lint io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl

# Check frontmatter
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```

### RDF Graph Validation

```bash
# Error: RDF graph validation failed
# Validate RDF graphs
ggen validate io.ggen.rust.cli-subcommand --rdf-only

# Check SPARQL queries
ggen show io.ggen.rust.cli-subcommand --sparql
```

## Local Template Issues

### Template Discovery

```bash
# Error: template 'cli subcommand' not found
# Check template location
ls -la templates/cli/subcommand/

# Verify template structure
ggen list
```

### Variable Resolution

```bash
# Error: unbound variable 'name'
# Check variable precedence
ggen gen cli subcommand --vars name=hello

# Verify template frontmatter
cat templates/cli/subcommand/rust.tmpl
```

## Performance Issues

### Slow Generation

```bash
# Enable tracing
GGEN_TRACE=1 ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Check for large RDF graphs
ggen show io.ggen.rust.cli-subcommand --rdf-size

# Use dry run for testing
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
```

### Memory Issues

```bash
# Error: out of memory
# Check RDF graph size
ggen graph export io.ggen.rust.cli-subcommand --fmt ttl | wc -l

# Use smaller graphs
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --vars graph_size=small
```

## Debugging Tips

### Enable Verbose Output

```bash
# Show detailed execution
GGEN_TRACE=1 ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Show variable resolution
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --vars name=hello --verbose
```

### Check System State

```bash
# Verify installation
ggen --version

# Check gpack cache
ls -la .ggen/gpacks/

# View lockfile
cat ggen.lock
```

### Test with Minimal Example

```bash
# Create minimal test template
echo '---\nto: test.txt\nvars:\n  name: world\n---\nHello {{ name }}!' > test.tmpl

# Test generation
ggen gen test.tmpl --vars name=world

# Verify output
cat test.txt
```
