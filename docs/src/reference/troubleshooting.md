# Troubleshooting

## General Issues

- **Missing output**: check `to:` and matrix query.
- **Unbound var**: pass `--vars` or add `sparql.vars`.
- **SHACL failure**: fix data to satisfy shape.
- **Nondeterminism**: ensure matrix query has `ORDER BY` and seed is fixed.
- **No writes**: same `K`; use `--dry-run` to inspect.

## Marketplace Issues

### Rpack Not Found

```bash
# Error: rpack 'io.ggen.rust.cli-subcommand' not found
rgen add io.ggen.rust.cli-subcommand

# Check if rpack exists
rgen search rust cli

# Verify correct rpack ID
rgen show io.ggen.rust.cli-subcommand
```

### Version Conflicts

```bash
# Error: version conflict for io.ggen.rust.cli-subcommand
# Check installed versions
rgen packs

# Remove conflicting version
rgen remove io.ggen.rust.cli-subcommand

# Install specific version
rgen add io.ggen.rust.cli-subcommand@0.2.1
```

### Dependency Resolution Failures

```bash
# Error: dependency resolution failed
# Check rpack dependencies
rgen show io.ggen.rust.cli-subcommand

# Install missing dependencies
rgen add io.ggen.macros.std

# Update all rpacks
rgen update
```

### Template Not Found in Rpack

```bash
# Error: template 'cli/subcommand/rust.tmpl' not found in rpack
# List available templates
rgen show io.ggen.rust.cli-subcommand

# Use correct template path
rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
```

### Cache Corruption

```bash
# Error: corrupted rpack cache
# Clear cache
rm -rf .rgen/rpacks/

# Reinstall rpacks
rgen add io.ggen.rust.cli-subcommand
```

### Network/Registry Connectivity

```bash
# Error: failed to connect to registry
# Check network connectivity
ping registry.rgen.io

# Verify registry URL
rgen search --help

# Try with verbose output
rgen search rust cli --verbose
```

## Rpack Validation and Linting Errors

### Invalid Rpack Manifest

```bash
# Error: invalid ggen.toml manifest
# Check manifest syntax
rgen pack lint

# Validate against schema
rgen validate io.ggen.rust.cli-subcommand
```

### Template Schema Validation

```bash
# Error: template schema validation failed
# Lint template
rgen lint io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl

# Check frontmatter
rgen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```

### RDF Graph Validation

```bash
# Error: RDF graph validation failed
# Validate RDF graphs
rgen validate io.ggen.rust.cli-subcommand --rdf-only

# Check SPARQL queries
rgen show io.ggen.rust.cli-subcommand --sparql
```

## Local Template Issues

### Template Discovery

```bash
# Error: template 'cli subcommand' not found
# Check template location
ls -la templates/cli/subcommand/

# Verify template structure
rgen list
```

### Variable Resolution

```bash
# Error: unbound variable 'name'
# Check variable precedence
rgen gen cli subcommand --vars name=hello

# Verify template frontmatter
cat templates/cli/subcommand/rust.tmpl
```

## Performance Issues

### Slow Generation

```bash
# Enable tracing
RGEN_TRACE=1 rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Check for large RDF graphs
rgen show io.ggen.rust.cli-subcommand --rdf-size

# Use dry run for testing
rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
```

### Memory Issues

```bash
# Error: out of memory
# Check RDF graph size
rgen graph export io.ggen.rust.cli-subcommand --fmt ttl | wc -l

# Use smaller graphs
rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --vars graph_size=small
```

## Debugging Tips

### Enable Verbose Output

```bash
# Show detailed execution
RGEN_TRACE=1 rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Show variable resolution
rgen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --vars name=hello --verbose
```

### Check System State

```bash
# Verify installation
rgen --version

# Check rpack cache
ls -la .rgen/rpacks/

# View lockfile
cat rgen.lock
```

### Test with Minimal Example

```bash
# Create minimal test template
echo '---\nto: test.txt\nvars:\n  name: world\n---\nHello {{ name }}!' > test.tmpl

# Test generation
rgen gen test.tmpl --vars name=world

# Verify output
cat test.txt
```
