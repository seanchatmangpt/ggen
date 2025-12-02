# Debug CLI Commands

Troubleshoot and resolve common CLI command issues using ggen's debugging tools.

## Common Issues and Solutions

### Issue: Command Not Found

**Problem**: `ggen: command not found`

**Solutions**:

1. **Verify installation**:
   ```bash
   which ggen
   ggen --version
   ```

2. **Reinstall if needed**:
   ```bash
   cargo install ggen --force
   ```

3. **Check PATH**:
   ```bash
   echo $PATH
   ls ~/.cargo/bin/ggen
   ```

### Issue: Verb Not Recognized

**Problem**: `ggen ontology unknown-verb: no such verb`

**Solution**: Use correct verb names

```bash
# ❌ Wrong
ggen ontology extract-types schema.ttl

# ✅ Correct
ggen ontology extract schema.ttl
```

**List available verbs**:
```bash
ggen ontology --help
```

### Issue: File Not Found

**Problem**: `Error: File not found: schema.ttl`

**Solution**: Use correct file paths

```bash
# ✅ List files to verify
ls -la *.ttl

# ✅ Use absolute paths
ggen ontology extract /Users/sac/ggen/schema.ttl

# ✅ Or use relative paths from correct directory
cd /path/to/files
ggen ontology extract schema.ttl
```

### Issue: Invalid RDF Syntax

**Problem**: `Error: Failed to parse RDF: expected token`

**Solution**: Validate Turtle syntax

```bash
# Use ggen's validator
ggen ontology validate schema.ttl

# Check for common errors:
# - Missing semicolons at end of triples
# - Missing prefixes
# - Invalid URIs
```

Example valid Turtle:

```turtle
@prefix ex: <http://example.org/> .

ex:Class1 a rdfs:Class ;
  rdfs:label "Class 1" .
```

### Issue: Namespace Conflicts

**Problem**: `Error: Duplicate namespace prefix`

**Solution**: Use unique prefixes

```bash
# ❌ Conflicting
ggen generate --namespace ex: --namespace ex:

# ✅ Unique
ggen generate --namespace app: --namespace domain:
```

## Debugging with Verbose Output

Enable detailed logging:

```bash
# Maximum verbosity
ggen ontology generate schema.ttl \
  --verbose \
  --debug

# Output includes:
# [DEBUG] Loading schema.ttl
# [DEBUG] Parsing RDF triples
# [DEBUG] Found 42 classes
# [DEBUG] Generating TypeScript...
```

## Using the Doctor Tool

Check your ggen installation health:

```bash
ggen utils doctor
```

Output shows:
- ✓ ggen version
- ✓ Cargo version
- ✓ Rust toolchain
- ✓ Available templates
- ✓ Workspace status

## Testing Commands Locally

Test before running in CI/CD:

```bash
# 1. Test with sample data
echo '@prefix ex: <http://example.org/> .' > test.ttl
ggen ontology validate test.ttl

# 2. Test with your real ontology
ggen ontology validate your-schema.ttl

# 3. Test generation
ggen ontology generate your-schema.ttl \
  --language typescript \
  --output test-output.ts

# 4. Verify output
cat test-output.ts
```

## Tracing Execution

Use environment variables to trace execution:

```bash
# Trace all operations
RUST_LOG=debug ggen ontology generate schema.ttl --output out.ts

# Trace specific module
RUST_LOG=ggen_core::rdf=debug ggen ontology generate schema.ttl

# Full backtrace on error
RUST_BACKTRACE=1 ggen ontology generate schema.ttl
```

## Checking Generated Code

Validate the generated code compiles:

```bash
# TypeScript
npx tsc --noEmit generated.ts

# Python
python -m py_compile generated.py

# Rust
rustc --crate-type lib generated.rs
```

## Getting Help

```bash
# Command help
ggen --help
ggen ontology --help
ggen ontology generate --help

# Report issues
ggen utils doctor > diagnostics.txt
cat diagnostics.txt
```

## Summary

You now know how to:
- ✅ Diagnose command issues
- ✅ Validate RDF syntax
- ✅ Enable verbose logging
- ✅ Test locally before CI/CD
- ✅ Check generated code compilation
