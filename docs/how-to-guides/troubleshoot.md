<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Troubleshoot Common Issues](#how-to-troubleshoot-common-issues)
  - [Installation Issues](#installation-issues)
    - [Command Not Found](#command-not-found)
    - [Version Flag Not Working](#version-flag-not-working)
  - [Generation Issues](#generation-issues)
    - [Template Not Found](#template-not-found)
    - [SPARQL Query Failed](#sparql-query-failed)
    - [Invalid RDF Syntax](#invalid-rdf-syntax)
  - [Ontology Issues](#ontology-issues)
    - [Missing Properties](#missing-properties)
    - [Type Mismatches](#type-mismatches)
  - [Performance Issues](#performance-issues)
    - [Slow Generation](#slow-generation)
    - [Memory Issues](#memory-issues)
  - [Marketplace Issues](#marketplace-issues)
    - [Connection Failed](#connection-failed)
    - [Package Not Found](#package-not-found)
  - [Build Issues](#build-issues)
    - [Generated Code Doesn't Compile](#generated-code-doesnt-compile)
  - [Getting Help](#getting-help)
    - [Enable Verbose Output](#enable-verbose-output)
    - [Check Logs](#check-logs)
    - [Report Issues](#report-issues)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Troubleshoot Common Issues

Solutions to common problems when using ggen.

## Installation Issues

### Command Not Found

**Problem:** `ggen: command not found`

**Solution:**
```bash
# Check installation
which ggen

# Add to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Verify
ggen --version
```

### Version Flag Not Working

**Problem:** `ggen --version` shows error

**Solution:**
```bash
# Rebuild and reinstall
cargo build --release -p ggen-cli-lib --bin ggen
cargo install --path crates/ggen-cli --bin ggen --force
```

## Generation Issues

### Template Not Found

**Problem:** `Template not found: rust-models`

**Solution:**
```bash
# List available templates
ggen template list

# Install from marketplace
ggen marketplace search "rust-models"
ggen marketplace install io.ggen.templates.rust-models
```

### SPARQL Query Failed

**Problem:** `SPARQL query execution failed`

**Solution:**
```bash
# Validate ontology
ggen graph validate domain.ttl

# Test query manually
ggen graph query domain.ttl --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Check query syntax
cat queries/extract-classes.rq
```

### Invalid RDF Syntax

**Problem:** `Invalid RDF syntax in domain.ttl`

**Solution:**
```bash
# Validate RDF
ggen graph validate domain.ttl --verbose

# Use AI to fix
ggen ai generate-ontology --prompt "Fix RDF: $(cat domain.ttl)" --output domain-fixed.ttl
```

## Ontology Issues

### Missing Properties

**Problem:** Generated code missing expected fields

**Solution:**
```bash
# Inspect ontology
ggen graph query domain.ttl --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Verify property definitions
grep -A 5 "rdf:Property" domain.ttl
```

### Type Mismatches

**Problem:** Generated types don't match expectations

**Solution:**
```bash
# Check type mappings
ggen graph query domain.ttl --sparql "SELECT ?property ?type WHERE { ?property rdfs:range ?type }"

# Verify RDF types
grep "rdfs:range" domain.ttl
```

## Performance Issues

### Slow Generation

**Problem:** Code generation takes too long

**Solution:**
```bash
# Profile generation
time ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/

# Check ontology size
wc -l domain.ttl

# Optimize SPARQL queries
# Use LIMIT and specific patterns
```

### Memory Issues

**Problem:** Out of memory during generation

**Solution:**
```bash
# Check memory usage
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/ --verbose

# Split large ontologies
# Generate in chunks
```

## Marketplace Issues

### Connection Failed

**Problem:** `Failed to connect to marketplace`

**Solution:**
```bash
# Test connectivity
ping registry.ggen.io

# Check DNS
nslookup registry.ggen.io

# Configure proxy if needed
export HTTPS_PROXY="http://proxy:8080"
```

### Package Not Found

**Problem:** `Package not found: io.ggen.template.xyz`

**Solution:**
```bash
# Search for correct name
ggen marketplace search "xyz"

# List installed packages
ggen marketplace list

# Check package name format
# Should be: io.ggen.<category>.<name>
```

## Build Issues

### Generated Code Doesn't Compile

**Problem:** Rust/TypeScript compilation errors

**Solution:**
```bash
# Review generated code
cat src/models.rs

# Check for syntax issues
# Verify template is correct
# Regenerate with verbose output
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/ --verbose
```

## Getting Help

### Enable Verbose Output

```bash
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/ --verbose
```

### Check Logs

```bash
# View ggen logs
cat ~/.cache/ggen/logs/ggen.log
```

### Report Issues

1. Check existing issues: https://github.com/seanchatmangpt/ggen/issues
2. Create new issue with:
   - Error message
   - Command used
   - Ontology/template files (if possible)
   - Verbose output

## Next Steps

- **CLI reference:** [CLI Reference](../reference/cli.md)
- **Getting started:** [Getting Started Tutorial](../tutorials/getting-started.md)
- **Installation:** [Installation Guide](installation.md)

