# How to Deploy to Production

Production deployment checklist and strategies for ggen-generated code.

## Pre-Deployment Checklist

### 1. Validate Ontology

```bash
# Validate RDF syntax
ggen graph validate domain.ttl

# Validate with SHACL
ggen graph validate domain.ttl --shacl shapes.ttl
```

### 2. Verify Generated Code

```bash
# Generate code
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/

# Build and test
cargo build --release
cargo test
```

### 3. Check Determinism

```bash
# Generate twice and compare
ggen template generate-rdf --ontology domain.ttl --template rust-models --output output1/
ggen template generate-rdf --ontology domain.ttl --template rust-models --output output2/

# Verify identical
diff -r output1/ output2/
```

## Deployment Strategies

### Strategy 1: Generate at Build Time

Generate code during CI/CD build:

```yaml
# .github/workflows/deploy.yml
- name: Generate code
  run: |
    ggen template generate-rdf \
      --ontology domain.ttl \
      --template rust-models \
      --output src/

- name: Build
  run: cargo build --release
```

### Strategy 2: Pre-Generated Code

Commit generated code to repository:

```bash
# Generate and commit
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
git add src/
git commit -m "Regenerate models from ontology"
```

### Strategy 3: Runtime Generation

Generate code at application startup (advanced):

```rust
// Generate on startup
std::process::Command::new("ggen")
    .args(&["template", "generate-rdf", "--ontology", "domain.ttl"])
    .status()?;
```

## Production Considerations

### Version Pinning

Pin ontology and template versions:

```bash
# Use specific versions
ggen template generate-rdf \
  --ontology domain-v1.2.3.ttl \
  --template rust-models@1.0.0 \
  --output src/
```

### Caching

Cache generated code:

```bash
# Enable caching
export GGEN_CACHE_DIR="/var/cache/ggen"
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
```

### Monitoring

Monitor generation performance:

```bash
# Time generation
time ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
```

## Security

### Validate Inputs

Always validate ontologies before generation:

```bash
ggen graph validate domain.ttl --strict
```

### Sanitize Outputs

Review generated code before deployment:

```bash
# Review generated code
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/ --dry-run
```

## Rollback Strategy

### Version Control

Keep ontology versions in Git:

```bash
git tag ontology-v1.0.0
```

### Regenerate from Tag

```bash
git checkout ontology-v1.0.0
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
```

## Best Practices

1. **Validate before deploy:** Always validate ontology
2. **Test generated code:** Run full test suite
3. **Version everything:** Pin ontology and template versions
4. **Monitor performance:** Track generation time
5. **Document process:** Keep deployment runbooks

## Next Steps

- **Production readiness:** See production readiness guide
- **Troubleshooting:** [Troubleshooting Guide](troubleshoot.md)
- **CLI reference:** [CLI Reference](../reference/cli.md)

