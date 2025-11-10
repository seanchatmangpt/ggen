# Check Deterministic Outputs

Verify that ggen produces deterministic outputs (same inputs → identical outputs).

## Commands

### Run Deterministic Tests
```bash
cargo make deterministic
```

This command:
- Runs tests with fixed seeds
- Verifies deterministic outputs
- Checks that same inputs produce identical outputs
- Validates snapshot tests

### Single-Threaded Tests
```bash
cargo make test-single-threaded
```

Runs tests in single-threaded mode for deterministic execution order.

### Snapshot Testing
```bash
# Run snapshot tests
cargo make test

# Update snapshots if needed (review changes carefully)
cargo test --package <package> -- --nocapture
```

## Determinism Requirements

### Code Generation
- Template rendering must be deterministic
- RDF graph processing must be deterministic
- SPARQL query execution must be deterministic
- File generation must produce identical bytes for same inputs

### Testing
- Use fixed seeds for all random operations
- Single-threaded async tests (`--test-threads=1`)
- Mock FS, network, time for deterministic tests
- Snapshot tests with `insta` crate

### Verification

Check that:
- [ ] Same template + same RDF graph → same output
- [ ] Snapshot tests pass consistently
- [ ] No timestamps or random values in generated code
- [ ] Tests use fixed seeds
- [ ] Tests run deterministically

## Examples

### Deterministic Template Rendering
```rust
#[test]
fn test_deterministic_generation() {
    let template = Template::load("templates/example.tmpl")?;
    let graph = Graph::load("data.ttl")?;
    let vars = graph.query_sparql(&sparql_query)?;
    
    // First generation
    let output1 = template.render(&vars)?;
    
    // Second generation (should be identical)
    let output2 = template.render(&vars)?;
    
    assert_eq!(output1, output2);
}
```

### Snapshot Testing
```rust
#[test]
fn test_template_snapshot() {
    let output = generate_code(&template, &graph)?;
    insta::assert_snapshot!(output);
}
```

### Fixed Seed Testing
```rust
#[test]
fn test_with_fixed_seed() {
    let mut rng = StdRng::seed_from_u64(42); // Fixed seed
    // Use rng for deterministic random operations
}
```

## Common Issues

### Non-Deterministic Outputs

**Problem**: Generated code differs between runs

**Solutions**:
- Remove timestamps from templates
- Use fixed seeds for random operations
- Ensure deterministic graph processing order
- Use snapshot tests to catch regressions

### Flaky Tests

**Problem**: Tests pass sometimes, fail other times

**Solutions**:
- Use `--test-threads=1` for single-threaded execution
- Mock external dependencies (FS, network, time)
- Use fixed seeds for random operations
- Ensure test isolation

## Validation Commands

```bash
# Run deterministic tests
cargo make deterministic

# Run single-threaded tests
cargo make test-single-threaded

# Run snapshot tests
cargo make test

# Verify SLOs
cargo make slo-check
```

