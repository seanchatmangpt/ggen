# Testing Framework - ggen v6.0.0

## Overview

Comprehensive testing strategy combining Chicago TDD, mutation testing, AI security validation, and performance benchmarking. Target: 80%+ coverage with 60%+ mutation score.

## Testing Philosophy

**Chicago Style TDD (State-Based Testing)**:
- Test observable outputs and state changes
- Use real collaborators, not mocks
- Verify behavior, not implementation
- AAA pattern: Arrange/Act/Assert

**Quality Gates**:
- Unit tests: <150s total, individual <16s
- Integration tests: <30s
- Mutation score: ≥60%
- Coverage: ≥80%
- Zero unwrap/expect in production code
- All error paths tested

## Test Types

### 1. Unit Tests (Chicago TDD)
```rust
#[test]
fn test_rdf_processing_with_valid_ontology() {
    // Arrange: Setup real state
    let ontology = create_test_ontology();
    let processor = RdfProcessor::new();

    // Act: Perform operation
    let result = processor.process(&ontology);

    // Assert: Verify observable output
    assert!(result.is_ok());
    let artifacts = result.unwrap();
    assert_eq!(artifacts.len(), 3);
    assert_eq!(artifacts[0].artifact_type, ArtifactType::Rust);
}
```

### 2. Integration Tests
```rust
#[test]
fn test_full_pipeline_sync() {
    let temp_dir = TempDir::new().unwrap();
    let spec_path = temp_dir.path().join(".specify/specs/test/test.ttl");

    // Create real TTL file
    fs::write(&spec_path, TEST_ONTOLOGY).unwrap();

    // Run full pipeline
    let result = ggen_sync(&spec_path, &temp_dir.path());

    // Verify generated files exist and are correct
    assert!(temp_dir.path().join("crates/test/src/lib.rs").exists());
}
```

### 3. Property-Based Tests (proptest)
```rust
proptest! {
    #[test]
    fn test_rdf_serialization_roundtrip(content in ".*") {
        let graph = parse_rdf(&content)?;
        let serialized = serialize_rdf(&graph)?;
        let reparsed = parse_rdf(&serialized)?;
        prop_assert_eq!(graph, reparsed);
    }
}
```

### 4. Snapshot Tests (insta)
```rust
#[test]
fn test_template_rendering_snapshot() {
    let rendered = render_template("rust_struct.tera", &context);
    insta::assert_snapshot!(rendered);
}
```

### 5. Determinism Tests
```rust
#[test]
fn test_generation_determinism() {
    std::env::set_var("RNG_SEED", "42");

    let result1 = generate_code(&ontology);
    let result2 = generate_code(&ontology);

    assert_eq!(result1, result2);
    assert_eq!(hash_output(&result1), hash_output(&result2));
}
```

### 6. Performance Tests (Criterion)
```rust
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("rdf_processing_1k_triples", |b| {
        b.iter(|| process_rdf(black_box(&large_ontology)))
    });
}
```

### 7. Mutation Tests (cargo-mutants)
See `mutation/README.md` for detailed guide.

### 8. AI Security Tests
See `ai-security/vulnerability-checks.md` for AI-specific security validation.

## Running Tests

**Quick Validation (pre-commit)**:
```bash
cargo make check          # Compiler check (<5s)
cargo make test-unit      # Fast unit tests (<16s)
cargo make lint           # Clippy + rustfmt (<60s)
```

**Full Test Suite**:
```bash
cargo make test           # All tests (<30s)
cargo make test-coverage  # With coverage report
cargo make bench          # Performance benchmarks
```

**Mutation Testing**:
```bash
cargo make test-mutation  # Run cargo-mutants (60%+ required)
```

**Security Validation**:
```bash
cargo make audit          # Dependency vulnerabilities
cargo make test-security  # AI security checks
```

## Test Organization

```
crates/ggen-core/
├── src/
│   ├── lib.rs
│   └── feature.rs
├── tests/                    # Integration tests
│   ├── integration_test.rs
│   └── fixtures/
│       └── test_data.ttl
└── benches/                  # Benchmarks
    └── performance_bench.rs

tests/                        # Workspace-level tests
├── e2e/
│   └── full_pipeline_test.rs
└── fixtures/
    └── sample_ontologies/
```

## Test Data Management

**Fixture Principles**:
- Real RDF ontologies, not synthetic
- Minimal but representative
- Version controlled
- Self-contained (no external dependencies)

**Example Fixture**:
```rust
pub fn create_test_ontology() -> Graph {
    let ttl = r#"
        @prefix : <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        :Entity1 rdf:type :Class1 ;
                 :property1 "value1" .
    "#;
    parse_turtle(ttl).unwrap()
}
```

## Error Path Testing

**Requirement**: Every `Result<T, E>` return must have error path tested.

```rust
#[test]
fn test_invalid_rdf_syntax_returns_error() {
    let invalid_ttl = "invalid { syntax";
    let result = parse_rdf(invalid_ttl);

    assert!(result.is_err());
    match result {
        Err(RdfError::ParseError(msg)) => {
            assert!(msg.contains("syntax"));
        }
        _ => panic!("Expected ParseError"),
    }
}

#[test]
fn test_missing_required_field_returns_error() {
    let incomplete = create_incomplete_spec();
    let result = validate_spec(&incomplete);

    assert!(result.is_err());
    assert_eq!(result.unwrap_err().kind(), ErrorKind::MissingRequiredField);
}
```

## Concurrency Testing

```rust
#[test]
fn test_concurrent_rdf_processing() {
    use std::sync::Arc;
    use tokio::task::JoinSet;

    let processor = Arc::new(RdfProcessor::new());
    let mut set = JoinSet::new();

    for i in 0..10 {
        let proc = Arc::clone(&processor);
        set.spawn(async move {
            proc.process(&create_test_ontology()).await
        });
    }

    // All should succeed without deadlocks or races
    while let Some(result) = set.join_next().await {
        assert!(result.is_ok());
    }
}
```

## Test Hygiene

**Golden Rules**:
1. Tests must be deterministic (no flaky tests)
2. Tests must be isolated (no shared mutable state)
3. Tests must be fast (<16s per unit test)
4. Tests must verify observable behavior
5. No meaningless assertions (e.g., `assert!(true)`)
6. Use `unwrap()` in tests, `Result<T,E>` in production

**Andon Signals**:
- 🔴 CRITICAL: Test failure (`test ... FAILED`)
- 🟡 HIGH: Flaky test (passes sometimes)
- 🟡 HIGH: Slow test (>16s unit test)
- 🟢 GREEN: All tests pass deterministically

## CI/CD Integration

**GitHub Actions Workflow**:
```yaml
- name: Run tests
  run: |
    cargo make check
    cargo make test
    cargo make lint
    cargo make test-mutation
    cargo make slo-check
    cargo make audit
```

## Coverage Requirements

**Minimum Thresholds**:
- Overall coverage: 80%
- Critical paths (RDF processing, code generation): 95%
- Error handling: 100%
- Public APIs: 100%

**Generate Coverage Report**:
```bash
cargo make test-coverage
# Opens coverage report in browser
```

## Performance Benchmarks

**SLO Targets**:
- RDF processing: ≤5s/1k+ triples
- Code generation: ≤3s end-to-end
- First build: ≤15s
- Incremental build: ≤2s
- Memory usage: ≤100MB

**Benchmark Workflow**:
```bash
cargo make bench              # Run all benchmarks
cargo make bench-compare      # Compare against baseline
cargo make bench-profile      # Profile with flamegraph
```

## Test-Driven Development Cycle

```
1. Write failing test (RED)
   cargo make test-unit

2. Implement minimal code (GREEN)
   cargo make test-unit

3. Refactor while maintaining GREEN
   cargo make pre-commit

4. Validate mutation score
   cargo make test-mutation
```

## Common Patterns

**Testing Async Code**:
```rust
#[tokio::test]
async fn test_async_processing() {
    let result = async_process().await;
    assert!(result.is_ok());
}
```

**Testing File I/O**:
```rust
#[test]
fn test_file_generation() {
    let temp = TempDir::new().unwrap();
    let output = temp.path().join("output.rs");

    generate_file(&output, &context).unwrap();

    assert!(output.exists());
    let content = fs::read_to_string(&output).unwrap();
    assert!(content.contains("pub struct"));
}
```

**Testing CLI Commands**:
```rust
#[test]
fn test_cli_sync_command() {
    let output = Command::new("ggen")
        .arg("sync")
        .arg("--dry_run")
        .arg("true")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("Preview:"));
}
```

## Resources

- Chicago TDD Patterns: `chicago-tdd/patterns.md`
- Mutation Testing Guide: `mutation/README.md`
- AI Security Checks: `ai-security/vulnerability-checks.md`
- Rust Testing Book: https://doc.rust-lang.org/book/ch11-00-testing.html
- cargo-mutants: https://mutants.rs/

## Validation Checklist

Before marking any implementation complete:

- [ ] `cargo make check` passes (no compiler errors)
- [ ] `cargo make test` passes (all tests green)
- [ ] `cargo make lint` passes (no warnings)
- [ ] `cargo make test-mutation` ≥60% score
- [ ] `cargo make test-coverage` ≥80% coverage
- [ ] `cargo make slo-check` meets performance targets
- [ ] All error paths tested
- [ ] No unwrap/expect in production code
- [ ] Determinism verified (RNG_SEED=42)
- [ ] AI security checks pass

## Contact

Questions? Open issue at https://github.com/seanchatmangpt/ggen
