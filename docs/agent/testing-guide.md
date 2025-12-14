<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Guide (Chicago TDD)](#testing-guide-chicago-tdd)
  - [State-Based Testing Philosophy](#state-based-testing-philosophy)
  - [Test Organization Structure](#test-organization-structure)
  - [AAA Pattern (Arrange-Act-Assert)](#aaa-pattern-arrange-act-assert)
  - [Real Collaborators Pattern](#real-collaborators-pattern)
    - [When to Mock](#when-to-mock)
  - [Test Categories](#test-categories)
    - [Unit Tests (20% effort, 80% coverage)](#unit-tests-20-effort-80-coverage)
    - [Integration Tests](#integration-tests)
    - [Performance Tests](#performance-tests)
    - [Security Tests](#security-tests)
  - [Test Data & Fixtures](#test-data--fixtures)
  - [Coverage Strategy (80/20 Rule)](#coverage-strategy-8020-rule)
    - [HIGH PRIORITY (Test thoroughly):](#high-priority-test-thoroughly)
    - [MEDIUM PRIORITY (Test selectively):](#medium-priority-test-selectively)
    - [LOW PRIORITY (Test minimally):](#low-priority-test-minimally)
  - [Test Metrics](#test-metrics)
  - [Common Pitfalls to Avoid](#common-pitfalls-to-avoid)
  - [Running Tests](#running-tests)
  - [Critical Rules](#critical-rules)
  - [Examples](#examples)
    - [Complete Unit Test](#complete-unit-test)
    - [Complete Integration Test](#complete-integration-test)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Guide (Chicago TDD)

## State-Based Testing Philosophy

Chicago TDD (also called Detroit School) emphasizes **observable state changes** rather than mocking internals:

- **Verify outputs** - Assert return values match expected results
- **Verify state changes** - Check that objects/files/database changed as expected
- **Real collaborators** - Use actual dependencies (databases, files, RNG)
- **Minimal mocks** - Only mock truly external systems (HTTP APIs, system time)
- **Black-box testing** - Test public interfaces, not implementation

## Test Organization Structure

```
crates/ggen-core/
├── src/
│   ├── graph.rs
│   ├── template.rs
│   └── ...
└── tests/
    ├── unit/
    │   ├── graph_tests.rs
    │   ├── template_tests.rs
    │   └── ...
    ├── integration/
    │   ├── end_to_end_tests.rs
    │   └── workflow_tests.rs
    └── common/
        ├── fixtures.rs      # Shared test data
        ├── mocks.rs         # Mock implementations
        └── helpers.rs       # Test utilities
```

## AAA Pattern (Arrange-Act-Assert)

Every test follows three clear phases:

```rust
#[test]
fn test_template_generation() {
    // ARRANGE: Set up test data
    let project = Project::new("test-project")
        .with_framework("axum");
    let ontology = load_test_ontology("domain.ttl");
    let template = load_template("rust-api");

    // ACT: Perform the action being tested
    let result = template.generate(&project, &ontology);

    // ASSERT: Verify observable results
    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.files_created, 42);
    assert!(output.generated_path.exists());
    assert!(output.generated_path.join("src/main.rs").exists());
}
```

## Real Collaborators Pattern

Use actual implementations rather than mocks:

```rust
#[tokio::test]
async fn test_marketplace_search() {
    // Create actual Store (real RDF engine)
    let store = Store::new().unwrap();

    // Insert real test data
    insert_test_packages(&store).unwrap();

    // Use actual search implementation
    let results = marketplace::search_packages(&store, "auth").unwrap();

    // Verify actual results
    assert_eq!(results.len(), 3);
    assert!(results[0].name.contains("auth"));
}

fn insert_test_packages(store: &Store) -> Result<()> {
    // Real data insertion (not mocked)
    store.insert(Subject::Named(...), ...)?;
    Ok(())
}
```

### When to Mock

Only mock when:
1. **External HTTP APIs** - Replace with `mockito` or `wiremock`
2. **System time** - Use `freezegun` or time mocking
3. **Expensive operations** - Database backups, email delivery
4. **Truly external systems** - AWS, third-party APIs

```rust
#[tokio::test]
async fn test_ai_generation_with_mock_api() {
    // Mock external API
    let _mock = mock("POST", "https://api.openai.com/v1/completions")
        .with_status(200)
        .with_body(json!({"choices": [{"text": "generated code"}]}))
        .create();

    // Test code that calls API
    let result = generate_with_ai("prompt").await;

    assert_eq!(result.code, "generated code");
}
```

## Test Categories

### Unit Tests (20% effort, 80% coverage)

Test individual functions/modules in isolation:

```rust
#[test]
fn test_parse_semver() {
    // Arrange
    let version = "1.2.3";

    // Act
    let parsed = parse_version(version);

    // Assert
    assert_eq!(parsed.major, 1);
    assert_eq!(parsed.minor, 2);
    assert_eq!(parsed.patch, 3);
}

#[test]
fn test_parse_invalid_semver() {
    let result = parse_version("invalid");
    assert!(result.is_err());
}
```

Run with:
```bash
cargo make test-unit  # All unit tests
cargo make test test_parse_semver  # Specific test
```

### Integration Tests

Test component interactions:

```rust
#[test]
fn test_package_installation_workflow() {
    // Arrange
    let store = Store::new().unwrap();
    insert_test_packages(&store).unwrap();
    let install_dir = tempdir().unwrap();

    // Act
    let result = install_package(
        &store,
        "rust-api",
        "1.0.5",
        install_dir.path(),
    );

    // Assert
    assert!(result.is_ok());
    assert!(install_dir.path().join("Cargo.toml").exists());
    assert!(install_dir.path().join("src/main.rs").exists());
}
```

Run with:
```bash
cargo make test  # All tests (unit + integration)
```

### Performance Tests

Verify performance characteristics:

```rust
#[test]
fn test_template_rendering_performance() {
    // Arrange
    let template = load_template("large-template");
    let context = create_large_context(1000);

    // Act
    let start = Instant::now();
    let result = template.render(&context).unwrap();
    let duration = start.elapsed();

    // Assert - Must complete in <100ms
    assert!(duration.as_millis() < 100, "Rendering took {}ms", duration.as_millis());
    assert!(!result.is_empty());
}
```

### Security Tests

Verify security properties:

```rust
#[test]
fn test_path_traversal_prevention() {
    // Arrange
    let installer = PackageInstaller::new();
    let malicious_path = "../../../etc/passwd";

    // Act
    let result = installer.validate_extraction_path(malicious_path);

    // Assert - Must be rejected
    assert!(result.is_err());
}

#[test]
fn test_signature_verification() {
    // Arrange
    let package = load_tampered_package();

    // Act
    let result = verify_package_signature(&package);

    // Assert - Must be invalid
    assert!(result.is_err());
}
```

## Test Data & Fixtures

Centralize test data:

```rust
// tests/common/fixtures.rs
pub fn test_ontology() -> Store {
    let store = Store::new().unwrap();
    store.insert_all(vec![
        (subject("entity/User"), predicate("type"), object("class")),
        (subject("entity/User"), predicate("prop"), object("id:string")),
        (subject("entity/User"), predicate("prop"), object("name:string")),
    ]).unwrap();
    store
}

pub fn test_project() -> Project {
    Project::new("test-project")
        .with_framework("axum")
        .with_ontology(test_ontology())
}

pub fn test_template() -> Template {
    Template::load("templates/rust-api").unwrap()
}
```

Use in tests:

```rust
#[test]
fn test_generate() {
    let template = test_template();
    let project = test_project();
    let result = template.generate(&project);
    assert!(result.is_ok());
}
```

## Coverage Strategy (80/20 Rule)

Focus testing on the critical 20% that prevents 80% of bugs:

### HIGH PRIORITY (Test thoroughly):
- **Error paths** - All Err() cases
- **Boundary conditions** - Empty inputs, null, limits
- **Security boundaries** - Authentication, validation
- **Core algorithms** - Business logic, parsing
- **State transitions** - Valid sequences, invalid ones
- **Concurrency** - Race conditions, deadlocks

### MEDIUM PRIORITY (Test selectively):
- **Helper functions** - Internal utilities
- **Logging/metrics** - Informational output
- **Configuration parsing** - Happy path mostly
- **Documentation examples** - Verify they work

### LOW PRIORITY (Test minimally):
- **Debug formatting** - Nice-to-have
- **Trivial accessors** - Getters returning fields
- **Re-exported types** - If tested upstream

## Test Metrics

Target **80%+ coverage on critical paths**:

```bash
cargo make test:coverage  # Generate coverage report
```

View results in `target/coverage/index.html`

## Common Pitfalls to Avoid

❌ **Overly complex tests:**
```rust
// BAD: Too much setup, hard to understand
#[test]
fn test_thing() {
    let a = setup_complex_a();
    let b = setup_complex_b();
    let c = combine(a, b);
    let d = transform(c);
    assert!(verify_deeply_nested_property(d));
}
```

✅ **Simple, focused tests:**
```rust
// GOOD: Clear arrange-act-assert, one assertion focus
#[test]
fn test_transform_output_format() {
    let input = create_simple_input();
    let result = transform(input);
    assert_eq!(result.format, "expected");
}
```

❌ **Tests that verify implementation:**
```rust
// BAD: Testing how, not what
#[test]
fn test_function_calls_helper() {
    let mut mock = MockHelper::new();
    mock.expect_help().times(1);
    function(&mock);
}
```

✅ **Tests that verify observable behavior:**
```rust
// GOOD: Testing results
#[test]
fn test_function_returns_correct_result() {
    let result = function(test_input());
    assert_eq!(result.value, expected_value());
}
```

## Running Tests

```bash
# All tests
cargo make test

# Specific crate
cargo make test-core

# Specific test
cargo make test test_name

# With output
cargo make test -- --nocapture

# Parallel (default)
cargo make test

# Serial (deterministic async)
cargo make test -- --test-threads=1
```

## Critical Rules

1. **ALWAYS test error paths** - Not just happy path
2. **ONE ASSERTION PER TEST** - Or closely related assertions
3. **REAL DATA** - Use actual files, databases, not just mocks
4. **AAA PATTERN** - Arrange, Act, Assert clearly
5. **MEANINGFUL ASSERTIONS** - Not just `assert_ok!()`
6. **TEST DOCUMENTATION** - Comment non-obvious test setup
7. **ISOLATE TESTS** - Each test must pass independently
8. **DETERMINISTIC RESULTS** - No flaky timing-dependent tests

---

## Examples

### Complete Unit Test

```rust
#[test]
fn test_parse_entity_from_ontology() {
    // Arrange
    let ttl = r#"
        @prefix : <http://example.com/> .
        :User a :Entity ;
            rdfs:label "User" ;
            :hasProperty :id, :name .
    "#;
    let store = parse_ttl(ttl).unwrap();

    // Act
    let entity = Entity::from_store(&store, "User").unwrap();

    // Assert
    assert_eq!(entity.name, "User");
    assert_eq!(entity.properties.len(), 2);
    assert!(entity.properties.contains(&"id".to_string()));
}
```

### Complete Integration Test

```rust
#[tokio::test]
async fn test_end_to_end_project_scaffolding() {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let ontology = load_test_ontology();
    let template = load_template("rust-api");

    // Act
    let result = scaffold_project(
        "my-api",
        template,
        ontology,
        temp_dir.path(),
    ).await;

    // Assert
    assert!(result.is_ok());
    assert!(temp_dir.path().join("src/main.rs").exists());
    assert!(temp_dir.path().join("Cargo.toml").exists());

    // Verify generated code compiles
    let compile_result = run_cargo_check(temp_dir.path()).await;
    assert!(compile_result.is_ok());
}
```
