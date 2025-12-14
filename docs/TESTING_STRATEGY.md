<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Strategy](#testing-strategy)
  - [Testing Philosophy](#testing-philosophy)
    - [Chicago School vs. London School](#chicago-school-vs-london-school)
  - [Test Categories](#test-categories)
    - [1. Unit Tests (80% Coverage Minimum)](#1-unit-tests-80-coverage-minimum)
    - [2. Integration Tests (60% Coverage Minimum)](#2-integration-tests-60-coverage-minimum)
    - [3. End-to-End Tests (Smoke Tests)](#3-end-to-end-tests-smoke-tests)
    - [4. Property-Based Tests (Advanced)](#4-property-based-tests-advanced)
  - [Test Organization](#test-organization)
    - [Directory Structure](#directory-structure)
    - [Test Naming Conventions](#test-naming-conventions)
  - [AAA Pattern (Arrange-Act-Assert)](#aaa-pattern-arrange-act-assert)
  - [Test Data Management](#test-data-management)
    - [Fixtures Directory](#fixtures-directory)
    - [Test Utilities (Common Module)](#test-utilities-common-module)
  - [Coverage Requirements](#coverage-requirements)
    - [Target Coverage by Component](#target-coverage-by-component)
    - [How to Check Coverage](#how-to-check-coverage)
  - [Test-Driven Development Workflow](#test-driven-development-workflow)
    - [1. Red: Write Failing Test](#1-red-write-failing-test)
    - [2. Green: Make Test Pass (Minimum Code)](#2-green-make-test-pass-minimum-code)
    - [3. Refactor: Improve Code (Tests Still Pass)](#3-refactor-improve-code-tests-still-pass)
    - [4. Repeat: Add More Tests](#4-repeat-add-more-tests)
  - [Continuous Integration](#continuous-integration)
    - [GitHub Actions Test Pipeline](#github-actions-test-pipeline)
  - [Performance Testing](#performance-testing)
    - [Benchmarks (criterion)](#benchmarks-criterion)
    - [SLO Verification](#slo-verification)
  - [Key Takeaways](#key-takeaways)
  - [Detailed Testing Documentation](#detailed-testing-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Strategy

**Philosophy**: Chicago School TDD - State-based testing with real collaborators
**Last Updated**: 2025-12-11

---

## Testing Philosophy

### Chicago School vs. London School

**ggen uses Chicago School TDD** (state-based, classical TDD)

**Core Principle**: Tests verify observable behavior using real objects, NOT mocks

**Comparison**:

| Aspect | Chicago School (ggen) | London School |
|--------|----------------------|---------------|
| Collaborators | Real objects | Mocks/stubs |
| Test focus | Observable state changes | Method interactions |
| Coupling | Low (test public API) | High (test implementation) |
| Refactoring | Safe (tests still pass) | Brittle (tests break) |
| Test setup | More complex | Simple |
| Debugging | Easier (real code paths) | Harder (mock behavior) |

**Example**:
```rust
// ✅ Chicago School (ggen style)
#[test]
fn test_lockfile_upsert() {
    // Arrange: Real objects
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Act: Call public API
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();

    // Assert: Verify observable state
    let entry = manager.get("pkg").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");
    assert_eq!(entry.checksum, "sha256");
}

// ❌ London School (NOT ggen style)
#[test]
fn test_with_mocks() {
    let mut mock_repo = MockRepository::new();
    mock_repo.expect_save()
        .with(eq("pkg"), eq("1.0.0"))
        .times(1)
        .returning(|_, _| Ok(()));

    let manager = LockfileManager::new(mock_repo);
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();

    // Fragile: Breaks if implementation changes
}
```

**Why Chicago School**:
- ✅ Tests survive refactoring
- ✅ Tests verify actual behavior
- ✅ No mock maintenance burden
- ✅ Easier to understand for new contributors

**See**: `docs/contributing/TESTING.md` for detailed guide

---

## Test Categories

### 1. Unit Tests (80% Coverage Minimum)

**Purpose**: Test individual functions and modules in isolation

**Scope**: Single function or small module

**Location**: `crates/*/src/*.rs` (inline) or `crates/*/tests/unit/`

**Example**:
```rust
// crates/ggen-utils/src/string_utils.rs
pub fn to_pascal_case(s: &str) -> String {
    // Implementation...
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("hello_world"), "HelloWorld");
        assert_eq!(to_pascal_case("foo-bar-baz"), "FooBarBaz");
        assert_eq!(to_pascal_case(""), "");
    }

    #[test]
    fn test_to_pascal_case_unicode() {
        assert_eq!(to_pascal_case("привет_мир"), "ПриветМир");
    }
}
```

**Coverage Targets**:
- **Core domain logic**: 90%+ (critical business logic)
- **RDF engine**: 85%+ (complex data operations)
- **Template rendering**: 80%+ (deterministic outputs)
- **CLI commands**: 60%+ (integration tests cover rest)
- **Utilities**: 40%+ (simple helper functions)

**Run Command**:
```bash
cargo make test-unit        # Run unit tests only
cargo make coverage         # Generate coverage report
```

---

### 2. Integration Tests (60% Coverage Minimum)

**Purpose**: Test interactions between modules

**Scope**: Multiple modules working together, but still in-process

**Location**: `crates/*/tests/integration/` or `tests/`

**Example**:
```rust
// tests/integration/rdf_to_template.rs
#[test]
fn test_rdf_to_rust_struct_generation() {
    // Arrange: Real RDF store
    let store = RdfStore::new_in_memory();
    store.load_file("tests/fixtures/schema.ttl").unwrap();

    // Act: Query RDF, render template
    let query = "SELECT ?class ?property WHERE { ?class a owl:Class }";
    let results = store.query(query).unwrap();

    let template = Template::load("templates/rust/struct.rs.tera").unwrap();
    let output = template.render(&results).unwrap();

    // Assert: Generated code compiles
    let temp_file = NamedTempFile::new().unwrap();
    fs::write(&temp_file, output).unwrap();

    let rustc_output = Command::new("rustc")
        .arg("--crate-type").arg("lib")
        .arg(temp_file.path())
        .output()
        .unwrap();

    assert!(rustc_output.status.success(),
            "Generated Rust code failed to compile:\n{}",
            String::from_utf8_lossy(&rustc_output.stderr));
}
```

**Coverage Targets**:
- **RDF → Template pipeline**: 80%+
- **AI integration**: 70%+
- **Marketplace operations**: 60%+
- **Lifecycle hooks**: 60%+

**Run Command**:
```bash
cargo make test-integration    # Run integration tests only
```

---

### 3. End-to-End Tests (Smoke Tests)

**Purpose**: Test complete user workflows from CLI entry point

**Scope**: Full application, including CLI parsing, file I/O, external tools

**Location**: `tests/e2e/`

**Example**:
```rust
// tests/e2e/full_generation_workflow.rs
#[test]
fn test_full_generation_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let schema_path = temp_dir.path().join("schema.ttl");
    let output_path = temp_dir.path().join("generated.rs");

    // Step 1: Create schema file
    fs::write(&schema_path, r#"
        @prefix ex: <http://example.org/> .
        ex:User a owl:Class ;
            ex:hasProperty ex:name, ex:age .
    "#).unwrap();

    // Step 2: Load schema
    let load_output = Command::new("ggen")
        .args(&["graph", "load", "--file"])
        .arg(&schema_path)
        .output()
        .unwrap();
    assert!(load_output.status.success());

    // Step 3: Generate code
    let gen_output = Command::new("ggen")
        .args(&["generate", "--template", "rust/struct.rs.tera", "--output"])
        .arg(&output_path)
        .output()
        .unwrap();
    assert!(gen_output.status.success());

    // Step 4: Verify generated code
    let generated_code = fs::read_to_string(&output_path).unwrap();
    assert!(generated_code.contains("struct User"));
    assert!(generated_code.contains("name: String"));
    assert!(generated_code.contains("age: i32"));

    // Step 5: Verify it compiles
    let rustc_output = Command::new("rustc")
        .arg("--crate-type").arg("lib")
        .arg(&output_path)
        .output()
        .unwrap();
    assert!(rustc_output.status.success());
}
```

**Coverage**: Not measured (smoke tests, not unit coverage)

**Run Command**:
```bash
cargo make test-e2e           # Run e2e tests only
cargo make test               # Run all tests including e2e
```

---

### 4. Property-Based Tests (Advanced)

**Purpose**: Test properties that should hold for all inputs

**Scope**: Functions with clear mathematical properties

**Tool**: `proptest` crate

**Example**:
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_to_pascal_case_roundtrip(s in "[a-z_]+") {
        let pascal = to_pascal_case(&s);
        let snake = to_snake_case(&pascal);
        let pascal_again = to_pascal_case(&snake);

        // Property: Converting back and forth should stabilize
        prop_assert_eq!(pascal, pascal_again);
    }

    #[test]
    fn test_rdf_parse_serialize_roundtrip(triples in prop::collection::vec(any::<(String, String, String)>(), 1..100)) {
        let store = RdfStore::new_in_memory();

        // Load triples
        for (s, p, o) in &triples {
            store.insert(s, p, o).unwrap();
        }

        // Serialize
        let turtle = store.export_turtle().unwrap();

        // Deserialize
        let store2 = RdfStore::new_in_memory();
        store2.load_turtle(&turtle).unwrap();

        // Property: Store contents should be identical
        prop_assert_eq!(store.count(), store2.count());
    }
}
```

**When to Use**: Mathematical functions, data transformations, serialization

**Run Command**:
```bash
cargo test proptest          # Run only property tests
```

---

## Test Organization

### Directory Structure

```
ggen/
├── crates/
│   ├── ggen-core/
│   │   ├── src/
│   │   │   ├── lib.rs            # Inline unit tests
│   │   │   └── rdf_store.rs      # Inline unit tests
│   │   └── tests/
│   │       ├── unit/             # Separate unit tests (if too large)
│   │       └── integration/      # Integration tests
│   │
│   ├── ggen-cli/
│   │   ├── src/
│   │   │   └── cmds/             # Inline unit tests
│   │   └── tests/
│   │       └── integration/      # CLI integration tests
│   │
│   └── ggen-utils/
│       └── src/
│           └── *.rs              # Inline unit tests only
│
└── tests/
    ├── e2e/                      # End-to-end tests
    ├── fixtures/                 # Shared test data
    └── common/                   # Shared test utilities
```

---

### Test Naming Conventions

**Pattern**: `test_<function>_<scenario>`

**Examples**:
```rust
#[test]
fn test_parse_success() { ... }            // Happy path

#[test]
fn test_parse_invalid_input() { ... }      // Error case

#[test]
fn test_parse_empty_string() { ... }       // Edge case

#[test]
fn test_concurrent_writes() { ... }        // Concurrency

#[test]
fn test_unicode_handling() { ... }         // Special characters
```

**Rule**: Test name should describe what is being tested and the scenario

---

## AAA Pattern (Arrange-Act-Assert)

**All tests MUST follow AAA structure**

```rust
#[test]
fn test_example() {
    // Arrange: Set up test data and dependencies
    let store = RdfStore::new_in_memory();
    let data = load_test_data("schema.ttl");

    // Act: Execute the function under test
    let result = store.load_turtle(&data);

    // Assert: Verify the expected outcome
    assert!(result.is_ok());
    assert_eq!(store.count(), 42);
}
```

**Why AAA**:
- ✅ Clear structure
- ✅ Easy to read
- ✅ Easy to debug when failing

---

## Test Data Management

### Fixtures Directory

**Location**: `tests/fixtures/`

**Contents**:
- Sample RDF files (schema.ttl, data.ttl)
- Sample templates (class.rs.tera, endpoint.py.tera)
- Sample configuration files (ggen.toml)

**Example**:
```rust
fn load_fixture(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name);
    fs::read_to_string(path).unwrap()
}

#[test]
fn test_with_fixture() {
    let schema = load_fixture("schema.ttl");
    let store = RdfStore::new_in_memory();
    store.load_turtle(&schema).unwrap();
    // ...
}
```

---

### Test Utilities (Common Module)

**Location**: `tests/common/mod.rs`

**Purpose**: Shared helpers for tests

**Example**:
```rust
// tests/common/mod.rs
use std::path::PathBuf;
use tempfile::TempDir;

pub fn create_test_store() -> RdfStore {
    RdfStore::new_in_memory()
}

pub fn create_temp_dir() -> TempDir {
    TempDir::new().unwrap()
}

pub fn load_test_schema() -> String {
    fs::read_to_string("tests/fixtures/schema.ttl").unwrap()
}

// Usage in tests:
// tests/e2e/example.rs
mod common;

#[test]
fn test_something() {
    let store = common::create_test_store();
    let schema = common::load_test_schema();
    // ...
}
```

---

## Coverage Requirements

### Target Coverage by Component

| Component | Minimum Coverage | Target Coverage |
|-----------|------------------|-----------------|
| ggen-core (domain) | 80% | 90% |
| ggen-rdf (RDF engine) | 80% | 85% |
| ggen-templates | 80% | 85% |
| ggen-cli (commands) | 60% | 70% |
| ggen-utils | 40% | 60% |
| ggen-ai | 70% | 80% |

### How to Check Coverage

```bash
# Generate coverage report
cargo make coverage

# Open HTML report
open target/coverage/index.html

# CI checks minimum coverage automatically
cargo make ci
```

**Current Status**: 82.4% overall coverage (exceeds 80% target)

---

## Test-Driven Development Workflow

### 1. Red: Write Failing Test

```rust
#[test]
fn test_new_feature() {
    let result = new_feature_function("input");
    assert_eq!(result, "expected output");
}

// Compile error or test failure
```

---

### 2. Green: Make Test Pass (Minimum Code)

```rust
pub fn new_feature_function(input: &str) -> String {
    // Simplest implementation to make test pass
    "expected output".to_string()
}

// Test passes ✅
```

---

### 3. Refactor: Improve Code (Tests Still Pass)

```rust
pub fn new_feature_function(input: &str) -> String {
    // Better implementation
    input.chars()
        .map(|c| c.to_uppercase())
        .collect()
}

// Tests still pass ✅
```

---

### 4. Repeat: Add More Tests

```rust
#[test]
fn test_new_feature_empty_input() {
    let result = new_feature_function("");
    assert_eq!(result, "");
}

#[test]
fn test_new_feature_unicode() {
    let result = new_feature_function("привет");
    assert_eq!(result, "ПРИВЕТ");
}

// All tests pass ✅
```

---

## Continuous Integration

### GitHub Actions Test Pipeline

**Workflow**: `.github/workflows/ci.yml`

**Steps**:
```yaml
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3

    - name: Install Rust
      uses: actions-rs/toolchain@v1
      with:
        toolchain: stable

    - name: Run unit tests
      run: cargo make test-unit

    - name: Run integration tests
      run: cargo make test-integration

    - name: Run e2e tests
      run: cargo make test-e2e

    - name: Check coverage
      run: |
        cargo make coverage
        if [ $(cargo tarpaulin --output-dir coverage | grep -oP '\d+\.\d+(?=%)') -lt 80 ]; then
          echo "Coverage below 80%"
          exit 1
        fi
```

---

## Performance Testing

### Benchmarks (criterion)

**Purpose**: Track performance regressions

**Location**: `benches/`

**Example**:
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_rdf_query(c: &mut Criterion) {
    let store = RdfStore::new_in_memory();
    store.load_file("benches/fixtures/large_schema.ttl").unwrap();

    c.bench_function("sparql query 1k triples", |b| {
        b.iter(|| {
            let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100";
            store.query(black_box(query)).unwrap()
        });
    });
}

criterion_group!(benches, bench_rdf_query);
criterion_main!(benches);
```

**Run Command**:
```bash
cargo make bench              # Run benchmarks
cargo make bench-compare      # Compare with baseline
```

---

### SLO Verification

**Purpose**: Ensure performance meets service level objectives

**SLOs**:
- First build: ≤ 15s
- Incremental: ≤ 2s
- cargo check: ≤ 5s
- RDF processing: ≤ 5s for 1k+ triples
- Template rendering: < 1ms
- CLI startup: ≤ 50ms

**Command**:
```bash
cargo make slo-check         # Verify all SLOs
```

**CI Enforcement**: SLO checks run on every PR

---

## Key Takeaways

**Focus on these testing practices (80/20)**:

1. ✅ **Chicago School TDD**: Real objects, not mocks
2. ✅ **AAA Pattern**: Arrange-Act-Assert structure
3. ✅ **80% Coverage**: Minimum on critical paths
4. ✅ **Test Categories**: Unit, integration, e2e
5. ✅ **TDD Workflow**: Red-Green-Refactor
6. ✅ **CI Automation**: All tests run on every commit
7. ✅ **Performance Tests**: Benchmarks + SLO checks
8. ✅ **Test Organization**: Clear directory structure
9. ✅ **Fixtures**: Shared test data management
10. ✅ **Test Utilities**: Common helpers for reuse

---

## Detailed Testing Documentation

This is a **quick reference**. For detailed documentation, see:

- **Testing Guide**: `docs/contributing/TESTING.md`
  - Writing effective tests
  - Debugging test failures
  - Advanced testing patterns

- **Test Examples**: `tests/`
  - Reference implementations
  - Common patterns
  - Edge case handling

- **CI Configuration**: `.github/workflows/`
  - Pipeline details
  - Coverage enforcement
  - Performance tracking

---

**Next Steps**:
- Writing tests? → `docs/contributing/TESTING.md`
- Setting up TDD workflow? → `docs/contributing/GETTING_STARTED.md`
- Understanding Chicago School? → This document (Testing Philosophy section)
