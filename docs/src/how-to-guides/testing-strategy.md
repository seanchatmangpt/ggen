# Testing Strategy Guide

Complete guide to ggen's multi-strategy testing approach. ggen uses 5 complementary testing strategies suited to different scenarios. Choose the right strategy for your test to maximize effectiveness and maintainability.

## Testing Strategies Overview

| Strategy | Style | Use When | Test File Location |
|----------|-------|----------|-------------------|
| **BDD** | Behavior-driven | Writing features in business language | `tests/bdd/features/` |
| **Chicago TDD** | Inside-out unit testing | Building units with pure logic | `tests/chicago_tdd/` |
| **London TDD** | Outside-in mocking | Testing I/O boundaries | `tests/london_tdd/` |
| **Property-based** | Generative testing | Testing algorithms with infinite inputs | `crates/*/tests/property/` |
| **E2E** | Full workflow integration | Testing complete user workflows | `tests/e2e/` |

---

## BDD (Behavior-Driven Development)

**Best for:** Features described in natural language, stakeholder communication, acceptance criteria.

### When to Use BDD

- Testing **user-facing features** (CLI commands, marketplace workflows)
- Writing **acceptance criteria** that stakeholders can read
- Building **feature documentation** through tests
- Requirements are **still being refined**

### How to Write BDD Tests

BDD tests use Gherkin syntax (Given-When-Then):

```gherkin
# tests/bdd/features/marketplace.feature
Feature: Marketplace Package Search
  As a user
  I want to search for packages
  So that I can find packages I need

  Scenario: Search for rust packages
    Given the marketplace is available
    And there are packages tagged "rust"
    When I search for "rust"
    Then I should get rust packages
    And results should include package metadata
```

### Implementing BDD Steps

```rust
// tests/bdd/steps/marketplace_steps.rs
use cucumber::{gherkin::Step, World};

#[derive(World)]
pub struct MarketplaceWorld {
    client: MarketplaceClient,
    search_results: Vec<Package>,
}

#[given("the marketplace is available")]
async fn marketplace_available(world: &mut MarketplaceWorld) {
    world.client = MarketplaceClient::new()
        .await
        .expect("marketplace should be available");
}

#[when(expr = "I search for \"(.*)\"")]
async fn search_for(world: &mut MarketplaceWorld, query: String) {
    world.search_results = world.client
        .search(&query)
        .await
        .expect("search should succeed");
}

#[then("I should get rust packages")]
fn check_rust_packages(world: &MarketplaceWorld) {
    for package in &world.search_results {
        assert!(package.tags.contains(&"rust".to_string()));
    }
}
```

### BDD Best Practices

1. **Write scenarios before implementation** - Drive feature development
2. **Use plain language** - Non-technical people should understand tests
3. **Focus on "what"** - Behavior, not implementation details
4. **Share scenarios** - Post in issue/PR for stakeholder feedback
5. **Avoid implementation details** - Use page objects or world fixture for abstraction

### Running BDD Tests

```bash
# Run all BDD features
cargo test --test cucumber

# Run specific feature
cargo test --test cucumber -- --name marketplace

# Run with output
RUST_LOG=debug cargo test --test cucumber

# Generate HTML report
cargo test --test cucumber -- --format pretty
```

**Test location:** `tests/bdd/`
**Framework:** Cucumber (Gherkin syntax)

---

## Chicago TDD (Inside-Out Unit Testing)

**Best for:** Core business logic, mathematical algorithms, pure functions, builder patterns.

### When to Use Chicago TDD

- Building **unit tests for domain logic** (ggen-domain functions)
- Testing **pure functions** with no side effects
- Testing **algorithms** and complex calculations
- Behavior is **well-understood** and unlikely to change
- Need **fast feedback** during development

### How to Write Chicago Tests

Chicago TDD tests focus on **units in isolation** with **real dependencies**:

```rust
// crates/ggen-domain/tests/chicago_tdd/marketplace/package_search.rs
#[cfg(test)]
mod package_search_tests {
    use ggen_domain::marketplace::search::PackageSearcher;
    use ggen_domain::marketplace::models::Package;

    #[test]
    fn search_returns_all_matching_packages() {
        // Arrange - set up real database with test data
        let db = setup_test_database();
        let packages = vec![
            Package::new("pkg-rust", vec!["rust", "cli"]),
            Package::new("pkg-python", vec!["python", "web"]),
            Package::new("pkg-rust-web", vec!["rust", "web"]),
        ];
        db.insert_packages(packages);
        let searcher = PackageSearcher::new(db);

        // Act
        let results = searcher.search("rust");

        // Assert
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|p| p.tags.contains(&"rust")));
    }

    #[test]
    fn search_case_insensitive() {
        let db = setup_test_database();
        db.insert_packages(vec![
            Package::new("UPPERCASE", vec!["rust"]),
        ]);
        let searcher = PackageSearcher::new(db);

        let results = searcher.search("uppercase");

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn search_empty_query_returns_all() {
        let db = setup_test_database();
        db.insert_packages(vec![
            Package::new("pkg1", vec!["rust"]),
            Package::new("pkg2", vec!["python"]),
        ]);
        let searcher = PackageSearcher::new(db);

        let results = searcher.search("");

        assert_eq!(results.len(), 2);
    }
}
```

### Chicago Test Structure

```
Arrange → Act → Assert
   ↓        ↓        ↓
Setup   Execute   Verify
```

### Chicago Best Practices

1. **One logical assertion per test** - Clear failure messages
2. **Use real dependencies** - Test interactions, not mocks
3. **Test behavior, not implementation** - Change implementation without breaking test
4. **Meaningful test names** - Tell what's being tested: `search_returns_all_matching_packages`
5. **Setup helpers** - Extract `setup_test_database()` to reduce duplication
6. **No mocks** - Use real objects (or testcontainers for heavy dependencies)

### Running Chicago Tests

```bash
# Run Chicago TDD tests in ggen-domain
cargo test --lib --package ggen-domain chicago_tdd

# Run with output
cargo test --lib chicago_tdd -- --nocapture

# Run specific test
cargo test search_returns_all_matching_packages
```

**Test location:** `crates/ggen-domain/tests/chicago_tdd/`
**Style:** Real dependencies, AAA pattern, no mocks

---

## London TDD (Outside-In Testing)

**Best for:** API boundaries, command handling, orchestration logic, integration points.

### When to Use London TDD

- Testing **CLI command handlers** with external dependencies
- Testing **API endpoints** (orchestration, not business logic)
- Dependencies are **expensive** (network, database, files)
- Need to **mock external systems** (APIs, databases)
- Testing **error handling** from dependencies

### How to Write London Tests

London TDD tests use **mocks/stubs** to isolate the system under test:

```rust
// crates/ggen-cli/tests/london_tdd/marketplace_cmd.rs
#[cfg(test)]
mod marketplace_install_tests {
    use ggen_cli::commands::marketplace::install;
    use ggen_domain::marketplace::models::Package;
    use mockito::{mock, Matcher};

    #[tokio::test]
    async fn install_command_fetches_and_installs() {
        // Arrange - Mock the marketplace API
        let _m = mock("GET", "/packages/my-pkg")
            .with_status(200)
            .with_body(json!({"name": "my-pkg", "version": "1.0"}))
            .expect(1)
            .create();

        let mock_installer = MockInstaller::new();
        let cmd = InstallCommand::new(mock_installer);

        // Act
        let result = cmd.install("my-pkg").await;

        // Assert
        assert!(result.is_ok());
        mock_installer.assert_called_with("my-pkg");
    }

    #[tokio::test]
    async fn install_command_shows_error_when_package_not_found() {
        // Arrange
        let _m = mock("GET", "/packages/nonexistent")
            .with_status(404)
            .expect(1)
            .create();

        let cmd = InstallCommand::new(MockInstaller::new());

        // Act
        let result = cmd.install("nonexistent").await;

        // Assert
        assert!(result.is_err());
        assert_contains_message(&result, "not found");
    }
}

// Mock implementation
struct MockInstaller {
    installed: Arc<Mutex<Vec<String>>>,
}

impl MockInstaller {
    fn new() -> Self {
        Self {
            installed: Arc::new(Mutex::new(Vec::new())),
        }
    }

    async fn assert_called_with(&self, pkg: &str) {
        let installed = self.installed.lock().unwrap();
        assert!(installed.contains(&pkg.to_string()));
    }
}
```

### London Test Structure

```
Given Mocked Dependencies
  ↓
When System Under Test (SUT) is invoked
  ↓
Then Verify SUT called dependencies correctly
  ↓
And Verify SUT returned correct result
```

### London Best Practices

1. **Mock only external systems** - Database, API, filesystem, network
2. **Don't mock your code** - Only mock actual external dependencies
3. **Verify interactions** - Did SUT call the right dependencies?
4. **Use realistic test data** - Mocks should return realistic data
5. **Test error paths** - Mock failures (404, timeout, etc.)
6. **One interaction per test** - Clear what's being verified

### Running London Tests

```bash
# Run London TDD tests
cargo test --lib london_tdd

# Run with mock output
MOCKITO_LOG=true cargo test --lib london_tdd -- --nocapture

# Run specific test
cargo test install_command_fetches_and_installs
```

**Test location:** `tests/london_tdd/`
**Style:** Mocked external dependencies, behavior verification, isolated units

---

## Property-Based Testing

**Best for:** Algorithms, mathematical properties, invariants, edge cases, infinite test cases.

### When to Use Property-Based Testing

- Testing **algorithms** that should work for ANY valid input
- Testing **mathematical properties** (e.g., sorting is idempotent)
- Need to find **edge cases** you didn't think of
- Testing **SPARQL query optimization** invariants
- Testing **type mappings** for all type combinations

### How to Write Property-Based Tests

Property-based tests generate random inputs and verify properties:

```rust
// crates/ggen-core/tests/property/type_mapping.rs
#[cfg(test)]
mod type_mapping_properties {
    use proptest::prelude::*;
    use ggen_core::type_mapper::TypeMapper;

    // Property: Round-trip conversion should preserve type
    proptest! {
        #[test]
        fn type_roundtrip_preserves_value(value in any::<i64>()) {
            let mapped = TypeMapper::to_rust(value.to_string());
            let unmapped = mapped.parse::<i64>();

            prop_assert_eq!(Ok(value), unmapped);
        }
    }

    // Property: All non-null values should map to Some
    proptest! {
        #[test]
        fn non_null_values_map_to_some(s in "\\PC*") {
            let result = TypeMapper::to_optional(&s);

            match result {
                Some(_) => prop_assert!(!s.is_empty()),
                None => prop_assert!(s.is_empty()),
            }
        }
    }

    // Property: Type mapping should be deterministic
    proptest! {
        #[test]
        fn type_mapping_deterministic(value in any::<String>()) {
            let result1 = TypeMapper::map_type("xsd:string", &value);
            let result2 = TypeMapper::map_type("xsd:string", &value);

            prop_assert_eq!(result1, result2);
        }
    }

    // Custom generator: Generate valid SPARQL queries
    fn valid_sparql_query() -> impl Strategy<Value = String> {
        (
            prop::string::string_regex("[A-Z][a-z]*").unwrap(),
            prop::string::string_regex("[a-z]+").unwrap(),
        )
            .prop_map(|(var, pred)| {
                format!("SELECT ?{} WHERE {{ ?x {} ?{} }}", var, pred, var)
            })
    }

    proptest! {
        #[test]
        fn sparql_parser_accepts_valid_queries(
            query in valid_sparql_query()
        ) {
            let result = parse_sparql(&query);

            prop_assert!(result.is_ok());
        }
    }
}
```

### Property-Based Best Practices

1. **Think in properties** - What should ALWAYS be true?
2. **Use shrinking** - Framework finds minimal failing case
3. **Custom generators** - Generate realistic test data
4. **Document properties** - Why is this property important?
5. **Combine strategies** - Test multiple properties together
6. **Set seed for CI** - Reproduce failures in CI/CD

### Running Property-Based Tests

```bash
# Run property tests
cargo test --lib property

# Run with verbose output
RUST_LOG=debug cargo test --lib property -- --nocapture

# Run with custom seed (reproduce failure)
PROPTEST_REGRESSIONS=test_failures.txt cargo test --lib property
```

**Test location:** `crates/*/tests/property/`
**Framework:** PropTest
**Style:** Property verification, infinite random inputs, shrinking

---

## E2E (End-to-End) Integration Testing

**Best for:** Complete workflows, user-facing features, multi-crate integration, deployment validation.

### When to Use E2E Testing

- Testing **complete user workflows** (CLI → Domain → Storage)
- Testing **multi-crate interactions** (ggen-cli + ggen-domain + ggen-core)
- Testing **actual file I/O** and filesystem operations
- Testing **real database** interactions
- Testing **deployment scenarios**
- Validating **complete features** from user perspective

### How to Write E2E Tests

E2E tests execute actual workflows without mocks:

```rust
// tests/e2e/marketplace_workflow.rs
#[tokio::test]
async fn complete_marketplace_workflow() {
    // Arrange - Set up test environment
    let test_dir = TempDir::new().unwrap();
    let marketplace = RealMarketplaceClient::new();
    let config = GgenConfig::load_from_dir(&test_dir).unwrap();

    // Act 1: Search for packages
    let packages = marketplace.search("rust-template").await.unwrap();
    assert!(!packages.is_empty(), "Should find rust packages");

    // Act 2: Install package
    let package = &packages[0];
    marketplace.install(&package.id, &test_dir).await.unwrap();

    // Act 3: Verify files
    let manifest = test_dir.join("ggen.toml");
    assert!(manifest.exists(), "Package manifest should exist");

    // Act 4: Use package in generation
    let ontology = r#"
        @prefix ex: <http://example.com/> .
        ex:User a rdfs:Class .
    "#;
    std::fs::write(test_dir.join("domain.ttl"), ontology).unwrap();

    let generated = ggen::generate(
        &config,
        &test_dir.join("domain.ttl"),
        package.id.as_str(),
    ).await.unwrap();

    // Assert
    assert!(generated.contains("struct User"));
}

#[tokio::test]
async fn e2e_cli_command() {
    let test_dir = TempDir::new().unwrap();

    // Run actual CLI command
    let output = Command::new("ggen")
        .arg("marketplace")
        .arg("search")
        .arg("rust")
        .current_dir(&test_dir)
        .output()
        .expect("should run ggen command");

    // Assert
    assert!(output.status.success(), "Command should succeed");
    assert!(String::from_utf8_lossy(&output.stdout).contains("rust"));
}

#[tokio::test]
async fn e2e_ontology_to_code() {
    let test_dir = TempDir::new().unwrap();

    // Create ontology
    let ontology = r#"
        @prefix ex: <http://example.com/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:User a rdfs:Class ;
            rdfs:label "User" .

        ex:name a rdf:Property ;
            rdfs:domain ex:User ;
            rdfs:label "name" .
    "#;

    let ttl_file = test_dir.join("domain.ttl");
    std::fs::write(&ttl_file, ontology).unwrap();

    // Generate code
    let result = ggen::generate_from_ontology(
        &ttl_file,
        &test_dir.join("generated"),
        GenerationOptions::default(),
    ).await.unwrap();

    // Assert - Code was generated
    assert!(result.generated_files > 0);

    // Assert - Generated code is valid
    let generated_code = std::fs::read_to_string(
        test_dir.join("generated/models.rs")
    ).unwrap();
    assert!(generated_code.contains("struct User"));
    assert!(generated_code.contains("name"));

    // Assert - Code compiles
    let compile_result = std::process::Command::new("rustc")
        .arg("--crate-type").arg("lib")
        .arg(&test_dir.join("generated/models.rs"))
        .output();
    assert!(compile_result.is_ok(), "Generated code should compile");
}
```

### E2E Test Structure

```
Setup Real Environment
  ↓
Execute Real Workflow
  ↓
Verify End Result
  ↓
Cleanup
```

### E2E Best Practices

1. **Use real systems** - Actual CLI, database, filesystem
2. **Test complete workflows** - Not individual units
3. **Use temporary directories** - Isolate test side effects
4. **Clean up resources** - Use RAII patterns (TempDir, Drop)
5. **Test error scenarios** - Files missing, network down, invalid input
6. **Document setup** - What does test environment need?

### Running E2E Tests

```bash
# Run E2E tests
cargo test --test e2e

# Run with environment variables
RUST_LOG=debug cargo test --test e2e -- --nocapture

# Run specific E2E test
cargo test --test e2e ontology_to_code
```

**Test location:** `tests/e2e/`
**Style:** Real systems, complete workflows, verified results

---

## Choosing Your Testing Strategy

### Decision Tree

```
Is it a user-facing feature?
├─ Yes, needs business language docs? → BDD
├─ No, is it a CLI command with I/O?
│  ├─ Yes → London TDD (mocked I/O)
│  ├─ No, is it pure business logic?
│  │  └─ Yes → Chicago TDD
│  │  └─ No, is it an algorithm?
│  │     └─ Yes → Property-based
└─ No, testing complete workflow?
   └─ Yes → E2E
```

### By Component

**CLI Layer** (ggen-cli):
- London TDD for command handlers (mock domain/storage)
- E2E for complete workflows

**Domain Layer** (ggen-domain):
- Chicago TDD for business functions (real storage)
- E2E for workflows involving multiple domains

**Core Layer** (ggen-core):
- Chicago TDD for RDF/SPARQL operations
- Property-based for algorithms and type mappings
- E2E for template rendering workflows

**AI Layer** (ggen-ai):
- London TDD for API calls (mock LLM providers)
- Property-based for token counting algorithms

---

## Test Organization

```
ggen/
├── tests/
│   ├── bdd/                    # User-facing features
│   │   ├── features/           # .feature files
│   │   └── steps/              # Step implementations
│   ├── chicago_tdd/            # Domain logic
│   ├── london_tdd/             # Boundary testing
│   ├── e2e/                    # Complete workflows
│   └── fixtures/               # Test data
│
├── crates/
│   └── ggen-core/
│       └── tests/
│           ├── property/       # Algorithm properties
│           └── unit/           # Pure unit tests
```

---

## Running Test Suites

```bash
# All tests
cargo test

# By strategy
cargo test --test cucumber          # BDD
cargo test --lib chicago_tdd        # Chicago
cargo test --lib london_tdd         # London
cargo test --lib property           # Property
cargo test --test e2e               # E2E

# With coverage
cargo tarpaulin -o html

# Specific test
cargo test marketplace_workflow

# Show test output
cargo test -- --nocapture
```

---

## Continuous Integration

GitHub Actions workflow automatically runs all test strategies:

```yaml
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - uses: dtolnay/rust-toolchain@stable

    - name: Unit & Chicago Tests
      run: cargo test --lib chicago_tdd

    - name: BDD Tests
      run: cargo test --test cucumber

    - name: London Tests
      run: cargo test --lib london_tdd

    - name: Property Tests
      run: cargo test --lib property

    - name: E2E Tests
      run: cargo test --test e2e

    - name: Coverage
      run: cargo tarpaulin -o xml
```

---

## Key Takeaways

1. **BDD** for user-facing feature validation (business language)
2. **Chicago** for domain logic without mocking (real dependencies)
3. **London** for boundaries with I/O (mocked external systems)
4. **Property** for algorithms and invariants (infinite test cases)
5. **E2E** for complete workflows (real end-to-end)

Each strategy serves a purpose. **Use all five** for comprehensive coverage.
