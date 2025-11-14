# Comprehensive Test Strategy - ggen v2.6.0

**Author**: Tester Agent (Hive Mind)
**Created**: 2025-11-14
**Status**: Active Testing Strategy
**Coverage Goal**: >90% with 80/20 focus on critical paths

## Executive Summary

This document defines the comprehensive testing strategy for ggen v2.6.0, ensuring test coverage prevents gaps while following the 80/20 principle: **focus on the 20% of tests that catch 80% of bugs**.

### Current State Analysis

- **Source Files**: 289 across 7 crates
- **Test Files**: 252 (130 in crates/, 122 in tests/)
- **Chicago TDD Migration**: ~50% complete
- **Test Framework**: chicago-tdd-tools v1.1.0
- **Coverage Target**: >90% with focus on critical paths

### Quality Metrics (Target)

```
┌─────────────────────────────────────┐
│   Coverage: >90%                    │
│   Test Execution: <30s (unit+int)  │
│   Mutation Score: >80%              │
│   Property Tests: 100% critical     │
│   E2E Tests: User journeys          │
└─────────────────────────────────────┘
```

## 1. Testing Pyramid (80/20 Distribution)

```
         ┌─────────────────┐
         │   E2E Tests     │  ← 5% (10-15 tests)
         │   (Few, Slow)   │     Critical user journeys
         ├─────────────────┤
         │ Integration     │  ← 15% (40-50 tests)
         │ Tests (Some)    │     Module integration
         ├─────────────────┤
         │   Unit Tests    │  ← 80% (200+ tests)
         │  (Many, Fast)   │     Function-level logic
         └─────────────────┘
```

### Test Distribution by Priority

| Priority | Test Type | Count | Execution Time | Focus Area |
|----------|-----------|-------|----------------|------------|
| **Critical** | Unit | 200+ | <10s | Core logic, RDF, templates |
| **High** | Integration | 40-50 | <15s | Marketplace, lifecycle, CLI |
| **Medium** | E2E | 10-15 | <5s | User journeys, workflows |
| **Low** | Property | 20+ | <5s | Edge cases, fuzzing |

## 2. Module-Level Test Requirements

### 2.1 ggen-core (Core Engine)

**Purpose**: Graph-aware code generation engine
**Coverage Target**: >95% (most critical)

#### Mandatory Tests

**Unit Tests** (80/20 focus):
```rust
// Critical 20% that catches 80% of bugs
✅ RDF graph operations (load, query, transform)
✅ Template rendering (Tera integration)
✅ Lifecycle phases (loader, validation, production)
✅ Error handling (type safety, validation)
✅ Cache operations (template cache, dependency cache)
```

**Integration Tests** (high value):
```rust
✅ End-to-end template generation flow
✅ Multi-template composition
✅ Lifecycle phase transitions
✅ RDF query workflow
✅ Cleanroom environment validation
```

**Required Coverage**:
- Line coverage: >95%
- Branch coverage: >90%
- Critical paths: 100%

**Test Structure**:
```
crates/ggen-core/tests/
├── unit/                    # Unit tests (200+ tests)
│   ├── error_handling.rs    # Error type safety
│   ├── graph_operations.rs  # RDF operations
│   └── template_cache.rs    # Cache logic
├── integration/             # Integration tests (30+ tests)
│   ├── lifecycle_tests.rs   # Full lifecycle
│   ├── end_to_end_flow.rs   # Template generation
│   └── clnrm_harness.rs     # Cleanroom validation
└── property/                # Property tests (10+ tests)
    ├── search_properties.rs # Fuzzing search
    └── serialization.rs     # Round-trip tests
```

### 2.2 ggen-marketplace (Package Distribution)

**Purpose**: Distributed package marketplace
**Coverage Target**: >90%

#### Mandatory Tests

**Unit Tests** (critical business logic):
```rust
✅ Package search (local, registry)
✅ Package installation (validation, extraction)
✅ Crypto operations (signing, verification)
✅ Cache management (quality, recommendations)
```

**Integration Tests** (high-risk paths):
```rust
✅ Install from GitHub
✅ Install from local registry
✅ Concurrent operations
✅ Error scenarios (network, validation)
```

**Required Coverage**:
- Search accuracy: 100% (fuzzing)
- Crypto verification: 100% (security critical)
- Installation flows: >90%

**Test Structure**:
```
crates/ggen-marketplace/tests/
├── search_tests.rs           # Search algorithms
├── install_tests.rs          # Installation flows
├── crypto_ed25519.rs         # Cryptographic operations
├── error_scenarios.rs        # Error handling
└── backend_tests.rs          # Backend implementations
```

### 2.3 ggen-cli (Command-Line Interface)

**Purpose**: User-facing CLI
**Coverage Target**: >85%

#### Mandatory Tests

**Integration Tests** (user workflows):
```rust
✅ project new <name>
✅ project gen <template>
✅ marketplace install <package>
✅ marketplace search <query>
✅ template list
✅ utils doctor (health check)
```

**Error Tests** (user experience):
```rust
✅ Invalid arguments
✅ Missing dependencies
✅ Permission errors
✅ Network failures
```

**Required Coverage**:
- All CLI commands: 100%
- Error messages: 100%
- Help text: 100%

**Test Structure**:
```
crates/ggen-cli/tests/
├── integration/              # CLI integration tests
│   ├── marketplace_tests.rs  # Marketplace commands
│   ├── complete_marketplace.rs # End-to-end flows
│   └── testcontainers.rs     # Container validation
├── conventions/              # Convention-over-config
│   ├── planner_tests.rs
│   ├── resolver_tests.rs
│   └── e2e_tests.rs
└── domain/                   # Domain command tests
    ├── marketplace/
    ├── project/
    └── utils/
```

### 2.4 ggen-domain (Business Logic)

**Purpose**: Pure domain logic
**Coverage Target**: >90%

#### Mandatory Tests

**Unit Tests** (business rules):
```rust
✅ Graph operations (load, query, export)
✅ Template operations (list, show, regenerate)
✅ Marketplace logic (search, install, publish)
✅ Audit and security
✅ CI workflow generation
```

**Integration Tests** (cross-module):
```rust
✅ Graph export with RDF validation
✅ Template generation with hooks
✅ Marketplace search with filters
```

**Required Coverage**:
- Business rules: >95%
- Validation logic: 100%
- Error handling: >90%

**Test Structure**:
```
crates/ggen-domain/tests/
├── graph/                   # Graph operations
│   ├── load_tests.rs
│   ├── query_tests.rs
│   ├── export_tests.rs
│   └── integration_tests.rs
├── marketplace/             # Marketplace logic
│   └── search_tests.rs
├── project/                 # Project operations
│   ├── init_tests.rs
│   └── build_tests.rs
└── utils/                   # Utilities
    ├── doctor_tests.rs
    └── env_tests.rs
```

### 2.5 ggen-ai (AI Integration)

**Purpose**: LLM integration wrapper
**Coverage Target**: >85%

#### Mandatory Tests

**Unit Tests** (AI logic):
```rust
✅ Provider adapters (Anthropic, OpenAI, Ollama)
✅ Prompt generation (template, SPARQL, code)
✅ Response parsing
✅ Streaming integration
✅ Error handling (API failures, rate limits)
```

**Integration Tests** (mock LLM):
```rust
✅ Template generation workflow
✅ SPARQL query generation
✅ Code refactoring suggestions
✅ Natural language search
```

**Required Coverage**:
- Prompt templates: 100%
- Error handling: >90%
- Provider adapters: >85%

**Test Structure**:
```
crates/ggen-ai/tests/
├── rdf_basic_test.rs         # RDF integration
├── rdf_module_test.rs        # Module generation
└── provider_tests.rs         # Provider adapters
```

### 2.6 ggen-utils (Shared Utilities)

**Purpose**: Shared utilities
**Coverage Target**: >80%

#### Mandatory Tests

**Unit Tests** (utility functions):
```rust
✅ Configuration loading (app_config, project_config)
✅ Time utilities
✅ Logging configuration
✅ Type conversions
```

**Required Coverage**:
- Config parsing: >90%
- Type safety: 100%

### 2.7 ggen-node (Node.js Integration)

**Purpose**: Node.js bindings
**Coverage Target**: >75%

#### Mandatory Tests

**Integration Tests** (Node.js FFI):
```rust
✅ Template generation from Node.js
✅ RDF operations from Node.js
✅ Error propagation to JavaScript
```

## 3. Test Requirements Checklist

### 3.1 Per-Module Requirements

Every module MUST have:

- [ ] **Unit tests** for all public functions
- [ ] **Integration tests** for critical workflows
- [ ] **Error tests** for all error paths
- [ ] **Property tests** for complex logic (if applicable)
- [ ] **Benchmark tests** for performance-critical code

### 3.2 Per-Function Requirements

Every public function MUST have:

- [ ] **Happy path test** (normal operation)
- [ ] **Edge case tests** (boundary conditions)
- [ ] **Error tests** (failure modes)
- [ ] **Documentation** (examples in rustdoc)

Example:
```rust
/// Load RDF graph from file
///
/// # Examples
///
/// ```
/// use ggen_core::graph::load_graph;
/// let graph = load_graph("schema.ttl").unwrap();
/// assert!(!graph.is_empty());
/// ```
pub fn load_graph(path: &str) -> Result<Graph> {
    // Implementation
}

// Tests:
#[cfg(test)]
mod tests {
    use super::*;

    test!(test_load_graph_happy_path, {
        // Arrange
        let temp = create_test_file("schema.ttl", VALID_RDF);

        // Act
        let graph = load_graph(temp.path()).unwrap();

        // Assert
        assert!(!graph.is_empty());
    });

    test!(test_load_graph_invalid_file, {
        // Arrange: Non-existent file

        // Act
        let result = load_graph("nonexistent.ttl");

        // Assert
        assert!(result.is_err());
    });

    test!(test_load_graph_empty_file, {
        // Arrange
        let temp = create_test_file("empty.ttl", "");

        // Act
        let graph = load_graph(temp.path()).unwrap();

        // Assert
        assert!(graph.is_empty());
    });
}
```

### 3.3 Integration Test Requirements

Every integration test MUST:

- [ ] Use **AAA pattern** (Arrange-Act-Assert)
- [ ] Use **real objects** (Chicago TDD - no mocks)
- [ ] Verify **final state** (not method calls)
- [ ] Clean up **resources** (TempDir, containers)
- [ ] Have **clear failure messages**

Example:
```rust
async_test!(test_marketplace_install_integration, async {
    // Arrange: Create real test environment
    let temp_dir = TempDir::new().unwrap();
    let options = InstallOptions::new("test-package")
        .with_target(temp_dir.path());

    // Act: Execute real installation
    let result = install_package(&options).await.unwrap();

    // Assert: Verify actual file system state
    assert!(temp_dir.path().join("test-package").exists());
    assert_eq!(result.package_name, "test-package");
    assert_eq!(result.status, InstallStatus::Success);
});
```

### 3.4 E2E Test Requirements

Every E2E test MUST:

- [ ] Test **complete user journey**
- [ ] Use **real CLI commands** (no mocks)
- [ ] Verify **observable outcomes** (files, stdout)
- [ ] Handle **cleanup** (temp directories)
- [ ] Run in **<5 seconds** (use dry-run when possible)

Example:
```rust
async_test!(test_e2e_template_workflow, async {
    // Arrange: Fresh project
    let temp_dir = TempDir::new().unwrap();

    // Act: User journey - create project and generate code
    Command::cargo_bin("ggen").unwrap()
        .args(["project", "new", "my-app"])
        .current_dir(&temp_dir)
        .assert()
        .success();

    Command::cargo_bin("ggen").unwrap()
        .args(["project", "gen", "rust-cli"])
        .current_dir(temp_dir.path().join("my-app"))
        .assert()
        .success();

    // Assert: Verify generated artifacts
    assert!(temp_dir.path().join("my-app/src/main.rs").exists());
    assert!(temp_dir.path().join("my-app/Cargo.toml").exists());
});
```

## 4. Property-Based Testing (Critical Paths)

### 4.1 When to Use Property Tests

Use property tests for:
- **Serialization/Deserialization** (round-trip)
- **Search algorithms** (fuzzing inputs)
- **Parsing** (malformed inputs)
- **Validation** (boundary conditions)

### 4.2 Property Test Examples

```rust
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig::with_cases(property_test_cases()))]

    #[test]
    fn test_search_handles_any_query(query in "[a-zA-Z0-9 ]{0,100}") {
        // Arrange
        let registry = create_test_registry();

        // Act: Search should never panic
        let result = search_packages(&query, &registry);

        // Assert: Either success or expected error
        match result {
            Ok(results) => assert!(results.len() <= 100),
            Err(e) => assert!(e.to_string().contains("invalid")),
        }
    }

    #[test]
    fn test_rdf_round_trip(
        subject in "[a-z]+",
        predicate in "[a-z]+",
        object in "[a-z]+"
    ) {
        // Arrange: Create RDF triple
        let triple = format!(":{} :{} :{} .", subject, predicate, object);

        // Act: Parse and serialize
        let graph = parse_rdf(&triple).unwrap();
        let serialized = serialize_rdf(&graph).unwrap();
        let reparsed = parse_rdf(&serialized).unwrap();

        // Assert: Round-trip preserves data
        assert_eq!(graph, reparsed);
    }
}
```

## 5. Performance Testing

### 5.1 Benchmark Requirements

Benchmark tests for:
- **Template rendering** (<10ms per template)
- **RDF graph loading** (<100ms for 1k triples)
- **Marketplace search** (<50ms local, <500ms remote)
- **CLI startup time** (<200ms)

### 5.2 Benchmark Structure

```
benches/
├── runtime_overhead.rs         # CLI startup overhead
├── async_runtime.rs            # Async performance
├── memory_profiling.rs         # Memory usage
├── conventions_performance.rs  # Convention resolution
└── marketplace_performance.rs  # Search/install performance
```

## 6. Testcontainers Strategy

### 6.1 When to Use Testcontainers

Use testcontainers for:
- **Database integration** (PostgreSQL, Redis)
- **External services** (API mocking)
- **Distributed testing** (multi-node scenarios)
- **Production-like validation**

### 6.2 Container Test Pattern

```rust
async_test!(test_marketplace_with_postgres, async {
    // Arrange: Require Docker
    require_docker();

    let docker = testcontainers::Cli::default();
    let postgres = docker.run(PostgresImage::default());
    let port = postgres.get_host_port_ipv4(5432);

    // Act: Real database operations
    let conn = connect_postgres(port).await.unwrap();
    let packages = fetch_packages(&conn).await.unwrap();

    // Assert: Verify state
    assert!(!packages.is_empty());
});
```

## 7. Test Organization

### 7.1 Directory Structure

```
/Users/sac/ggen/
├── tests/                          # Root-level E2E tests
│   ├── common/                     # Shared test utilities
│   │   └── mod.rs                  # Test config helpers
│   ├── integration/                # Integration tests
│   │   ├── marketplace_*.rs        # Marketplace integration
│   │   ├── template_tests/         # Template integration
│   │   └── nextjs_ontology_*.rs    # Next.js tests
│   ├── e2e_v2/                     # E2E user journeys
│   │   ├── complete_user_journey.rs
│   │   ├── marketplace_discovery.rs
│   │   └── rdf_query_workflow.rs
│   ├── chicago_tdd/                # Chicago TDD tests
│   │   └── marketplace/
│   └── london_tdd/                 # London TDD tests (legacy)
├── crates/*/tests/                 # Per-crate tests
│   ├── unit/                       # Unit tests
│   ├── integration/                # Integration tests
│   └── property/                   # Property tests
└── benches/                        # Benchmarks
```

### 7.2 Test Naming Conventions

```rust
// Unit tests: test_<function>_<scenario>
test!(test_load_graph_happy_path, { ... });
test!(test_load_graph_invalid_file, { ... });

// Integration tests: test_<workflow>_<outcome>
async_test!(test_install_package_success, async { ... });
async_test!(test_install_package_network_error, async { ... });

// E2E tests: test_e2e_<user_journey>
async_test!(test_e2e_complete_project_workflow, async { ... });
```

## 8. Coverage Requirements by Module

| Module | Line Coverage | Branch Coverage | Critical Paths |
|--------|--------------|-----------------|----------------|
| ggen-core | >95% | >90% | 100% |
| ggen-marketplace | >90% | >85% | 100% |
| ggen-domain | >90% | >85% | 95% |
| ggen-cli | >85% | >80% | 100% |
| ggen-ai | >85% | >80% | 90% |
| ggen-utils | >80% | >75% | 90% |
| ggen-node | >75% | >70% | 85% |

## 9. Test Execution Strategy

### 9.1 Continuous Integration (CI)

```yaml
# .github/workflows/test.yml
name: Test Suite

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      # Fast unit tests (fail fast)
      - name: Unit Tests
        run: cargo make test-unit
        timeout-minutes: 2

      # Integration tests (with Docker)
      - name: Integration Tests
        run: cargo make test-integration
        timeout-minutes: 5

      # E2E tests
      - name: E2E Tests
        run: cargo make test-e2e
        timeout-minutes: 3

      # Coverage report
      - name: Coverage
        run: cargo tarpaulin --out Xml
        timeout-minutes: 10
```

### 9.2 Local Development

```bash
# Fast feedback loop (unit tests only)
cargo make test-unit           # <10s

# Full test suite (unit + integration)
cargo make test                # <30s

# Full validation (tests + coverage)
cargo make coverage            # <2min

# Specific module tests
cargo test -p ggen-core        # Core tests only
cargo test -p ggen-marketplace # Marketplace tests only
```

## 10. Quality Gates

### 10.1 Pre-Commit Requirements

- [ ] All unit tests pass
- [ ] No compiler warnings
- [ ] No clippy warnings
- [ ] Code formatted (cargo fmt)

### 10.2 Pre-Merge Requirements

- [ ] All tests pass (unit + integration + E2E)
- [ ] Coverage >90%
- [ ] No security vulnerabilities (cargo audit)
- [ ] Mutation score >80%
- [ ] Benchmarks within 5% of baseline

### 10.3 Pre-Release Requirements

- [ ] Full test suite passes
- [ ] Coverage report generated
- [ ] Performance benchmarks pass
- [ ] Security audit clean
- [ ] Documentation updated
- [ ] Testcontainers validation passes

## 11. Test Configuration

### 11.1 chicago-tdd-tools.toml

```toml
[test]
unit_timeout_seconds = 1
integration_timeout_seconds = 30

[property]
default_test_cases = 100

[testcontainers]
container_wait_timeout_seconds = 5
http_connection_timeout_seconds = 2

[performance]
hot_path_tick_budget = 8

[guards]
max_run_len = 8
max_batch_size = 1000
```

## 12. Migration Checklist

### 12.1 Chicago TDD Migration Status

**Completed** ✅:
- Integration tests (otel_validation, cli_generator_workspace)
- Core lifecycle tests (validation.rs placeholder implemented)

**In Progress** 🚧:
- Core crate tests (~50 files)
- Domain crate tests (~30 files)

**Remaining** 📋:
- CLI crate tests (~15 files)
- Marketplace crate tests (~10 files)
- Node crate tests (~5 files)
- Root-level tests (~100 files)

### 12.2 Migration Pattern

```rust
// Before (Standard Rust)
#[tokio::test]
async fn test_install_package() -> Result<()> {
    let result = install_package(&options).await?;
    assert!(result.is_ok());
    Ok(())
}

// After (Chicago TDD)
use chicago_tdd_tools::prelude::*;

async_test!(test_install_package, async {
    // Arrange
    let options = InstallOptions::new("package");

    // Act
    let result = install_package(&options).await.unwrap();

    // Assert
    assert_ok!(result);
});
```

## 13. Continuous Improvement

### 13.1 Metrics to Track

- **Test execution time** (keep <30s)
- **Coverage trends** (maintain >90%)
- **Mutation score** (improve to >85%)
- **Flaky test rate** (keep <1%)

### 13.2 Quarterly Goals

**Q1 2025**:
- [ ] Complete Chicago TDD migration
- [ ] Achieve 95% coverage on ggen-core
- [ ] Add property tests for all parsers

**Q2 2025**:
- [ ] Mutation score >85%
- [ ] Performance benchmarks in CI
- [ ] Testcontainers for all integration tests

## 14. References

- [Testing and Quality Assurance](./TESTING_AND_QUALITY_ASSURANCE.md)
- [Chicago TDD Guide](./chicago-tdd-guide.md)
- [Test Configuration](../../tests/common/mod.rs)
- [Cargo.toml Test Configuration](../../Cargo.toml)

---

**Status**: Active Testing Strategy
**Last Updated**: 2025-11-14
**Next Review**: 2025-12-14
**Owner**: Tester Agent (Hive Mind)
