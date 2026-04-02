<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Doctrine: Chicago TDD for Manufacturing Systems](#testing-doctrine-chicago-tdd-for-manufacturing-systems)
  - [TL;DR](#tldr)
  - [What is Chicago TDD?](#what-is-chicago-tdd)
    - [The Chicago School vs. London School](#the-chicago-school-vs-london-school)
    - [Why Chicago for ggen?](#why-chicago-for-ggen)
    - [Core Thesis](#core-thesis)
  - [The Constitutional Principle: No Mocks](#the-constitutional-principle-no-mocks)
    - [The Mock Problem](#the-mock-problem)
    - [Real Collaborators, Real Confidence](#real-collaborators-real-confidence)
    - [When Mocks Are Acceptable](#when-mocks-are-acceptable)
  - [Testing Philosophy: AAA Pattern](#testing-philosophy-aaa-pattern)
    - [Arrange-Act-Assert Structure](#arrange-act-assert-structure)
    - [Implementation Examples](#implementation-examples)
    - [AAA Anti-Patterns](#aaa-anti-patterns)
  - [Test Types and Requirements](#test-types-and-requirements)
    - [Test Hierarchy](#test-hierarchy)
    - [Coverage Requirements](#coverage-requirements)
    - [Mutation Testing](#mutation-testing)
  - [Real Transports, Not Mocks](#real-transports-not-mocks)
    - [Network Transport Testing](#network-transport-testing)
    - [File System Transport Testing](#file-system-transport-testing)
    - [Database Transport Testing](#database-transport-testing)
  - [Rolling Updates: Proof-First](#rolling-updates-proof-first)
    - [Update Philosophy](#update-philosophy)
    - [Canary Verification](#canary-verification)
    - [Drain and Readiness](#drain-and-readiness)
    - [Rollback Gates](#rollback-gates)
  - [Artifacts Must Survive Upgrade Topology](#artifacts-must-survive-upgrade-topology)
    - [Artifact Persistence Requirements](#artifact-persistence-requirements)
    - [Version Compatibility Testing](#version-compatibility-testing)
    - [Schema Evolution Testing](#schema-evolution-testing)
  - [Test Invariants for Manufacturing Systems](#test-invariants-for-manufacturing-systems)
    - [Manufacturing Invariants](#manufacturing-invariants)
    - [Determinism Invariants](#determinism-invariants)
    - [Reproducibility Invariants](#reproducibility-invariants)
  - [Testing Deterministic Compilation Pipelines](#testing-deterministic-compilation-pipelines)
    - [Pipeline Determinism Requirements](#pipeline-determinism-requirements)
    - [Five-Stage Pipeline Testing (μ₁-μ₅)](#five-stage-pipeline-testing-μ₁-μ₅)
    - [Hash Verification Testing](#hash-verification-testing)
  - [Coverage and Mutation Testing Requirements](#coverage-and-mutation-testing-requirements)
    - [Coverage Targets](#coverage-targets)
    - [Mutation Testing Protocol](#mutation-testing-protocol)
    - [Gap Detection](#gap-detection)
  - [Verification Strategies](#verification-strategies)
    - [Property-Based Testing](#property-based-testing)
    - [Snapshot Testing](#snapshot-testing)
    - [Behavior-Driven Development (BDD)](#behavior-driven-development-bdd)
  - [Test Infrastructure](#test-infrastructure)
    - [Testing Commands](#testing-commands)
    - [Test Organization](#test-organization)
    - [Continuous Integration](#continuous-integration)
  - [Definition of Done for Tests](#definition-of-done-for-tests)
    - [Test Checklist](#test-checklist)
    - [Quality Gates](#quality-gates)
    - [Andon Signals for Tests](#andon-signals-for-tests)
  - [Test Examples and Patterns](#test-examples-and-patterns)
    - [Example 1: Chicago TDD Unit Test](#example-1-chicago-tdd-unit-test)
    - [Example 2: Integration Test with Real Transport](#example-2-integration-test-with-real-transport)
    - [Example 3: Deterministic Pipeline Test](#example-3-deterministic-pipeline-test)
    - [Example 4: Property-Based Test](#example-4-property-based-test)
    - [Example 5: Rolling Update Test](#example-5-rolling-update-test)
  - [Anti-Patterns to Avoid](#anti-patterns-to-avoid)
    - [Anti-Pattern 1: Mock Explosion](#anti-pattern-1-mock-explosion)
    - [Anti-Pattern 2: Test Brittleness](#anti-pattern-2-test-brittleness)
    - [Anti-Pattern 3: Non-Deterministic Tests](#anti-pattern-3-non-deterministic-tests)
  - [Advanced Testing Topics](#advanced-testing-topics)
    - [Canary Analysis](#canary-analysis)
    - [Chaos Engineering for Code Generation](#chaos-engineering-for-code-generation)
    - [Performance Testing and SLO Validation](#performance-testing-and-slo-validation)
  - [Conclusion: Manufacturing-Grade Testing](#conclusion-manufacturing-grade-testing)
    - [The Vision](#the-vision)
    - [Getting Started](#getting-started)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Doctrine: Chicago TDD for Manufacturing Systems

**Reading Time**: 35-40 minutes | **Difficulty**: Intermediate-Advanced | **Prerequisites**: Chicago TDD basics, ggen pipeline understanding

---

## TL;DR

**Chicago TDD is the constitutional principle for ggen testing: state-based verification with real collaborators, never mocks.**

Traditional testing uses mocks to isolate components. Chicago TDD uses real dependencies to verify actual behavior. This is not a preference—it's a **manufacturing requirement** for deterministic code generation.

**Key Principles**:
- Real collaborators, not mocks (constitutional)
- State-based verification over interaction verification
- AAA pattern (Arrange-Act-Assert) enforced
- 80%+ coverage, 60%+ mutation score
- Determinism testing mandatory for pipelines
- Rolling updates require proof before deployment
- Artifacts must survive version upgrades

**The Formula**: `Test_Confidence = Real_Behavior × Observable_State`

**Goal**: Manufacturing-grade quality where tests prove correctness, not just check for regressions.

---

## What is Chicago TDD?

### The Chicago School vs. London School

There are two major schools of Test-Driven Development:

**London School (Mockist/Outside-In)**:
```rust
// London: Test interactions, mock dependencies
#[test]
fn london_style_test() {
    // Mock everything
    let mock_parser = MockParser::new();
    let mock_validator = MockValidator::new();

    mock_parser.expect_parse()
        .times(1)
        .returning(|_| Ok(graph));

    // Test that interactions happened
    let system = System::new(mock_parser, mock_validator);
    system.process(input);

    // Verify mocks were called correctly
    mock_parser.verify();
}
```

**Chicago School (Classicist/Inside-Out)**:
```rust
// Chicago: Test behavior, use real dependencies
#[test]
fn chicago_style_test() {
    // Arrange: Use REAL parser and validator
    let parser = RdfParser::new();
    let validator = ShaclValidator::new();
    let system = System::new(parser, validator);

    // Act: Exercise real system
    let result = system.process(input);

    // Assert: Verify observable state
    assert_eq!(result.graph.triple_count(), 42);
    assert!(result.validation_passed);
}
```

**Key Differences**:

| Aspect | London School | Chicago School |
|--------|---------------|----------------|
| **Focus** | Interaction verification | State verification |
| **Dependencies** | Mocked | Real |
| **Isolation** | High (unit level) | Moderate (component level) |
| **Refactoring** | Breaks tests often | Tests remain stable |
| **Confidence** | Low (mocks may not match reality) | High (tests real behavior) |
| **Speed** | Fast (no real I/O) | Moderate (real operations) |

### Why Chicago for ggen?

ggen is a **manufacturing system** for code generation. Manufacturing systems have different requirements than traditional software:

1. **Determinism is mandatory**: Output must be identical given identical input
2. **Real behavior matters**: Mocked RDF parsers don't test actual parsing
3. **Integration points are critical**: μ₁→μ₂→μ₃→μ₄→μ₅ pipeline must work end-to-end
4. **Artifacts must be real**: Generated code must compile, not just pass mock checks
5. **Upgrades must preserve behavior**: v1 artifacts must work with v2 system

**Chicago TDD ensures these requirements are met through real behavior verification.**

### Core Thesis

> **"Tests must verify real behavior in real conditions. Mocks test your mocks, not your system."**

For ggen, this means:
- RDF parsing tests use real TTL files, not mock graphs
- Code generation tests compile generated code, not mock templates
- Pipeline tests execute all five stages (μ₁-μ₅), not mock transformations
- Upgrade tests migrate real artifacts, not mock data

**The Chicago Principle**: If you can use the real thing in tests, you must. Mocks are a last resort, not a first choice.

---

## The Constitutional Principle: No Mocks

### The Mock Problem

**Why mocks fail for manufacturing systems**:

1. **Mock Drift**: Mock behavior diverges from real implementation
   ```rust
   // Mock says parsing succeeds
   mock_parser.expect_parse().returning(|_| Ok(graph));

   // But real parser fails on malformed TTL
   // Tests pass, production breaks
   ```

2. **Over-Specification**: Tests become brittle
   ```rust
   // Test specifies exact method calls
   mock.expect_method1().times(1);
   mock.expect_method2().times(1);

   // Refactor to call method2 twice → tests break
   // Even though behavior is identical
   ```

3. **False Confidence**: Mocks pass, reality fails
   ```rust
   // Mock graph has 10 triples
   let mock_graph = MockGraph::with_triples(10);

   // Real graph has 10,000 triples → performance issue
   // Mock test passes in 1ms, production times out
   ```

4. **Missing Integration Issues**:
   ```rust
   // Component A's mock of Component B
   mock_b.expect_format().returning(|| "json");

   // Real Component B returns "JSON" (uppercase)
   // Integration fails in production
   ```

### Real Collaborators, Real Confidence

**Chicago TDD Solution**: Use real collaborators wherever possible.

```rust
use chicago_tdd_tools::prelude::*;

test!(test_rdf_pipeline_with_real_components, {
    // Arrange: REAL components
    let parser = RdfParser::new();           // Real parser
    let validator = ShaclValidator::new();   // Real validator
    let query_engine = SparqlEngine::new();  // Real engine

    // Load REAL test data
    let ttl_content = std::fs::read_to_string("tests/fixtures/app.ttl")?;

    // Act: Exercise REAL pipeline
    let graph = parser.parse(&ttl_content)?;
    let validated = validator.validate(&graph)?;
    let results = query_engine.query(&validated, "SELECT * WHERE { ?s ?p ?o }")?;

    // Assert: Verify REAL observable state
    assert_eq!(results.len(), 42);
    assert!(results[0].get("s").is_some());
    assert_eq!(validated.triple_count(), 100);
});
```

**Benefits**:
- Tests verify actual behavior, not mock approximations
- Refactoring doesn't break tests (only observable state matters)
- Integration issues caught immediately
- Performance characteristics tested (real data, real operations)
- Confidence that if tests pass, production will work

### When Mocks Are Acceptable

**Exceptions to the no-mocks rule** (use sparingly):

1. **External Services** (network calls, APIs):
   ```rust
   // Acceptable: Mock external API to avoid network dependency
   let mock_llm = MockLlmService::new();
   mock_llm.expect_complete().returning(|_| Ok(response));
   ```

2. **Time-Dependent Behavior**:
   ```rust
   // Acceptable: Mock clock for determinism
   let mock_clock = MockClock::at(timestamp);
   ```

3. **Non-Deterministic Sources**:
   ```rust
   // Acceptable: Mock RNG for reproducibility
   let mock_rng = MockRng::with_seed(42);
   ```

4. **Expensive Operations** (database writes, file I/O):
   ```rust
   // WRONG: Mock file system
   let mock_fs = MockFs::new();

   // RIGHT: Use temporary directory
   let temp_dir = tempfile::tempdir()?;
   std::fs::write(temp_dir.path().join("test.txt"), "content")?;
   ```

**Rule of Thumb**: If you can use a real implementation in tests (temp files, in-memory databases, local services), do so. Mock only when absolutely necessary.

---

## Testing Philosophy: AAA Pattern

### Arrange-Act-Assert Structure

**All tests must follow AAA pattern**:

```rust
test!(test_name, {
    // ========================================
    // ARRANGE: Set up test fixtures and state
    // ========================================
    let input_data = create_test_data();
    let expected_output = calculate_expected();
    let system_under_test = System::new();

    // ========================================
    // ACT: Execute the behavior being tested
    // ========================================
    let actual_output = system_under_test.process(input_data)?;

    // ========================================
    // ASSERT: Verify observable outcomes
    // ========================================
    assert_eq!(actual_output, expected_output);
    assert!(actual_output.is_valid());
});
```

**Why AAA?**
- **Clarity**: Each phase has a single purpose
- **Maintainability**: Easy to understand what's being tested
- **Debugging**: Failures pinpoint which phase failed
- **Consistency**: Uniform structure across all tests

### Implementation Examples

**Example 1: Simple AAA Test**:
```rust
use chicago_tdd_tools::prelude::*;

test!(test_rdf_parser_parses_valid_turtle, {
    // Arrange
    let ttl_content = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
    "#;
    let parser = RdfParser::new();

    // Act
    let graph = parser.parse(ttl_content)?;

    // Assert
    assert_eq!(graph.triple_count(), 1);
    assert!(graph.contains_subject("http://example.org/subject"));
});
```

**Example 2: Complex AAA Test**:
```rust
async_test!(test_full_pipeline_generates_deterministic_code, {
    // Arrange
    let spec_path = "tests/fixtures/api-spec.ttl";
    let output_dir = tempfile::tempdir()?;
    let seed = 42u64;

    std::env::set_var("RNG_SEED", seed.to_string());

    let expected_hash = "sha256:abc123..."; // Known hash from previous run

    // Act
    let receipt = ggen::sync()
        .with_spec(spec_path)
        .with_output(output_dir.path())
        .execute()
        .await?;

    // Assert
    assert_eq!(receipt.hash, expected_hash);
    assert!(receipt.validation_passed);
    assert_eq!(receipt.generated_files.len(), 5);

    // Verify generated code compiles
    let compile_result = std::process::Command::new("cargo")
        .arg("check")
        .current_dir(output_dir.path())
        .output()?;

    assert!(compile_result.status.success());
});
```

**Example 3: Error Path AAA Test**:
```rust
test!(test_parser_rejects_malformed_turtle, {
    // Arrange
    let malformed_ttl = r#"
        @prefix ex: <http://example.org/>
        ex:subject ex:predicate  // Missing object - invalid
    "#;
    let parser = RdfParser::new();

    // Act
    let result = parser.parse(malformed_ttl);

    // Assert
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(matches!(error, ParseError::MalformedTriple(_)));
    assert!(error.to_string().contains("Missing object"));
});
```

### AAA Anti-Patterns

**Anti-Pattern 1: Mixing Phases**:
```rust
// WRONG: Act and Assert mixed
test!(bad_test, {
    let parser = RdfParser::new();
    assert_eq!(parser.parse(ttl)?.triple_count(), 10); // ACT + ASSERT mixed
});

// RIGHT: Separate phases
test!(good_test, {
    // Arrange
    let parser = RdfParser::new();
    let ttl = load_test_ttl();

    // Act
    let graph = parser.parse(ttl)?;

    // Assert
    assert_eq!(graph.triple_count(), 10);
});
```

**Anti-Pattern 2: Multiple Acts**:
```rust
// WRONG: Testing multiple behaviors in one test
test!(bad_test, {
    let system = System::new();

    let result1 = system.process(input1)?; // ACT 1
    assert_eq!(result1.status, Status::Ok);

    let result2 = system.process(input2)?; // ACT 2
    assert_eq!(result2.status, Status::Ok);
});

// RIGHT: Separate tests for separate behaviors
test!(test_process_input1, {
    let system = System::new();
    let result = system.process(input1)?;
    assert_eq!(result.status, Status::Ok);
});

test!(test_process_input2, {
    let system = System::new();
    let result = system.process(input2)?;
    assert_eq!(result.status, Status::Ok);
});
```

**Anti-Pattern 3: Arrange in Act**:
```rust
// WRONG: Setup during execution phase
test!(bad_test, {
    let system = System::new();

    // Act (with Arrange mixed in)
    let result = system.process(create_test_data())?; // Setup during Act

    assert!(result.is_valid());
});

// RIGHT: All setup in Arrange
test!(good_test, {
    // Arrange
    let system = System::new();
    let test_data = create_test_data();

    // Act
    let result = system.process(test_data)?;

    // Assert
    assert!(result.is_valid());
});
```

---

## Test Types and Requirements

### Test Hierarchy

ggen uses a comprehensive test hierarchy:

```
Test Pyramid (Chicago TDD)
    ┌─────────────────────────┐
    │   E2E / BDD Tests       │  <─ Full system, real environment
    │   (Cucumber features)   │
    ├─────────────────────────┤
    │  Integration Tests      │  <─ Multiple components, real collaborators
    │  (Real RDF, real FS)    │
    ├─────────────────────────┤
    │    Unit Tests           │  <─ Single component, real dependencies
    │  (Real parsing, real    │     (NOT mocked)
    │   validation, real gen) │
    └─────────────────────────┘
         Foundation Layer
    ┌─────────────────────────┐
    │  Property-Based Tests   │  <─ Invariant verification (proptest)
    ├─────────────────────────┤
    │   Snapshot Tests        │  <─ Regression detection (insta)
    ├─────────────────────────┤
    │  Determinism Tests      │  <─ Reproducibility verification
    ├─────────────────────────┤
    │  Performance Tests      │  <─ SLO validation (Criterion)
    └─────────────────────────┘
```

**Test Type Matrix**:

| Type | Framework | Timeout | Real Collaborators? | Coverage Target |
|------|-----------|---------|---------------------|-----------------|
| Unit | Standard Rust | <150s | ✅ YES | 80%+ |
| Integration | Workspace tests | <30s | ✅ YES | 70%+ |
| BDD | Cucumber | - | ✅ YES | Key flows |
| Property | proptest | - | ✅ YES | Critical logic |
| Snapshot | insta | - | ✅ YES | UI/Output |
| Determinism | Custom | - | ✅ YES | Pipelines |
| Performance | Criterion | - | ✅ YES | SLO validation |
| Security | Custom | - | ✅ YES | Auth/crypto |

### Coverage Requirements

**Definition of Done - Coverage**:

1. **Line Coverage**: 80%+ (measured by cargo-tllvm-cov)
2. **Branch Coverage**: 75%+ (all error paths tested)
3. **Mutation Score**: 60%+ (mutations caught by tests)
4. **Public API Coverage**: 100% (every public function tested)

**Commands**:
```bash
# Check coverage
cargo make test                # Must pass
cargo make test-mutation       # Mutation score ≥60%
cargo make detect-gaps         # No critical gaps

# Detailed coverage report
cargo llvm-cov --html --open
```

**Coverage Gaps are Andon Signals**:
- Missing tests for public APIs → 🔴 CRITICAL (stop the line)
- Missing error path tests → 🟡 HIGH (fix before release)
- Low branch coverage → 🟡 HIGH (investigate)

### Mutation Testing

**Mutation testing verifies test quality** by introducing bugs and checking if tests catch them.

**Protocol**:
```bash
# Run mutation tests
cargo make test-mutation

# Must achieve ≥60% mutation score
# Score = (Caught mutations) / (Total mutations)
```

**Example Mutation**:
```rust
// Original code
fn validate_graph(graph: &Graph) -> Result<bool> {
    if graph.triple_count() > 0 {  // Original condition
        Ok(true)
    } else {
        Err(Error::EmptyGraph)
    }
}

// Mutation 1: Change > to >=
if graph.triple_count() >= 0 {  // Always true - bug!

// Mutation 2: Change 0 to 1
if graph.triple_count() > 1 {  // Off-by-one - bug!

// Good tests catch both mutations
test!(test_validates_empty_graph_fails, {
    let graph = Graph::empty();
    assert!(validate_graph(&graph).is_err());  // Catches mutation 1
});

test!(test_validates_single_triple_succeeds, {
    let graph = Graph::with_triples(vec![triple]);
    assert!(validate_graph(&graph).is_ok());   // Catches mutation 2
});
```

**Mutation Testing SLO**:
- Mutation score ≥60%: PASS
- Mutation score <60%: FAIL (improve tests)
- Mutation score <40%: CRITICAL (tests provide false confidence)

---

## Real Transports, Not Mocks

### Network Transport Testing

**Principle**: Use real network transports in tests.

**Wrong (Mocked Network)**:
```rust
// BAD: Mock HTTP client
let mock_http = MockHttpClient::new();
mock_http.expect_get("/api/data")
    .returning(|_| Ok(Response::with_body("{}")));
```

**Right (Real Network)**:
```rust
// GOOD: Use local test server
test!(test_api_client_fetches_data, {
    // Arrange: Start real HTTP server
    let server = TestServer::start();
    server.expect_request("/api/data")
        .respond_with(json!({"status": "ok"}));

    let client = ApiClient::new(server.url());

    // Act: Real HTTP request
    let response = client.get("/api/data").await?;

    // Assert: Real response
    assert_eq!(response.status, "ok");
});
```

**Testcontainers for External Services**:
```rust
use testcontainers::{clients, images};

async_test!(test_with_real_postgres, {
    // Arrange: Real PostgreSQL in Docker
    let docker = clients::Cli::default();
    let postgres = docker.run(images::postgres::Postgres::default());

    let connection_string = format!(
        "postgres://postgres@localhost:{}/postgres",
        postgres.get_host_port_ipv4(5432)
    );

    // Act: Real database operations
    let client = tokio_postgres::connect(&connection_string, NoTls).await?;
    client.execute("CREATE TABLE test (id INT)", &[]).await?;

    // Assert: Real query
    let rows = client.query("SELECT * FROM test", &[]).await?;
    assert_eq!(rows.len(), 0);
});
```

### File System Transport Testing

**Principle**: Use real file system operations with temporary directories.

**Wrong (Mocked FS)**:
```rust
// BAD: Mock file system
let mock_fs = MockFs::new();
mock_fs.expect_write("output.txt", "content");
```

**Right (Real FS)**:
```rust
use tempfile::TempDir;

test!(test_file_writer_creates_output, {
    // Arrange: Real temporary directory
    let temp_dir = TempDir::new()?;
    let output_path = temp_dir.path().join("output.txt");

    let writer = FileWriter::new();

    // Act: Real file write
    writer.write(&output_path, "content")?;

    // Assert: Real file read
    let actual_content = std::fs::read_to_string(&output_path)?;
    assert_eq!(actual_content, "content");

    // Cleanup automatic when temp_dir dropped
});
```

**Directory Structure Testing**:
```rust
test!(test_generator_creates_correct_structure, {
    // Arrange
    let temp_dir = TempDir::new()?;
    let generator = CodeGenerator::new();

    // Act
    generator.generate_project(temp_dir.path())?;

    // Assert: Verify real directory structure
    assert!(temp_dir.path().join("src").is_dir());
    assert!(temp_dir.path().join("src/lib.rs").is_file());
    assert!(temp_dir.path().join("Cargo.toml").is_file());
    assert!(temp_dir.path().join("tests").is_dir());

    // Assert: Verify real file contents
    let lib_content = std::fs::read_to_string(
        temp_dir.path().join("src/lib.rs")
    )?;
    assert!(lib_content.contains("pub fn"));
});
```

### Database Transport Testing

**Principle**: Use real databases (in-memory or containers).

**In-Memory Databases** (SQLite, Oxigraph):
```rust
test!(test_rdf_store_queries_triples, {
    // Arrange: Real in-memory RDF store
    let store = oxigraph::Store::new()?;

    store.load_graph(
        r#"@prefix ex: <http://example.org/> .
           ex:subject ex:predicate ex:object ."#.as_bytes(),
        GraphFormat::Turtle,
        GraphNameRef::DefaultGraph,
        None,
    )?;

    // Act: Real SPARQL query
    let results: Vec<_> = store
        .query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")?
        .collect();

    // Assert: Real results
    assert_eq!(results.len(), 1);
});
```

**Testcontainers for Production DBs**:
```rust
async_test!(test_with_real_redis, {
    // Arrange: Real Redis in Docker
    let docker = clients::Cli::default();
    let redis = docker.run(images::redis::Redis::default());

    let port = redis.get_host_port_ipv4(6379);
    let client = redis::Client::open(format!("redis://127.0.0.1:{}", port))?;
    let mut con = client.get_connection()?;

    // Act: Real Redis operations
    redis::cmd("SET").arg("key").arg("value").execute(&mut con);
    let value: String = redis::cmd("GET").arg("key").query(&mut con)?;

    // Assert: Real result
    assert_eq!(value, "value");
});
```

---

## Rolling Updates: Proof-First

### Update Philosophy

**Principle**: Never deploy updates without proof they work.

Traditional deployment:
```
Code → Build → Deploy → Hope → (Rollback if broken)
```

ggen deployment (proof-first):
```
Code → Build → Prove → Canary → Verify → Deploy → (Rollback gates)
```

**The Proof Requirement**: Before deploying v2, prove:
1. v2 works with v2 artifacts (basic functionality)
2. v2 works with v1 artifacts (backward compatibility)
3. v1→v2 migration succeeds (upgrade path)
4. v2→v1 rollback succeeds (safety net)

### Canary Verification

**Canary testing deploys updates to a small subset first.**

```rust
#[test]
fn test_canary_deployment_succeeds() {
    // Arrange: Deploy to canary environment
    let canary = Environment::new_canary();
    let production = Environment::production();

    let v1_system = deploy_version(&production, "v1.0.0");
    let v2_system = deploy_version(&canary, "v2.0.0");

    // Act: Run canary workload (10% of prod traffic)
    let canary_results = run_canary_workload(&v2_system, traffic_percent: 0.10);
    let prod_results = run_production_workload(&v1_system);

    // Assert: Canary metrics match or exceed production
    assert!(canary_results.success_rate >= prod_results.success_rate);
    assert!(canary_results.p95_latency <= prod_results.p95_latency * 1.1); // Max 10% slower
    assert!(canary_results.error_rate <= prod_results.error_rate);

    // Assert: No canary-specific errors
    assert_eq!(canary_results.unique_errors.len(), 0);
}
```

**Canary Verification Checklist**:
- [ ] Success rate ≥ production
- [ ] Latency within 10% of production
- [ ] Error rate ≤ production
- [ ] No new error types
- [ ] Memory usage stable
- [ ] CPU usage within bounds

### Drain and Readiness

**Drain**: Remove instance from load balancer before shutdown.
**Readiness**: Only route traffic to instances that pass health checks.

```rust
#[test]
fn test_graceful_drain_completes_in_flight_requests() {
    // Arrange: System with active requests
    let system = System::new();
    let active_requests = vec![
        Request::new("req1"),
        Request::new("req2"),
        Request::new("req3"),
    ];

    for req in &active_requests {
        system.start_processing(req);
    }

    // Act: Initiate drain
    let drain_future = system.drain();

    // Wait for drain to complete (max 30 seconds)
    let drain_result = tokio::time::timeout(
        Duration::from_secs(30),
        drain_future
    ).await;

    // Assert: All requests completed
    assert!(drain_result.is_ok());
    assert_eq!(system.active_request_count(), 0);
    assert_eq!(system.completed_request_count(), 3);

    // Assert: New requests rejected
    let new_request = Request::new("req4");
    let result = system.start_processing(&new_request);
    assert!(matches!(result, Err(Error::Draining)));
}

#[test]
fn test_readiness_probe_fails_during_startup() {
    // Arrange: System starting up
    let system = System::new();

    // Act & Assert: Readiness fails until initialized
    assert!(!system.is_ready());

    system.initialize()?;

    // Assert: Readiness succeeds after initialization
    assert!(system.is_ready());
}
```

### Rollback Gates

**Rollback gates automatically revert if metrics degrade.**

```rust
#[test]
fn test_rollback_gate_triggers_on_error_spike() {
    // Arrange: Deployment with rollback gate
    let deployment = Deployment::new("v2.0.0")
        .with_rollback_gate(RollbackGate {
            max_error_rate: 0.05,  // 5% error threshold
            evaluation_window: Duration::from_secs(60),
            min_requests: 100,
        });

    // Act: Simulate error spike
    for i in 0..100 {
        let result = if i < 90 {
            deployment.process_request(request(i))  // 90% success
        } else {
            Err(Error::ProcessingFailed)  // 10% errors (exceeds 5% threshold)
        };
    }

    // Assert: Rollback triggered automatically
    assert!(deployment.rollback_triggered());
    assert_eq!(deployment.current_version(), "v1.0.0");  // Rolled back

    // Assert: Rollback logged
    assert!(deployment.rollback_log().contains("Error rate 10% exceeded threshold 5%"));
}

#[test]
fn test_rollback_gate_does_not_trigger_on_normal_errors() {
    // Arrange
    let deployment = Deployment::new("v2.0.0")
        .with_rollback_gate(RollbackGate {
            max_error_rate: 0.05,
            evaluation_window: Duration::from_secs(60),
            min_requests: 100,
        });

    // Act: Normal error rate (3%)
    for i in 0..100 {
        let result = if i < 97 {
            deployment.process_request(request(i))
        } else {
            Err(Error::ProcessingFailed)  // 3% errors (below threshold)
        };
    }

    // Assert: No rollback
    assert!(!deployment.rollback_triggered());
    assert_eq!(deployment.current_version(), "v2.0.0");  // Still on v2
}
```

---

## Artifacts Must Survive Upgrade Topology

### Artifact Persistence Requirements

**Principle**: Artifacts created by version N must work with version N+1, N+2, ..., N+k.

```rust
#[test]
fn test_v1_artifacts_work_with_v2_system() {
    // Arrange: Generate artifact with v1 system
    let v1_system = System::at_version("1.0.0");
    let v1_artifact = v1_system.generate("tests/fixtures/spec.ttl")?;

    // Persist artifact
    v1_artifact.save("output/v1_artifact.json")?;

    // Act: Load artifact with v2 system
    let v2_system = System::at_version("2.0.0");
    let loaded_artifact = v2_system.load("output/v1_artifact.json")?;

    // Assert: Artifact still valid
    assert!(loaded_artifact.validate().is_ok());
    assert_eq!(loaded_artifact.content_hash, v1_artifact.content_hash);

    // Assert: v2 can use v1 artifact
    let result = v2_system.process(loaded_artifact)?;
    assert!(result.is_valid());
}

#[test]
fn test_artifacts_survive_three_version_upgrades() {
    // Arrange: Create artifact with v1
    let versions = vec!["1.0.0", "2.0.0", "3.0.0", "4.0.0"];
    let v1 = System::at_version(versions[0]);
    let artifact = v1.generate("tests/fixtures/spec.ttl")?;

    // Act: Migrate through versions
    let mut current_artifact = artifact;
    for version in &versions[1..] {
        let system = System::at_version(version);
        current_artifact = system.migrate(current_artifact)?;

        // Assert: Valid at each step
        assert!(current_artifact.validate().is_ok());
    }

    // Assert: Final artifact semantically equivalent to original
    assert_eq!(current_artifact.semantic_hash(), artifact.semantic_hash());
}
```

### Version Compatibility Testing

**Test all version combinations**:

```rust
#[test]
fn test_cross_version_compatibility_matrix() {
    let versions = vec!["1.0.0", "1.1.0", "2.0.0", "2.1.0"];

    for generator_version in &versions {
        for loader_version in &versions {
            // Arrange
            let generator = System::at_version(generator_version);
            let loader = System::at_version(loader_version);

            // Act
            let artifact = generator.generate("tests/fixtures/spec.ttl")?;
            let load_result = loader.load_artifact(&artifact);

            // Assert: Forward compatibility (newer loads older)
            if is_newer(loader_version, generator_version) {
                assert!(
                    load_result.is_ok(),
                    "Version {} failed to load artifact from version {}",
                    loader_version, generator_version
                );
            }

            // Assert: Backward compatibility within major version
            if same_major_version(loader_version, generator_version) {
                assert!(
                    load_result.is_ok(),
                    "Version {} failed to load artifact from {} (same major version)",
                    loader_version, generator_version
                );
            }
        }
    }
}
```

### Schema Evolution Testing

**Test schema migrations**:

```rust
#[test]
fn test_schema_v1_to_v2_migration() {
    // Arrange: v1 schema artifact
    let v1_artifact = json!({
        "version": "1.0.0",
        "schema": "v1",
        "data": {
            "name": "example",
            "value": 42
        }
    });

    // Act: Migrate to v2 schema
    let migrator = SchemaMigrator::new();
    let v2_artifact = migrator.migrate(v1_artifact, "v1", "v2")?;

    // Assert: v2 schema valid
    assert_eq!(v2_artifact["version"], "2.0.0");
    assert_eq!(v2_artifact["schema"], "v2");

    // Assert: Data preserved
    assert_eq!(v2_artifact["data"]["name"], "example");
    assert_eq!(v2_artifact["data"]["value"], 42);

    // Assert: New fields added with defaults
    assert_eq!(v2_artifact["data"]["created_at"], "1970-01-01T00:00:00Z");
}

#[test]
fn test_bidirectional_schema_migration() {
    // Arrange
    let v1_artifact = create_v1_artifact();
    let migrator = SchemaMigrator::new();

    // Act: v1 → v2 → v1
    let v2_artifact = migrator.migrate(v1_artifact.clone(), "v1", "v2")?;
    let v1_roundtrip = migrator.migrate(v2_artifact, "v2", "v1")?;

    // Assert: Round-trip preserves data
    assert_eq!(v1_roundtrip["data"], v1_artifact["data"]);
}
```

---

## Test Invariants for Manufacturing Systems

### Manufacturing Invariants

**Invariants that must hold for all code generation**:

1. **Determinism**: Same input → Same output
2. **Idempotence**: f(f(x)) = f(x)
3. **Commutativity** (where applicable): f(g(x)) = g(f(x))
4. **Reproducibility**: Different machines → Same output
5. **Version Stability**: Same spec → Same output across versions

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_determinism_invariant(
        spec in any::<String>(),
        seed in any::<u64>()
    ) {
        // Arrange
        std::env::set_var("RNG_SEED", seed.to_string());
        let system = System::new();

        // Act: Generate twice
        let output1 = system.generate(&spec)?;
        let output2 = system.generate(&spec)?;

        // Assert: Identical outputs
        prop_assert_eq!(output1.hash(), output2.hash());
        prop_assert_eq!(output1.content(), output2.content());
    }
}

proptest! {
    #[test]
    fn test_idempotence_invariant(spec in any::<String>()) {
        // Arrange
        let system = System::new();

        // Act: Apply transformation twice
        let once = system.transform(&spec)?;
        let twice = system.transform(&once)?;

        // Assert: Second application has no effect
        prop_assert_eq!(once, twice);
    }
}
```

### Determinism Invariants

**All pipelines must be deterministic**:

```rust
#[test]
fn test_pipeline_determinism_with_fixed_seed() {
    // Arrange: Set fixed seed
    std::env::set_var("RNG_SEED", "42");

    let spec = load_test_spec("api.ttl");
    let expected_hash = "sha256:abc123..."; // Known hash

    // Act: Run pipeline 100 times
    for i in 0..100 {
        let receipt = ggen::sync()
            .with_spec(&spec)
            .execute()?;

        // Assert: Hash identical every time
        assert_eq!(
            receipt.hash,
            expected_hash,
            "Non-deterministic output on iteration {}",
            i
        );
    }
}

#[test]
fn test_pipeline_determinism_across_machines() {
    // Arrange: Simulate different machines (different temp dirs)
    let machine1_dir = TempDir::new_in("/tmp/machine1")?;
    let machine2_dir = TempDir::new_in("/tmp/machine2")?;

    std::env::set_var("RNG_SEED", "42");

    // Act: Run on both "machines"
    let receipt1 = ggen::sync()
        .with_spec("tests/fixtures/spec.ttl")
        .with_output(machine1_dir.path())
        .execute()?;

    let receipt2 = ggen::sync()
        .with_spec("tests/fixtures/spec.ttl")
        .with_output(machine2_dir.path())
        .execute()?;

    // Assert: Identical outputs
    assert_eq!(receipt1.hash, receipt2.hash);

    // Assert: File contents match
    let files1 = collect_generated_files(machine1_dir.path())?;
    let files2 = collect_generated_files(machine2_dir.path())?;

    for (file1, file2) in files1.iter().zip(files2.iter()) {
        assert_eq!(
            std::fs::read_to_string(file1)?,
            std::fs::read_to_string(file2)?
        );
    }
}
```

### Reproducibility Invariants

**Builds must be reproducible**:

```rust
#[test]
fn test_reproducible_builds() {
    // Arrange: Same source, different timestamps
    let source = "tests/fixtures/app.ttl";

    // Act: Build at T1
    let build1 = ggen::build(source)
        .with_timestamp(Timestamp::fixed(0))
        .execute()?;

    // Wait 1 second
    std::thread::sleep(Duration::from_secs(1));

    // Act: Build at T2
    let build2 = ggen::build(source)
        .with_timestamp(Timestamp::fixed(0))  // Same fixed timestamp
        .execute()?;

    // Assert: Bit-for-bit identical
    assert_eq!(build1.binary_hash(), build2.binary_hash());
}
```

---

## Testing Deterministic Compilation Pipelines

### Pipeline Determinism Requirements

**Five-stage pipeline (μ₁-μ₅) must be deterministic**:

```
μ₁ (Parse)    →  μ₂ (Validate)  →  μ₃ (Query)  →  μ₄ (Generate)  →  μ₅ (Hash)
   ↓                 ↓                ↓               ↓                 ↓
 RDF Graph      Validated        Query Results    Generated Code    Cryptographic
                 Graph                                                Receipt
```

**Each stage must:**
1. Produce identical output for identical input
2. Not depend on execution order (within stage)
3. Not depend on time, randomness, or external state
4. Be verifiable via hash

### Five-Stage Pipeline Testing (μ₁-μ₅)

```rust
#[test]
fn test_five_stage_pipeline_determinism() {
    std::env::set_var("RNG_SEED", "42");

    // Arrange: Input spec
    let spec = load_test_spec("tests/fixtures/full-app.ttl");

    // Expected hashes for each stage (from baseline run)
    let expected_mu1_hash = "sha256:..."; // Parse stage
    let expected_mu2_hash = "sha256:..."; // Validate stage
    let expected_mu3_hash = "sha256:..."; // Query stage
    let expected_mu4_hash = "sha256:..."; // Generate stage
    let expected_mu5_hash = "sha256:..."; // Final hash

    // Act: Execute pipeline
    let pipeline = Pipeline::new();

    // μ₁: Parse
    let mu1_result = pipeline.stage_mu1_parse(&spec)?;
    assert_eq!(mu1_result.hash(), expected_mu1_hash);

    // μ₂: Validate
    let mu2_result = pipeline.stage_mu2_validate(mu1_result)?;
    assert_eq!(mu2_result.hash(), expected_mu2_hash);

    // μ₃: Query
    let mu3_result = pipeline.stage_mu3_query(mu2_result)?;
    assert_eq!(mu3_result.hash(), expected_mu3_hash);

    // μ₄: Generate
    let mu4_result = pipeline.stage_mu4_generate(mu3_result)?;
    assert_eq!(mu4_result.hash(), expected_mu4_hash);

    // μ₅: Hash (receipt)
    let mu5_result = pipeline.stage_mu5_hash(mu4_result)?;
    assert_eq!(mu5_result.hash(), expected_mu5_hash);
}

#[test]
fn test_pipeline_commutativity_where_applicable() {
    // μ₃ (Query) stage should be commutative for independent queries

    // Arrange
    let validated_graph = create_test_graph();

    let query_a = "SELECT ?s WHERE { ?s a :TypeA }";
    let query_b = "SELECT ?s WHERE { ?s a :TypeB }";

    // Act: Execute in different orders
    let result_ab = execute_queries(&validated_graph, vec![query_a, query_b])?;
    let result_ba = execute_queries(&validated_graph, vec![query_b, query_a])?;

    // Assert: Results equivalent (order may differ, but content same)
    assert_eq!(
        result_ab.into_sorted(),
        result_ba.into_sorted()
    );
}
```

### Hash Verification Testing

```rust
#[test]
fn test_cryptographic_receipt_generation() {
    // Arrange
    let spec = "tests/fixtures/app.ttl";

    // Act: Generate with audit trail
    let receipt = ggen::sync()
        .with_spec(spec)
        .with_audit(true)
        .execute()?;

    // Assert: Receipt contains all stage hashes
    assert!(receipt.mu1_hash.starts_with("sha256:"));
    assert!(receipt.mu2_hash.starts_with("sha256:"));
    assert!(receipt.mu3_hash.starts_with("sha256:"));
    assert!(receipt.mu4_hash.starts_with("sha256:"));
    assert!(receipt.mu5_hash.starts_with("sha256:"));

    // Assert: Final hash is hash of all stage hashes
    let computed_final = compute_hash(&[
        &receipt.mu1_hash,
        &receipt.mu2_hash,
        &receipt.mu3_hash,
        &receipt.mu4_hash,
    ]);

    assert_eq!(receipt.mu5_hash, computed_final);
}

#[test]
fn test_receipt_verification() {
    // Arrange: Generate receipt
    let receipt = ggen::sync()
        .with_spec("tests/fixtures/app.ttl")
        .with_audit(true)
        .execute()?;

    // Act: Verify receipt signature
    let verification = receipt.verify()?;

    // Assert: Verification succeeds
    assert!(verification.is_valid());
    assert_eq!(verification.verified_at, SystemTime::now());

    // Assert: Tampering detected
    let mut tampered_receipt = receipt.clone();
    tampered_receipt.mu4_hash = "sha256:tampered".to_string();

    let tampered_verification = tampered_receipt.verify();
    assert!(tampered_verification.is_err());
}
```

---

## Coverage and Mutation Testing Requirements

### Coverage Targets

**Definition of Done**:
- Line coverage: ≥80%
- Branch coverage: ≥75%
- Public API coverage: 100%
- Error path coverage: 100%

```bash
# Check coverage
cargo make test                     # Must pass
cargo llvm-cov --html --open        # View coverage report

# Enforce coverage
cargo make enforce-coverage          # Fails if <80%
```

**Coverage Report Analysis**:
```rust
#[test]
fn test_coverage_meets_requirements() {
    // This test verifies coverage report exists and meets thresholds
    // Run after: cargo llvm-cov --json --output-path coverage.json

    let coverage = CoverageReport::load("coverage.json")?;

    // Assert: Overall coverage ≥80%
    assert!(
        coverage.line_coverage() >= 0.80,
        "Line coverage {}% below 80% threshold",
        coverage.line_coverage() * 100.0
    );

    // Assert: Branch coverage ≥75%
    assert!(
        coverage.branch_coverage() >= 0.75,
        "Branch coverage {}% below 75% threshold",
        coverage.branch_coverage() * 100.0
    );

    // Assert: No untested public APIs
    let untested_public = coverage.untested_public_functions();
    assert!(
        untested_public.is_empty(),
        "Untested public APIs: {:?}",
        untested_public
    );
}
```

### Mutation Testing Protocol

**Mutation testing verifies test quality**:

```bash
# Run mutation tests (takes ~5 minutes)
cargo make test-mutation

# Required score: ≥60%
# Score = (Mutations caught by tests) / (Total mutations)
```

**Mutation Test Example**:
```rust
// Original function
pub fn validate_triple_count(graph: &Graph) -> Result<()> {
    if graph.triple_count() > 0 {
        Ok(())
    } else {
        Err(Error::EmptyGraph)
    }
}

// Cargo-mutants will generate mutations:
// Mutation 1: > becomes >=
// Mutation 2: 0 becomes 1
// Mutation 3: Ok(()) becomes Err(...)

// Tests must catch ALL mutations
#[test]
fn test_validates_empty_graph_fails() {
    let graph = Graph::empty();
    assert!(validate_triple_count(&graph).is_err());  // Catches mutation 1
}

#[test]
fn test_validates_single_triple_succeeds() {
    let graph = Graph::with_triples(vec![triple]);
    assert!(validate_triple_count(&graph).is_ok());   // Catches mutation 2
}

#[test]
fn test_validates_multiple_triples_succeeds() {
    let graph = Graph::with_triples(vec![triple1, triple2]);
    assert!(validate_triple_count(&graph).is_ok());   // Ensures correctness
}
```

### Gap Detection

**Automated gap detection finds missing tests**:

```bash
cargo make detect-gaps              # Find coverage gaps
cargo make gap-report               # Generate JSON report
```

**Gap Detection Test**:
```rust
#[test]
fn test_no_critical_coverage_gaps() {
    // Run gap detection
    let gaps = GapDetector::analyze_workspace()?;

    // Assert: No untested public APIs
    assert!(
        gaps.untested_public_apis.is_empty(),
        "Untested public APIs: {:?}",
        gaps.untested_public_apis
    );

    // Assert: No missing error path tests
    assert!(
        gaps.missing_error_paths.is_empty(),
        "Missing error path tests: {:?}",
        gaps.missing_error_paths
    );

    // Assert: No critical files below threshold
    let critical_files = gaps.files_below_threshold(0.80);
    assert!(
        critical_files.is_empty(),
        "Files below 80% coverage: {:?}",
        critical_files
    );
}
```

---

## Verification Strategies

### Property-Based Testing

**Use proptest for invariant verification**:

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_parser_never_panics(input in "\\PC*") {
        // Property: Parser never panics on any input
        let parser = RdfParser::new();
        let _ = parser.parse(&input); // May error, but never panic
    }

    #[test]
    fn test_hash_stability(content in any::<Vec<u8>>()) {
        // Property: Same content always produces same hash
        let hash1 = compute_hash(&content);
        let hash2 = compute_hash(&content);
        prop_assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_generated_code_always_compiles(
        spec in valid_ttl_spec()
    ) {
        // Property: Valid spec always generates compilable code
        let code = generate_code(&spec)?;
        let compile_result = compile_generated_code(&code)?;
        prop_assert!(compile_result.success);
    }
}
```

### Snapshot Testing

**Use insta for regression detection**:

```rust
use insta::assert_snapshot;

#[test]
fn test_code_generation_output() {
    // Arrange
    let spec = load_test_spec("api.ttl");

    // Act
    let generated_code = generate_rust_code(&spec)?;

    // Assert: Snapshot test (catches regressions)
    assert_snapshot!(generated_code);

    // First run: Creates snapshot
    // Subsequent runs: Compares against snapshot
    // If changed: Reviewer must approve or reject
}

#[test]
fn test_error_message_formatting() {
    // Arrange
    let error = ParseError::MalformedTriple {
        line: 42,
        column: 10,
        reason: "Missing object".to_string(),
    };

    // Act
    let formatted = format!("{}", error);

    // Assert: Snapshot ensures error messages don't regress
    assert_snapshot!(formatted);
}
```

### Behavior-Driven Development (BDD)

**Use Cucumber for acceptance tests**:

```gherkin
# features/code_generation.feature
Feature: Code Generation from RDF Ontology
  As a developer
  I want to generate code from ontology specifications
  So that code stays in sync with domain models

  Scenario: Generate Rust struct from OWL class
    Given an RDF ontology with class "Person"
    And the class has properties "name" and "age"
    When I run "ggen sync"
    Then a Rust struct "Person" should be generated
    And the struct should have fields "name: String" and "age: u32"
    And the generated code should compile without errors

  Scenario: Deterministic generation
    Given an RDF ontology "app.ttl"
    And a fixed random seed "42"
    When I generate code 5 times
    Then all outputs should have identical hashes
```

```rust
// steps/code_generation.rs
use cucumber::{given, when, then, World};

#[given("an RDF ontology with class {string}")]
fn create_ontology_with_class(world: &mut CodeGenWorld, class_name: String) {
    world.ontology = Ontology::new();
    world.ontology.add_class(&class_name);
}

#[when("I run {string}")]
async fn run_command(world: &mut CodeGenWorld, command: String) {
    world.output = execute_command(&command).await?;
}

#[then("a Rust struct {string} should be generated")]
fn verify_struct_generated(world: &mut CodeGenWorld, struct_name: String) {
    assert!(world.output.contains(&format!("pub struct {}", struct_name)));
}
```

---

## Test Infrastructure

### Testing Commands

**All testing via `cargo make` (NEVER direct cargo)**:

```bash
# Core testing commands
cargo make test               # All tests (<30s)
cargo make test-unit          # Unit tests only (<150s)
cargo make test-integration   # Integration tests (<30s)
cargo make test-doc           # Doc tests

# Specialized tests
cargo make test-bdd           # Cucumber features
cargo make test-proptest      # Property-based tests
cargo make deterministic      # Determinism tests (RNG_SEED=42)
cargo make test-mutation      # Mutation testing (≥60% score)

# Coverage
cargo make enforce-coverage   # Fail if <80%
cargo make detect-gaps        # Find coverage gaps

# Pre-commit validation
cargo make pre-commit         # Format + lint + test-unit
cargo make ci                 # Full CI suite
```

### Test Organization

**Directory structure**:

```
ggen/
├── crates/
│   ├── ggen-core/
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   └── pipeline.rs
│   │   └── tests/              ← Integration tests
│   │       ├── chicago_tdd_smoke_test.rs
│   │       ├── determinism_framework.rs
│   │       └── pipeline_e2e.rs
│   │
│   └── ggen-cli/
│       ├── src/
│       └── tests/
│           ├── chicago_tdd_smoke_test.rs
│           └── cli_integration.rs
│
├── tests/                      ← Workspace-level tests
│   ├── fixtures/
│   │   ├── app.ttl
│   │   └── expected/
│   └── integration/
│
└── features/                   ← BDD tests
    └── code_generation.feature
```

### Continuous Integration

**GitHub Actions quality gates**:

```yaml
# .github/workflows/test.yml
name: Test Suite

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      # Quality gate 1: Format
      - name: Check formatting
        run: cargo make fmt

      # Quality gate 2: Compilation
      - name: Check compilation
        run: cargo make check

      # Quality gate 3: Linting
      - name: Run clippy
        run: cargo make lint

      # Quality gate 4: Tests
      - name: Run tests
        run: cargo make test

      # Quality gate 5: Coverage
      - name: Check coverage
        run: cargo make enforce-coverage

      # Quality gate 6: Mutation (on main only)
      - name: Mutation testing
        if: github.ref == 'refs/heads/main'
        run: cargo make test-mutation
```

---

## Definition of Done for Tests

### Test Checklist

**Before marking a feature "done"**:

- [ ] All public APIs have tests (100%)
- [ ] All error paths tested (100%)
- [ ] AAA pattern followed in all tests
- [ ] Real collaborators used (no mocks except when necessary)
- [ ] Line coverage ≥80%
- [ ] Branch coverage ≥75%
- [ ] Mutation score ≥60%
- [ ] Determinism tested (for pipelines)
- [ ] Integration tests pass
- [ ] BDD acceptance tests pass (if applicable)
- [ ] Documentation tests pass
- [ ] `cargo make pre-commit` passes
- [ ] No Andon signals

### Quality Gates

**Tests are quality gates, not checkboxes**:

```bash
# All gates must pass
cargo make timeout-check        # Gate 0: Timeout available
cargo make check                # Gate 1: Compiles
cargo make lint                 # Gate 2: No warnings
cargo make test                 # Gate 3: All tests pass
cargo make slo-check            # Gate 4: Performance SLOs met
cargo make enforce-coverage     # Gate 5: Coverage ≥80%
```

**If any gate fails → 🔴 STOP THE LINE**

### Andon Signals for Tests

**Test-related Andon signals**:

| Signal | Pattern | Action |
|--------|---------|--------|
| 🔴 **CRITICAL** | `test ... FAILED` | HALT - Fix immediately |
| 🔴 **CRITICAL** | Coverage <80% | HALT - Add tests |
| 🟡 **HIGH** | Mutation score <60% | STOP - Improve tests |
| 🟡 **HIGH** | Flaky test (non-deterministic) | STOP - Fix or remove |
| 🟢 **MEDIUM** | Missing error path test | INVESTIGATE |

**Response to Andon signals**:
1. Stop new work
2. Fix root cause (not symptom)
3. Add test to prevent recurrence
4. Document in postmortem

---

## Test Examples and Patterns

### Example 1: Chicago TDD Unit Test

```rust
use chicago_tdd_tools::prelude::*;

test!(test_rdf_parser_parses_valid_turtle, {
    // ========================================
    // ARRANGE: Set up test fixtures
    // ========================================
    let ttl_content = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Person rdf:type owl:Class ;
            rdfs:label "Person" ;
            rdfs:comment "Represents a person" .
    "#;

    // Use REAL parser (Chicago TDD: no mocks)
    let parser = RdfParser::new();

    // ========================================
    // ACT: Execute behavior under test
    // ========================================
    let graph = parser.parse(ttl_content)?;

    // ========================================
    // ASSERT: Verify observable state
    // ========================================
    assert_eq!(graph.triple_count(), 3);

    assert!(graph.contains_subject("http://example.org/Person"));

    assert!(graph.contains_triple(
        "http://example.org/Person",
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "http://www.w3.org/2002/07/owl#Class"
    ));

    let label = graph.get_literal(
        "http://example.org/Person",
        "http://www.w3.org/2000/01/rdf-schema#label"
    )?;

    assert_eq!(label, "Person");
});
```

### Example 2: Integration Test with Real Transport

```rust
use tempfile::TempDir;

async_test!(test_full_generation_pipeline_with_real_filesystem, {
    // ========================================
    // ARRANGE
    // ========================================
    // Real temporary directory (not mocked filesystem)
    let temp_dir = TempDir::new()?;
    let spec_path = temp_dir.path().join("spec.ttl");
    let output_dir = temp_dir.path().join("output");

    // Write real spec file
    std::fs::write(&spec_path, r#"
        @prefix ggen: <http://ggen.tech/ns#> .
        @prefix ex: <http://example.org/> .

        ex:MyApp a ggen:Application ;
            ggen:targetLanguage ggen:Rust ;
            ggen:outputPath "src/generated" .
    "#)?;

    // Real code generator
    let generator = CodeGenerator::new();

    // ========================================
    // ACT
    // ========================================
    let result = generator
        .with_spec(&spec_path)
        .with_output(&output_dir)
        .generate()
        .await?;

    // ========================================
    // ASSERT
    // ========================================
    // Verify real files were created
    assert!(output_dir.join("src/generated").is_dir());
    assert!(output_dir.join("src/generated/lib.rs").is_file());
    assert!(output_dir.join("Cargo.toml").is_file());

    // Verify real file contents
    let lib_content = std::fs::read_to_string(
        output_dir.join("src/generated/lib.rs")
    )?;
    assert!(lib_content.contains("pub mod"));

    // Verify generated code compiles (REAL compiler check)
    let compile_result = std::process::Command::new("cargo")
        .arg("check")
        .current_dir(&output_dir)
        .output()?;

    assert!(
        compile_result.status.success(),
        "Generated code failed to compile:\n{}",
        String::from_utf8_lossy(&compile_result.stderr)
    );
});
```

### Example 3: Deterministic Pipeline Test

```rust
test!(test_pipeline_produces_identical_output_with_fixed_seed, {
    // ========================================
    // ARRANGE
    // ========================================
    std::env::set_var("RNG_SEED", "42");

    let spec = "tests/fixtures/determinism-test.ttl";
    let temp_dir1 = TempDir::new()?;
    let temp_dir2 = TempDir::new()?;

    let pipeline = Pipeline::new();

    // ========================================
    // ACT
    // ========================================
    // Run 1
    let receipt1 = pipeline
        .execute(spec, temp_dir1.path())
        .await?;

    // Run 2 (fresh environment)
    let receipt2 = pipeline
        .execute(spec, temp_dir2.path())
        .await?;

    // ========================================
    // ASSERT
    // ========================================
    // Cryptographic receipts must match
    assert_eq!(receipt1.hash, receipt2.hash);

    // All stage hashes must match
    assert_eq!(receipt1.mu1_hash, receipt2.mu1_hash);
    assert_eq!(receipt1.mu2_hash, receipt2.mu2_hash);
    assert_eq!(receipt1.mu3_hash, receipt2.mu3_hash);
    assert_eq!(receipt1.mu4_hash, receipt2.mu4_hash);
    assert_eq!(receipt1.mu5_hash, receipt2.mu5_hash);

    // Generated files must be bit-for-bit identical
    let files1 = collect_files(temp_dir1.path())?;
    let files2 = collect_files(temp_dir2.path())?;

    assert_eq!(files1.len(), files2.len());

    for (file1, file2) in files1.iter().zip(files2.iter()) {
        let content1 = std::fs::read(file1)?;
        let content2 = std::fs::read(file2)?;

        assert_eq!(
            content1, content2,
            "File contents differ: {:?} vs {:?}",
            file1, file2
        );
    }
});
```

### Example 4: Property-Based Test

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_parser_roundtrip_property(
        // Generate arbitrary valid RDF graphs
        graph in arb_rdf_graph()
    ) {
        // ========================================
        // ARRANGE
        // ========================================
        let serializer = RdfSerializer::new();
        let parser = RdfParser::new();

        // ========================================
        // ACT
        // ========================================
        // Serialize graph to Turtle
        let ttl = serializer.to_turtle(&graph)?;

        // Parse back to graph
        let parsed_graph = parser.parse(&ttl)?;

        // ========================================
        // ASSERT
        // ========================================
        // Property: Roundtrip preserves semantics
        prop_assert_eq!(
            graph.triple_count(),
            parsed_graph.triple_count()
        );

        prop_assert!(graphs_are_isomorphic(&graph, &parsed_graph));
    }
}

// Custom strategy for generating valid RDF graphs
fn arb_rdf_graph() -> impl Strategy<Value = Graph> {
    (
        prop::collection::vec(arb_triple(), 1..100)
    ).prop_map(|triples| {
        let mut graph = Graph::new();
        for triple in triples {
            graph.add_triple(triple);
        }
        graph
    })
}

fn arb_triple() -> impl Strategy<Value = Triple> {
    (
        arb_uri(),
        arb_uri(),
        prop::option::of(arb_literal()).prop_union(arb_uri().prop_map(Some))
    ).prop_map(|(s, p, o)| {
        Triple::new(s, p, o.unwrap())
    })
}
```

### Example 5: Rolling Update Test

```rust
#[test]
fn test_rolling_update_v1_to_v2_with_canary() {
    // ========================================
    // ARRANGE
    // ========================================
    // Deploy v1 to production
    let production = Environment::production();
    let canary = Environment::canary();

    let v1_deployment = Deployment::deploy(&production, "v1.0.0")?;

    // Generate artifacts with v1
    let v1_artifacts = generate_test_artifacts(&v1_deployment, count: 100)?;

    // ========================================
    // ACT: Rolling update
    // ========================================

    // Step 1: Deploy v2 to canary
    let v2_deployment = Deployment::deploy(&canary, "v2.0.0")?;

    // Step 2: Verify v2 works with v1 artifacts
    for artifact in v1_artifacts.sample(10) {
        let result = v2_deployment.process(artifact)?;
        assert!(result.is_valid());
    }

    // Step 3: Run canary workload
    let canary_metrics = run_canary_test(
        &v2_deployment,
        duration: Duration::from_secs(300),
        traffic_percent: 0.10
    )?;

    // ========================================
    // ASSERT: Canary verification
    // ========================================
    assert!(canary_metrics.success_rate >= 0.99);
    assert!(canary_metrics.error_rate <= 0.01);
    assert!(canary_metrics.p95_latency <= Duration::from_millis(100));

    // Step 4: Progressive rollout
    let rollout = ProgressiveRollout::new()
        .add_stage(traffic: 0.25, duration: Duration::from_secs(300))
        .add_stage(traffic: 0.50, duration: Duration::from_secs(300))
        .add_stage(traffic: 1.00, duration: Duration::from_secs(600))
        .with_rollback_gate(max_error_rate: 0.05);

    let rollout_result = rollout.execute(&production, "v2.0.0")?;

    // ========================================
    // ASSERT: Rollout success
    // ========================================
    assert!(rollout_result.completed_successfully);
    assert_eq!(production.current_version(), "v2.0.0");
    assert!(!rollout_result.rollback_triggered);
}
```

---

## Anti-Patterns to Avoid

### Anti-Pattern 1: Mock Explosion

**Problem**: Overuse of mocks leads to tests that don't test reality.

```rust
// WRONG: Mock explosion
#[test]
fn bad_test_with_too_many_mocks() {
    let mock_parser = MockParser::new();
    let mock_validator = MockValidator::new();
    let mock_query_engine = MockQueryEngine::new();
    let mock_generator = MockGenerator::new();
    let mock_hasher = MockHasher::new();

    // Configure 50 lines of mock expectations...

    // Test passes, but tests NOTHING real
}

// RIGHT: Use real components
test!(good_test_with_real_components, {
    // Arrange: Real components
    let parser = RdfParser::new();
    let validator = ShaclValidator::new();
    let query_engine = SparqlEngine::new();
    let generator = CodeGenerator::new();

    // Act: Real behavior
    let graph = parser.parse(ttl)?;
    let validated = validator.validate(&graph)?;
    let results = query_engine.query(&validated, sparql)?;
    let code = generator.generate(&results)?;

    // Assert: Real outcomes
    assert!(code.compiles());
});
```

### Anti-Pattern 2: Test Brittleness

**Problem**: Tests break on internal refactoring (over-specification).

```rust
// WRONG: Over-specified test
#[test]
fn brittle_test() {
    let mock = MockService::new();

    // Over-specified: Exact call order matters
    mock.expect_method1().times(1);
    mock.expect_method2().times(1);
    mock.expect_method3().times(1);

    system.process();

    // Refactor to change call order → test breaks
    // Even though observable behavior unchanged
}

// RIGHT: State-based verification
test!(robust_test, {
    // Arrange
    let system = System::new();

    // Act
    let result = system.process(input)?;

    // Assert: Observable state only
    assert_eq!(result.status, Status::Success);
    assert_eq!(result.output, expected_output);

    // Refactor internal implementation → test still passes
});
```

### Anti-Pattern 3: Non-Deterministic Tests

**Problem**: Flaky tests reduce confidence, waste time debugging.

```rust
// WRONG: Non-deterministic test
#[test]
fn flaky_test() {
    let timestamp = SystemTime::now(); // Changes every run!
    let random_value = rand::random::<u64>(); // Different every time!

    let result = process(timestamp, random_value);

    // Sometimes passes, sometimes fails
    assert_eq!(result.hash, "sha256:...");
}

// RIGHT: Deterministic test
test!(deterministic_test, {
    // Arrange: Fixed inputs
    std::env::set_var("RNG_SEED", "42");
    let timestamp = Timestamp::fixed(0);

    // Act: Deterministic execution
    let result = process(timestamp)?;

    // Assert: Reproducible outcome
    assert_eq!(result.hash, "sha256:known_hash");
});
```

---

## Advanced Testing Topics

### Canary Analysis

**Automated canary analysis** detects regressions:

```rust
#[test]
fn test_canary_automated_analysis() {
    // Arrange: Baseline metrics
    let baseline = Metrics {
        success_rate: 0.999,
        p50_latency: Duration::from_millis(10),
        p95_latency: Duration::from_millis(50),
        p99_latency: Duration::from_millis(100),
        error_rate: 0.001,
    };

    // Act: Canary deployment
    let canary_metrics = run_canary_deployment("v2.0.0")?;

    // Assert: Automated analysis
    let analysis = CanaryAnalysis::compare(&baseline, &canary_metrics);

    assert!(analysis.success_rate_delta() >= -0.001); // Max 0.1% decrease
    assert!(analysis.p95_latency_delta() <= 0.10);    // Max 10% increase
    assert!(analysis.error_rate_delta() <= 0.0);      // No increase

    // Assert: No anomalies detected
    assert!(analysis.anomalies().is_empty());
}
```

### Chaos Engineering for Code Generation

**Inject faults to verify resilience**:

```rust
#[test]
fn test_generator_handles_partial_filesystem_failure() {
    // Arrange: Chaos injection
    let chaos_fs = ChaosFilesystem::new()
        .inject_failure_rate(0.10) // 10% of writes fail
        .inject_latency(Duration::from_millis(100)); // 100ms delay

    let generator = CodeGenerator::with_filesystem(chaos_fs);

    // Act: Generate with chaos
    let result = generator.generate("tests/fixtures/spec.ttl");

    // Assert: Either succeeds or fails gracefully
    match result {
        Ok(output) => {
            // Success: All files written despite failures
            assert!(output.is_complete());
        }
        Err(e) => {
            // Graceful failure: Clear error, no partial state
            assert!(matches!(e, Error::FilesystemFailure(_)));
            assert!(!has_partial_output());
        }
    }
}
```

### Performance Testing and SLO Validation

**Verify performance SLOs**:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_pipeline_performance(c: &mut Criterion) {
    c.bench_function("pipeline_end_to_end", |b| {
        let spec = load_test_spec();

        b.iter(|| {
            let receipt = ggen::sync()
                .with_spec(black_box(&spec))
                .execute()
                .unwrap();

            black_box(receipt);
        });
    });
}

#[test]
fn test_pipeline_meets_slo() {
    // SLO: Pipeline completes in <5s for 1000+ triples
    let spec = generate_large_spec(triple_count: 1000);

    let start = std::time::Instant::now();
    let receipt = ggen::sync()
        .with_spec(&spec)
        .execute()?;
    let duration = start.elapsed();

    assert!(
        duration < Duration::from_secs(5),
        "Pipeline took {}s, exceeded 5s SLO",
        duration.as_secs_f64()
    );
}

criterion_group!(benches, bench_pipeline_performance);
criterion_main!(benches);
```

---

## Conclusion: Manufacturing-Grade Testing

### The Vision

**Chicago TDD enables manufacturing-grade quality for code generation:**

- **Real behavior tested**: No mocks, no illusions
- **Determinism verified**: Same input → same output, always
- **Artifacts survive upgrades**: Forward compatibility guaranteed
- **Rolling updates safe**: Canaries catch regressions
- **Coverage comprehensive**: 80%+ lines, 60%+ mutations
- **Confidence high**: Tests prove correctness, not just check for bugs

**The Formula**:
```
Manufacturing_Quality = Real_Tests × Determinism × Coverage × Invariants
```

### Getting Started

**Immediate actions**:

1. **Adopt Chicago TDD**:
   ```bash
   # Use real collaborators
   # Follow AAA pattern
   # Verify observable state
   ```

2. **Run test suite**:
   ```bash
   cargo make test              # All tests must pass
   cargo make test-mutation     # Mutation score ≥60%
   cargo make detect-gaps       # No critical gaps
   ```

3. **Add determinism tests**:
   ```bash
   cargo make deterministic     # RNG_SEED=42
   ```

4. **Verify coverage**:
   ```bash
   cargo make enforce-coverage  # ≥80% required
   ```

5. **Test upgrades**:
   ```bash
   # Test v1 artifacts work with v2 system
   ```

**Resources**:
- Chicago TDD examples: `crates/*/tests/chicago_tdd_*.rs`
- Testing commands: `Makefile.toml`
- Coverage reports: `cargo llvm-cov --html --open`
- BDD features: `features/*.feature`

---

## Further Reading

**Testing Philosophy**:
- [Chicago TDD Rules](/.claude/rules/rust/testing.md) - Project testing requirements
- [Elite Mindset](/.claude/rules/rust/elite-mindset.md) - Type-first, zero-cost patterns
- [Andon Signals](/.claude/rules/andon/signals.md) - Stop-the-line protocol

**Code Examples**:
- `crates/ggen-cli/tests/chicago_tdd_smoke_test.rs` - Basic Chicago TDD
- `crates/ggen-core/tests/determinism_framework.rs` - Determinism testing
- `crates/ggen-core/tests/swarm_consensus_tests.rs` - Integration testing

**External Resources**:
- Kent Beck: "Test-Driven Development by Example"
- Martin Fowler: "Mocks Aren't Stubs"
- Gerard Meszaros: "xUnit Test Patterns"

**Academic Foundations**:
- NASA: "The Power of 10: Rules for Developing Safety-Critical Code"
- IEEE: "Standard for Software Test Documentation"

---

**Document Version**: 1.0
**Created**: 2026-02-09
**Status**: Comprehensive Testing Doctrine
**Audience**: All ggen contributors

**Feedback**: GitHub Issues with label `testing-doctrine`

---

*"Tests are not checkboxes. Tests are proofs. Chicago TDD provides the rigor to make code generation a manufacturing discipline."* — ggen Testing Philosophy
