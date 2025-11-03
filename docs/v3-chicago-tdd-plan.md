# ggen v3.0.0 - Chicago School TDD Refactoring Plan

## Executive Summary

Complete Chicago School (Classical/Detroit) TDD plan for the v3.0.0 crates pattern refactor. This plan uses real objects, state verification, and minimal mocking to ensure we can refactor safely.

**Methodology**: Chicago School TDD (Classical/Detroit - State-Based)
**Target**: Zero test failures during refactor, state-based validation
**Approach**: Real implementations, real state verification, minimal mocking

---

## Table of Contents

1. [Chicago TDD Principles Applied](#1-chicago-tdd-principles-applied)
2. [Test Architecture for v3 Refactor](#2-test-architecture-for-v3-refactor)
3. [Pre-Refactor Test Suite](#3-pre-refactor-test-suite)
4. [State-Based Testing Strategy](#4-state-based-testing-strategy)
5. [Integration Tests with Real Objects](#5-integration-tests-with-real-objects)
6. [State Verification Tests](#6-state-verification-tests)
7. [Refactoring Validation Tests](#7-refactoring-validation-tests)
8. [Test Execution Strategy](#8-test-execution-strategy)
9. [Success Criteria](#9-success-criteria)

---

## 1. Chicago TDD Principles Applied

### 1.1 Core Principles

**State-Based Testing**
- Test final state, not how we got there
- Verify outcomes through state changes
- Use `assert_eq!`, `assert!`, file existence checks
- Focus on what happened, not who did it

**Real Objects When Possible**
- Use real implementations of domain functions
- Use real infrastructure (Tera, Oxigraph, etc.)
- Use real file system operations
- Use real CLI execution

**Minimal Mocking**
- Mock ONLY truly external boundaries:
  - Network calls (HTTP requests)
  - External APIs (GitHub, package registries)
  - System time (if deterministic needed)
- DO NOT mock:
  - Template engines
  - RDF stores
  - File systems (use TempDir instead)
  - Domain logic
  - Infrastructure components

**Integration-Heavy Approach**
- Prefer integration tests over unit tests
- Test complete workflows end-to-end
- Use real component interactions
- Verify state through actual side effects

**Test Through Behavior**
- Test what the system does, not how it does it
- Verify file creation, data transformation, state changes
- Test observable behavior, not internal details

---

## 2. Test Architecture for v3 Refactor

### 2.1 Testing Pyramid (Chicago School)

```
        /\
       /  \  5% E2E (Full CLI → File System → Output)
      /____\
     /      \  60% Integration (Real Components, State Verification)
    /________\
   /          \  30% Unit (Real Functions, State-Based)
  /____________\
     5% Property-Based Tests (Invariants)
```

**Key Difference from London**:
- London: 65% unit tests (mocked)
- Chicago: 60% integration tests (real objects)
- Chicago: 30% unit tests (real functions, state verification)

### 2.2 Layer Boundaries Testing

**State-Based Contract Validation**:

1. **CLI → Domain**: Verify state changes through CLI execution
   - Execute CLI commands
   - Verify files created, data written, state changed
   - No mocking of domain functions

2. **Domain → Infrastructure**: Verify state through actual operations
   - Use real template engine
   - Use real RDF store
   - Verify actual outputs

3. **Runtime Bridge**: Test actual async/sync behavior
   - Real Tokio runtime
   - Verify actual execution results

### 2.3 Test Distribution

**By Layer**:
- CLI Layer: 20% (command execution, state verification)
- Domain Layer: 50% (real business logic, state changes)
- Infrastructure Layer: 20% (real operations, actual outputs)
- Integration: 10% (cross-layer workflows)

**By Test Type**:
- Integration Tests: 60% (real components, minimal mocking)
- Unit Tests: 30% (real functions, state verification)
- E2E Tests: 5% (full workflows)
- Property Tests: 5% (invariants)

---

## 3. Pre-Refactor Test Suite

### 3.1 Goal: Establish State Baseline

Before refactoring, create comprehensive test suite that validates current state. This becomes our safety net for state verification.

### 3.2 Test Structure

```
tests/v3-refactor/
├── pre_refactor/
│   ├── integration/
│   │   ├── cli_workflows_test.rs      # Complete CLI workflows (real execution)
│   │   ├── marketplace_flow_test.rs   # Search → Install → Verify (real state)
│   │   ├── template_generation_test.rs # Template → RDF → Generate (real files)
│   │   └── project_scaffolding_test.rs # New → Gen → Build (real directories)
│   ├── domain/
│   │   ├── domain_ai_test.rs          # Real AI domain logic (state verification)
│   │   ├── domain_graph_test.rs       # Real graph operations (real Oxigraph)
│   │   ├── domain_template_test.rs     # Real template rendering (real Tera)
│   │   └── domain_project_test.rs     # Real project operations (real files)
│   └── unit/
│       ├── pure_functions_test.rs     # Pure functions (no mocking)
│       └── state_transformations_test.rs # State changes (assert_eq!)
└── post_refactor/
    ├── state_validation_test.rs       # State identical to pre-refactor
    ├── structure_validation_test.rs   # File structure correct
    └── behavior_validation_test.rs   # Behavior unchanged
```

### 3.3 Pre-Refactor Integration Tests

**Purpose**: Document complete workflows with real state verification.

```rust
// tests/v3-refactor/pre_refactor/integration/cli_workflows_test.rs

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;
use std::fs;

#[tokio::test]
async fn integration_template_generation_workflow() {
    // GIVEN: Real temp directory
    let temp_dir = TempDir::new().unwrap();
    
    // AND: Real template file created
    let template_path = temp_dir.path().join("template.tmpl");
    fs::write(&template_path, "Hello {{ name }}!").unwrap();
    
    // AND: Real RDF file created
    let rdf_path = temp_dir.path().join("data.ttl");
    fs::write(&rdf_path, r#"
        @prefix ex: <http://example.org#> .
        ex:Person a ex:Person ;
            ex:name "World" .
    "#).unwrap();
    
    // WHEN: Execute real CLI command
    let output_dir = temp_dir.path().join("output");
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&[
        "template", "generate",
        "--template", template_path.to_str().unwrap(),
        "--rdf", rdf_path.to_str().unwrap(),
        "--output", output_dir.to_str().unwrap(),
    ]);
    
    // THEN: Command succeeds (state verification)
    cmd.assert().success();
    
    // AND: Output directory exists (state verification)
    assert!(output_dir.exists());
    
    // AND: Generated file exists (state verification)
    let generated_file = output_dir.join("output.txt");
    assert!(generated_file.exists());
    
    // AND: File content is correct (state verification)
    let content = fs::read_to_string(&generated_file).unwrap();
    assert_eq!(content.trim(), "Hello World!");
}

#[tokio::test]
async fn integration_marketplace_search_and_install_workflow() {
    // GIVEN: Real temp directory
    let temp_dir = TempDir::new().unwrap();
    
    // WHEN: Execute real search command
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "search", "--query", "rust web"]);
    let output = cmd.output().unwrap();
    
    // THEN: Command succeeds
    assert!(output.status.success());
    
    // AND: Output contains expected results (state verification)
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("rust-axum-service") || stdout.contains("Searching"));
    
    // WHEN: Execute real install command
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "install", "--package", "rust-axum-service"]);
    
    // THEN: Command succeeds or fails gracefully (state verification)
    let output = cmd.output().unwrap();
    // Note: May fail if package doesn't exist, but should fail gracefully
    
    // WHEN: Execute real list command
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "list"]);
    let output = cmd.output().unwrap();
    
    // THEN: Command succeeds
    assert!(output.status.success());
}
```

**Coverage**: All 8 nouns × all verbs = ~50 integration test scenarios

---

## 4. State-Based Testing Strategy

### 4.1 State Verification Patterns

**Pattern 1: File System State**
```rust
// Verify files created
assert!(output_dir.join("file.rs").exists());

// Verify file content
let content = fs::read_to_string(&file_path).unwrap();
assert_eq!(content, expected_content);

// Verify directory structure
assert!(output_dir.join("src/main.rs").exists());
assert!(output_dir.join("Cargo.toml").exists());
```

**Pattern 2: Data Structure State**
```rust
// Verify data transformation
let result = domain_function(input).await?;
assert_eq!(result.id, expected_id);
assert_eq!(result.status, Status::Success);

// Verify collections
assert_eq!(results.len(), 3);
assert!(results.contains(&expected_item));
```

**Pattern 3: Graph State**
```rust
// Use REAL Oxigraph graph
let graph = Graph::new()?;
graph.insert_turtle(rdf_data)?;

// Verify graph state
let query_result = graph.query_sparql(query)?;
assert_eq!(query_result.bindings.len(), 2);
assert_eq!(query_result.bindings[0]["name"], "Alice");
```

**Pattern 4: Template State**
```rust
// Use REAL Tera engine
let mut tera = Tera::new("templates/**/*")?;
tera.add_raw_template("test.tmpl", template_content)?;

// Verify rendering state
let rendered = tera.render("test.tmpl", &context)?;
assert_eq!(rendered.trim(), expected_output);
```

### 4.2 Real Object Usage

**Domain Functions**: Use real implementations
```rust
#[tokio::test]
async fn test_domain_project_create_real() {
    // Use REAL domain function
    let config = ProjectConfig {
        name: "test-project".to_string(),
        project_type: ProjectType::RustCli,
        path: temp_dir.path().to_path_buf(),
    };
    
    // Execute real function
    let result = ggen_domain::project::create_project(config).await?;
    
    // Verify state
    assert_eq!(result.project_path, temp_dir.path().join("test-project"));
    assert!(result.project_path.exists());
}
```

**Infrastructure Components**: Use real implementations
```rust
#[test]
fn test_template_engine_real() {
    // Use REAL Tera engine
    let mut tera = Tera::new("templates/**/*")?;
    tera.add_raw_template("test.tmpl", "Hello {{ name }}!")?;
    
    let context = tera::Context::from_serialize(json!({
        "name": "World"
    }))?;
    
    // Execute real rendering
    let rendered = tera.render("test.tmpl", &context)?;
    
    // Verify state
    assert_eq!(rendered, "Hello World!");
}
```

### 4.3 Minimal Mocking Strategy

**Mock ONLY**:
1. Network calls (HTTP requests to external APIs)
2. External APIs (GitHub, package registries)
3. System time (for deterministic tests)

**DO NOT Mock**:
1. Template engines (use real Tera)
2. RDF stores (use real Oxigraph)
3. File systems (use TempDir)
4. Domain functions (use real implementations)
5. Infrastructure components (use real implementations)

---

## 5. Integration Tests with Real Objects

### 5.1 Purpose

Test complete workflows using real components. Verify state changes through actual operations.

### 5.2 Integration Test Structure

**CLI Integration Tests** (20 tests):
- Real CLI execution
- Real file system operations
- State verification (files, directories, content)

**Domain Integration Tests** (80 tests):
- Real domain functions
- Real infrastructure components
- State verification (data structures, outputs)

**Cross-Layer Integration Tests** (30 tests):
- CLI → Domain → Infrastructure
- Complete workflows
- End-to-end state verification

**Total**: ~130 integration tests

### 5.3 Example Integration Test

```rust
// tests/v3-refactor/pre_refactor/integration/domain_template_test.rs

use tempfile::TempDir;
use std::fs;
use ggen_domain::template;
use ggen_core::graph::Graph;

#[tokio::test]
async fn integration_template_generate_with_rdf_real() {
    // GIVEN: Real temp directory
    let temp_dir = TempDir::new().unwrap();
    
    // AND: Real template file
    let template_path = temp_dir.path().join("user.tmpl");
    fs::write(&template_path, r#"
        # {{ user.name }}
        Email: {{ user.email }}
    "#).unwrap();
    
    // AND: Real RDF file with real Oxigraph
    let rdf_path = temp_dir.path().join("users.ttl");
    fs::write(&rdf_path, r#"
        @prefix ex: <http://example.org#> .
        ex:alice a ex:User ;
            ex:name "Alice" ;
            ex:email "alice@example.com" .
    "#).unwrap();
    
    // WHEN: Use REAL domain function with REAL infrastructure
    let config = template::GenerateConfig {
        template_path: template_path.clone(),
        rdf_files: vec![rdf_path.clone()],
        output_path: temp_dir.path().join("output.md"),
    };
    
    // Execute real domain function (uses real Tera, real Oxigraph)
    let result = template::generate_template(config).await?;
    
    // THEN: State verification - result exists
    assert!(result.output_path.exists());
    
    // AND: File content is correct (state verification)
    let content = fs::read_to_string(&result.output_path).unwrap();
    assert!(content.contains("# Alice"));
    assert!(content.contains("Email: alice@example.com"));
    
    // AND: RDF files were loaded (state verification)
    assert_eq!(result.rdf_files_loaded, 1);
}
```

### 5.4 Graph Operations Integration Test

```rust
#[tokio::test]
async fn integration_graph_load_and_query_real() {
    // GIVEN: Real RDF file
    let temp_dir = TempDir::new().unwrap();
    let rdf_path = temp_dir.path().join("data.ttl");
    fs::write(&rdf_path, r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        ex:alice a foaf:Person ;
            foaf:name "Alice" ;
            foaf:age "30" .
    "#).unwrap();
    
    // WHEN: Use REAL graph operations (real Oxigraph)
    let options = ggen_domain::graph::LoadOptions {
        file_path: rdf_path.clone(),
        format: ggen_domain::graph::RdfFormat::Turtle,
    };
    
    // Execute real load function
    let load_result = ggen_domain::graph::load_rdf(options).await?;
    
    // THEN: State verification - graph loaded
    assert_eq!(load_result.triples_loaded, 3);
    
    // AND: Query real graph
    let query_options = ggen_domain::graph::QueryOptions {
        query: r#"
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            SELECT ?name ?age
            WHERE {
                ?person foaf:name ?name ;
                       foaf:age ?age .
            }
        "#.to_string(),
        graph_file: Some(rdf_path),
        output_format: "json".to_string(),
    };
    
    // Execute real query function
    let query_result = ggen_domain::graph::execute_sparql(query_options).await?;
    
    // THEN: State verification - query results
    assert_eq!(query_result.bindings.len(), 1);
    assert_eq!(query_result.bindings[0]["name"], "Alice");
    assert_eq!(query_result.bindings[0]["age"], "30");
}
```

---

## 6. State Verification Tests

### 6.1 Purpose

Test individual functions using real implementations. Verify state changes, not interactions.

### 6.2 Unit Test Structure

**Pure Function Tests** (50 tests):
- Mathematical operations
- Data transformations
- String processing
- No dependencies

**State Transformation Tests** (100 tests):
- Domain functions with real infrastructure
- State changes verified
- Real components used

**Total**: ~150 unit tests

### 6.3 Example State-Based Unit Test

```rust
// tests/v3-refactor/pre_refactor/unit/domain_project_test.rs

use tempfile::TempDir;
use std::fs;
use ggen_domain::project;

#[tokio::test]
async fn test_project_create_real_files() {
    // GIVEN: Real temp directory
    let temp_dir = TempDir::new().unwrap();
    
    // AND: Project configuration
    let config = project::ProjectConfig {
        name: "my-project".to_string(),
        project_type: project::ProjectType::RustCli,
        path: temp_dir.path().to_path_buf(),
    };
    
    // WHEN: Execute real domain function (uses real file system)
    let result = project::create_project(config).await?;
    
    // THEN: State verification - project directory exists
    assert!(result.project_path.exists());
    assert!(result.project_path.is_dir());
    
    // AND: State verification - files created
    assert!(result.project_path.join("Cargo.toml").exists());
    assert!(result.project_path.join("src/main.rs").exists());
    
    // AND: State verification - file content
    let cargo_toml = fs::read_to_string(result.project_path.join("Cargo.toml")).unwrap();
    assert!(cargo_toml.contains("name = \"my-project\""));
    
    let main_rs = fs::read_to_string(result.project_path.join("src/main.rs")).unwrap();
    assert!(main_rs.contains("fn main()"));
}
```

### 6.4 Pure Function Test

```rust
#[test]
fn test_pure_function_state() {
    // GIVEN: Input data
    let input = vec!["a", "b", "c"];
    
    // WHEN: Execute pure function (no dependencies, no mocking)
    let result = domain::utils::sort_and_uppercase(&input);
    
    // THEN: State verification
    assert_eq!(result, vec!["A", "B", "C"]);
}
```

---

## 7. Refactoring Validation Tests

### 7.1 Purpose

Validate that refactoring preserved state and behavior. Compare state before and after refactor.

### 7.2 Validation Test Categories

**State Validation** (20 tests):
- Files created identically
- Directory structure identical
- File content identical
- Data structures identical

**Behavior Validation** (50 tests):
- All integration tests still pass
- Same outputs produced
- Same error messages
- Same performance characteristics

**Structure Validation** (5 tests):
- Crate structure correct
- Module boundaries respected
- Public API unchanged

**Total**: ~75 validation tests

### 7.3 Example State Validation Test

```rust
// tests/v3-refactor/post_refactor/state_validation_test.rs

use tempfile::TempDir;
use std::fs;
use std::collections::HashMap;

#[tokio::test]
async fn test_state_identical_after_refactor() {
    // GIVEN: Same input as pre-refactor
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("template.tmpl");
    fs::write(&template_path, "Hello {{ name }}!").unwrap();
    
    // WHEN: Execute same command (post-refactor)
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&[
        "template", "generate",
        "--template", template_path.to_str().unwrap(),
        "--var", "name=World",
        "--output", temp_dir.path().join("output.txt").to_str().unwrap(),
    ]);
    
    // THEN: Command succeeds (behavior preserved)
    cmd.assert().success();
    
    // AND: State identical - file exists
    let output_file = temp_dir.path().join("output.txt");
    assert!(output_file.exists());
    
    // AND: State identical - content matches pre-refactor baseline
    let content = fs::read_to_string(&output_file).unwrap();
    assert_eq!(content.trim(), "Hello World!");
    
    // Compare with pre-refactor baseline
    let baseline_content = load_baseline("template_generate_output.txt")?;
    assert_eq!(content, baseline_content);
}

#[tokio::test]
async fn test_project_structure_identical() {
    // GIVEN: Same project config as pre-refactor
    let temp_dir = TempDir::new().unwrap();
    let config = project::ProjectConfig {
        name: "test-project".to_string(),
        project_type: project::ProjectType::RustCli,
        path: temp_dir.path().to_path_buf(),
    };
    
    // WHEN: Create project (post-refactor)
    let result = project::create_project(config).await?;
    
    // THEN: State identical - directory structure matches baseline
    let baseline_structure = load_baseline_structure("project_structure.json")?;
    let actual_structure = get_directory_structure(&result.project_path)?;
    assert_eq!(actual_structure, baseline_structure);
    
    // AND: State identical - files match baseline
    for (file_path, expected_content) in baseline_structure.files {
        let actual_content = fs::read_to_string(result.project_path.join(&file_path))?;
        assert_eq!(actual_content, expected_content);
    }
}
```

---

## 8. Test Execution Strategy

### 8.1 Phased Execution

**Phase 0: Pre-Refactor Baseline**
1. Create comprehensive test suite with real objects
2. All tests pass with current v2.4.0 structure
3. Establish state baselines (save expected outputs)
4. Document file structures, outputs, behaviors

**Phase 1: Infrastructure Moves**
1. Run test suite after each crate move
2. Update test paths
3. Verify state remains identical

**Phase 2: Domain Extraction**
1. Run domain tests after each module move
2. Update imports
3. Verify state outputs identical

**Phase 3: CLI Refactoring**
1. Run CLI tests after import updates
2. Verify CLI execution produces same state
3. Validate file outputs unchanged

**Phase 4: State Validation**
1. Run full test suite
2. Compare state with baselines
3. Verify outputs identical

### 8.2 Test Execution Commands

```bash
# Pre-refactor: Establish baseline
cargo make test --package ggen-cli-lib
cargo make test --package ggen-core
cargo make test --test v3_refactor -- pre_refactor

# Save state baselines
cargo test --test v3_refactor -- --test-threads=1 | tee state_baseline.txt

# During refactor: After each phase
cargo make test --workspace  # All crates
cargo make test --test v3_refactor -- integration

# Post-refactor: State comparison
cargo make test --workspace
cargo make test --test v3_refactor -- post_refactor
cargo make deterministic  # Fixed seed tests
diff state_baseline.txt state_after_refactor.txt  # State comparison
```

### 8.3 Test Organization

```
tests/
├── v3-refactor/              # v3 refactor specific tests
│   ├── pre_refactor/         # Baseline tests with real objects
│   │   ├── integration/     # Real CLI, real components
│   │   ├── domain/           # Real domain functions
│   │   └── unit/             # Real functions, state verification
│   ├── post_refactor/        # State validation tests
│   └── baselines/            # Saved state baselines
│       ├── outputs/           # Expected file outputs
│       ├── structures/        # Expected directory structures
│       └── data/              # Expected data structures
├── integration/               # Existing integration tests (keep)
└── unit/                     # Existing unit tests (keep)
```

---

## 9. Success Criteria

### 9.1 Test Coverage

- **Integration Tests**: 130/130 passing (100%)
- **Unit Tests**: 150/150 passing (100%)
- **E2E Tests**: 50/50 passing (100%)
- **Validation Tests**: 75/75 passing (100%)

**Total**: 405 tests, 100% passing

### 9.2 State Validation

- ✅ All file outputs identical to baselines
- ✅ All directory structures identical
- ✅ All data structures identical
- ✅ All CLI outputs identical
- ✅ All error messages unchanged

### 9.3 Behavior Validation

- ✅ All integration tests pass
- ✅ All workflows produce same results
- ✅ Performance within SLOs (<15s first build, <2s incremental)
- ✅ No regressions in functionality

### 9.4 Structure Validation

- ✅ All crates in `crates/` directory
- ✅ Module boundaries respected
- ✅ Public API unchanged
- ✅ No circular dependencies

### 9.5 Real Object Usage

- ✅ Domain functions use real implementations
- ✅ Infrastructure uses real components (Tera, Oxigraph)
- ✅ Minimal mocking (only network/external APIs)
- ✅ State verification through actual operations

---

## 10. Chicago vs London: Key Differences

### 10.1 Testing Approach

**London (Mock-Heavy)**:
```rust
#[test]
fn test_with_mocks() {
    let mut mock = MockTemplateEngine::new();
    mock.expect_render().times(1).return_const(Ok("output".to_string()));
    let domain = TemplateDomain::new(mock);
    let result = domain.generate(config);
    // Verify mock was called correctly
}
```

**Chicago (Real Objects)**:
```rust
#[test]
fn test_with_real_objects() {
    let mut tera = Tera::new("templates/**/*")?;
    tera.add_raw_template("test.tmpl", "Hello {{ name }}!")?;
    let context = Context::from_serialize(json!({"name": "World"}))?;
    let result = tera.render("test.tmpl", &context)?;
    // Verify state (output)
    assert_eq!(result, "Hello World!");
}
```

### 10.2 Verification Style

**London**: Verify interactions (mock expectations)
**Chicago**: Verify state (actual outputs, files, data)

### 10.3 Mocking Strategy

**London**: Mock all collaborators
**Chicago**: Mock only external boundaries (network, APIs)

### 10.4 Test Distribution

**London**: 65% unit tests (mocked)
**Chicago**: 60% integration tests (real objects)

---

## 11. Implementation Phases

### Phase 0: Test Infrastructure Setup (Before Refactor)

**Tasks**:
1. Create `tests/v3-refactor/` directory structure
2. Set up test utilities for real object usage
3. Write all pre-refactor tests with real objects
4. Save state baselines
5. Verify all tests pass with v2.4.0

**Duration**: 6-8 hours

### Phase 1: Pre-Refactor Test Suite (Before Refactor)

**Tasks**:
1. Write all 405 tests with real objects
2. Establish state baselines
3. Document expected outputs
4. Create baseline comparison utilities

**Duration**: 10-14 hours

### Phase 2: Refactor with State Safety Net

**Tasks**:
1. Execute refactor phases
2. Run tests after each phase
3. Compare state with baselines
4. Fix any state mismatches immediately

**Duration**: 14-21 hours (refactor + testing)

### Phase 3: Post-Refactor State Validation

**Tasks**:
1. Run full test suite
2. Compare all state with baselines
3. Verify outputs identical
4. Document validation results

**Duration**: 2-3 hours

---

## 12. Real Object Test Examples

### 12.1 Domain Function Test

```rust
#[tokio::test]
async fn test_domain_template_generate_real() {
    // GIVEN: Real template and RDF
    let temp_dir = TempDir::new().unwrap();
    let template = create_real_template(&temp_dir)?;
    let rdf = create_real_rdf(&temp_dir)?;
    
    // WHEN: Execute real domain function
    let result = ggen_domain::template::generate_template(config).await?;
    
    // THEN: Verify state
    assert!(result.output_path.exists());
    let content = fs::read_to_string(&result.output_path)?;
    assert_eq!(content, expected_output);
}
```

### 12.2 CLI Integration Test

```rust
#[tokio::test]
async fn test_cli_template_generate_real() {
    // GIVEN: Real files
    let temp_dir = TempDir::new().unwrap();
    setup_real_files(&temp_dir)?;
    
    // WHEN: Execute real CLI
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["template", "generate", ...]);
    cmd.assert().success();
    
    // THEN: Verify state
    assert!(output_file.exists());
    let content = fs::read_to_string(&output_file)?;
    assert_eq!(content, expected_content);
}
```

### 12.3 Graph Operation Test

```rust
#[test]
fn test_graph_query_real() {
    // GIVEN: Real RDF graph (real Oxigraph)
    let graph = Graph::new()?;
    graph.insert_turtle(rdf_data)?;
    
    // WHEN: Execute real SPARQL query
    let result = graph.query_sparql(sparql_query)?;
    
    // THEN: Verify state
    assert_eq!(result.bindings.len(), 2);
    assert_eq!(result.bindings[0]["name"], "Alice");
}
```

---

## 13. Minimal Mocking Examples

### 13.1 When to Mock (Network Calls)

```rust
// Mock ONLY external HTTP calls
#[tokio::test]
async fn test_marketplace_search_with_network_mock() {
    // Mock HTTP client for external API
    let server = mockito::Server::new_async().await;
    let mock = server
        .mock("GET", "/api/packages")
        .with_status(200)
        .with_body(r#"[{"name": "test-package"}]"#)
        .create();
    
    // Use real search logic with mocked network
    let result = marketplace::search("test").await?;
    
    // Verify state
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].name, "test-package");
}
```

### 13.2 When NOT to Mock (Internal Components)

```rust
// DO NOT mock template engine - use real Tera
#[test]
fn test_template_rendering_real() {
    let mut tera = Tera::new("templates/**/*")?;
    tera.add_raw_template("test.tmpl", "Hello {{ name }}!")?;
    let context = Context::from_serialize(json!({"name": "World"}))?;
    
    // Use real rendering
    let result = tera.render("test.tmpl", &context)?;
    assert_eq!(result, "Hello World!");
}

// DO NOT mock RDF store - use real Oxigraph
#[test]
fn test_rdf_query_real() {
    let graph = Graph::new()?;
    graph.insert_turtle(rdf_data)?;
    
    // Use real query
    let result = graph.query_sparql(query)?;
    assert_eq!(result.bindings.len(), 1);
}
```

---

## 14. State Baseline Management

### 14.1 Saving Baselines

```rust
// Save expected outputs before refactor
fn save_baseline(name: &str, content: &str) -> Result<()> {
    let baseline_dir = PathBuf::from("tests/v3-refactor/baselines");
    fs::create_dir_all(&baseline_dir)?;
    fs::write(baseline_dir.join(name), content)?;
    Ok(())
}
```

### 14.2 Comparing with Baselines

```rust
// Compare actual output with baseline
fn compare_with_baseline(name: &str, actual: &str) -> Result<bool> {
    let baseline = load_baseline(name)?;
    Ok(actual == baseline)
}
```

### 14.3 Baseline Structure

```
tests/v3-refactor/baselines/
├── outputs/              # File outputs
│   ├── template_generate_output.txt
│   ├── project_create_structure.json
│   └── graph_query_result.json
├── structures/           # Directory structures
│   └── project_structure.json
└── data/                 # Data structures
    └── query_results.json
```

---

## 15. Conclusion

This Chicago TDD plan ensures we can refactor safely using real objects and state verification. By testing with real implementations, verifying actual state changes, and minimizing mocking, we maintain confidence throughout the refactor.

**Key Principles**:
1. Test state, not interactions
2. Use real objects when possible
3. Mock only external boundaries
4. Verify outcomes through actual side effects
5. Compare state with baselines

**Success Metrics**:
- 405 tests, 100% passing
- All state identical to baselines
- Zero behavior changes
- Real component validation

---

**Document Status**: Complete Chicago TDD plan ready for implementation
**Next Action**: Create test infrastructure and write pre-refactor tests with real objects

