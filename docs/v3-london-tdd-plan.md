# ggen v3.0.0 - London School TDD Refactoring Plan

## Executive Summary

Complete London School (mockist, outside-in) TDD plan for the v3.0.0 crates pattern refactor. This plan ensures we can refactor safely with comprehensive test coverage, validating contracts between layers.

**Methodology**: London School TDD (Mockist, Outside-In)
**Target**: Zero test failures during refactor, complete contract validation
**Framework**: mockall for trait mocking

---

## Table of Contents

1. [London TDD Principles Applied](#1-london-tdd-principles-applied)
2. [Test Architecture for v3 Refactor](#2-test-architecture-for-v3-refactor)
3. [Pre-Refactor Test Suite](#3-pre-refactor-test-suite)
4. [Layer Contract Tests](#4-layer-contract-tests)
5. [Outside-In Acceptance Tests](#5-outside-in-acceptance-tests)
6. [Component Integration Tests](#6-component-integration-tests)
7. [Unit Tests with Mocks](#7-unit-tests-with-mocks)
8. [Refactoring Validation Tests](#8-refactoring-validation-tests)
9. [Test Execution Strategy](#9-test-execution-strategy)
10. [Success Criteria](#10-success-criteria)

---

## 1. London TDD Principles Applied

### 1.1 Core Principles

**Outside-In Development**
- Start with acceptance tests (complete user workflows)
- Work inward to component tests (layer interactions)
- End with unit tests (isolated behavior)
- Drive design through test expectations

**Mock-First Approach**
- Define collaborator interfaces through mocks
- Test object conversations, not implementations
- Focus on behavior verification
- Mock all external dependencies (filesystem, network, infrastructure)

**Sociable vs Solitary Testing**
- **Solitary (Unit)**: Mock ALL collaborators (CLI → Domain → Infrastructure)
- **Sociable (Component)**: Mock boundaries only (filesystem, network, external APIs)
- **Guideline**: Mock I/O, test real business logic

**Contract Testing**
- Test contracts between layers (CLI ↔ Domain, Domain ↔ Infrastructure)
- Verify interfaces remain stable during refactor
- Ensure no circular dependencies introduced
- Validate dependency directions

**Listening to Tests**
- Let test pain drive refactoring decisions
- Hard-to-test code = poorly designed code
- Mock explosion = too many dependencies
- Test pain reveals architectural issues

---

## 2. Test Architecture for v3 Refactor

### 2.1 Testing Pyramid

```
        /\
       /  \  5% E2E (Acceptance Tests - Full CLI workflows)
      /____\
     /      \  25% Integration (Component Tests - Layer interactions)
    /________\
   /          \  65% Unit Tests (Solitary - Mocked collaborators)
  /____________\
     5% Contract Tests (Layer boundary validation)
```

### 2.2 Layer Boundaries to Test

**Critical Contracts**:
1. **CLI → Domain**: `crates/ggen-cli/src/cmds/*` → `crates/ggen-domain/src/*`
   - CLI commands call domain functions
   - Domain has zero CLI dependencies
   - Arguments transformed correctly

2. **Domain → Infrastructure**: `crates/ggen-domain/src/*` → `crates/ggen-core/*`, `crates/ggen-ai/*`, `crates/ggen-marketplace/*`
   - Domain uses infrastructure via traits
   - Infrastructure doesn't depend on domain
   - Contracts defined via trait abstractions

3. **Runtime Bridge**: `crates/ggen-cli/src/runtime.rs`
   - Async/sync boundary
   - No domain leakage into CLI

### 2.3 Test Distribution

**By Layer**:
- CLI Layer: 15% (command parsing, routing, async bridge)
- Domain Layer: 65% (business logic, highest complexity)
- Infrastructure Layer: 10% (template engine, RDF, etc.)
- Contract Tests: 10% (layer boundaries)

**By Test Type**:
- Unit Tests: 65% (mocked collaborators)
- Component Tests: 25% (selective mocking)
- Acceptance Tests: 5% (minimal mocking)
- Contract Tests: 5% (interface validation)

---

## 3. Pre-Refactor Test Suite

### 3.1 Goal: Establish Baseline

Before refactoring, create comprehensive test suite that validates current behavior. This becomes our safety net.

### 3.2 Test Structure

```
tests/v3-refactor/
├── pre_refactor/
│   ├── acceptance/
│   │   ├── cli_workflows_test.rs      # Complete CLI workflows
│   │   ├── marketplace_flow_test.rs   # Search → Install → Use
│   │   ├── template_generation_test.rs # Template → RDF → Generate
│   │   └── project_scaffolding_test.rs # New → Gen → Build
│   ├── component/
│   │   ├── cli_to_domain_test.rs      # CLI → Domain contracts
│   │   ├── domain_to_infrastructure_test.rs # Domain → Core/AI contracts
│   │   └── runtime_bridge_test.rs      # Async/sync bridge
│   └── unit/
│       ├── domain_ai_test.rs          # AI domain logic
│       ├── domain_graph_test.rs       # Graph domain logic
│       ├── domain_template_test.rs    # Template domain logic
│       └── domain_project_test.rs     # Project domain logic
└── post_refactor/
    ├── contract_validation_test.rs     # Layer contracts preserved
    ├── dependency_validation_test.rs   # No circular deps
    └── behavior_validation_test.rs     # Behavior unchanged
```

### 3.3 Pre-Refactor Acceptance Tests

**Purpose**: Document complete user workflows that must continue working after refactor.

```rust
// tests/v3-refactor/pre_refactor/acceptance/cli_workflows_test.rs

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

#[tokio::test]
async fn acceptance_template_generation_workflow() {
    // GIVEN: User wants to generate code from template
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("template.tmpl");
    let rdf_path = temp_dir.path().join("data.ttl");
    
    // Setup template and RDF
    std::fs::write(&template_path, "Hello {{ name }}!").unwrap();
    std::fs::write(&rdf_path, r#"@prefix ex: <http://example.org#> . ex:Person a ex:Person ; ex:name "World" ."#).unwrap();
    
    // WHEN: User runs generation command
    let output_dir = temp_dir.path().join("output");
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&[
        "template", "generate",
        "--template", template_path.to_str().unwrap(),
        "--rdf", rdf_path.to_str().unwrap(),
        "--output", output_dir.to_str().unwrap(),
    ]);
    
    // THEN: Code is generated successfully
    cmd.assert().success();
    
    // AND: Generated file contains correct content
    let generated = std::fs::read_to_string(output_dir.join("output.txt")).unwrap();
    assert_eq!(generated, "Hello World!");
}

#[tokio::test]
async fn acceptance_marketplace_search_and_install_workflow() {
    // GIVEN: User wants to find and install a package
    let temp_dir = TempDir::new().unwrap();
    
    // WHEN: User searches for package
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "search", "--query", "rust web"]);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("rust-axum-service"));
    
    // AND: User installs package
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "install", "--package", "rust-axum-service"]);
    cmd.assert().success();
    
    // THEN: Package is available
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(&["marketplace", "list"]);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("rust-axum-service"));
}
```

**Coverage**: All 8 nouns × all verbs = ~50 acceptance test scenarios

---

## 4. Layer Contract Tests

### 4.1 Purpose

Validate that layer boundaries remain stable during refactor. These tests verify contracts between layers using trait abstractions.

### 4.2 CLI → Domain Contract

**Goal**: Verify CLI calls domain functions correctly, domain has no CLI dependencies.

```rust
// tests/v3-refactor/pre_refactor/component/cli_to_domain_test.rs

use mockall::predicate::*;
use mockall::*;

// Mock domain trait (what domain provides)
#[automock]
pub trait ProjectDomain: Send + Sync {
    async fn create_project(&self, config: ProjectConfig) -> Result<ProjectResult>;
    async fn generate_project(&self, config: GenerateConfig) -> Result<GenerateResult>;
    async fn apply_plan(&self, plan: Plan) -> Result<ApplyResult>;
}

#[tokio::test]
async fn test_cli_project_new_calls_domain() {
    // ARRANGE: Mock domain
    let mut mock_domain = MockProjectDomain::new();
    mock_domain
        .expect_create_project()
        .withf(|config| config.name == "my-project")
        .times(1)
        .returning(|_| Ok(ProjectResult { id: "123".to_string() }));
    
    // ACT: CLI command (using mock domain)
    // Note: In real code, CLI would get domain via dependency injection
    // For now, we test the contract: CLI must call domain correctly
    
    // TODO: After refactor, test actual CLI → domain interaction
}

#[test]
fn test_domain_has_no_clap_dependency() {
    // ARRANGE: Check domain crate dependencies
    // ACT: Try to use clap in domain
    
    // ASSERT: Compilation fails if domain imports clap
    // This test verifies architectural constraint at compile time
    
    // In practice, this is enforced by Cargo.toml, but we can verify:
    // - Domain Cargo.toml has NO clap dependency
    // - Domain code has NO `use clap::*` imports
    // - Domain code has NO clap-noun-verb dependencies
}

#[tokio::test]
async fn test_cli_argument_transformation() {
    // ARRANGE: CLI arguments
    let cli_args = cli::cmds::project::NewArgs {
        name: "my-project".to_string(),
        template: Some("rust-cli".to_string()),
        // ... other args
    };
    
    // ACT: Transform CLI args to domain input
    let domain_input = transform_cli_to_domain(cli_args);
    
    // ASSERT: Domain input is correct
    assert_eq!(domain_input.name, "my-project");
    assert_eq!(domain_input.template, Some("rust-cli".to_string()));
}
```

### 4.3 Domain → Infrastructure Contract

**Goal**: Verify domain uses infrastructure via traits, infrastructure doesn't depend on domain.

```rust
// tests/v3-refactor/pre_refactor/component/domain_to_infrastructure_test.rs

use mockall::*;

// Mock infrastructure traits (what infrastructure provides)
#[automock]
pub trait TemplateEngine: Send + Sync {
    fn render(&self, template: &str, context: &TemplateContext) -> Result<String>;
    fn validate(&self, template: &str) -> Result<ValidationResult>;
}

#[automock]
pub trait GraphStore: Send + Sync {
    fn load_rdf(&self, path: &Path) -> Result<Graph>;
    fn query_sparql(&self, query: &str) -> Result<QueryResult>;
}

#[tokio::test]
async fn test_domain_template_uses_infrastructure() {
    // ARRANGE: Mock infrastructure
    let mut mock_engine = MockTemplateEngine::new();
    mock_engine
        .expect_render()
        .withf(|template, ctx| template == "Hello {{ name }}!" && ctx.get("name") == Some(&"World".to_string()))
        .times(1)
        .returning(|_, _| Ok("Hello World!".to_string()));
    
    // ACT: Domain function uses infrastructure
    let domain = TemplateDomain::new(Arc::new(mock_engine));
    let result = domain.generate_template(template_config).await?;
    
    // ASSERT: Domain called infrastructure correctly
    assert_eq!(result.content, "Hello World!");
}

#[test]
fn test_infrastructure_has_no_domain_dependency() {
    // ARRANGE: Infrastructure crate
    // ACT: Try to use domain types in infrastructure
    
    // ASSERT: Compilation fails if infrastructure imports domain
    // - ggen-core Cargo.toml has NO ggen-domain dependency
    // - ggen-core code has NO `use ggen_domain::*` imports
}
```

---

## 5. Outside-In Acceptance Tests

### 5.1 Test Structure

Start from user perspective, work inward. Each acceptance test validates a complete workflow.

### 5.2 Acceptance Test Categories

**Category 1: Template Generation** (5 tests)
- Generate from template + RDF
- Generate file tree
- Template linting
- Template listing
- Template creation

**Category 2: Project Scaffolding** (5 tests)
- Create new project
- Generate project from template
- Create generation plan
- Apply plan
- Project initialization

**Category 3: Marketplace Operations** (5 tests)
- Search packages
- Install package
- List packages
- Publish package
- Update package

**Category 4: Graph Operations** (4 tests)
- Load RDF data
- Query SPARQL
- Export graph
- Visualize graph

**Category 5: AI Operations** (3 tests)
- Analyze code
- Generate code
- Generate SPARQL

**Category 6: Hook Management** (4 tests)
- Create hook
- List hooks
- Remove hook
- Monitor hooks

**Category 7: Utilities** (2 tests)
- System diagnostics (doctor)
- Environment management

**Total**: ~28 acceptance tests covering all nouns and verbs

### 5.3 Example Acceptance Test

```rust
// tests/v3-refactor/pre_refactor/acceptance/template_generation_test.rs

#[tokio::test]
async fn acceptance_template_generate_with_rdf_context() {
    // GIVEN: User has template and RDF data
    let test_env = TestEnvironment::new().await?;
    
    let template = test_env.create_template("user_profile.tmpl", r#"
        # {{ user.name }}
        Email: {{ user.email }}
        Role: {{ user.role }}
    "#)?;
    
    let rdf = test_env.create_rdf("users.ttl", r#"
        @prefix ex: <http://example.org#> .
        ex:alice a ex:User ;
            ex:name "Alice" ;
            ex:email "alice@example.com" ;
            ex:role "Developer" .
    "#)?;
    
    // WHEN: User generates code
    let output_dir = test_env.create_output_dir();
    let result = test_env.run_command(&[
        "template", "generate",
        "--template", template.path().to_str().unwrap(),
        "--rdf", rdf.path().to_str().unwrap(),
        "--output", output_dir.path().to_str().unwrap(),
    ]).await?;
    
    // THEN: Code is generated
    result.assert().success();
    
    // AND: Generated file has correct content
    let generated = std::fs::read_to_string(output_dir.path().join("user_profile.md"))?;
    assert!(generated.contains("# Alice"));
    assert!(generated.contains("Email: alice@example.com"));
    assert!(generated.contains("Role: Developer"));
}
```

---

## 6. Component Integration Tests

### 6.1 Purpose

Test component interactions with selective mocking. Mock external boundaries (filesystem, network), use real implementations for domain logic.

### 6.2 Component Test Structure

**CLI Component Tests** (20 tests):
- Command parsing
- Argument validation
- Routing to domain
- Error handling
- Async/sync bridge

**Domain Component Tests** (80 tests):
- Domain logic with mocked infrastructure
- Each domain module (ai, graph, template, project, marketplace, etc.)
- Success and failure paths
- Edge cases

**Infrastructure Component Tests** (30 tests):
- Template engine
- RDF processing
- Graph operations
- Marketplace backend

**Total**: ~130 component tests

### 6.3 Example Component Test

```rust
// tests/v3-refactor/pre_refactor/component/domain_template_test.rs

use mockall::*;

#[automock]
pub trait TemplateEngine: Send + Sync {
    fn render(&self, template: &str, context: &TemplateContext) -> Result<String>;
}

#[automock]
pub trait GraphStore: Send + Sync {
    fn query_sparql(&self, query: &str) -> Result<QueryResult>;
}

#[tokio::test]
async fn test_template_domain_generate_with_rdf() {
    // ARRANGE: Mock infrastructure
    let mut mock_engine = MockTemplateEngine::new();
    let mut mock_graph = MockGraphStore::new();
    
    // Setup mock expectations
    mock_graph
        .expect_query_sparql()
        .withf(|query| query.contains("SELECT ?name"))
        .times(1)
        .returning(|_| Ok(QueryResult {
            bindings: vec![
                btreemap! {
                    "name".to_string() => "Alice".to_string(),
                    "email".to_string() => "alice@example.com".to_string(),
                }
            ],
        }));
    
    mock_engine
        .expect_render()
        .withf(|template, ctx| {
            template.contains("{{ user.name }}") &&
            ctx.get("user.name") == Some(&"Alice".to_string())
        })
        .times(1)
        .returning(|_, _| Ok("# Alice\nEmail: alice@example.com".to_string()));
    
    // ACT: Domain function uses mocked infrastructure
    let domain = TemplateDomain::new(
        Arc::new(mock_engine),
        Arc::new(mock_graph),
    );
    
    let config = GenerateConfig {
        template_path: PathBuf::from("template.tmpl"),
        rdf_path: PathBuf::from("data.ttl"),
        output_path: PathBuf::from("output.md"),
    };
    
    let result = domain.generate_template(config).await?;
    
    // ASSERT: Domain orchestrated infrastructure correctly
    assert_eq!(result.content, "# Alice\nEmail: alice@example.com");
}
```

---

## 7. Unit Tests with Mocks

### 7.1 Purpose

Test individual units in complete isolation. Mock ALL collaborators. Verify interactions, not state.

### 7.2 Unit Test Structure

**Domain Unit Tests** (200+ tests):
- Each domain function tested in isolation
- All collaborators mocked
- Success and failure paths
- Edge cases and error handling

**CLI Unit Tests** (50 tests):
- Command parsing
- Argument transformation
- Routing logic
- Error formatting

**Infrastructure Unit Tests** (100 tests):
- Template rendering
- RDF parsing
- Graph operations
- Cache operations

**Total**: ~350 unit tests

### 7.3 Example Unit Test

```rust
// tests/v3-refactor/pre_refactor/unit/domain_project_test.rs

use mockall::*;

#[automock]
pub trait ProjectGenerator: Send + Sync {
    fn generate_structure(&self, config: &ProjectConfig) -> Result<ProjectStructure>;
}

#[automock]
pub trait FileSystem: Send + Sync {
    fn write_file(&self, path: &Path, content: &str) -> Result<()>;
    fn create_dir(&self, path: &Path) -> Result<()>;
}

#[automock]
pub trait GitRepo: Send + Sync {
    fn initialize(&self, path: &Path) -> Result<()>;
}

#[tokio::test]
async fn test_project_domain_create_new_project() {
    // ARRANGE: All collaborators mocked
    let mut mock_generator = MockProjectGenerator::new();
    let mut mock_fs = MockFileSystem::new();
    let mut mock_git = MockGitRepo::new();
    
    // Setup expectations
    let project_structure = ProjectStructure {
        files: vec![
            ("Cargo.toml".to_string(), "[package]\nname = \"my-project\"".to_string()),
            ("src/main.rs".to_string(), "fn main() {}".to_string()),
        ],
        directories: vec!["src".to_string()],
    };
    
    mock_generator
        .expect_generate_structure()
        .withf(|config| config.name == "my-project")
        .times(1)
        .returning(|_| Ok(project_structure));
    
    mock_fs
        .expect_create_dir()
        .withf(|path| path.ends_with("src"))
        .times(1)
        .returning(|_| Ok(()));
    
    mock_fs
        .expect_write_file()
        .withf(|path, content| {
            (path.ends_with("Cargo.toml") && content.contains("name = \"my-project\"")) ||
            (path.ends_with("main.rs") && content == "fn main() {}")
        })
        .times(2)
        .returning(|_, _| Ok(()));
    
    mock_git
        .expect_initialize()
        .withf(|path| path.ends_with("my-project"))
        .times(1)
        .returning(|_| Ok(()));
    
    // ACT: Domain function orchestrates collaborators
    let domain = ProjectDomain::new(
        Arc::new(mock_generator),
        Arc::new(mock_fs),
        Arc::new(mock_git),
    );
    
    let config = ProjectConfig {
        name: "my-project".to_string(),
        project_type: ProjectType::RustCli,
        path: PathBuf::from("/tmp/my-project"),
    };
    
    let result = domain.create_project(config).await?;
    
    // ASSERT: Domain coordinated all collaborators correctly
    assert_eq!(result.project_path, PathBuf::from("/tmp/my-project"));
    // Mock expectations verify interactions automatically
}
```

---

## 8. Refactoring Validation Tests

### 8.1 Purpose

Validate that refactoring preserved behavior and contracts. These tests run after each refactoring phase.

### 8.2 Validation Test Categories

**Contract Validation** (10 tests):
- CLI → Domain contract preserved
- Domain → Infrastructure contract preserved
- No circular dependencies introduced
- Dependency directions correct

**Behavior Validation** (50 tests):
- All acceptance tests still pass
- Outputs identical (deterministic)
- Error messages unchanged
- Performance unchanged

**Structure Validation** (5 tests):
- Crate structure correct
- Module boundaries respected
- Public API unchanged
- Dependency graph correct

**Total**: ~65 validation tests

### 8.3 Example Validation Test

```rust
// tests/v3-refactor/post_refactor/contract_validation_test.rs

#[test]
fn test_cli_depends_on_domain() {
    // ARRANGE: Check Cargo.toml
    // ACT: Parse ggen-cli Cargo.toml
    
    // ASSERT: ggen-cli depends on ggen-domain
    assert!(cli_cargo_toml.has_dependency("ggen-domain"));
    
    // AND: ggen-cli does NOT contain domain code
    assert!(!cli_src_dir.contains("domain/"));
}

#[test]
fn test_domain_has_no_cli_dependency() {
    // ARRANGE: Check Cargo.toml
    // ACT: Parse ggen-domain Cargo.toml
    
    // ASSERT: ggen-domain does NOT depend on ggen-cli
    assert!(!domain_cargo_toml.has_dependency("ggen-cli"));
    
    // AND: ggen-domain does NOT import clap
    assert!(!domain_cargo_toml.has_dependency("clap"));
    assert!(!domain_code_contains("use clap"));
}

#[test]
fn test_no_circular_dependencies() {
    // ARRANGE: Dependency graph
    // ACT: Traverse dependency graph
    
    // ASSERT: No cycles
    assert!(!has_cycles(dependency_graph));
    
    // Dependency directions:
    // - ggen-cli → ggen-domain ✓
    // - ggen-domain → ggen-core ✓
    // - ggen-domain → ggen-ai ✓
    // - ggen-domain → ggen-marketplace ✓
    // - ggen-core → ggen-utils ✓
    // - ggen-ai → ggen-core ✓
    // - ggen-marketplace → (no ggen deps) ✓
}

#[tokio::test]
async fn test_behavior_preserved() {
    // ARRANGE: Pre-refactor acceptance tests
    // ACT: Run all acceptance tests
    
    // ASSERT: All tests pass
    let results = run_acceptance_test_suite().await?;
    assert_eq!(results.passed, results.total);
    
    // AND: Outputs identical
    assert!(outputs_match_pre_refactor());
}
```

---

## 9. Test Execution Strategy

### 9.1 Phased Execution

**Phase 0: Pre-Refactor**
1. Create comprehensive test suite
2. All tests pass with current v2.4.0 structure
3. Establish baseline

**Phase 1: Infrastructure Moves**
1. Run test suite after each crate move
2. Update test paths
3. Verify no behavior changes

**Phase 2: Domain Extraction**
1. Run domain tests after each module move
2. Update imports
3. Verify contracts preserved

**Phase 3: CLI Refactoring**
1. Run CLI tests after import updates
2. Verify CLI → Domain contracts
3. Validate no CLI leakage into domain

**Phase 4: Validation**
1. Run full test suite
2. Contract validation tests
3. Behavior comparison tests

### 9.2 Test Execution Commands

```bash
# Pre-refactor: Establish baseline
cargo make test --package ggen-cli-lib
cargo make test --package ggen-core
cargo make test --test v3_refactor -- pre_refactor

# During refactor: After each phase
cargo make test --workspace  # All crates
cargo make test --test v3_refactor -- contract_validation

# Post-refactor: Full validation
cargo make test --workspace
cargo make test --test v3_refactor -- post_refactor
cargo make deterministic  # Fixed seed tests
```

### 9.3 Test Organization

```
tests/
├── v3-refactor/              # v3 refactor specific tests
│   ├── pre_refactor/         # Baseline tests
│   ├── post_refactor/        # Validation tests
│   └── contracts/            # Contract tests
├── integration/              # Existing integration tests (keep)
└── unit/                     # Existing unit tests (keep)
```

---

## 10. Success Criteria

### 10.1 Test Coverage

- **Acceptance Tests**: 28/28 passing (100%)
- **Component Tests**: 130/130 passing (100%)
- **Unit Tests**: 350/350 passing (100%)
- **Contract Tests**: 10/10 passing (100%)
- **Validation Tests**: 65/65 passing (100%)

**Total**: 583 tests, 100% passing

### 10.2 Contract Validation

- ✅ CLI → Domain contract preserved
- ✅ Domain → Infrastructure contract preserved
- ✅ Domain has zero CLI dependencies
- ✅ No circular dependencies
- ✅ Dependency directions correct

### 10.3 Behavior Validation

- ✅ All acceptance tests pass
- ✅ Outputs identical (deterministic)
- ✅ Error messages unchanged
- ✅ Performance within SLOs (<15s first build, <2s incremental)

### 10.4 Structure Validation

- ✅ All crates in `crates/` directory
- ✅ Module boundaries respected
- ✅ Public API unchanged
- ✅ Cargo.toml structure correct

### 10.5 Documentation

- ✅ Test documentation complete
- ✅ Contract documentation updated
- ✅ Architecture diagrams updated
- ✅ Migration guide created

---

## 11. Trait Abstractions Needed

### 11.1 CLI → Domain Contracts

**Traits to Define**:
- `ProjectDomain` - Project operations
- `TemplateDomain` - Template operations
- `GraphDomain` - Graph operations
- `MarketplaceDomain` - Marketplace operations
- `AiDomain` - AI operations

**Location**: `crates/ggen-domain/src/traits.rs` (new)

### 11.2 Domain → Infrastructure Contracts

**Traits Already Exist** (in infrastructure):
- `TemplateEngine` - Template rendering
- `GraphStore` - RDF/graph operations
- `Registry` - Marketplace backend
- `LlmClient` - AI integration

**Location**: Existing in infrastructure crates

---

## 12. Implementation Phases

### Phase 0: Test Infrastructure Setup (Before Refactor)

**Tasks**:
1. Create `tests/v3-refactor/` directory structure
2. Set up test utilities and mocks
3. Write all pre-refactor tests
4. Verify all tests pass with v2.4.0

**Duration**: 4-6 hours

### Phase 1: Pre-Refactor Test Suite (Before Refactor)

**Tasks**:
1. Write all 583 tests
2. Establish baseline behavior
3. Document contracts
4. Create test utilities

**Duration**: 8-12 hours

### Phase 2: Refactor with Test Safety Net

**Tasks**:
1. Execute refactor phases
2. Run tests after each phase
3. Fix any test failures immediately
4. Validate contracts continuously

**Duration**: 14-21 hours (refactor + testing)

### Phase 3: Post-Refactor Validation

**Tasks**:
1. Run full test suite
2. Validate all contracts
3. Compare behaviors
4. Document results

**Duration**: 2-3 hours

---

## 13. Risk Mitigation

### Risk 1: Tests Reveal Architectural Issues

**Mitigation**: 
- Let test pain drive refactoring decisions
- If tests are hard to write, refactor code first
- Tests reveal dependencies - address them

### Risk 2: Test Maintenance Overhead

**Mitigation**:
- Focus on contracts, not implementation details
- Use trait abstractions for stable interfaces
- Test behavior, not internals

### Risk 3: Test Flakiness

**Mitigation**:
- Mock all external dependencies
- Use fixed seeds for deterministic tests
- Isolate tests completely

### Risk 4: Incomplete Test Coverage

**Mitigation**:
- Outside-in approach ensures critical paths covered
- Acceptance tests validate user workflows
- Contract tests validate boundaries

---

## 14. Test Utilities Needed

### 14.1 Mock Builders

```rust
// tests/v3-refactor/utils/mock_builders.rs

pub struct MockProjectDomainBuilder {
    // Builder pattern for setting up mocks
}

pub struct MockTemplateEngineBuilder {
    // Builder pattern for template engine mocks
}
```

### 14.2 Test Environments

```rust
// tests/v3-refactor/utils/test_environment.rs

pub struct TestEnvironment {
    // Temporary directories
    // Mock setup
    // Test data generation
}
```

### 14.3 Contract Validators

```rust
// tests/v3-refactor/utils/contract_validators.rs

pub fn validate_cli_domain_contract() -> Result<()>;
pub fn validate_domain_infrastructure_contract() -> Result<()>;
pub fn validate_dependency_graph() -> Result<()>;
```

---

## 15. Conclusion

This London TDD plan ensures we can refactor safely with comprehensive test coverage. By following outside-in development, mocking all collaborators, and validating contracts between layers, we maintain confidence throughout the refactor.

**Key Principles**:
1. Test contracts, not implementations
2. Mock boundaries, test business logic
3. Outside-in ensures critical paths covered
4. Contract tests validate architecture
5. Behavior validation ensures correctness

**Success Metrics**:
- 583 tests, 100% passing
- All contracts preserved
- Zero behavior changes
- Clean architecture validated

---

**Document Status**: Complete London TDD plan ready for implementation
**Next Action**: Create test infrastructure and write pre-refactor tests

