# London School TDD Strategy - Ggen v1.2.0

**Created**: 2025-10-30
**Strategist**: London TDD Swarm Agent
**Status**: 🎯 **READY FOR IMPLEMENTATION**

---

## Executive Summary

This document provides a comprehensive London School (mockist) TDD strategy to systematically implement missing features and enhance test coverage for ggen v1.2.0. Based on the existing testing architecture and production readiness assessments, this strategy focuses on the critical 20% of tests that provide 80% of value.

### Key Metrics

- **Current Production Readiness**: 94/100 ✅
- **Current Test Coverage**: ~85% (estimated)
- **Target Test Coverage**: ~95%
- **Critical Path Coverage**: 100% ✅
- **Tests Needed**: ~50-60 additional tests (focused on gaps)

---

## 1. London School TDD Principles Applied

### 1.1 Core Principles

**Outside-In Development** 🎯
- Start with acceptance tests (user scenarios)
- Work inward to unit tests
- Drive design through test expectations

**Mock-First Approach** 🔧
- Define collaborator interfaces through mocks
- Test object conversations, not implementations
- Focus on behavior verification

**Sociable vs Solitary Testing** 🤝
- **Solitary**: Mock external dependencies (network, filesystem, registry)
- **Sociable**: Use real implementations for domain logic
- **Guideline**: Mock I/O, use real business logic

**Listening to Tests** 👂
- Let test pain drive refactoring
- Hard-to-test code = poorly designed code
- Mock explosion = too many dependencies

**Continuous Refactoring** ♻️
- Red → Green → Refactor cycle
- Keep tests and code clean
- Maintain clear interfaces

### 1.2 Testing Pyramid for Ggen

```
        /\
       /  \  5% E2E (Acceptance Tests)
      /____\
     /      \  20% Integration Tests (Sociable)
    /________\
   /          \  70% Unit Tests (Solitary)
  /____________\
     5% Property-Based Tests
```

**Distribution**:
- **Unit Tests (70%)**: Fast, isolated, comprehensive coverage
- **Integration Tests (20%)**: Multi-component interactions
- **Acceptance Tests (5%)**: End-to-end user scenarios
- **Property Tests (5%)**: Invariant verification

---

## 2. Test Layer Architecture

### 2.1 Layer 1: Acceptance Tests (Outside)

**Purpose**: Validate complete user workflows end-to-end

**Characteristics**:
- Test from user's perspective
- Real CLI invocations
- Minimal mocking (only external services)
- Document user stories

**Example Structure**:
```rust
#[test]
fn acceptance_marketplace_search_and_install_workflow() {
    // GIVEN: User wants to find and install a package
    let cleanroom = CleanroomEnv::new().unwrap();

    // WHEN: User searches for a package
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("rust web")
        .assert()
        .success();

    // THEN: User can install found package
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("rust-axum-service")
        .assert()
        .success();

    // AND: Package is available for use
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("list")
        .assert()
        .stdout(predicate::str::contains("rust-axum-service"));
}
```

### 2.2 Layer 2: Integration Tests (Sociable)

**Purpose**: Test component interactions with selective mocking

**Mock Strategy**:
- ✅ Mock: External services (registry, network, filesystem I/O)
- ✅ Real: Domain logic (search algorithms, version resolution)
- ✅ Real: Data structures (Package, Template, Config)

**Example Structure**:
```rust
#[test]
fn integration_marketplace_search_with_mock_registry() {
    // Arrange: Mock registry, real search logic
    let mut mock_registry = MockRegistryClient::new();
    mock_registry
        .expect_fetch_index()
        .returning(|| Ok(fake_registry_index()));

    let search_engine = SearchEngine::new(Arc::new(mock_registry));

    // Act: Real search algorithm with mock data
    let results = search_engine.search("rust web").unwrap();

    // Assert: Verify behavior
    assert_eq!(results.len(), 2);
    assert!(results[0].relevance_score > results[1].relevance_score);
}
```

### 2.3 Layer 3: Unit Tests (Solitary)

**Purpose**: Test individual units in isolation

**Mock Strategy**:
- ✅ Mock: ALL external collaborators
- ✅ Focus: Object conversations and contracts
- ✅ Verify: Interaction patterns, not state

**Example Structure**:
```rust
#[test]
fn unit_lifecycle_executor_coordinates_phases_correctly() {
    // Arrange: All collaborators mocked
    let mut mock_runner = MockCommandRunner::new();
    let mut mock_cache = MockLifecycleCache::new();
    let mut mock_validator = MockPhaseValidator::new();

    // Define interaction expectations
    mock_cache
        .expect_get()
        .with(eq("test"))
        .returning(|_| None);

    mock_runner
        .expect_execute()
        .with(eq("cargo test"))
        .returning(|_| Ok(CommandResult { exit_code: 0, stdout: "".into() }));

    mock_cache
        .expect_set()
        .with(eq("test"), always())
        .returning(|_, _| Ok(()));

    let executor = LifecycleExecutor::new(
        Arc::new(mock_runner),
        Arc::new(mock_cache),
        Arc::new(mock_validator)
    );

    // Act
    let result = executor.run_phase("test").unwrap();

    // Assert: Verify the conversation
    assert_eq!(result.status, PhaseStatus::Success);
    // Mocks verify expectations automatically
}
```

---

## 3. Gap Analysis & Test Specifications

### 3.1 Priority 0 (P0) - Critical Gaps

#### P0-1: Marketplace Registry Client Error Recovery

**Current State**: Partial error handling
**Gap**: Network failure recovery, circuit breaker pattern
**Test Specification**:

```rust
// tests/london_tdd/marketplace/registry_resilience_test.rs

#[test]
fn test_registry_client_retries_on_network_failure() {
    // Arrange: Registry fails 2 times, succeeds on 3rd
    let mut mock_http = MockHttpClient::new();
    mock_http
        .expect_get()
        .times(3)
        .returning({
            let mut call_count = 0;
            move |_| {
                call_count += 1;
                if call_count < 3 {
                    Err(anyhow::anyhow!("Network timeout"))
                } else {
                    Ok(r#"{"packages": []}"#.into())
                }
            }
        });

    let client = RegistryClient::with_http(Arc::new(mock_http));

    // Act: Should retry and eventually succeed
    let result = client.fetch_index();

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_registry_client_opens_circuit_after_threshold() {
    // Arrange: Registry consistently fails
    let mut mock_http = MockHttpClient::new();
    mock_http
        .expect_get()
        .times(5)
        .returning(|_| Err(anyhow::anyhow!("Service unavailable")));

    let client = RegistryClient::with_http(Arc::new(mock_http));

    // Act: First 5 requests trigger failures
    for _ in 0..5 {
        let _ = client.fetch_index();
    }

    // Assert: 6th request returns circuit open immediately
    let start = Instant::now();
    let result = client.fetch_index();
    let elapsed = start.elapsed();

    assert!(result.is_err());
    assert!(elapsed < Duration::from_millis(10), "Circuit should fail fast");
}

#[test]
fn test_registry_client_falls_back_to_cache() {
    // Arrange: Registry unavailable, cache has data
    let mut mock_http = MockHttpClient::new();
    mock_http
        .expect_get()
        .returning(|_| Err(anyhow::anyhow!("Network down")));

    let mut mock_cache = MockRegistryCache::new();
    mock_cache
        .expect_get_cached_index()
        .returning(|| Some(fake_registry_index()));

    let client = RegistryClient::with_dependencies(
        Arc::new(mock_http),
        Arc::new(mock_cache)
    );

    // Act: Should fall back to cache
    let result = client.fetch_index_with_fallback();

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap().is_from_cache);
}
```

**Mock Traits**:
```rust
#[automock]
pub trait HttpClient: Send + Sync {
    fn get(&self, url: &str) -> Result<String>;
    fn post(&self, url: &str, body: &str) -> Result<String>;
}

#[automock]
pub trait RegistryCache: Send + Sync {
    fn get_cached_index(&self) -> Option<RegistryIndex>;
    fn set_cached_index(&self, index: RegistryIndex) -> Result<()>;
}
```

#### P0-2: Lifecycle Phase Dependency Resolution

**Current State**: Basic phase execution
**Gap**: Circular dependency detection, parallel execution
**Test Specification**:

```rust
// tests/london_tdd/lifecycle/dependency_resolution_test.rs

#[test]
fn test_lifecycle_detects_circular_dependencies() {
    // Arrange: Create circular dependency (A → B → C → A)
    let mut mock_config = MockLifecycleConfig::new();
    mock_config
        .expect_get_dependencies()
        .with(eq("phase_a"))
        .returning(|_| vec!["phase_b".into()]);

    mock_config
        .expect_get_dependencies()
        .with(eq("phase_b"))
        .returning(|_| vec!["phase_c".into()]);

    mock_config
        .expect_get_dependencies()
        .with(eq("phase_c"))
        .returning(|_| vec!["phase_a".into()]);

    let resolver = DependencyResolver::new(Arc::new(mock_config));

    // Act
    let result = resolver.resolve_execution_order(&["phase_a"]);

    // Assert: Should detect and report cycle
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Circular dependency"));
}

#[test]
fn test_lifecycle_executes_independent_phases_in_parallel() {
    // Arrange: Three independent phases
    let mut mock_runner = MockPhaseRunner::new();

    // Track execution order to verify parallelism
    let execution_log = Arc::new(Mutex::new(Vec::new()));
    let log_clone = execution_log.clone();

    mock_runner
        .expect_run()
        .times(3)
        .returning(move |phase_name| {
            let start = Instant::now();
            thread::sleep(Duration::from_millis(100)); // Simulate work
            log_clone.lock().unwrap().push((phase_name.into(), start));
            Ok(PhaseResult::success())
        });

    let executor = ParallelLifecycleExecutor::new(Arc::new(mock_runner));

    // Act: Execute three independent phases
    let start = Instant::now();
    executor.run_phases(&["lint", "test", "audit"]).unwrap();
    let total_time = start.elapsed();

    // Assert: Should complete in ~100ms (parallel) not ~300ms (sequential)
    assert!(total_time < Duration::from_millis(200),
           "Phases should run in parallel, took {:?}", total_time);
}

#[test]
fn test_lifecycle_respects_dependency_order() {
    // Arrange: Phase chain (A → B → C)
    let execution_order = Arc::new(Mutex::new(Vec::new()));
    let order_clone = execution_order.clone();

    let mut mock_runner = MockPhaseRunner::new();
    mock_runner
        .expect_run()
        .times(3)
        .returning(move |phase_name| {
            order_clone.lock().unwrap().push(phase_name.into());
            Ok(PhaseResult::success())
        });

    let mut mock_config = MockLifecycleConfig::new();
    mock_config
        .expect_get_dependencies()
        .returning(|phase| match phase {
            "phase_c" => vec!["phase_b".into()],
            "phase_b" => vec!["phase_a".into()],
            _ => vec![],
        });

    let executor = LifecycleExecutor::with_dependencies(
        Arc::new(mock_runner),
        Arc::new(mock_config)
    );

    // Act
    executor.run_phase("phase_c").unwrap();

    // Assert: Should execute in order A → B → C
    let order = execution_order.lock().unwrap();
    assert_eq!(order[0], "phase_a");
    assert_eq!(order[1], "phase_b");
    assert_eq!(order[2], "phase_c");
}
```

#### P0-3: Template Rendering Error Context

**Current State**: Basic error messages
**Gap**: Line number reporting, variable context
**Test Specification**:

```rust
// tests/london_tdd/template_engine/error_context_test.rs

#[test]
fn test_template_error_includes_line_number() {
    // Arrange: Template with error on line 5
    let template_content = r#"
        Line 1: Valid content
        Line 2: {{ valid_var }}
        Line 3: More content
        Line 4: {{ another_var }}
        Line 5: {{ undefined_var }}
        Line 6: Final content
    "#;

    let mut mock_parser = MockTemplateParser::new();
    mock_parser
        .expect_parse()
        .returning(|_| Ok(ParsedTemplate {
            variables: vec!["valid_var".into(), "another_var".into()],
            line_map: vec![(2, "valid_var"), (4, "another_var"), (5, "undefined_var")]
        }));

    let mut mock_context = MockRenderContext::new();
    mock_context
        .expect_get()
        .with(eq("undefined_var"))
        .returning(|_| None);

    let renderer = TemplateRenderer::new(
        Arc::new(mock_parser),
        Arc::new(mock_context)
    );

    // Act
    let result = renderer.render(template_content);

    // Assert: Error should include line number
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("line 5"));
    assert!(error.contains("undefined_var"));
}

#[test]
fn test_template_error_shows_available_variables() {
    // Arrange
    let mut mock_context = MockRenderContext::new();
    mock_context
        .expect_get()
        .returning(|_| None);

    mock_context
        .expect_available_keys()
        .returning(|| vec!["valid_var1".into(), "valid_var2".into()]);

    let renderer = TemplateRenderer::new_with_context(Arc::new(mock_context));

    // Act
    let result = renderer.render("{{ typo_var }}");

    // Assert: Should suggest similar variables
    let error = result.unwrap_err().to_string();
    assert!(error.contains("Did you mean"));
    assert!(error.contains("Available variables"));
}
```

### 3.2 Priority 1 (P1) - Important Gaps

#### P1-1: Search Ranking Algorithm Validation

**Test Specification**:
```rust
// tests/london_tdd/marketplace/search_ranking_test.rs

#[test]
fn test_search_ranks_exact_matches_highest() {
    // Arrange: Multiple packages with varying relevance
    let packages = vec![
        Package::new("rust-web-framework", "Web framework"),  // Exact match
        Package::new("rust-web-server", "Web server"),        // Close match
        Package::new("rust-framework", "Framework"),          // Partial match
    ];

    let mut mock_registry = MockRegistry::new();
    mock_registry
        .expect_get_all_packages()
        .returning(move || packages.clone());

    let ranker = SearchRanker::new(Arc::new(mock_registry));

    // Act
    let results = ranker.rank("rust web framework");

    // Assert: Exact match should be first
    assert_eq!(results[0].name, "rust-web-framework");
    assert!(results[0].score > 0.9);
}

#[test]
fn test_search_boosts_popular_packages() {
    // Arrange: Two packages, one more popular
    let mut mock_stats = MockPackageStats::new();
    mock_stats
        .expect_get_download_count()
        .with(eq("package-a"))
        .returning(|_| 10000);

    mock_stats
        .expect_get_download_count()
        .with(eq("package-b"))
        .returning(|_| 100);

    let ranker = SearchRanker::with_stats(Arc::new(mock_stats));

    // Act
    let results = ranker.rank_with_popularity("rust web", &["package-a", "package-b"]);

    // Assert: Popular package should rank higher
    assert_eq!(results[0], "package-a");
}
```

#### P1-2: Concurrent Marketplace Operations

**Test Specification**:
```rust
// tests/london_tdd/marketplace/concurrent_ops_test.rs

#[test]
fn test_concurrent_searches_are_thread_safe() {
    // Arrange: Shared registry client
    let mock_registry = Arc::new(MockRegistry::new_thread_safe());
    let search_engine = Arc::new(SearchEngine::new(mock_registry));

    // Act: Launch concurrent searches
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let engine = search_engine.clone();
            thread::spawn(move || {
                engine.search(&format!("query{}", i))
            })
        })
        .collect();

    // Assert: All should complete without panics
    for handle in handles {
        let result = handle.join().expect("Thread panicked");
        assert!(result.is_ok());
    }
}

#[test]
fn test_concurrent_installs_dont_corrupt_lockfile() {
    // Arrange: Mock filesystem with contention
    let mock_fs = Arc::new(MockFilesystem::new_thread_safe());
    let installer = Arc::new(PackageInstaller::new(mock_fs.clone()));

    // Act: Try to install different packages concurrently
    let handles: Vec<_> = vec!["pkg-a", "pkg-b", "pkg-c"]
        .into_iter()
        .map(|pkg| {
            let inst = installer.clone();
            thread::spawn(move || inst.install(pkg))
        })
        .collect();

    // Assert: Lockfile should be consistent
    for handle in handles {
        handle.join().unwrap().unwrap();
    }

    let lockfile = mock_fs.read_lockfile().unwrap();
    assert_eq!(lockfile.packages.len(), 3);
    // All packages present, no corruption
}
```

### 3.3 Priority 2 (P2) - Nice-to-Have

#### P2-1: Property-Based Tests for Version Resolution

**Test Specification**:
```rust
// tests/london_tdd/marketplace/version_properties_test.rs

use proptest::prelude::*;

proptest! {
    #[test]
    fn prop_version_ordering_is_transitive(
        v1 in version_strategy(),
        v2 in version_strategy(),
        v3 in version_strategy()
    ) {
        // If v1 < v2 and v2 < v3, then v1 < v3
        if v1 < v2 && v2 < v3 {
            prop_assert!(v1 < v3);
        }
    }

    #[test]
    fn prop_latest_version_is_always_selected(
        versions in prop::collection::vec(version_strategy(), 1..10)
    ) {
        let resolver = VersionResolver::new();
        let latest = resolver.select_latest(&versions);

        // Latest should be >= all other versions
        for version in &versions {
            prop_assert!(latest >= version);
        }
    }
}
```

---

## 4. Mock Strategy by Component

### 4.1 Marketplace Subsystem

**What to Mock** (External Dependencies):
- ✅ `HttpClient` - Network I/O
- ✅ `RegistryClient` - External registry API
- ✅ `Filesystem` - Disk operations
- ✅ `CacheStore` - Cache backend

**What NOT to Mock** (Domain Logic):
- ❌ `SearchEngine` - Search algorithm
- ❌ `VersionResolver` - Version comparison logic
- ❌ `PackageMetadata` - Data structures
- ❌ `DependencyGraph` - Graph algorithms

**Rationale**: Business logic should be tested with real implementations. Only mock I/O boundaries.

### 4.2 Lifecycle Subsystem

**What to Mock**:
- ✅ `CommandRunner` - System command execution
- ✅ `LifecycleCache` - Cache layer
- ✅ `PhaseValidator` - External validation
- ✅ `FileWatcher` - Filesystem events

**What NOT to Mock**:
- ❌ `DependencyResolver` - Dependency graph logic
- ❌ `PhaseConfig` - Configuration logic
- ❌ `ExecutionPlan` - Planning algorithms

### 4.3 Template Engine

**What to Mock**:
- ✅ `RdfTripleStore` - RDF database
- ✅ `SparqlEngine` - Query execution
- ✅ `Filesystem` - Template file I/O

**What NOT to Mock**:
- ❌ `TemplateParser` - Parsing logic
- ❌ `VariableSubstitution` - Substitution algorithm
- ❌ `RenderContext` - Context management

---

## 5. Implementation Plan

### 5.1 Phase 1: Critical Gaps (P0) - Week 1

**Priority**: P0-1, P0-2, P0-3

**Tasks**:
1. ✅ Create mock trait definitions (Day 1)
2. ✅ Write acceptance tests for each gap (Day 2)
3. ✅ Implement red tests (all failing) (Day 3)
4. ✅ Make tests green (minimal implementation) (Day 4-5)

**Deliverables**:
- `/tests/london_tdd/marketplace/registry_resilience_test.rs`
- `/tests/london_tdd/lifecycle/dependency_resolution_test.rs`
- `/tests/london_tdd/template_engine/error_context_test.rs`

**Success Criteria**:
- All P0 tests passing (15-20 tests)
- Zero new production panics
- Error messages improved

### 5.2 Phase 2: Important Gaps (P1) - Week 2

**Priority**: P1-1, P1-2

**Tasks**:
1. ✅ Add integration tests for search ranking
2. ✅ Add concurrency tests
3. ✅ Refactor based on test feedback

**Deliverables**:
- `/tests/london_tdd/marketplace/search_ranking_test.rs`
- `/tests/london_tdd/marketplace/concurrent_ops_test.rs`

**Success Criteria**:
- All P1 tests passing (10-15 tests)
- Thread-safety verified
- Search quality improved

### 5.3 Phase 3: Property Tests (P2) - Week 3

**Priority**: P2-1

**Tasks**:
1. ✅ Add property-based tests for version resolution
2. ✅ Add property tests for template rendering
3. ✅ Add property tests for graph operations

**Deliverables**:
- `/tests/london_tdd/marketplace/version_properties_test.rs`
- `/tests/london_tdd/template_engine/render_properties_test.rs`

**Success Criteria**:
- 20+ property tests
- Edge cases discovered and handled
- Invariants documented

### 5.4 Phase 4: Refactoring & Documentation - Week 4

**Tasks**:
1. ✅ Refactor based on test insights
2. ✅ Document mock interfaces
3. ✅ Create test patterns guide
4. ✅ Update production readiness report

**Deliverables**:
- `/docs/TESTING_PATTERNS.md`
- `/docs/MOCK_INTERFACES.md`
- Updated `/docs/v1-production-readiness.md`

---

## 6. Test-Driven Refactoring Opportunities

### 6.1 Extract Interface Pattern

**Current**: Tight coupling to implementation
**Opportunity**: Extract traits for better testability

**Example**:
```rust
// Before: Hard to test
pub struct RegistryClient {
    base_url: String,
    http_client: reqwest::Client,
}

// After: Easy to mock
pub struct RegistryClient<H: HttpClient> {
    base_url: String,
    http_client: Arc<H>,
}

#[automock]
pub trait HttpClient: Send + Sync {
    fn get(&self, url: &str) -> Result<String>;
}
```

### 6.2 Dependency Injection Pattern

**Current**: Hard-coded dependencies
**Opportunity**: Inject dependencies for flexibility

**Example**:
```rust
// Before: Cannot mock
impl LifecycleExecutor {
    pub fn new() -> Self {
        Self {
            runner: CommandRunner::new(),  // Hard-coded
            cache: FileCache::new(),       // Hard-coded
        }
    }
}

// After: Mockable
impl<R: PhaseRunner, C: Cache> LifecycleExecutor<R, C> {
    pub fn new(runner: Arc<R>, cache: Arc<C>) -> Self {
        Self { runner, cache }
    }
}
```

### 6.3 Builder Pattern for Tests

**Opportunity**: Simplify test setup

**Example**:
```rust
// Test builder for complex scenarios
pub struct LifecycleExecutorTestBuilder {
    runner: Option<Arc<dyn PhaseRunner>>,
    cache: Option<Arc<dyn Cache>>,
    config: Option<Arc<dyn LifecycleConfig>>,
}

impl LifecycleExecutorTestBuilder {
    pub fn with_mock_runner(mut self) -> Self {
        self.runner = Some(Arc::new(MockPhaseRunner::new()));
        self
    }

    pub fn with_cached_phase(mut self, phase: &str) -> Self {
        let mut cache = MockCache::new();
        cache.expect_get()
            .with(eq(phase))
            .returning(|_| Some(cached_result()));
        self.cache = Some(Arc::new(cache));
        self
    }

    pub fn build(self) -> LifecycleExecutor {
        LifecycleExecutor::new(
            self.runner.unwrap_or_else(|| Arc::new(MockPhaseRunner::new())),
            self.cache.unwrap_or_else(|| Arc::new(MockCache::new())),
            self.config.unwrap_or_else(|| Arc::new(MockLifecycleConfig::new()))
        )
    }
}

// Usage in tests
#[test]
fn test_with_builder() {
    let executor = LifecycleExecutorTestBuilder::new()
        .with_mock_runner()
        .with_cached_phase("test")
        .build();

    let result = executor.run_phase("test").unwrap();
    assert!(result.from_cache);
}
```

---

## 7. Mock Trait Definitions

### 7.1 Marketplace Mocks

```rust
// tests/london_tdd/mocks/marketplace.rs

#[automock]
pub trait RegistryClient: Send + Sync {
    fn fetch_index(&self) -> Result<RegistryIndex>;
    fn fetch_package(&self, id: &str) -> Result<PackageData>;
    fn search(&self, query: &str) -> Result<Vec<Package>>;
}

#[automock]
pub trait PackageCache: Send + Sync {
    fn get(&self, package_id: &str) -> Option<CachedPackage>;
    fn set(&self, package_id: &str, package: CachedPackage) -> Result<()>;
    fn invalidate(&self, package_id: &str) -> Result<()>;
}

#[automock]
pub trait PackageInstaller: Send + Sync {
    fn install(&self, package_id: &str) -> Result<InstallResult>;
    fn uninstall(&self, package_id: &str) -> Result<()>;
    fn verify(&self, package_id: &str) -> Result<bool>;
}
```

### 7.2 Lifecycle Mocks

```rust
// tests/london_tdd/mocks/lifecycle.rs

#[automock]
pub trait PhaseRunner: Send + Sync {
    fn run(&self, phase: &str) -> Result<PhaseResult>;
    fn validate(&self, phase: &str) -> Result<ValidationResult>;
}

#[automock]
pub trait LifecycleCache: Send + Sync {
    fn get(&self, phase: &str) -> Option<CachedResult>;
    fn set(&self, phase: &str, result: CachedResult) -> Result<()>;
    fn clear(&self) -> Result<()>;
}

#[automock]
pub trait DependencyResolver: Send + Sync {
    fn resolve(&self, phases: &[&str]) -> Result<ExecutionPlan>;
    fn detect_cycles(&self, phases: &[&str]) -> Result<Vec<Vec<String>>>;
}
```

### 7.3 Template Engine Mocks

```rust
// tests/london_tdd/mocks/template.rs

#[automock]
pub trait TemplateParser: Send + Sync {
    fn parse(&self, content: &str) -> Result<ParsedTemplate>;
    fn validate(&self, content: &str) -> Result<Vec<ValidationError>>;
}

#[automock]
pub trait RenderContext: Send + Sync {
    fn get(&self, key: &str) -> Option<String>;
    fn set(&self, key: &str, value: String);
    fn available_keys(&self) -> Vec<String>;
}

#[automock]
pub trait TripleStore: Send + Sync {
    fn query(&self, sparql: &str) -> Result<QueryResult>;
    fn insert(&self, triples: &[Triple]) -> Result<()>;
}
```

---

## 8. Acceptance Test Specifications

### 8.1 Marketplace Workflows

**User Story 1**: Search and Install Package
```rust
#[test]
fn acceptance_user_searches_and_installs_package() {
    // GIVEN: User is starting fresh
    let cleanroom = CleanroomEnv::new().unwrap();

    // WHEN: User searches for a web framework
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("rust web framework")
        .assert()
        .success()
        .stdout(predicate::str::contains("rust-axum-service"));

    // AND: User installs the found package
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("rust-axum-service")
        .assert()
        .success();

    // THEN: Package is available in project
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("rust-axum-service"));

    // AND: Package can be used to generate code
    cleanroom.ggen_cmd()
        .arg("template")
        .arg("generate")
        .arg("rust-axum-service:main.tmpl")
        .assert()
        .success();
}
```

**User Story 2**: Recover from Network Failure
```rust
#[test]
fn acceptance_user_works_offline_with_cached_data() {
    // GIVEN: User has previously fetched marketplace data
    let cleanroom = CleanroomEnv::new().unwrap();
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("rust")
        .assert()
        .success();

    // WHEN: Network becomes unavailable
    cleanroom.disable_network();

    // THEN: User can still search cached data
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("rust")
        .assert()
        .success()
        .stderr(predicate::str::contains("Using cached data"));

    // AND: User sees helpful message about offline mode
    cleanroom.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("new-query")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Offline mode"));
}
```

### 8.2 Lifecycle Workflows

**User Story 3**: Run Full Lifecycle Pipeline
```rust
#[test]
fn acceptance_user_runs_full_lifecycle_pipeline() {
    // GIVEN: User has a Rust project
    let cleanroom = CleanroomEnv::new().unwrap();
    cleanroom.init_rust_project();

    // WHEN: User runs full lifecycle
    cleanroom.ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("all")
        .assert()
        .success();

    // THEN: All phases complete successfully
    // - format ✅
    // - lint ✅
    // - build ✅
    // - test ✅
    // - audit ✅

    // AND: User sees progress for each phase
    // AND: User sees timing information
    // AND: Build artifacts are created
}
```

---

## 9. TDD Cycle Examples

### 9.1 Red-Green-Refactor Cycle

**Cycle 1: Red (Write Failing Test)**
```rust
#[test]
fn test_registry_client_retries_on_timeout() {
    let mut mock_http = MockHttpClient::new();
    mock_http
        .expect_get()
        .times(3)
        .returning({
            let mut count = 0;
            move |_| {
                count += 1;
                if count < 3 {
                    Err(anyhow!("Timeout"))
                } else {
                    Ok("success".into())
                }
            }
        });

    let client = RegistryClient::new(Arc::new(mock_http));
    let result = client.fetch_index();

    assert!(result.is_ok()); // FAILS: retry not implemented
}
```

**Cycle 2: Green (Minimal Implementation)**
```rust
impl<H: HttpClient> RegistryClient<H> {
    pub fn fetch_index(&self) -> Result<RegistryIndex> {
        let mut retries = 0;
        loop {
            match self.http_client.get(&self.index_url) {
                Ok(data) => return serde_json::from_str(&data)
                    .context("Failed to parse index"),
                Err(e) if retries < 3 => {
                    retries += 1;
                    continue;
                }
                Err(e) => return Err(e),
            }
        }
    }
}
```

**Cycle 3: Refactor (Improve Design)**
```rust
impl<H: HttpClient> RegistryClient<H> {
    pub fn fetch_index(&self) -> Result<RegistryIndex> {
        self.fetch_with_retry(&self.index_url, 3)
            .and_then(|data| self.parse_index(&data))
    }

    fn fetch_with_retry(&self, url: &str, max_retries: usize) -> Result<String> {
        for attempt in 0..=max_retries {
            match self.http_client.get(url) {
                Ok(data) => return Ok(data),
                Err(e) if attempt < max_retries => {
                    tracing::warn!("Retry {} of {}", attempt + 1, max_retries);
                    thread::sleep(Duration::from_millis(100 * 2_u64.pow(attempt as u32)));
                }
                Err(e) => return Err(e),
            }
        }
        unreachable!()
    }

    fn parse_index(&self, data: &str) -> Result<RegistryIndex> {
        serde_json::from_str(data)
            .context("Failed to parse registry index")
    }
}
```

---

## 10. Success Criteria

### 10.1 Quantitative Metrics

**Test Coverage**:
- ✅ Overall: 95% (target)
- ✅ Critical paths: 100% (required)
- ✅ Error handling: 100% (required)
- ✅ Public APIs: 100% (required)

**Test Counts**:
- ✅ Unit tests: +30 (total ~90)
- ✅ Integration tests: +15 (total ~55)
- ✅ Acceptance tests: +10 (total ~15)
- ✅ Property tests: +20 (total ~50)

**Performance**:
- ✅ Test suite: <60s (currently ~45s)
- ✅ Individual tests: <100ms average
- ✅ No test timeouts

### 10.2 Qualitative Metrics

**Test Quality**:
- ✅ Clear test names (behavior-focused)
- ✅ Minimal setup (use builders)
- ✅ Single assertion focus
- ✅ Good error messages

**Code Quality**:
- ✅ No test-induced damage
- ✅ Clear interfaces
- ✅ Proper dependency injection
- ✅ Improved modularity

### 10.3 Production Readiness

**Target Score**: 97/100 (from 94/100)

| Category | Current | Target | Improvement |
|----------|---------|--------|-------------|
| Code Quality | 95/100 | 97/100 | +2 |
| Testing | 92/100 | 98/100 | +6 |
| Error Handling | 98/100 | 99/100 | +1 |
| Monitoring | 85/100 | 90/100 | +5 |
| **Overall** | **94/100** | **97/100** | **+3** |

---

## 11. Resources & References

### 11.1 Testing Tools

**Mocking**:
- `mockall` - Mock trait implementations
- `mockito` - HTTP mock server
- Custom mocks for domain-specific needs

**Property Testing**:
- `proptest` - Property-based testing
- `quickcheck` - Alternative property testing

**Acceptance Testing**:
- `assert_cmd` - CLI testing
- `predicates` - Assertion helpers
- Cleanroom framework (custom)

### 11.2 Documentation

**Internal**:
- `/Users/sac/ggen/ggen-marketplace/docs/diagrams/testing-strategy.puml`
- `/Users/sac/ggen/ggen-marketplace/docs/diagrams/error-handling.puml`
- `/Users/sac/ggen/docs/v1-production-readiness.md`
- `/Users/sac/ggen/docs/testing/production-readiness-assessment.md`

**External**:
- Growing Object-Oriented Software, Guided by Tests (Freeman & Pryce)
- London School TDD patterns
- Mockall documentation

### 11.3 Test File Organization

```
tests/
├── london_tdd/
│   ├── lib.rs                          # Common mocks and utilities
│   ├── mocks/
│   │   ├── marketplace.rs              # Marketplace mock traits
│   │   ├── lifecycle.rs                # Lifecycle mock traits
│   │   └── template.rs                 # Template mock traits
│   ├── marketplace/
│   │   ├── registry_resilience_test.rs # P0-1
│   │   ├── search_ranking_test.rs      # P1-1
│   │   ├── concurrent_ops_test.rs      # P1-2
│   │   └── version_properties_test.rs  # P2-1
│   ├── lifecycle/
│   │   ├── dependency_resolution_test.rs # P0-2
│   │   ├── parallel_execution_test.rs    # P1-2
│   │   └── cache_invalidation_test.rs    # P2
│   ├── template_engine/
│   │   ├── error_context_test.rs         # P0-3
│   │   ├── render_properties_test.rs     # P2-1
│   │   └── rdf_integration_test.rs       # Existing
│   └── acceptance/
│       ├── marketplace_workflows_test.rs
│       ├── lifecycle_workflows_test.rs
│       └── error_recovery_test.rs
```

---

## 12. Next Steps

### 12.1 Immediate Actions (This Week)

1. ✅ **Review and approve this strategy** (1 hour)
2. ✅ **Create mock trait definitions** (2 hours)
3. ✅ **Write first P0 test** (registry resilience) (2 hours)
4. ✅ **Implement to make test pass** (4 hours)

### 12.2 Short-Term (Next 2 Weeks)

1. ✅ Complete all P0 tests (15-20 tests)
2. ✅ Complete all P1 tests (10-15 tests)
3. ✅ Refactor based on test feedback
4. ✅ Update documentation

### 12.3 Medium-Term (Next Month)

1. ✅ Add property-based tests (P2)
2. ✅ Create test patterns guide
3. ✅ Update production readiness report
4. ✅ Prepare for v1.3 release

---

## 13. Conclusion

This London School TDD strategy provides a systematic approach to implementing missing features and enhancing test coverage for ggen v1.2.0. By focusing on:

1. **Outside-in development** - Starting with acceptance tests
2. **Mock-first design** - Using mocks to define interfaces
3. **Behavior verification** - Testing object collaborations
4. **Continuous refactoring** - Improving design iteratively

We will achieve:

- ✅ **97/100 production readiness score** (from 94/100)
- ✅ **~95% test coverage** (from ~85%)
- ✅ **50-60 additional focused tests**
- ✅ **Improved error handling and resilience**
- ✅ **Better designed, more testable code**

**The London School approach emphasizes testing object conversations and behaviors, leading to cleaner interfaces and more maintainable code.**

---

**Strategy Document Version**: 1.0
**Created**: 2025-10-30
**Strategist**: London TDD Swarm Agent (Hive Mind)
**Status**: ✅ **READY FOR IMPLEMENTATION**
**Next Review**: After Phase 1 completion (Week 1)
