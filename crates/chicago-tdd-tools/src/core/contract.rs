//! Test Contracts as First-Class Types
//!
//! Provides compile-time test contracts that declare:
//! - Covered modules/capabilities/invariants
//! - Expected resource envelope (time, memory, network)
//! - Required environment (Docker, Weaver, OTEL, μ-kernel, etc.)
//! - Expected Q constraints (e.g. τ ≤ 8, "no panics", "no blocking IO")
//!
//! Test contracts exist before any test executes, enabling:
//! - Static verification that all invariants have coverage
//! - Static verification that all capabilities have tests
//! - Contract queries without running tests
//! - Building test ontology Σₜ for reasoning
//!
//! # Architecture
//!
//! ```text
//! TestContract (const-evaluable)
//!     ↓
//! Coverage: &[&str]      // Modules/capabilities
//! Invariants: &[&str]    // Q₁, Q₂, ... Qₙ
//! Resources: ResourceEnvelope
//! Environment: &[&str]   // Docker, Weaver, etc.
//!     ↓
//! Registry: &[TestContract]  // Compile-time array
//! ```

use core::fmt;

/// Resource envelope for test execution
///
/// Declares expected resource usage for a test. All values are const-evaluable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResourceEnvelope {
    /// Maximum time budget in ticks (τ for hot path = 8)
    pub max_ticks: u64,
    /// Maximum memory usage in bytes
    pub max_memory_bytes: u64,
    /// Requires network access
    pub requires_network: bool,
    /// Requires storage access
    pub requires_storage: bool,
    /// Requires privileged operations
    pub requires_privileged: bool,
}

impl ResourceEnvelope {
    /// Create a new resource envelope with specified constraints
    #[must_use]
    pub const fn new(
        max_ticks: u64, max_memory_bytes: u64, requires_network: bool, requires_storage: bool,
        requires_privileged: bool,
    ) -> Self {
        Self {
            max_ticks,
            max_memory_bytes,
            requires_network,
            requires_storage,
            requires_privileged,
        }
    }

    /// Hot path envelope: τ ≤ 8, minimal resources, no IO
    #[must_use]
    pub const fn hot_path() -> Self {
        Self {
            max_ticks: 8,
            max_memory_bytes: 4096, // 4KB stack only
            requires_network: false,
            requires_storage: false,
            requires_privileged: false,
        }
    }

    /// Warm envelope: sub-millisecond, heap allowed, no external IO
    #[must_use]
    pub const fn warm() -> Self {
        Self {
            max_ticks: 500_000,          // ~125μs at 4GHz
            max_memory_bytes: 1_048_576, // 1MB
            requires_network: false,
            requires_storage: false,
            requires_privileged: false,
        }
    }

    /// Cold envelope: integration test, full resources
    #[must_use]
    pub const fn cold() -> Self {
        Self {
            max_ticks: u64::MAX,        // No tick limit for integration
            max_memory_bytes: u64::MAX, // No memory limit
            requires_network: true,
            requires_storage: true,
            requires_privileged: false,
        }
    }

    /// Check if this envelope requires Docker
    #[must_use]
    pub const fn requires_docker(&self) -> bool {
        // Docker is required for tests that need network AND storage (typical integration)
        self.requires_network && self.requires_storage
    }

    /// Check if this is a hot path test
    #[must_use]
    pub const fn is_hot_path(&self) -> bool {
        self.max_ticks <= 8
    }

    /// Check if this is a warm path test
    #[must_use]
    pub const fn is_warm_path(&self) -> bool {
        self.max_ticks > 8 && self.max_ticks < 1_000_000
    }

    /// Check if this is a cold path test
    #[must_use]
    pub const fn is_cold_path(&self) -> bool {
        self.max_ticks >= 1_000_000
    }
}

/// Test classification by thermal profile
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestThermalClass {
    /// Hot: τ ≤ 8, no allocations, no syscalls, μ-kernel timing discipline
    Hot,
    /// Warm: sub-ms, heap allowed, no external IO
    Warm,
    /// Cold: integration, full resources, Docker/Weaver/OTEL
    Cold,
}

impl fmt::Display for TestThermalClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Hot => write!(f, "hot"),
            Self::Warm => write!(f, "warm"),
            Self::Cold => write!(f, "cold"),
        }
    }
}

/// Test contract: compile-time declaration of test properties
///
/// **Poka-Yoke**: Test contracts are const-evaluable and exist at compile time.
/// This enables static verification of coverage and constraints without running tests.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::contract::{TestContract, ResourceEnvelope};
///
/// const MY_TEST_CONTRACT: TestContract = TestContract {
///     name: "test_hot_path_operation",
///     coverage: &["core::hot_path", "validation::guards"],
///     invariants: &["τ ≤ 8", "no_allocations", "no_panics"],
///     resources: ResourceEnvelope::hot_path(),
///     environment: &[], // No special environment needed
/// };
/// ```
#[derive(Debug, Clone, Copy)]
pub struct TestContract {
    /// Test name (function name)
    pub name: &'static str,
    /// Covered modules/capabilities (e.g., `"core::fixture"`, `"validation::guards"`)
    pub coverage: &'static [&'static str],
    /// Invariants checked by this test (e.g., `"τ ≤ 8"`, `"no_panics"`, `"no_allocations"`)
    pub invariants: &'static [&'static str],
    /// Resource envelope
    pub resources: ResourceEnvelope,
    /// Required environment (e.g., "Docker", "Weaver", "OTEL", "μ-kernel")
    pub environment: &'static [&'static str],
}

impl TestContract {
    /// Create a new test contract
    #[must_use]
    pub const fn new(
        name: &'static str, coverage: &'static [&'static str], invariants: &'static [&'static str],
        resources: ResourceEnvelope, environment: &'static [&'static str],
    ) -> Self {
        Self {
            name,
            coverage,
            invariants,
            resources,
            environment,
        }
    }

    /// Create a hot path test contract
    ///
    /// Hot path contracts enforce:
    /// - τ ≤ 8 ticks
    /// - No allocations
    /// - No syscalls
    /// - No panics
    #[must_use]
    pub const fn hot_path(name: &'static str, coverage: &'static [&'static str]) -> Self {
        Self {
            name,
            coverage,
            invariants: &["τ ≤ 8", "no_allocations", "no_syscalls", "no_panics"],
            resources: ResourceEnvelope::hot_path(),
            environment: &["μ-kernel"],
        }
    }

    /// Create a warm path test contract
    #[must_use]
    pub const fn warm_path(
        name: &'static str, coverage: &'static [&'static str], invariants: &'static [&'static str],
    ) -> Self {
        Self {
            name,
            coverage,
            invariants,
            resources: ResourceEnvelope::warm(),
            environment: &[],
        }
    }

    /// Create a cold path test contract
    #[must_use]
    pub const fn cold_path(
        name: &'static str, coverage: &'static [&'static str], environment: &'static [&'static str],
    ) -> Self {
        Self {
            name,
            coverage,
            invariants: &["integration"], // Cold tests verify integration
            resources: ResourceEnvelope::cold(),
            environment,
        }
    }

    /// Get thermal classification of this test
    #[must_use]
    pub const fn thermal_class(&self) -> TestThermalClass {
        if self.resources.is_hot_path() {
            TestThermalClass::Hot
        } else if self.resources.is_warm_path() {
            TestThermalClass::Warm
        } else {
            TestThermalClass::Cold
        }
    }

    /// Check if this test requires Docker
    #[must_use]
    pub const fn requires_docker(&self) -> bool {
        self.resources.requires_docker()
    }

    /// Check if this test covers a specific module
    #[must_use]
    pub fn covers_module(&self, module: &str) -> bool {
        self.coverage.contains(&module)
    }

    /// Check if this test verifies a specific invariant (runtime)
    #[must_use]
    pub fn verifies_invariant(&self, invariant: &str) -> bool {
        self.invariants.contains(&invariant)
    }

    /// Check if this test covers a specific module (runtime)
    #[must_use]
    pub fn covers_module_runtime(&self, module: &str) -> bool {
        self.coverage.contains(&module)
    }
}

/// Test contract registry: compile-time array of all test contracts
///
/// This enables static queries like:
/// - "Which tests cover capability C?"
/// - "Do we have tests for all invariants?"
/// - "Which tests require Docker?"
#[derive(Debug, Clone, Copy)]
pub struct TestContractRegistry {
    contracts: &'static [TestContract],
}

impl TestContractRegistry {
    /// Create a new registry from a static array of contracts
    #[must_use]
    pub const fn new(contracts: &'static [TestContract]) -> Self {
        Self { contracts }
    }

    /// Get all contracts
    #[must_use]
    pub const fn all(&self) -> &'static [TestContract] {
        self.contracts
    }

    /// Get all hot path tests
    #[must_use]
    pub fn hot_path_tests(&self) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.resources.is_hot_path())
            .collect()
    }

    /// Get all warm path tests
    #[must_use]
    pub fn warm_path_tests(&self) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.resources.is_warm_path())
            .collect()
    }

    /// Get all cold path tests
    #[must_use]
    pub fn cold_path_tests(&self) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.resources.is_cold_path())
            .collect()
    }

    /// Get all tests that cover a specific module
    #[must_use]
    pub fn tests_covering_module(&self, module: &str) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.covers_module_runtime(module))
            .collect()
    }

    /// Get all tests that verify a specific invariant
    #[must_use]
    pub fn tests_verifying_invariant(&self, invariant: &str) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.verifies_invariant(invariant))
            .collect()
    }

    /// Get all tests that require a specific environment
    #[must_use]
    pub fn tests_requiring_environment(&self, env: &str) -> Vec<&'static TestContract> {
        self.contracts
            .iter()
            .filter(|c| c.environment.contains(&env))
            .collect()
    }

    /// Check coverage: do we have at least one test for each invariant?
    ///
    /// Returns a list of invariants that have no test coverage.
    #[must_use]
    pub fn uncovered_invariants<'a>(&self, required_invariants: &[&'a str]) -> Vec<&'a str> {
        required_invariants
            .iter()
            .filter(|&&inv| !self.contracts.iter().any(|c| c.verifies_invariant(inv)))
            .copied()
            .collect()
    }

    /// Check coverage: do we have at least one test for each module?
    ///
    /// Returns a list of modules that have no test coverage.
    #[must_use]
    pub fn uncovered_modules<'a>(&self, required_modules: &[&'a str]) -> Vec<&'a str> {
        required_modules
            .iter()
            .filter(|&&module| {
                !self
                    .contracts
                    .iter()
                    .any(|c| c.covers_module_runtime(module))
            })
            .copied()
            .collect()
    }

    /// Total number of contracts in registry
    #[must_use]
    pub const fn len(&self) -> usize {
        self.contracts.len()
    }

    /// Check if registry is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.contracts.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resource_envelope_hot_path() {
        let envelope = ResourceEnvelope::hot_path();
        assert_eq!(envelope.max_ticks, 8);
        assert_eq!(envelope.max_memory_bytes, 4096);
        assert!(!envelope.requires_network);
        assert!(!envelope.requires_storage);
        assert!(!envelope.requires_privileged);
        assert!(envelope.is_hot_path());
    }

    #[test]
    fn test_resource_envelope_warm_path() {
        let envelope = ResourceEnvelope::warm();
        assert!(envelope.max_ticks > 8);
        assert!(envelope.max_ticks < 1_000_000);
        assert!(!envelope.requires_network);
        assert!(envelope.is_warm_path());
    }

    #[test]
    fn test_resource_envelope_cold_path() {
        let envelope = ResourceEnvelope::cold();
        assert!(envelope.requires_network);
        assert!(envelope.requires_storage);
        assert!(envelope.is_cold_path());
        assert!(envelope.requires_docker());
    }

    #[test]
    fn test_contract_hot_path() {
        const CONTRACT: TestContract =
            TestContract::hot_path("test_hot_operation", &["core::hot_path"]);

        assert_eq!(CONTRACT.name, "test_hot_operation");
        assert_eq!(CONTRACT.coverage.len(), 1);
        assert_eq!(CONTRACT.coverage[0], "core::hot_path");
        assert!(CONTRACT.verifies_invariant("τ ≤ 8"));
        assert!(CONTRACT.verifies_invariant("no_allocations"));
        assert!(CONTRACT.resources.is_hot_path());
    }

    #[test]
    fn test_contract_warm_path() {
        const CONTRACT: TestContract = TestContract::warm_path(
            "test_warm_operation",
            &["validation::guards"],
            &["no_panics", "bounded_memory"],
        );

        assert_eq!(CONTRACT.name, "test_warm_operation");
        assert!(CONTRACT.verifies_invariant("no_panics"));
        assert!(CONTRACT.verifies_invariant("bounded_memory"));
        assert!(CONTRACT.resources.is_warm_path());
    }

    #[test]
    fn test_contract_cold_path() {
        const CONTRACT: TestContract = TestContract::cold_path(
            "test_integration",
            &["integration::docker"],
            &["Docker", "Weaver"],
        );

        assert_eq!(CONTRACT.name, "test_integration");
        assert!(CONTRACT.resources.is_cold_path());
        assert!(CONTRACT.requires_docker());
    }

    #[test]
    fn test_registry_queries() {
        const CONTRACTS: &[TestContract] = &[
            TestContract::hot_path("test_hot", &["core::hot"]),
            TestContract::warm_path("test_warm", &["core::warm"], &["no_panics"]),
            TestContract::cold_path("test_cold", &["integration"], &["Docker"]),
        ];

        let registry = TestContractRegistry::new(CONTRACTS);

        assert_eq!(registry.len(), 3);
        assert!(!registry.is_empty());

        let hot_tests = registry.hot_path_tests();
        assert_eq!(hot_tests.len(), 1);
        assert_eq!(hot_tests[0].name, "test_hot");

        let warm_tests = registry.warm_path_tests();
        assert_eq!(warm_tests.len(), 1);
        assert_eq!(warm_tests[0].name, "test_warm");

        let cold_tests = registry.cold_path_tests();
        assert_eq!(cold_tests.len(), 1);
        assert_eq!(cold_tests[0].name, "test_cold");
    }

    #[test]
    fn test_registry_coverage_analysis() {
        const CONTRACTS: &[TestContract] = &[
            TestContract::hot_path("test_hot", &["core::hot"]),
            TestContract::warm_path("test_warm", &["core::warm"], &["no_panics"]),
        ];

        let registry = TestContractRegistry::new(CONTRACTS);

        // Check module coverage
        let core_hot_tests = registry.tests_covering_module("core::hot");
        assert_eq!(core_hot_tests.len(), 1);

        let core_warm_tests = registry.tests_covering_module("core::warm");
        assert_eq!(core_warm_tests.len(), 1);

        // Check invariant coverage
        let no_panic_tests = registry.tests_verifying_invariant("no_panics");
        assert_eq!(no_panic_tests.len(), 2); // Both hot and warm paths include no_panics

        let tau_tests = registry.tests_verifying_invariant("τ ≤ 8");
        assert_eq!(tau_tests.len(), 1); // Only hot path test

        // Check uncovered modules
        let required_modules = vec!["core::hot", "core::warm", "core::missing"];
        let uncovered = registry.uncovered_modules(&required_modules);
        assert_eq!(uncovered.len(), 1);
        assert_eq!(uncovered[0], "core::missing");

        // Check uncovered invariants
        let required_invariants = vec!["no_panics", "τ ≤ 8", "missing_invariant"];
        let uncovered = registry.uncovered_invariants(&required_invariants);
        assert_eq!(uncovered.len(), 1);
        assert_eq!(uncovered[0], "missing_invariant");
    }

    #[test]
    fn test_thermal_classification() {
        let hot = TestContract::hot_path("test", &[]);
        assert_eq!(hot.thermal_class(), TestThermalClass::Hot);

        let warm = TestContract::warm_path("test", &[], &[]);
        assert_eq!(warm.thermal_class(), TestThermalClass::Warm);

        let cold = TestContract::cold_path("test", &[], &[]);
        assert_eq!(cold.thermal_class(), TestThermalClass::Cold);
    }
}
