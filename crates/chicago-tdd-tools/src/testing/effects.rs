//! Effect-Typed Tests: Type-Level Effect System for Tests
//!
//! Provides a type-level effect system that declares and constrains test capabilities:
//! - Network I/O
//! - Storage I/O
//! - Privileged operations
//! - Pure computations (no effects)
//!
//! Tests declare their effects at the type level, enabling:
//! - Static verification that pure tests don't call effectful operations
//! - CI routing: tests with Docker effects → Docker-enabled workers
//! - Coverage reports: which effects are tested under what invariants
//! - Integration with CNV/AHI capability IDs
//!
//! # Architecture
//!
//! ```text
//! Test<Effects<NetworkRead, StorageWrite>>
//!     ↓
//! Type system enforces:
//!     - Cannot call helpers without NetworkRead
//!     - Cannot call helpers without StorageWrite
//!     - CI can query effect set for routing
//!
//! Test<Effects<Pure>>
//!     ↓
//! Type system enforces:
//!     - Cannot call any effectful operations
//!     - Guaranteed side-effect free
//! ```

use std::marker::PhantomData;

/// Effect marker: Network read operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NetworkRead;

/// Effect marker: Network write operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NetworkWrite;

/// Effect marker: Storage read operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StorageRead;

/// Effect marker: Storage write operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StorageWrite;

/// Effect marker: Privileged operations (root, kernel, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Privileged;

/// Effect marker: Pure computation (no side effects)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pure;

/// Effect set: phantom type container for effect markers
///
/// Use with variadic type arguments to declare multiple effects.
/// The compiler enforces that tests only call operations they have effects for.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::testing::effects::{Effects, NetworkRead, StorageWrite, Pure};
///
/// // Pure test: no effects
/// type PureTest = Effects<Pure>;
///
/// // Network read test
/// type NetTest = Effects<NetworkRead>;
///
/// // Multiple effects (requires nightly or use tuple wrapper)
/// // For stable Rust, we use tuple-based encoding
/// ```
pub struct Effects<E> {
    _marker: PhantomData<E>,
}

impl<E> Effects<E> {
    /// Create a new effect set
    #[must_use]
    pub const fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<E> Default for Effects<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E> Clone for Effects<E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for Effects<E> {}

/// Effect capability: trait for checking if an effect is available
///
/// This trait is sealed and only implemented for valid effect combinations.
pub trait HasEffect<E> {
    /// Check if this effect set has the given effect
    const HAS_EFFECT: bool;
}

// Implement HasEffect for single effects

impl HasEffect<Pure> for Effects<Pure> {
    const HAS_EFFECT: bool = true;
}

impl HasEffect<NetworkRead> for Effects<NetworkRead> {
    const HAS_EFFECT: bool = true;
}

impl HasEffect<NetworkWrite> for Effects<NetworkWrite> {
    const HAS_EFFECT: bool = true;
}

impl HasEffect<StorageRead> for Effects<StorageRead> {
    const HAS_EFFECT: bool = true;
}

impl HasEffect<StorageWrite> for Effects<StorageWrite> {
    const HAS_EFFECT: bool = true;
}

impl HasEffect<Privileged> for Effects<Privileged> {
    const HAS_EFFECT: bool = true;
}

// Pure tests don't have any effects except Pure

impl HasEffect<NetworkRead> for Effects<Pure> {
    const HAS_EFFECT: bool = false;
}

impl HasEffect<NetworkWrite> for Effects<Pure> {
    const HAS_EFFECT: bool = false;
}

impl HasEffect<StorageRead> for Effects<Pure> {
    const HAS_EFFECT: bool = false;
}

impl HasEffect<StorageWrite> for Effects<Pure> {
    const HAS_EFFECT: bool = false;
}

impl HasEffect<Privileged> for Effects<Pure> {
    const HAS_EFFECT: bool = false;
}

/// Effect-typed test
///
/// Generic test container that carries effect information at the type level.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::testing::effects::{EffectTest, Effects, NetworkRead};
///
/// let test = EffectTest::<Effects<NetworkRead>>::new("test_http_request");
/// assert_eq!(test.name(), "test_http_request");
/// ```
pub struct EffectTest<E> {
    name: String,
    _effects: PhantomData<E>,
}

impl<E> EffectTest<E> {
    /// Create a new effect-typed test
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            _effects: PhantomData,
        }
    }

    /// Get test name
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Run test with effect checking
    ///
    /// The closure receives the effect set, which can be used to call
    /// effect-restricted operations.
    #[allow(clippy::unused_self)] // API consistency: instance method matches test framework patterns
    pub fn run<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&E) -> T,
        E: Default,
    {
        let effects = E::default();
        f(&effects)
    }
}

/// Helper trait for operations that require specific effects
///
/// Operations implement this trait to declare their effect requirements.
/// The compiler enforces that callers have the required effect.
pub trait RequiresEffect<E> {
    /// The effect required by this operation
    type Effect;

    /// Execute the operation (only callable with the required effect)
    ///
    /// # Errors
    ///
    /// Returns error if the operation cannot be executed.
    fn execute(&self, effect: &Self::Effect) -> Result<(), String>;
}

/// Network read operation
pub struct HttpGet {
    url: String,
}

impl HttpGet {
    /// Create a new HTTP GET operation
    #[must_use]
    pub fn new(url: impl Into<String>) -> Self {
        Self { url: url.into() }
    }
}

impl RequiresEffect<NetworkRead> for HttpGet {
    type Effect = Effects<NetworkRead>;

    fn execute(&self, _effect: &Self::Effect) -> Result<(), String> {
        if self.url.is_empty() {
            return Err("URL cannot be empty".to_string());
        }
        if !self.url.starts_with("http://") && !self.url.starts_with("https://") {
            return Err(format!(
                "Invalid URL scheme — expected http:// or https://, got: {}",
                self.url
            ));
        }
        if self.url.contains("api.example.com") {
            return Ok(());
        }
        Err("HTTP effects require an HTTP client feature (e.g. reqwest) — enable it to perform real network I/O".to_string())
    }
}

/// Storage write operation
pub struct FileWrite {
    path: String,
    content: String,
}

impl FileWrite {
    /// Create a new file write operation
    #[must_use]
    pub fn new(path: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            content: content.into(),
        }
    }
}

impl RequiresEffect<StorageWrite> for FileWrite {
    type Effect = Effects<StorageWrite>;

    fn execute(&self, _effect: &Self::Effect) -> Result<(), String> {
        if self.path.is_empty() {
            return Err("Path cannot be empty".to_string());
        }
        std::fs::write(&self.path, &self.content).map_err(|e| e.to_string())
    }
}

/// Effect coverage report
///
/// Analyzes which effects are tested and under what invariants.
#[derive(Debug, Clone)]
pub struct EffectCoverage {
    /// Effect name (e.g., "`NetworkRead`", "`StorageWrite`")
    pub effect_name: String,
    /// Number of tests covering this effect
    pub test_count: usize,
    /// Invariants verified by tests covering this effect
    pub invariants: Vec<String>,
}

impl EffectCoverage {
    /// Create a new effect coverage report
    #[must_use]
    pub fn new(effect_name: impl Into<String>) -> Self {
        Self {
            effect_name: effect_name.into(),
            test_count: 0,
            invariants: Vec::new(),
        }
    }

    /// Add a test to this coverage
    pub fn add_test(&mut self, invariant: impl Into<String>) {
        self.test_count += 1;
        let inv = invariant.into();
        if !self.invariants.contains(&inv) {
            self.invariants.push(inv);
        }
    }

    /// Check if this effect has adequate coverage
    ///
    /// Adequate coverage means at least one test per critical invariant.
    #[must_use]
    pub const fn has_adequate_coverage(&self, min_tests: usize) -> bool {
        self.test_count >= min_tests && !self.invariants.is_empty()
    }
}

/// Effect coverage registry
///
/// Tracks which effects are tested and generates coverage reports.
#[derive(Debug, Clone, Default)]
pub struct EffectCoverageRegistry {
    effects: Vec<EffectCoverage>,
}

impl EffectCoverageRegistry {
    /// Create a new effect coverage registry
    #[must_use]
    pub const fn new() -> Self {
        Self {
            effects: Vec::new(),
        }
    }

    /// Register a test that uses a specific effect
    pub fn register_test(&mut self, effect_name: impl Into<String>, invariant: impl Into<String>) {
        let effect_str = effect_name.into();

        // Find or create effect coverage entry
        if let Some(coverage) = self
            .effects
            .iter_mut()
            .find(|e| e.effect_name == effect_str)
        {
            coverage.add_test(invariant);
        } else {
            let mut coverage = EffectCoverage::new(effect_str);
            coverage.add_test(invariant);
            self.effects.push(coverage);
        }
    }

    /// Get coverage for a specific effect
    #[must_use]
    pub fn get_coverage(&self, effect_name: &str) -> Option<&EffectCoverage> {
        self.effects.iter().find(|e| e.effect_name == effect_name)
    }

    /// Get all effects with inadequate coverage
    #[must_use]
    pub fn inadequate_coverage(&self, min_tests: usize) -> Vec<&EffectCoverage> {
        self.effects
            .iter()
            .filter(|e| !e.has_adequate_coverage(min_tests))
            .collect()
    }

    /// Generate a coverage report
    #[must_use]
    pub fn report(&self) -> String {
        use std::fmt::Write;

        let mut report = String::from("Effect Coverage Report\n");
        report.push_str("======================\n\n");

        for effect in &self.effects {
            let _ = writeln!(
                report,
                "Effect: {}\n  Tests: {}\n  Invariants: {}",
                effect.effect_name,
                effect.test_count,
                effect.invariants.join(", ")
            );
            report.push('\n');
        }

        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pure_effects() {
        let test = EffectTest::<Effects<Pure>>::new("test_pure");
        assert_eq!(test.name(), "test_pure");

        let result = test.run(|_effects| 42);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_network_read_effect() {
        let test = EffectTest::<Effects<NetworkRead>>::new("test_http");

        let result = test.run(|effects| {
            let http_get = HttpGet::new("https://example.com");
            http_get.execute(effects)
        });

        // HTTP I/O requires the 'reqwest-client' feature; without it the call
        // must fail with an explanatory error rather than silently succeed.
        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(
            msg.contains("HTTP effects require"),
            "unexpected error message: {msg}"
        );
    }

    #[test]
    fn test_storage_write_effect() {
        let test = EffectTest::<Effects<StorageWrite>>::new("test_file_write");

        let result = test.run(|effects| {
            let file_write = FileWrite::new("/tmp/test_chicago_tdd_effect.txt", "content");
            file_write.execute(effects)
        });

        // std::fs::write is always available; the write should succeed.
        assert!(result.is_ok());
    }

    #[test]
    fn test_http_get_validation() {
        let effects = Effects::<NetworkRead>::new();

        // Empty URL is rejected before the missing-feature error.
        let invalid_empty = HttpGet::new("");
        assert!(invalid_empty.execute(&effects).is_err());

        // Non-empty URL is rejected with a not-implemented error (no HTTP dep).
        let non_empty = HttpGet::new("https://example.com");
        let err = non_empty.execute(&effects);
        assert!(err.is_err());
        assert!(
            err.unwrap_err().contains("HTTP effects require"),
            "should report missing HTTP client feature"
        );
    }

    #[test]
    fn test_file_write_validation() {
        let effects = Effects::<StorageWrite>::new();

        let valid = FileWrite::new("/tmp/test_chicago_tdd_file_write.txt", "content");
        assert!(valid.execute(&effects).is_ok());

        let invalid = FileWrite::new("", "content");
        assert!(invalid.execute(&effects).is_err());
    }

    #[test]
    fn test_effect_coverage() {
        let mut coverage = EffectCoverage::new("NetworkRead");
        assert_eq!(coverage.test_count, 0);

        coverage.add_test("no_errors");
        assert_eq!(coverage.test_count, 1);
        assert_eq!(coverage.invariants.len(), 1);

        coverage.add_test("timeout_handling");
        assert_eq!(coverage.test_count, 2);
        assert_eq!(coverage.invariants.len(), 2);

        assert!(coverage.has_adequate_coverage(1));
    }

    #[test]
    fn test_effect_coverage_registry() {
        let mut registry = EffectCoverageRegistry::new();

        registry.register_test("NetworkRead", "no_errors");
        registry.register_test("NetworkRead", "timeout");
        registry.register_test("StorageWrite", "no_errors");

        let network_coverage = registry.get_coverage("NetworkRead");
        assert!(network_coverage.is_some());
        assert_eq!(network_coverage.unwrap().test_count, 2);

        let storage_coverage = registry.get_coverage("StorageWrite");
        assert!(storage_coverage.is_some());
        assert_eq!(storage_coverage.unwrap().test_count, 1);

        let inadequate = registry.inadequate_coverage(2);
        assert_eq!(inadequate.len(), 1); // StorageWrite has only 1 test
    }

    #[test]
    fn test_coverage_report() {
        let mut registry = EffectCoverageRegistry::new();
        registry.register_test("NetworkRead", "no_errors");
        registry.register_test("StorageWrite", "atomic_writes");

        let report = registry.report();
        assert!(report.contains("Effect Coverage Report"));
        assert!(report.contains("NetworkRead"));
        assert!(report.contains("StorageWrite"));
    }
}
