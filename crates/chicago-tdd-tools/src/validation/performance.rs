//! Performance Validation
//!
//! Provides hardware-counter benchmarking and tick measurement utilities for hot
//! path validation.
//!
//! # The Chatman Constant is a logical step count, not a time budget
//!
//! The Chatman Constant (8 ticks) is a count of deterministic logical operations.
//! The target runtime is WASM, where timing guarantees do not exist. The timer-based
//! measurements in this module are an INFORMATIONAL, native-only signal and must not
//! be used as the Chatman Constant gate. Assert the constant via explicit step
//! counting or structural verification of the operation, not via timers.
//!
//! # What a "tick" means here (platform-dependent)
//!
//! - `x86_64`: `rdtsc` — CPU reference cycles (frequency-dependent).
//! - `aarch64` (e.g. Apple Silicon): `cntvct_el0` — a fixed-frequency wall-clock
//!   timer (~1 GHz), so 1 tick ≈ 1 ns of wall time, NOT CPU cycles. "8 ticks = 2ns"
//!   does not hold here.
//! - Other platforms: `SystemTime` nanoseconds fallback.
//!
//! # Poka-Yoke: Type-Level Validation
//!
//! This module provides both runtime validation (for dynamic cases) and compile-time
//! validation (for known budgets). Use `ValidatedTickBudget<const BUDGET: u64>` for
//! compile-time validated tick budgets.

use crate::core::const_assert::Validated;
use thiserror::Error;

/// Performance validation error
#[derive(Error, Debug)]
pub enum PerformanceValidationError {
    /// Tick budget exceeded
    #[error("Tick budget exceeded: {0} > {1} (Chatman Constant violation)")]
    TickBudgetExceeded(u64, u64),
    /// Invalid measurement
    #[error("Invalid measurement: {0}")]
    InvalidMeasurement(String),
    /// Measurement failed
    #[error("Measurement failed: {0}")]
    MeasurementFailed(String),
}

/// Result type for performance validation
pub type PerformanceValidationResult<T> = Result<T, PerformanceValidationError>;

/// Tick budget for hot path operations (Chatman Constant: 8 logical steps).
///
/// This is a deterministic operation count, not a wall-clock budget. Timer-based
/// checks against it are informational and platform-dependent (see module docs);
/// on `aarch64` one hardware "tick" is ~1 ns of wall time, not a cycle.
pub const HOT_PATH_TICK_BUDGET: u64 = 8;

/// Tick counter using RDTSC (Read Time-Stamp Counter)
///
/// On `x86_64`, uses `rdtsc` instruction for cycle counting.
/// On other platforms, uses `std::time::Instant` as fallback.
pub struct TickCounter {
    /// Start tick count
    start_ticks: u64,
}

impl TickCounter {
    /// Create a new tick counter and start counting
    #[must_use]
    pub fn start() -> Self {
        Self { start_ticks: Self::read_ticks() }
    }

    /// Read current tick count
    fn read_ticks() -> u64 {
        #[cfg(target_arch = "x86_64")]
        {
            // SAFETY: RDTSC is safe on x86_64 - it's a read-only instruction
            #[allow(unsafe_code)]
            unsafe {
                std::arch::x86_64::_rdtsc()
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            // ARM64: Use CNTVCT_EL0 (Virtual Count Register)
            // SAFETY: Reading CNTVCT_EL0 is safe - it's a read-only register
            let val: u64;
            #[allow(unsafe_code)]
            unsafe {
                std::arch::asm!(
                    "mrs {}, cntvct_el0",
                    out(reg) val,
                    options(nostack, nomem)
                );
            }
            val
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            // Fallback: use SystemTime for non-x86_64/ARM64 platforms
            #[allow(clippy::cast_possible_truncation)]
            // Nanoseconds won't exceed u64::MAX for reasonable durations
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos() as u64
        }
    }

    /// Get elapsed ticks since start
    #[must_use]
    pub fn elapsed_ticks(&self) -> u64 {
        Self::read_ticks().saturating_sub(self.start_ticks)
    }

    /// Check if elapsed ticks exceed budget
    #[must_use]
    pub fn exceeds_budget(&self, budget: u64) -> bool {
        self.elapsed_ticks() > budget
    }

    /// Assert that elapsed ticks are within budget
    ///
    /// # Errors
    ///
    /// Returns an error if ticks exceed the specified budget.
    pub fn assert_within_budget(&self, budget: u64) -> PerformanceValidationResult<()> {
        // τ enforcement is uniform across debug and release builds — see Chatman Constant
        let elapsed = self.elapsed_ticks();
        if elapsed > budget {
            return Err(PerformanceValidationError::TickBudgetExceeded(elapsed, budget));
        }
        Ok(())
    }

    /// Assert that elapsed ticks are within hot path budget (≤8 ticks)
    ///
    /// # Errors
    ///
    /// Returns an error if ticks exceed hot path budget.
    pub fn assert_within_hot_path_budget(&self) -> PerformanceValidationResult<()> {
        self.assert_within_budget(HOT_PATH_TICK_BUDGET)
    }
}

// ============================================================================
// Poka-Yoke: Compile-Time Validated Tick Budget
// ============================================================================

/// Compile-time validated tick budget
///
/// **Poka-Yoke**: This type enforces tick budget validation at compile time using const generics.
/// Use this for known tick budgets to prevent errors at compile time.
///
/// # Example
///
/// ```rust,no_run
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use chicago_tdd_tools::performance::{ValidatedTickBudget, TickCounter};
///
/// // Compile-time validated - BUDGET must be known at compile time
/// fn validate_hot_path<const BUDGET: u64>(counter: &TickCounter) -> chicago_tdd_tools::performance::PerformanceValidationResult<()> {
///     let budget = ValidatedTickBudget::<BUDGET>::new();
///     budget.assert_within_budget(counter)
/// }
///
/// // Valid - BUDGET = 8 (hot path budget)
/// let counter = TickCounter::start();
/// // ... do work ...
/// let budget = ValidatedTickBudget::<8>::new();
/// budget.assert_within_budget(&counter)?;
///
/// // Runtime validation for dynamic budgets
/// counter.assert_within_budget(10)?; // Dynamic budget
/// # Ok(())
/// # }
/// ```
pub struct ValidatedTickBudget<const BUDGET: u64> {
    /// Validated budget value
    _inner: Validated<u64>,
}

impl<const BUDGET: u64> ValidatedTickBudget<BUDGET> {
    /// Create a new validated tick budget
    ///
    /// The budget is validated at compile time through the const generic parameter.
    #[must_use]
    pub const fn new() -> Self {
        Self { _inner: Validated::new(BUDGET) }
    }

    /// Get the budget value
    ///
    /// This is guaranteed to be BUDGET at compile time.
    #[must_use]
    #[allow(clippy::unused_self)] // Required for trait consistency - const fn needs self
    pub const fn budget(&self) -> u64 {
        BUDGET
    }

    /// Assert that elapsed ticks are within this budget
    ///
    /// # Errors
    ///
    /// Returns `PerformanceValidationError::TickBudgetExceeded` if elapsed ticks exceed the budget.
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn assert_within_budget(&self, counter: &TickCounter) -> PerformanceValidationResult<()> {
        counter.assert_within_budget(BUDGET)
    }
}

impl<const BUDGET: u64> Default for ValidatedTickBudget<BUDGET> {
    fn default() -> Self {
        Self::new()
    }
}

/// Measure ticks for a closure
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::performance::measure_ticks;
///
/// // Helper function for doctest
/// # fn hot_path_operation() -> i32 {
/// #     42
/// # }
///
/// let (result, ticks) = measure_ticks(|| {
///     // Hot path operation
///     hot_path_operation()
/// });
///
/// assert_eq!(result, 42);
/// // Note: ticks may vary - very fast operations may have 0 ticks
/// assert!(ticks >= 0);
/// ```
pub fn measure_ticks<F, T>(f: F) -> (T, u64)
where
    F: FnOnce() -> T,
{
    let counter = TickCounter::start();
    let result = f();
    let ticks = counter.elapsed_ticks();
    (result, ticks)
}

/// Measure ticks for an async operation
///
/// # Example
///
/// ```rust
/// # #[tokio::main]
/// # async fn main() {
/// use chicago_tdd_tools::performance::measure_ticks_async;
///
/// let (result, ticks) = measure_ticks_async(async || {
///     // Async hot path operation
///     42
/// }).await;
///
/// assert_eq!(result, 42);
/// assert!(ticks >= 0);
/// # }
/// ```
#[cfg(feature = "async")]
pub async fn measure_ticks_async<F, Fut, T>(f: F) -> (T, u64)
where
    F: FnOnce() -> Fut,
    Fut: std::future::Future<Output = T>,
{
    let counter = TickCounter::start();
    let result = f().await;
    let ticks = counter.elapsed_ticks();
    (result, ticks)
}

/// Zero-cost abstraction for tick measurement
///
/// This wrapper provides a zero-cost abstraction over tick measurement,
/// allowing for flexible measurement strategies without runtime overhead.
pub struct TickMeasurer<F> {
    /// Function to measure
    f: F,
}

impl<F> TickMeasurer<F> {
    /// Create a new tick measurer
    pub const fn new(f: F) -> Self {
        Self { f }
    }

    /// Measure ticks for the function
    pub fn measure<T>(self) -> (T, u64)
    where
        F: FnOnce() -> T,
    {
        let counter = TickCounter::start();
        let result = (self.f)();
        let ticks = counter.elapsed_ticks();
        (result, ticks)
    }
}

/// Zero-cost abstraction for async tick measurement
#[cfg(feature = "async")]
pub struct AsyncTickMeasurer<F> {
    /// Async function to measure
    f: F,
}

#[cfg(feature = "async")]
impl<F> AsyncTickMeasurer<F> {
    /// Create a new async tick measurer
    pub const fn new(f: F) -> Self {
        Self { f }
    }

    /// Measure ticks for the async function
    pub async fn measure<T, Fut>(self) -> (T, u64)
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = T>,
    {
        let counter = TickCounter::start();
        let result = (self.f)().await;
        let ticks = counter.elapsed_ticks();
        (result, ticks)
    }
}

/// Performance benchmark result
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    /// Operation name
    pub operation: String,
    /// Number of iterations
    pub iterations: u64,
    /// Total ticks
    pub total_ticks: u64,
    /// Average ticks per iteration
    pub avg_ticks: f64,
    /// Minimum ticks
    pub min_ticks: u64,
    /// Maximum ticks
    pub max_ticks: u64,
    /// P50 ticks (median)
    pub p50_ticks: u64,
    /// P95 ticks
    pub p95_ticks: u64,
    /// P99 ticks
    pub p99_ticks: u64,
}

impl BenchmarkResult {
    /// Check if benchmark meets hot path budget
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Hot path budget comparison - precision loss acceptable
    pub fn meets_hot_path_budget(&self) -> bool {
        self.avg_ticks <= HOT_PATH_TICK_BUDGET as f64
    }

    /// Check if P95 meets hot path budget
    #[must_use]
    pub const fn p95_meets_hot_path_budget(&self) -> bool {
        self.p95_ticks <= HOT_PATH_TICK_BUDGET
    }

    /// Format benchmark result as string
    #[must_use]
    pub fn format(&self) -> String {
        let compliant = if self.meets_hot_path_budget() { "YES" } else { "NO" };
        format!(
            "Operation: {}\n  Iterations: {}\n  Avg ticks: {:.2}\n  Min: {} | Max: {} | P50: {} | P95: {} | P99: {}\n  Hot path compliant: {}",
            self.operation,
            self.iterations,
            self.avg_ticks,
            self.min_ticks,
            self.max_ticks,
            self.p50_ticks,
            self.p95_ticks,
            self.p99_ticks,
            compliant
        )
    }
}

/// Benchmark a closure multiple times
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::performance::benchmark;
///
/// // Helper function for doctest
/// # fn hot_path_operation() -> i32 {
/// #     42
/// # }
///
/// let result = benchmark("hot_path_operation", 1000, || {
///     hot_path_operation()
/// });
///
/// assert_eq!(result.operation, "hot_path_operation");
/// assert_eq!(result.iterations, 1000);
/// // Note: Benchmark results are timing-dependent, just verify data is collected
/// assert!(result.iterations > 0);
/// ```
pub fn benchmark<F, T>(operation: &str, iterations: u64, f: F) -> BenchmarkResult
where
    F: Fn() -> T,
{
    // Constants must be declared before statements (Rust requirement)
    const BENCHMARK_WARMUP_ITERATIONS: u64 = 100;
    const PERCENTILE_50: u8 = 50;
    const PERCENTILE_95: u8 = 95;
    const PERCENTILE_99: u8 = 99;

    #[allow(clippy::cast_possible_truncation)]
    // u64 to usize - iterations won't exceed usize::MAX in practice
    let mut tick_samples = Vec::with_capacity(iterations as usize);

    // Warmup
    for _ in 0..BENCHMARK_WARMUP_ITERATIONS {
        let _ = f();
    }

    // Measure
    // Note: measure_ticks takes FnOnce, so we need a closure to call f multiple times
    #[allow(clippy::redundant_closure)]
    for _ in 0..iterations {
        let (_, ticks) = measure_ticks(|| f());
        tick_samples.push(ticks);
    }

    // Calculate statistics
    tick_samples.sort_unstable();

    // Handle empty samples case
    if tick_samples.is_empty() {
        return BenchmarkResult {
            operation: operation.to_string(),
            iterations: 0,
            total_ticks: 0,
            avg_ticks: 0.0,
            min_ticks: 0,
            max_ticks: 0,
            p50_ticks: 0,
            p95_ticks: 0,
            p99_ticks: 0,
        };
    }

    let total_ticks: u64 = tick_samples.iter().sum();
    #[allow(clippy::cast_precision_loss)]
    // Average calculation - precision loss acceptable for benchmark statistics
    let avg_ticks = total_ticks as f64 / iterations as f64;
    let min_ticks = tick_samples[0];
    let max_ticks = tick_samples[tick_samples.len() - 1];
    let p50_idx = (tick_samples.len() * PERCENTILE_50 as usize / 100).saturating_sub(1);
    let p95_idx = (tick_samples.len() * PERCENTILE_95 as usize / 100).saturating_sub(1);
    let p99_idx = (tick_samples.len() * PERCENTILE_99 as usize / 100).saturating_sub(1);

    // **Kaizen improvement**: Clarified percentile fallback strategy.
    // When percentile index is out of bounds (empty or very small sample),
    // use max_ticks as fallback - this provides a conservative upper bound
    // that represents the worst-case performance observed.
    BenchmarkResult {
        operation: operation.to_string(),
        iterations,
        total_ticks,
        avg_ticks,
        min_ticks,
        max_ticks,
        p50_ticks: tick_samples.get(p50_idx).copied().unwrap_or(max_ticks),
        p95_ticks: tick_samples.get(p95_idx).copied().unwrap_or(max_ticks),
        p99_ticks: tick_samples.get(p99_idx).copied().unwrap_or(max_ticks),
    }
}

// ============================================================================
// Criterion Benchmarking Support (when benchmarking feature is enabled)
// ============================================================================

#[cfg(feature = "benchmarking")]
/// Criterion benchmark wrapper for Chicago TDD
///
/// Provides a Chicago TDD-friendly wrapper around criterion benchmarking.
/// This makes statistical benchmarking consistent with other testing utilities.
///
/// Note: Criterion is typically used in `benches/` directory with `cargo bench`.
/// This wrapper provides documentation and helper functions for using criterion.
///
/// # Example
///
/// In your `benches/my_bench.rs`:
///
/// ```rust,compile_fail
/// // This is a compile-fail example showing how to use criterion
/// // In actual benches/my_bench.rs, you would use:
/// use criterion::{black_box, criterion_group, criterion_main, Criterion};
///
/// fn bench_operation(c: &mut Criterion) {
///     c.bench_function("my_operation", |b| {
///         b.iter(|| {
///             black_box(my_operation());
///         });
///     });
/// }
///
/// criterion_group!(benches, bench_operation);
/// criterion_main!(benches);
/// ```
///
/// Run with: `cargo bench`
pub struct Benchmark;

#[cfg(feature = "benchmarking")]
impl Benchmark {
    /// Create a new benchmark (requires benchmarking feature)
    ///
    /// # Note
    ///
    /// Criterion is currently commented out in `Cargo.toml`. To use criterion benchmarking,
    /// uncomment the criterion dependency in `Cargo.toml`:
    ///
    /// ```toml
    /// [dev-dependencies]
    /// criterion = { version = "0.5", features = ["async_tokio"] }
    /// ```
    ///
    /// Then use criterion directly in your `benches/` directory as shown in the module documentation.
    #[must_use]
    pub const fn new(_name: &str) -> Self {
        Self
    }
}

#[cfg(not(feature = "benchmarking"))]
/// Benchmark wrapper (requires benchmarking feature)
///
/// Enable the `benchmarking` feature to use criterion benchmarking.
/// Criterion is typically used in `benches/` directory with `cargo bench`.
pub struct Benchmark;

#[cfg(not(feature = "benchmarking"))]
impl Benchmark {
    /// Create a new benchmark (requires benchmarking feature)
    #[must_use]
    pub const fn new(_name: &str) -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tick_counter_basic() {
        let counter = TickCounter::start();
        std::hint::black_box(42); // Prevent optimization
        let ticks = counter.elapsed_ticks();
        // ticks is u64, so it's always >= 0 - no need to check
        assert!(ticks < u64::MAX); // Just verify it's a valid value
    }

    #[test]
    fn test_tick_counter_within_budget() {
        let counter = TickCounter::start();
        std::hint::black_box(42);
        // Should pass for any reasonable operation
        assert!(counter.assert_within_budget(1_000_000).is_ok());
    }

    #[test]
    fn test_measure_ticks() {
        let (result, ticks) = measure_ticks(|| 42);
        assert_eq!(result, 42);
        // ticks is u64, so it's always >= 0 - no need to check
        assert!(ticks < u64::MAX); // Just verify it's a valid value
    }

    #[test]
    fn test_benchmark() {
        let result = benchmark("test_operation", 100, || std::hint::black_box(42));
        assert_eq!(result.operation, "test_operation");
        assert_eq!(result.iterations, 100);
        assert!(result.avg_ticks >= 0.0);
        assert!(result.min_ticks <= result.max_ticks);
    }

    #[test]
    fn test_tick_measurer() {
        let measurer = TickMeasurer::new(|| 42);
        let (result, ticks) = measurer.measure();
        assert_eq!(result, 42);
        // ticks is u64, so it's always >= 0 - no need to check
        assert!(ticks < u64::MAX); // Just verify it's a valid value
    }
}
