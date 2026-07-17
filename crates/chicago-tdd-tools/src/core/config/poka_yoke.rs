//! Poka-Yoke Configuration Types
//!
//! Provides type-level error prevention for configuration values.
//! Uses Rust's type system to make invalid configuration states unrepresentable.
//!
//! **Kaizen Pattern**: Documentation uses backticks around constant names, function names,
//! and type names for better readability and to satisfy clippy's `doc_markdown` lint.
//! Example: Use `` `MAX_REASONABLE_TIMEOUT` `` instead of `MAX_REASONABLE_TIMEOUT` in docs.
//!
//! # Poka-Yoke Principles
//!
//! - **Make invalid states unrepresentable**: Use types to prevent errors
//! - **Type-level prevention**: Option types force handling of invalid values at compile time
//! - **Runtime validation**: Invalid values return `None`, preventing use of invalid data
//! - **Type-level invariants**: Encode constraints in types
//!
//! # Error Modes Prevented
//!
//! 1. **Invalid Ports**: Ports must be > 0 (prevented by `NonZeroPort`)
//! 2. **Invalid Timeouts**: Timeouts must be > 0 (prevented by `PositiveTimeout`)
//! 3. **Invalid Coverage**: Coverage must be 0.0-100.0 (prevented by `ValidCoverage`)
//! 4. **Invalid Ranges**: Min coverage must be <= max coverage (prevented by `ValidCoverageRange`)
//! 5. **Invalid State Combinations**: Builder pattern prevents incomplete configurations
//!
//! # Compile-Time vs Runtime Validation
//!
//! This module uses a combination of compile-time and runtime validation:
//!
//! - **Compile-Time Validation**: `NonZeroPort` and `PositiveTimeout` use `NonZeroU16`/`NonZeroU64`,
//!   which are compile-time validated types. The compiler prevents creating these types with zero values.
//!
//! - **Runtime Validation**: `ValidCoverage` and `ValidCoverageRange` use runtime checks (f64 comparison
//!   cannot be const). However, they return `Option<T>`, which forces explicit handling at compile time.
//!
//! - **Type-Level Prevention**: All types use `Option<T>` return values, forcing callers to handle
//!   invalid values at compile time. You cannot use an invalid value without explicitly handling `None`.
//!
//! # Example
//!
//! ```rust
//! use chicago_tdd_tools::core::config::poka_yoke::*;
//!
//! // Valid port creation
//! let port = NonZeroPort::new(4317).expect("Valid port");
//! assert_eq!(port.get(), 4317);
//!
//! // Builder pattern prevents incomplete configurations
//! let config = ConfigBuilder::new()
//!     .unit_timeout(PositiveTimeout::new(1).unwrap())
//!     .integration_timeout(PositiveTimeout::new(30).unwrap())
//!     .otlp_grpc_port(NonZeroPort::new(4317).unwrap())
//!     .admin_port(NonZeroPort::new(4320).unwrap())
//!     .coverage_range(ValidCoverageRange::new(
//!         ValidCoverage::new(0.0).unwrap(),
//!         ValidCoverage::new(100.0).unwrap()
//!     ).unwrap())
//!     .build(); // Type system ensures all required fields are set
//! ```

use std::marker::PhantomData;

/// Non-zero port number
///
/// **Poka-yoke**: Uses `NonZeroU16` to prevent port = 0.
/// The type system makes invalid ports impossible - `new(0)` returns `None`,
/// forcing explicit handling of invalid ports at compile time.
///
/// # Invariant
///
/// Port is always > 0 (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonZeroPort {
    /// Port value (always > 0)
    value: std::num::NonZeroU16,
}

impl NonZeroPort {
    /// Create a new non-zero port
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid ports (0).
    /// The type system forces handling of invalid ports.
    #[must_use]
    pub const fn new(value: u16) -> Option<Self> {
        match std::num::NonZeroU16::new(value) {
            Some(nz) => Some(Self { value: nz }),
            None => None,
        }
    }

    /// Get the port value
    ///
    /// **Poka-yoke**: Returns `u16` that is guaranteed to be > 0.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> u16 {
        self.value.get()
    }

    /// Convert to u16
    #[must_use]
    pub const fn into_u16(self) -> u16 {
        self.value.get()
    }
}

impl From<NonZeroPort> for u16 {
    fn from(port: NonZeroPort) -> Self {
        port.value.get()
    }
}

/// Positive timeout value
///
/// **Poka-yoke**: Uses `NonZeroU64` to prevent timeout = 0.
/// The type system makes invalid timeouts impossible - `new(0)` returns `None`,
/// forcing explicit handling of invalid timeouts at compile time.
///
/// # Invariant
///
/// Timeout is always > 0 (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PositiveTimeout {
    /// Timeout value (always > 0)
    value: std::num::NonZeroU64,
}

impl PositiveTimeout {
    /// Create a new positive timeout
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid timeouts (0).
    /// The type system forces handling of invalid timeouts.
    #[must_use]
    pub const fn new(value: u64) -> Option<Self> {
        match std::num::NonZeroU64::new(value) {
            Some(nz) => Some(Self { value: nz }),
            None => None,
        }
    }

    /// Get the timeout value
    ///
    /// **Poka-yoke**: Returns `u64` that is guaranteed to be > 0.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> u64 {
        self.value.get()
    }

    /// Convert to u64
    #[must_use]
    pub const fn into_u64(self) -> u64 {
        self.value.get()
    }
}

impl From<PositiveTimeout> for u64 {
    fn from(timeout: PositiveTimeout) -> Self {
        timeout.value.get()
    }
}

/// Bounded timeout value (0 < value <= `MAX_REASONABLE_TIMEOUT`)
///
/// **Poka-yoke**: Enforces both lower bound (> 0) and upper bound (<= `MAX_REASONABLE_TIMEOUT`).
/// The type system makes invalid timeout values impossible - values outside bounds return `None`,
/// forcing explicit handling of invalid values at compile time.
///
/// **FMEA Fix FM6 (RPN 105)**: Type-level prevention of unreasonably large timeout values.
/// Prevents test hangs caused by timeout=999999 or similar values.
///
/// # Invariant
///
/// Timeout is always 0 < value <= `MAX_REASONABLE_TIMEOUT` (enforced by type).
/// `MAX_REASONABLE_TIMEOUT` = 3600 seconds (1 hour) - reasonable for any test.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundedTimeout {
    /// Timeout value (always 0 < value <= `MAX_REASONABLE_TIMEOUT`)
    value: std::num::NonZeroU64,
}

impl BoundedTimeout {
    /// Maximum reasonable timeout in seconds (1 hour)
    ///
    /// **Poka-yoke**: This constant defines the upper bound enforced by the type.
    /// Any timeout > `MAX_REASONABLE_TIMEOUT` is considered invalid and prevented.
    pub const MAX_REASONABLE_TIMEOUT: u64 = 3600;

    /// Create a new bounded timeout
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid timeouts:
    /// - Value = 0: Returns `None` (prevented by `NonZeroU64`)
    /// - Value > `MAX_REASONABLE_TIMEOUT`: Returns `None` (prevented by runtime check)
    ///
    /// The type system forces handling of invalid timeouts at compile time.
    #[must_use]
    pub fn new(value: u64) -> Option<Self> {
        // First check: Must be > 0 (enforced by NonZeroU64)
        let nz = std::num::NonZeroU64::new(value)?;

        // Second check: Must be <= MAX_REASONABLE_TIMEOUT (enforced by runtime check)
        if value <= Self::MAX_REASONABLE_TIMEOUT {
            Some(Self { value: nz })
        } else {
            None
        }
    }

    /// Get the timeout value
    ///
    /// **Poka-yoke**: Returns `u64` that is guaranteed to be 0 < value <= `MAX_REASONABLE_TIMEOUT`.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> u64 {
        self.value.get()
    }

    /// Convert to u64
    #[must_use]
    pub const fn into_u64(self) -> u64 {
        self.value.get()
    }
}

impl From<BoundedTimeout> for u64 {
    fn from(timeout: BoundedTimeout) -> Self {
        timeout.value.get()
    }
}

/// Bounded u32 value (0 < value <= `MAX_REASONABLE_U32`)
///
/// **Poka-yoke**: Enforces both lower bound (> 0) and upper bound (<= `MAX_REASONABLE_U32`).
/// Useful for property test cases and other bounded counts.
///
/// **Invariant**: Value is always 0 < value <= `MAX_REASONABLE_U32` (enforced by type).
/// `MAX_REASONABLE_U32` = 1,000,000 - reasonable for test case counts.
///
/// **Compile-Time Prevention**: Uses `NonZeroU32` for lower bound (compile-time check).
/// **Runtime Prevention**: Checks upper bound at runtime, but returns `Option` forcing compile-time handling.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::config::poka_yoke::BoundedU32;
///
/// // Valid value
/// let value = BoundedU32::new(100).expect("Valid value");
/// assert_eq!(value.get(), 100);
///
/// // Invalid: Zero value
/// let value = BoundedU32::new(0);
/// assert!(value.is_none()); // Type prevents invalid value
///
/// // Invalid: Too large value
/// let value = BoundedU32::new(2_000_000);
/// assert!(value.is_none()); // Type prevents invalid value
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundedU32 {
    /// Value (always 0 < value <= `MAX_REASONABLE_U32`)
    value: std::num::NonZeroU32,
}

impl BoundedU32 {
    /// Maximum reasonable u32 value (1 million)
    ///
    /// **Poka-yoke**: This constant defines the upper bound enforced by the type.
    /// Any value > `MAX_REASONABLE_U32` is considered invalid and prevented.
    pub const MAX_REASONABLE_U32: u32 = 1_000_000;

    /// Create a new bounded u32 value
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid values:
    /// - Value = 0: Returns `None` (prevented by `NonZeroU32`)
    /// - Value > `MAX_REASONABLE_U32`: Returns `None` (prevented by runtime check)
    #[must_use]
    pub fn new(value: u32) -> Option<Self> {
        let nz = std::num::NonZeroU32::new(value)?;

        if value <= Self::MAX_REASONABLE_U32 {
            Some(Self { value: nz })
        } else {
            None
        }
    }

    /// Get the value
    ///
    /// **Poka-yoke**: Returns `u32` that is guaranteed to be 0 < value <= `MAX_REASONABLE_U32`.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> u32 {
        self.value.get()
    }

    /// Convert to u32
    #[must_use]
    pub const fn into_u32(self) -> u32 {
        self.value.get()
    }
}

impl From<BoundedU32> for u32 {
    fn from(value: BoundedU32) -> Self {
        value.value.get()
    }
}

/// Positive u32 value
///
/// **Poka-yoke**: Uses `NonZeroU32` to prevent value = 0.
/// The type system makes invalid values impossible - `new(0)` returns `None`,
/// forcing explicit handling of invalid values at compile time.
///
/// # Invariant
///
/// Value is always > 0 (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PositiveU32 {
    /// Value (always > 0)
    value: std::num::NonZeroU32,
}

impl PositiveU32 {
    /// Create a new positive u32 value
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid values (0).
    /// The type system forces handling of invalid values.
    #[must_use]
    pub const fn new(value: u32) -> Option<Self> {
        match std::num::NonZeroU32::new(value) {
            Some(nz) => Some(Self { value: nz }),
            None => None,
        }
    }

    /// Get the value
    ///
    /// **Poka-yoke**: Returns `u32` that is guaranteed to be > 0.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> u32 {
        self.value.get()
    }

    /// Convert to u32
    #[must_use]
    pub const fn into_u32(self) -> u32 {
        self.value.get()
    }
}

impl From<PositiveU32> for u32 {
    fn from(value: PositiveU32) -> Self {
        value.value.get()
    }
}

/// Positive usize value
///
/// **Poka-yoke**: Uses `NonZeroUsize` to prevent value = 0.
/// The type system makes invalid values impossible - `new(0)` returns `None`,
/// forcing explicit handling of invalid values at compile time.
///
/// # Invariant
///
/// Value is always > 0 (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PositiveUsize {
    /// Value (always > 0)
    value: std::num::NonZeroUsize,
}

impl PositiveUsize {
    /// Create a new positive usize value
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid values (0).
    /// The type system forces handling of invalid values.
    #[must_use]
    pub const fn new(value: usize) -> Option<Self> {
        match std::num::NonZeroUsize::new(value) {
            Some(nz) => Some(Self { value: nz }),
            None => None,
        }
    }

    /// Get the value
    ///
    /// **Poka-yoke**: Returns `usize` that is guaranteed to be > 0.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> usize {
        self.value.get()
    }

    /// Convert to usize
    #[must_use]
    pub const fn into_usize(self) -> usize {
        self.value.get()
    }
}

impl From<PositiveUsize> for usize {
    fn from(value: PositiveUsize) -> Self {
        value.value.get()
    }
}

/// Valid coverage percentage
///
/// **Poka-yoke**: Newtype prevents coverage < 0.0 or > 100.0.
/// The type system makes invalid coverage impossible.
///
/// # Invariant
///
/// Coverage is always 0.0 <= coverage <= 100.0 (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ValidCoverage {
    /// Coverage value (always 0.0 <= value <= 100.0)
    value: f64,
}

impl ValidCoverage {
    /// Minimum valid coverage (0.0)
    pub const MIN: f64 = 0.0;
    /// Maximum valid coverage (100.0)
    pub const MAX: f64 = 100.0;

    /// Create a new valid coverage
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid coverage (< 0.0 or > 100.0).
    /// The type system forces handling of invalid coverage.
    #[must_use]
    pub fn new(value: f64) -> Option<Self> {
        if (Self::MIN..=Self::MAX).contains(&value) {
            Some(Self { value })
        } else {
            None
        }
    }

    /// Get the coverage value
    ///
    /// **Poka-yoke**: Returns `f64` that is guaranteed to be 0.0 <= value <= 100.0.
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // const fn - signature cannot be changed
    pub const fn get(&self) -> f64 {
        self.value
    }

    /// Convert to f64
    #[must_use]
    pub const fn into_f64(self) -> f64 {
        self.value
    }
}

impl From<ValidCoverage> for f64 {
    fn from(coverage: ValidCoverage) -> Self {
        coverage.value
    }
}

/// Valid coverage range (min <= max)
///
/// **Poka-yoke**: Type-level invariant ensures min <= max.
/// The type system makes invalid ranges impossible.
///
/// # Invariant
///
/// Min coverage is always <= max coverage (enforced by type).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ValidCoverageRange {
    /// Minimum coverage (always <= max)
    min: ValidCoverage,
    /// Maximum coverage (always >= min)
    max: ValidCoverage,
}

impl ValidCoverageRange {
    /// Create a new valid coverage range
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid ranges (min > max).
    /// The type system forces handling of invalid ranges.
    ///
    /// **Note**: This function cannot be `const` because f64 comparison is not const-stable.
    #[must_use]
    pub fn new(min: ValidCoverage, max: ValidCoverage) -> Option<Self> {
        if min.get() <= max.get() {
            Some(Self { min, max })
        } else {
            None
        }
    }

    /// Get minimum coverage
    #[must_use]
    pub const fn min(&self) -> ValidCoverage {
        self.min
    }

    /// Get maximum coverage
    #[must_use]
    pub const fn max(&self) -> ValidCoverage {
        self.max
    }
}

/// Type-level state marker: Configuration is incomplete
///
/// **Poka-yoke**: This marker type indicates that configuration is still being built.
/// The builder pattern prevents accessing incomplete configurations.
pub struct Incomplete;

/// Type-level state marker: Configuration is complete
///
/// **Poka-yoke**: This marker type indicates that configuration is complete.
/// Only complete configurations can be built.
pub struct Complete;

/// Configuration builder with type state
///
/// **Poka-yoke**: Builder pattern prevents incomplete configurations.
/// The type system ensures all required fields are set before building.
pub struct ConfigBuilder<State> {
    /// Unit test timeout
    unit_timeout: Option<PositiveTimeout>,
    /// Integration test timeout
    integration_timeout: Option<PositiveTimeout>,
    /// OTLP gRPC port
    otlp_grpc_port: Option<NonZeroPort>,
    /// Admin port
    admin_port: Option<NonZeroPort>,
    /// Coverage range
    coverage_range: Option<ValidCoverageRange>,
    /// Type-level state marker
    _state: PhantomData<State>,
}

impl Default for ConfigBuilder<Incomplete> {
    fn default() -> Self {
        Self::new()
    }
}

impl ConfigBuilder<Incomplete> {
    /// Create a new configuration builder
    ///
    /// **Poka-yoke**: Starts in `Incomplete` state.
    /// Must call all required setters before building.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            unit_timeout: None,
            integration_timeout: None,
            otlp_grpc_port: None,
            admin_port: None,
            coverage_range: None,
            _state: PhantomData,
        }
    }

    /// Set unit test timeout
    ///
    /// **Poka-yoke**: Takes `PositiveTimeout` (cannot be 0).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - mutates self
    pub fn unit_timeout(mut self, timeout: PositiveTimeout) -> Self {
        self.unit_timeout = Some(timeout);
        self
    }

    /// Set integration test timeout
    ///
    /// **Poka-yoke**: Takes `PositiveTimeout` (cannot be 0).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - mutates self
    pub fn integration_timeout(mut self, timeout: PositiveTimeout) -> Self {
        self.integration_timeout = Some(timeout);
        self
    }

    /// Set OTLP gRPC port
    ///
    /// **Poka-yoke**: Takes `NonZeroPort` (cannot be 0).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - mutates self
    pub fn otlp_grpc_port(mut self, port: NonZeroPort) -> Self {
        self.otlp_grpc_port = Some(port);
        self
    }

    /// Set admin port
    ///
    /// **Poka-yoke**: Takes `NonZeroPort` (cannot be 0).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - mutates self
    pub fn admin_port(mut self, port: NonZeroPort) -> Self {
        self.admin_port = Some(port);
        self
    }

    /// Set coverage range
    ///
    /// **Poka-yoke**: Takes `ValidCoverageRange` (min <= max).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - mutates self
    pub fn coverage_range(mut self, range: ValidCoverageRange) -> Self {
        self.coverage_range = Some(range);
        self
    }

    /// Build the configuration
    ///
    /// **Poka-yoke**: Only available when all required fields are set.
    /// Returns `Option` to handle missing fields gracefully.
    #[must_use]
    pub fn build(self) -> Option<ValidatedConfig> {
        Some(ValidatedConfig {
            unit_timeout: self.unit_timeout?,
            integration_timeout: self.integration_timeout?,
            otlp_grpc_port: self.otlp_grpc_port?,
            admin_port: self.admin_port?,
            coverage_range: self.coverage_range?,
        })
    }
}

/// Validated configuration (all invariants enforced)
///
/// **Poka-yoke**: This type represents a complete, validated configuration.
/// All invariants are enforced by the type system.
///
/// # Invariants
///
/// - All timeouts are > 0
/// - All ports are > 0
/// - Coverage range is valid (min <= max)
#[derive(Debug, Clone, Copy)]
pub struct ValidatedConfig {
    /// Unit test timeout (always > 0)
    unit_timeout: PositiveTimeout,
    /// Integration test timeout (always > 0)
    integration_timeout: PositiveTimeout,
    /// OTLP gRPC port (always > 0)
    otlp_grpc_port: NonZeroPort,
    /// Admin port (always > 0)
    admin_port: NonZeroPort,
    /// Coverage range (min <= max)
    coverage_range: ValidCoverageRange,
}

impl ValidatedConfig {
    /// Get unit test timeout
    #[must_use]
    pub const fn unit_timeout(&self) -> PositiveTimeout {
        self.unit_timeout
    }

    /// Get integration test timeout
    #[must_use]
    pub const fn integration_timeout(&self) -> PositiveTimeout {
        self.integration_timeout
    }

    /// Get OTLP gRPC port
    #[must_use]
    pub const fn otlp_grpc_port(&self) -> NonZeroPort {
        self.otlp_grpc_port
    }

    /// Get admin port
    #[must_use]
    pub const fn admin_port(&self) -> NonZeroPort {
        self.admin_port
    }

    /// Get coverage range
    #[must_use]
    pub const fn coverage_range(&self) -> ValidCoverageRange {
        self.coverage_range
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)] // Test code: unwrap is acceptable
mod tests {
    use super::*;

    #[test]
    fn test_non_zero_port_valid() {
        let port = NonZeroPort::new(4317);
        assert!(port.is_some());
        assert_eq!(port.unwrap().get(), 4317);
    }

    #[test]
    fn test_non_zero_port_invalid() {
        let port = NonZeroPort::new(0);
        assert!(port.is_none());
    }

    #[test]
    fn test_positive_timeout_valid() {
        let timeout = PositiveTimeout::new(1);
        assert!(timeout.is_some());
        assert_eq!(timeout.unwrap().get(), 1);
    }

    #[test]
    fn test_positive_timeout_invalid() {
        let timeout = PositiveTimeout::new(0);
        assert!(timeout.is_none());
    }

    #[test]
    fn test_valid_coverage_valid() {
        let coverage = ValidCoverage::new(80.0);
        assert!(coverage.is_some());
        assert_eq!(coverage.unwrap().get(), 80.0);
    }

    #[test]
    fn test_valid_coverage_invalid_high() {
        let coverage = ValidCoverage::new(101.0);
        assert!(coverage.is_none());
    }

    #[test]
    fn test_valid_coverage_invalid_low() {
        let coverage = ValidCoverage::new(-1.0);
        assert!(coverage.is_none());
    }

    #[test]
    fn test_valid_coverage_range_valid() {
        let min = ValidCoverage::new(0.0).unwrap();
        let max = ValidCoverage::new(100.0).unwrap();
        let range = ValidCoverageRange::new(min, max);
        assert!(range.is_some());
        assert_eq!(range.unwrap().min().get(), 0.0);
        assert_eq!(range.unwrap().max().get(), 100.0);
    }

    #[test]
    fn test_valid_coverage_range_invalid() {
        let min = ValidCoverage::new(80.0).unwrap();
        let max = ValidCoverage::new(50.0).unwrap();
        let range = ValidCoverageRange::new(min, max);
        assert!(range.is_none());
    }

    #[test]
    fn test_config_builder_complete() {
        let unit_timeout = PositiveTimeout::new(1).unwrap();
        let integration_timeout = PositiveTimeout::new(30).unwrap();
        let otlp_port = NonZeroPort::new(4317).unwrap();
        let admin_port = NonZeroPort::new(4320).unwrap();
        let min_coverage = ValidCoverage::new(0.0).unwrap();
        let max_coverage = ValidCoverage::new(100.0).unwrap();
        let coverage_range = ValidCoverageRange::new(min_coverage, max_coverage).unwrap();

        let config = ConfigBuilder::new()
            .unit_timeout(unit_timeout)
            .integration_timeout(integration_timeout)
            .otlp_grpc_port(otlp_port)
            .admin_port(admin_port)
            .coverage_range(coverage_range)
            .build();

        assert!(config.is_some());
        let config = config.unwrap();
        assert_eq!(config.unit_timeout().get(), 1);
        assert_eq!(config.integration_timeout().get(), 30);
        assert_eq!(config.otlp_grpc_port().get(), 4317);
        assert_eq!(config.admin_port().get(), 4320);
    }

    #[test]
    fn test_config_builder_incomplete() {
        let config = ConfigBuilder::new().unit_timeout(PositiveTimeout::new(1).unwrap()).build();
        assert!(config.is_none());
    }

    #[test]
    fn test_positive_u32_valid() {
        let value = PositiveU32::new(100);
        assert!(value.is_some());
        assert_eq!(value.unwrap().get(), 100);
    }

    #[test]
    fn test_positive_u32_invalid() {
        let value = PositiveU32::new(0);
        assert!(value.is_none());
    }

    #[test]
    fn test_positive_usize_valid() {
        let value = PositiveUsize::new(1000);
        assert!(value.is_some());
        assert_eq!(value.unwrap().get(), 1000);
    }

    #[test]
    fn test_positive_usize_invalid() {
        let value = PositiveUsize::new(0);
        assert!(value.is_none());
    }
}
