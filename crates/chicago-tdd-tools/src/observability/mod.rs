//! Unified Observability Testing
//!
//! Ground-up TRIZ redesign combining OTEL and Weaver testing into a single,
//! ergonomic API with automatic resource management and zero-cost abstractions.
//!
//! **Key Features**:
//! - Unified API for OTEL and Weaver testing
//! - RAII-based automatic lifecycle management
//! - Auto-detection of Weaver binary and registry
//! - Compile-time validation where possible (zero-cost)
//! - Runtime validation when needed (real collaborators)
//! - Type-safe API (invalid states unrepresentable)
//!
//! **Usage**:
//! ```ignore
//! use chicago_tdd_tools::observability::ObservabilityTest;
//!
//! # fn test() -> Result<(), Box<dyn std::error::Error>> {
//! // Simple usage - zero configuration for 80% of cases
//! let _test = ObservabilityTest::new()?;
//! // Your application generates spans here
//! // Automatic cleanup via Drop trait
//! # Ok(())
//! # }
//! ```
//!
//! **Required Features**:
//! - `otel`: Enable OTEL span/metric validation (`chicago-tdd-tools = { features = ["otel"] }`)
//! - `weaver`: Enable Weaver live validation (`chicago-tdd-tools = { features = ["weaver"] }`)

// Unified API (new implementation)
pub mod unified;

// Re-export unified API as main API
pub use unified::{ObservabilityError, ObservabilityResult, ObservabilityTest, TestConfig};

// Keep legacy modules for backward compatibility and type re-exports
// These modules provide the underlying types used by the unified API:
// - otel: SpanData, MetricData, validation types
// - weaver: WeaverLiveCheck, WeaverValidationResult, registry types
// - fixtures: Shared test fixtures for both OTEL and Weaver
// Users can access these types directly for advanced use cases
#[cfg(feature = "otel")]
pub mod otel;

// Poka-yoke types are re-exported through otel::poka_yoke module
#[cfg(feature = "weaver")]
pub mod weaver;

/// OCEL 2.0 (Object-Centric Event Log) Support
pub mod ocel;

/// BLAKE3 receipt chain validation — replay and tamper-evidence utilities.
/// Required by any project that claims BLAKE3 receipt chaining.
#[cfg(feature = "receipt-validation")]
pub mod receipt;

#[cfg(all(feature = "weaver", feature = "otel"))]
pub mod fixtures;
