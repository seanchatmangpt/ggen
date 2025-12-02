//! FMEA (Failure Mode and Effects Analysis) framework for systematic failure tracking.
//!
//! This module provides infrastructure for tracking and analyzing failures in the ggen CLI
//! following DfLSS (Design for Lean Six Sigma) principles with Pareto (80/20) analysis.
//!
//! # Core Components
//!
//! - [`types`]: Core FMEA types (Severity, Occurrence, Detection, RPN)
//! - [`registry`]: Thread-safe failure tracking registry
//! - [`catalog`]: Pre-registered critical failure modes
//! - [`context`]: Instrumentation trait for error tracking
//!
//! # Design Principles
//!
//! - **Zero-cost abstractions**: Success path has 0 overhead
//! - **Type-level safety**: Invalid states unrepresentable
//! - **Pareto focus**: Top 20% of failures = 80% of risk
//! - **DfLSS alignment**: Prevent defects at source

pub mod types;
pub mod registry;
pub mod catalog;
pub mod context;

// Re-export core types for ergonomic usage
pub use types::{
    Severity, Occurrence, Detection, RPN,
    FailureCategory, FailureMode, FailureModeBuilder,
};
pub use registry::{FmeaRegistry, FailureEvent, FMEA_REGISTRY};
pub use catalog::register_critical_failures;
pub use context::FmeaContext;

#[cfg(test)]
mod tests;
