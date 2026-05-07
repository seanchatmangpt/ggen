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

pub mod catalog;
pub mod context;
pub mod registry;
pub mod types;

// Re-export core types for ergonomic usage
pub use catalog::register_critical_failures;
pub use context::FmeaContext;
pub use registry::{FailureEvent, FmeaRegistry, FMEA_REGISTRY};
pub use types::{
    Detection, FailureCategory, FailureMode, FailureModeBuilder, Occurrence, Severity, RPN,
};

#[cfg(test)]
mod tests;
