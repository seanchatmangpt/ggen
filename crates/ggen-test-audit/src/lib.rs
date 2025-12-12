//! Test quality audit tooling for ggen
//!
//! This crate provides mutation testing, assertion analysis, and false positive detection
//! for the ggen test suite. Part of Feature 004: Test Quality Audit and Performance Optimization.
//!
//! # Overview
//!
//! - Mutation testing via cargo-mutants integration
//! - Assertion strength analysis via AST parsing
//! - False positive detection (tests passing when functionality broken)
//! - Test quality report generation
//!
//! # Usage
//!
//! ```rust,no_run
//! use ggen_test_audit::AuditResult;
//!
//! // Coming soon: Actual implementation
//! ```

#![deny(warnings)]
#![deny(unsafe_code)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

pub mod assertion_analyzer;
pub mod false_positive_detector;
pub mod mutation_analyzer;
pub mod report_generator;
pub mod types;

pub use assertion_analyzer::{AssertionAnalyzer, TestAssertion};
pub use false_positive_detector::{
    CriticalPathGap, FalsePositive, FalsePositiveDetector, FalsePositiveReport, Severity,
};
pub use mutation_analyzer::MutationAnalyzer;
pub use report_generator::{QualityReport, ReportGenerator};
pub use types::*;
