//! Audit utilities domain layer
//!
//! Pure business logic for security auditing, vulnerability scanning, and configuration analysis.

pub mod security;

pub use security::{ConfigAuditor, DependencyChecker, SecurityScanner};
