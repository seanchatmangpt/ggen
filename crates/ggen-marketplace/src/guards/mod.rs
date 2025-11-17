//! Guards module - Validation and quality assurance for ggen packages
//!
//! This module implements a guards system for validating packages against specific
//! quality standards and requirements. Guards enable automated quality checks that
//! prevent low-quality packages from being marked as production-ready or 8020 certified.
//!
//! ## Guards
//!
//! - **Guard8020Coverage**: Validates that a package meets the 80/20 criteria
//!   - Ontology present and valid RDF
//!   - Projections complete (models, APIs, docs)
//!   - Templates present (≥3)
//!   - Tests present (unit + integration)
//!   - Documentation present (README, examples, arch docs)
//!   - Validation guards defined (≥1)
//!   - Bundle integration (works with ≥1 other 8020 package)
//!
//! - **GuardChatmanCompliant**: For Rust packages - enforces no unwrap/panic, AAA tests
//!
//! - **GuardTelemetryComplete**: For observability packages - OTEL spans, metrics, SLOs

pub mod guard_8020;
pub mod validation_receipt;

pub use guard_8020::{Guard8020Check, Guard8020Coverage, Guard8020Result};
pub use validation_receipt::{ValidationReceipt, ValidationReceiptBuilder};

/// Trait for guard implementations
pub trait Guard: Send + Sync {
    /// Guard name (e.g., "Guard8020Coverage")
    fn name(&self) -> &'static str;

    /// Guard description
    fn description(&self) -> &'static str;

    /// Run the guard validation
    /// Returns Ok(validation_result) if guard can validate, Err if not applicable
    fn validate(
        &self, package_path: &str,
    ) -> Result<GuardValidationResult, crate::error::MarketplaceError>;
}

/// Result of a single guard validation check
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct GuardValidationResult {
    /// Guard name
    pub guard_name: String,

    /// Overall pass/fail
    pub passed: bool,

    /// Individual check results
    pub checks: Vec<GuardCheck>,

    /// Detailed feedback
    pub message: String,

    /// Score (0-100)
    pub score: u32,
}

/// Individual guard check
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct GuardCheck {
    /// Check name (e.g., "ontology_present", "tests_complete")
    pub name: String,

    /// Check passed?
    pub passed: bool,

    /// Feedback
    pub message: String,

    /// Weight (importance for overall score)
    pub weight: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guard_trait_exists() {
        // Placeholder test - actual guard tests in respective modules
    }
}
