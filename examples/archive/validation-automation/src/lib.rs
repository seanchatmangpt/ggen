//! # Developer Experience & Quality of Life Automation
//!
//! A unified system for running all validation approaches with better UX:
//! - Progress bars for long-running operations
//! - Colored output and visual feedback
//! - Automated report generation
//! - Unified CLI for all validation approaches
//! - Error recovery and helpful suggestions
//!
//! ## Usage
//!
//! ```rust,no_run
//! use validation_automation::ValidationRunner;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let runner = ValidationRunner::new().await?;
//!
//!     // Run all validation approaches with automatic progress tracking
//!     let report = runner.run_all(
//!         "SELECT * FROM {graph} WHERE ?s ?p ?o",
//!         100  // inputs per agent
//!     ).await?;
//!
//!     // Generate comprehensive report
//!     report.generate_markdown("VALIDATION_REPORT.md").await?;
//!     report.generate_html("VALIDATION_REPORT.html").await?;
//!     report.generate_json("VALIDATION_REPORT.json").await?;
//!
//!     Ok(())
//! }
//! ```

pub mod progress;
pub mod runner;
pub mod report;
pub mod error;
pub mod cli;

use runner::ValidationRunner;
use report::ValidationReport;

pub use error::{ValidationError, ValidationResult};

/// Main validation automation system
pub struct ValidationAutomation {
    runner: ValidationRunner,
}

impl ValidationAutomation {
    /// Create a new validation automation system
    pub async fn new() -> Result<Self, String> {
        let runner = ValidationRunner::new().await.map_err(|e| e.to_string())?;
        Ok(Self { runner })
    }

    /// Run all validation approaches with full UX automation
    ///
    /// # Arguments
    /// * `input` - Input to validate (code, query, etc.)
    /// * `intensity` - Number of inputs per agent (default: 100)
    ///
    /// # Returns
    /// Complete validation report with all approach results
    pub async fn run_all_validations(
        &self,
        input: &str,
        intensity: usize,
    ) -> Result<ValidationReport, String> {
        self.runner.run_all(input, intensity).await.map_err(|e| e.to_string())
    }

    /// Run specific validation approach
    pub async fn run_validation(
        &self,
        approach: ValidationApproach,
        input: &str,
        intensity: usize,
    ) -> Result<ValidationReport, String> {
        self.runner.run_approach(approach, input, intensity).await.map_err(|e| e.to_string())
    }

    /// Get the underlying runner
    pub fn runner(&self) -> &ValidationRunner {
        &self.runner
    }
}

/// Validation approaches available
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, clap::ValueEnum)]
pub enum ValidationApproach {
    /// Consensus-based validation (7-agent voting)
    Consensus,
    /// Property-based validation (7-property testing)
    Property,
    /// Mutation-based validation (7-agent adversarial)
    Mutation,
    /// Fuzzing-based validation (7-agent fuzzing)
    Fuzzing,
    /// All approaches combined
    All,
}

impl ValidationApproach {
    /// Get all approaches
    pub fn all() -> Vec<Self> {
        vec![
            Self::Consensus,
            Self::Property,
            Self::Mutation,
            Self::Fuzzing,
        ]
    }

    /// Get display name
    pub fn display_name(&self) -> &str {
        match self {
            Self::Consensus => "Consensus-Based (7-Agent)",
            Self::Property => "Property-Based (7-Property)",
            Self::Mutation => "Mutation-Based (7-Agent)",
            Self::Fuzzing => "Fuzzing-Based (7-Agent)",
            Self::All => "All Approaches Combined",
        }
    }

    /// Get short name
    pub fn short_name(&self) -> &str {
        match self {
            Self::Consensus => "consensus",
            Self::Property => "property",
            Self::Mutation => "mutation",
            Self::Fuzzing => "fuzzing",
            Self::All => "all",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_approach_names() {
        assert_eq!(ValidationApproach::Consensus.display_name(), "Consensus-Based (7-Agent)");
        assert_eq!(ValidationApproach::Consensus.short_name(), "consensus");
        assert_eq!(ValidationApproach::All.display_name(), "All Approaches Combined");
    }

    #[test]
    fn test_all_approaches() {
        let approaches = ValidationApproach::all();
        assert_eq!(approaches.len(), 4);
    }
}
