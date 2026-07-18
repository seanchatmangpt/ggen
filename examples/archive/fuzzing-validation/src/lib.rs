//! # Fuzzing-Based Validation System
//!
//! A novel validation approach where 7 agents perform COMPREHENSIVE FUZZING:
//! - Agent 1: Structure fuzzing (malformed RDF, broken syntax)
//! - Agent 2: Value fuzzing (boundary values, extreme numbers)
//! - Agent 3: Protocol fuzzing (invalid SPARQL, broken queries)
//! - Agent 4: Chaos engineering (random failures, timeouts)
//! - Agent 5: Performance fuzzing (extreme loads, stress testing)
//! - Agent 6: Security fuzzing (injection attacks, path traversal)
//! - Agent 7: Semantic fuzzing (edge cases, corner cases)
//!
//! **Key Innovation**: 7 different fuzzing strategies explore the entire input space
//! to find crashes, hangs, incorrect results, and errors that traditional testing misses.
//!
//! ## Usage
//!
//! ```rust,no_run
//! use fuzzing_validation::FuzzingValidationSystem;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let system = FuzzingValidationSystem::new().await?;
//!
//!     let report = system.run_fuzzing_campaign(
//!         "SELECT * FROM {graph} WHERE ?s ?p ?o",
//!         100  // 100 fuzzed inputs per agent
//!     ).await?;
//!
//!     println!("Error Rate: {:.1}%", report.result.error_rate() * 100.0);
//!     println!("Crash Rate: {:.1}%", report.result.crash_rate() * 100.0);
//!     println!("Assessment: {}", report.assessment);
//!
//!     // Show recommendations
//!     for rec in &report.recommendations {
//!         println!("Priority: {}", rec.priority);
//!         println!("Issue: {}", rec.issue);
//!         println!("Suggestion: {}", rec.suggestion);
//!         println!();
//!     }
//!
//!     Ok(())
//! }
//! ```

pub mod agents;
pub mod coordinator;
pub mod fuzzers;

use coordinator::FuzzingCoordinator;
use fuzzers::FuzzReport;

/// Main fuzzing validation system
pub struct FuzzingValidationSystem {
    coordinator: FuzzingCoordinator,
}

impl FuzzingValidationSystem {
    /// Create a new fuzzing validation system
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let coordinator = FuzzingCoordinator::new().await?;
        Ok(Self { coordinator })
    }

    /// Run complete fuzzing campaign
    ///
    /// # Arguments
    /// * `base_input` - Base input to fuzz (e.g., SPARQL query, RDF data)
    /// * `inputs_per_agent` - Number of fuzzed inputs per agent (total = 7 * inputs_per_agent)
    ///
    /// # Returns
    /// Complete fuzzing report with crash analysis and robustness recommendations
    pub async fn run_fuzzing_campaign(
        &self,
        base_input: &str,
        inputs_per_agent: usize,
    ) -> Result<FuzzReport, Box<dyn std::error::Error>> {
        self.coordinator
            .run_fuzzing_campaign(base_input, inputs_per_agent)
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_system_creation() {
        let system = FuzzingValidationSystem::new().await;
        assert!(system.is_ok());
    }
}
