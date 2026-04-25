//! # Mutation-Based Validation System
//!
//! A novel validation approach where 7 agents perform ADVERSARIAL testing:
//! - Agents 1-3: Inject mutations (try to BREAK the code)
//! - Agents 4-5: Run tests to detect mutations
//! - Agent 6: Calculate mutation score
//! - Agent 7: Suggest test improvements
//!
//! **Key Innovation**: Active adversarial testing (agents try to break code)
//! vs. passive validation (agents check if code is correct).
//!
//! ## Usage
//!
//! ```rust,no_run
//! use mutation_validation::MutationValidationSystem;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let system = MutationValidationSystem::new().await?;
//!
//!     let report = system.run_mutation_testing(
//!         "/path/to/package",
//!         "fn main() { println!(\"Hello\"); }",
//!         "cargo test"
//!     ).await?;
//!
//!     println!("Mutation Score: {:.1}%", report.result.mutation_score * 100.0);
//!     println!("Assessment: {}", report.assessment);
//!
//!     Ok(())
//! }
//! ```

pub mod agents;
pub mod coordinator;
pub mod mutations;

use coordinator::MutationCoordinator;
use mutations::MutationReport;

/// Main mutation validation system
pub struct MutationValidationSystem {
    coordinator: MutationCoordinator,
}

impl MutationValidationSystem {
    /// Create a new mutation validation system
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let coordinator = MutationCoordinator::new().await?;
        Ok(Self { coordinator })
    }

    /// Run complete mutation testing workflow
    ///
    /// # Arguments
    /// * `package_path` - Path to ggen-generated package
    /// * `original_code` - Original source code to mutate
    /// * `test_command` - Command to run tests (e.g., "cargo test")
    ///
    /// # Returns
    /// Complete mutation report with score and test suggestions
    pub async fn run_mutation_testing(
        &self,
        package_path: &str,
        original_code: &str,
        test_command: &str,
    ) -> Result<MutationReport, Box<dyn std::error::Error>> {
        self.coordinator
            .run_mutation_testing(package_path, original_code, test_command)
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_system_creation() {
        let system = MutationValidationSystem::new().await;
        assert!(system.is_ok());
    }
}
