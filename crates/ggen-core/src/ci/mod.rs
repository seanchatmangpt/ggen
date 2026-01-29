//! CI/CD workflow generation module
//!
//! This module provides functionality for generating CI/CD workflow files
//! from RDF specifications. It supports multiple CI platforms with a focus
//! on GitHub Actions.
//!
//! ## Features
//!
//! - **GitHub Actions**: Generate .github/workflows/*.yml files from RDF specs
//! - **RDF-Driven**: Extract test requirements and job configurations from ontologies
//! - **Optimization**: Pre-pull images, dependency caching, parallel job execution
//! - **Matrix Strategies**: Support for testing across multiple versions/platforms
//!
//! ## Examples
//!
//! ### Generate GitHub Actions Workflow
//!
//! ```rust,no_run
//! use ggen_core::ci::{WorkflowGenerator, WorkflowConfig};
//! use ggen_core::Graph;
//! use std::path::PathBuf;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_turtle(r#"
//!     @prefix ggen: <http://ggen.example.org/> .
//!     @prefix test: <http://ggen.example.org/test/> .
//!
//!     test:unit_tests a ggen:TestSuite ;
//!         ggen:testType "unit" ;
//!         ggen:timeout "10s" .
//!
//!     test:integration_tests a ggen:TestSuite ;
//!         ggen:testType "integration" ;
//!         ggen:timeout "30s" .
//! "#)?;
//!
//! let config = WorkflowConfig::from_graph(&graph)?;
//! let generator = WorkflowGenerator::new(config);
//! let yaml = generator.generate_workflow().await?;
//!
//! std::fs::write(".github/workflows/test.yml", yaml)?;
//! # Ok(())
//! # }
//! ```

pub mod github_actions;
pub mod workflow_config;

pub use github_actions::{WorkflowGenerator, WorkflowJob, WorkflowStep};
pub use workflow_config::{
    CacheConfig, JobConfig, MatrixConfig, WorkflowConfig, WorkflowOptimization, WorkflowTrigger,
};

/// CI platform types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CiPlatform {
    /// GitHub Actions
    GitHubActions,
    /// GitLab CI
    GitLabCi,
    /// CircleCI
    CircleCi,
}

impl CiPlatform {
    /// Get the file path for this CI platform's workflow file
    #[must_use]
    pub fn workflow_path(&self, name: &str) -> String {
        match self {
            Self::GitHubActions => format!(".github/workflows/{}.yml", name),
            Self::GitLabCi => ".gitlab-ci.yml".to_string(),
            Self::CircleCi => ".circleci/config.yml".to_string(),
        }
    }

    /// Get the default workflow name for this platform
    #[must_use]
    pub fn default_workflow_name(&self) -> &str {
        match self {
            Self::GitHubActions => "test-with-containers",
            Self::GitLabCi => "test",
            Self::CircleCi => "test",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ci_platform_workflow_path() {
        // Arrange
        let platform = CiPlatform::GitHubActions;
        let name = "test";

        // Act
        let path = platform.workflow_path(name);

        // Assert
        assert_eq!(path, ".github/workflows/test.yml");
    }

    #[test]
    fn test_ci_platform_default_name() {
        // Arrange
        let platform = CiPlatform::GitHubActions;

        // Act
        let name = platform.default_workflow_name();

        // Assert
        assert_eq!(name, "test-with-containers");
    }
}
