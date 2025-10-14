//! # Lifecycle Management System
//!
//! Complete ggen-style lifecycle management for cleanroom environments.
//! Provides init, test, deploy, validate, and readiness tracking phases.
//!
//! ## Features
//!
//! - **Project Initialization**: Bootstrap project structure and dependencies
//! - **Test Execution**: Run tests in cleanroom environments
//! - **Deployment**: Deploy to dev/staging/production environments
//! - **Validation**: Validate environment configuration and requirements
//! - **Readiness Tracking**: Track production readiness with detailed scoring
//!
//! ## Example
//!
//! ```no_run
//! use cleanroom::lifecycle::{LifecycleManager, LifecycleConfig};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let config = LifecycleConfig::load("lifecycle.toml").await?;
//!     let manager = LifecycleManager::new(config, None)?;
//!
//!     // Initialize project
//!     manager.init().await?;
//!
//!     // Run tests
//!     let test_results = manager.test().await?;
//!     println!("Tests: {} passed, {} failed",
//!         test_results.passed, test_results.failed);
//!
//!     // Check readiness
//!     let readiness = manager.readiness().await?;
//!     println!("Production readiness: {}%", readiness.score);
//!
//!     // Deploy if ready
//!     if readiness.score >= 80 {
//!         manager.deploy("production").await?;
//!     }
//!
//!     Ok(())
//! }
//! ```

pub mod config;
pub mod phases;
pub mod readiness;
pub mod validator;

pub use config::{LifecycleConfig, Phase, EnvironmentConfig, Requirement, Status};
pub use phases::{LifecycleManager, InitResult, TestResults, DeploymentResult};
pub use readiness::{ReadinessScore, ReadinessTracker, RequirementStatus};
pub use validator::{DeploymentValidator, ValidationReport, QualityReport};

use crate::error::Result;

/// Initialize lifecycle system with default configuration
pub async fn init_lifecycle(project_name: &str) -> Result<LifecycleConfig> {
    let config = LifecycleConfig::default_with_name(project_name);
    config.save("lifecycle.toml").await?;
    Ok(config)
}

/// Load lifecycle configuration from file
pub async fn load_lifecycle(path: &str) -> Result<LifecycleConfig> {
    LifecycleConfig::load(path).await
}
