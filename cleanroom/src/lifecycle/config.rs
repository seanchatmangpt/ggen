//! # Lifecycle Configuration
//!
//! Configuration structures for lifecycle management system.
//! Supports TOML-based configuration compatible with cargo-make format.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use crate::error::{Result, CleanroomError};

/// Main lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleConfig {
    /// Project name
    pub project_name: String,

    /// Project version
    #[serde(default = "default_version")]
    pub version: String,

    /// Lifecycle phases
    pub phases: Vec<Phase>,

    /// Environment configurations
    pub environments: HashMap<String, EnvironmentConfig>,

    /// Production readiness requirements
    pub readiness_requirements: Vec<Requirement>,

    /// Global settings
    #[serde(default)]
    pub settings: Settings,
}

/// Lifecycle phase definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Phase {
    /// Phase name (init, test, deploy, etc.)
    pub name: String,

    /// Command to execute
    pub command: String,

    /// Command arguments
    #[serde(default)]
    pub args: Vec<String>,

    /// Enable cleanroom isolation for this phase
    #[serde(default)]
    pub cleanroom_enabled: bool,

    /// Timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_seconds: u64,

    /// Environment variables
    #[serde(default)]
    pub env: HashMap<String, String>,

    /// Dependencies (phases that must run first)
    #[serde(default)]
    pub dependencies: Vec<String>,

    /// Continue on failure
    #[serde(default)]
    pub continue_on_failure: bool,
}

/// Environment configuration (dev, staging, production)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentConfig {
    /// Environment name
    pub name: String,

    /// Base URL or endpoint
    pub endpoint: Option<String>,

    /// Environment-specific variables
    #[serde(default)]
    pub variables: HashMap<String, String>,

    /// Deployment command
    pub deploy_command: Option<String>,

    /// Validation checks
    #[serde(default)]
    pub validation_checks: Vec<String>,

    /// Required services
    #[serde(default)]
    pub required_services: Vec<String>,
}

/// Production readiness requirement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Requirement {
    /// Requirement ID
    pub id: String,

    /// Requirement name
    pub name: String,

    /// Requirement description
    pub description: String,

    /// Current status
    #[serde(default)]
    pub status: Status,

    /// Requirement category
    pub category: RequirementCategory,

    /// Priority level (1-5, 5 is critical)
    #[serde(default = "default_priority")]
    pub priority: u8,

    /// Validation command (optional)
    pub validation_command: Option<String>,
}

/// Requirement completion status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    /// Not started
    NotStarted,

    /// In progress
    InProgress,

    /// Complete
    Complete,

    /// Blocked
    Blocked,
}

impl Default for Status {
    fn default() -> Self {
        Self::NotStarted
    }
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotStarted => write!(f, "not_started"),
            Self::InProgress => write!(f, "in_progress"),
            Self::Complete => write!(f, "complete"),
            Self::Blocked => write!(f, "blocked"),
        }
    }
}

/// Requirement category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RequirementCategory {
    /// Security requirements
    Security,

    /// Testing requirements
    Testing,

    /// Documentation requirements
    Documentation,

    /// Performance requirements
    Performance,

    /// Infrastructure requirements
    Infrastructure,

    /// Compliance requirements
    Compliance,
}

/// Global settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Settings {
    /// Enable verbose logging
    #[serde(default)]
    pub verbose: bool,

    /// Parallel execution
    #[serde(default = "default_true")]
    pub parallel: bool,

    /// Maximum parallel jobs
    #[serde(default = "default_max_jobs")]
    pub max_parallel_jobs: usize,

    /// Fail fast on errors
    #[serde(default = "default_true")]
    pub fail_fast: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            verbose: false,
            parallel: true,
            max_parallel_jobs: 4,
            fail_fast: true,
        }
    }
}

impl LifecycleConfig {
    /// Load configuration from TOML file
    pub async fn load(path: impl AsRef<Path>) -> Result<Self> {
        let content = tokio::fs::read_to_string(path.as_ref())
            .await
            .map_err(|e| CleanroomError::io_error(format!("Failed to read lifecycle config: {}", e)))?;

        let config: Self = toml::from_str(&content)
            .map_err(|e| CleanroomError::config_error(format!("Invalid TOML: {}", e)))?;

        config.validate()?;
        Ok(config)
    }

    /// Save configuration to TOML file
    pub async fn save(&self, path: impl AsRef<Path>) -> Result<()> {
        self.validate()?;

        let content = toml::to_string_pretty(self)
            .map_err(|e| CleanroomError::config_error(format!("Failed to serialize config: {}", e)))?;

        tokio::fs::write(path.as_ref(), content)
            .await
            .map_err(|e| CleanroomError::io_error(format!("Failed to write lifecycle config: {}", e)))?;

        Ok(())
    }

    /// Create default configuration with project name
    pub fn default_with_name(project_name: &str) -> Self {
        Self {
            project_name: project_name.to_string(),
            version: "0.1.0".to_string(),
            phases: Self::default_phases(),
            environments: Self::default_environments(),
            readiness_requirements: Self::default_requirements(),
            settings: Settings::default(),
        }
    }

    /// Get default phases
    fn default_phases() -> Vec<Phase> {
        vec![
            Phase {
                name: "init".to_string(),
                command: "cargo".to_string(),
                args: vec!["init".to_string(), "--lib".to_string()],
                cleanroom_enabled: false,
                timeout_seconds: 300,
                env: HashMap::new(),
                dependencies: vec![],
                continue_on_failure: false,
            },
            Phase {
                name: "build".to_string(),
                command: "cargo".to_string(),
                args: vec!["build".to_string()],
                cleanroom_enabled: true,
                timeout_seconds: 600,
                env: HashMap::new(),
                dependencies: vec!["init".to_string()],
                continue_on_failure: false,
            },
            Phase {
                name: "test".to_string(),
                command: "cargo".to_string(),
                args: vec!["test".to_string()],
                cleanroom_enabled: true,
                timeout_seconds: 600,
                env: HashMap::new(),
                dependencies: vec!["build".to_string()],
                continue_on_failure: false,
            },
            Phase {
                name: "validate".to_string(),
                command: "cargo".to_string(),
                args: vec!["check".to_string()],
                cleanroom_enabled: true,
                timeout_seconds: 300,
                env: HashMap::new(),
                dependencies: vec!["build".to_string()],
                continue_on_failure: false,
            },
        ]
    }

    /// Get default environments
    fn default_environments() -> HashMap<String, EnvironmentConfig> {
        let mut envs = HashMap::new();

        envs.insert("dev".to_string(), EnvironmentConfig {
            name: "dev".to_string(),
            endpoint: Some("http://localhost:8080".to_string()),
            variables: HashMap::new(),
            deploy_command: None,
            validation_checks: vec![],
            required_services: vec![],
        });

        envs.insert("staging".to_string(), EnvironmentConfig {
            name: "staging".to_string(),
            endpoint: Some("https://staging.example.com".to_string()),
            variables: HashMap::new(),
            deploy_command: Some("cargo build --release".to_string()),
            validation_checks: vec!["health_check".to_string()],
            required_services: vec!["database".to_string()],
        });

        envs.insert("production".to_string(), EnvironmentConfig {
            name: "production".to_string(),
            endpoint: Some("https://api.example.com".to_string()),
            variables: HashMap::new(),
            deploy_command: Some("cargo build --release".to_string()),
            validation_checks: vec![
                "health_check".to_string(),
                "security_audit".to_string(),
                "performance_test".to_string(),
            ],
            required_services: vec![
                "database".to_string(),
                "cache".to_string(),
                "monitoring".to_string(),
            ],
        });

        envs
    }

    /// Get default readiness requirements
    fn default_requirements() -> Vec<Requirement> {
        vec![
            Requirement {
                id: "auth-basic".to_string(),
                name: "Basic Authentication".to_string(),
                description: "Implement basic user authentication".to_string(),
                status: Status::NotStarted,
                category: RequirementCategory::Security,
                priority: 5,
                validation_command: None,
            },
            Requirement {
                id: "tests-unit".to_string(),
                name: "Unit Tests".to_string(),
                description: "80%+ unit test coverage".to_string(),
                status: Status::NotStarted,
                category: RequirementCategory::Testing,
                priority: 5,
                validation_command: Some("cargo test".to_string()),
            },
            Requirement {
                id: "docs-api".to_string(),
                name: "API Documentation".to_string(),
                description: "Complete API documentation".to_string(),
                status: Status::NotStarted,
                category: RequirementCategory::Documentation,
                priority: 3,
                validation_command: Some("cargo doc".to_string()),
            },
            Requirement {
                id: "perf-baseline".to_string(),
                name: "Performance Baseline".to_string(),
                description: "Establish performance benchmarks".to_string(),
                status: Status::NotStarted,
                category: RequirementCategory::Performance,
                priority: 4,
                validation_command: Some("cargo bench".to_string()),
            },
        ]
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        if self.project_name.is_empty() {
            return Err(CleanroomError::validation_error("Project name cannot be empty"));
        }

        if self.phases.is_empty() {
            return Err(CleanroomError::validation_error("At least one phase is required"));
        }

        // Validate phase dependencies
        let phase_names: Vec<&str> = self.phases.iter().map(|p| p.name.as_str()).collect();
        for phase in &self.phases {
            for dep in &phase.dependencies {
                if !phase_names.contains(&dep.as_str()) {
                    return Err(CleanroomError::validation_error(
                        format!("Phase '{}' depends on unknown phase '{}'", phase.name, dep)
                    ));
                }
            }
        }

        // Validate requirement priorities
        for req in &self.readiness_requirements {
            if req.priority < 1 || req.priority > 5 {
                return Err(CleanroomError::validation_error(
                    format!("Requirement '{}' has invalid priority (must be 1-5)", req.name)
                ));
            }
        }

        Ok(())
    }

    /// Get phase by name
    pub fn get_phase(&self, name: &str) -> Option<&Phase> {
        self.phases.iter().find(|p| p.name == name)
    }

    /// Get environment by name
    pub fn get_environment(&self, name: &str) -> Option<&EnvironmentConfig> {
        self.environments.get(name)
    }
}

// Helper functions for serde defaults
fn default_version() -> String {
    "0.1.0".to_string()
}

fn default_timeout() -> u64 {
    300
}

fn default_priority() -> u8 {
    3
}

fn default_true() -> bool {
    true
}

fn default_max_jobs() -> usize {
    4
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = LifecycleConfig::default_with_name("test-project");
        assert_eq!(config.project_name, "test-project");
        assert!(!config.phases.is_empty());
        assert!(!config.environments.is_empty());
        assert!(!config.readiness_requirements.is_empty());
    }

    #[test]
    fn test_config_validation() {
        let config = LifecycleConfig::default_with_name("test");
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_empty_project_name_validation() {
        let mut config = LifecycleConfig::default_with_name("test");
        config.project_name = String::new();
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_status_display() {
        assert_eq!(Status::NotStarted.to_string(), "not_started");
        assert_eq!(Status::InProgress.to_string(), "in_progress");
        assert_eq!(Status::Complete.to_string(), "complete");
        assert_eq!(Status::Blocked.to_string(), "blocked");
    }
}
