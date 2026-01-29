//! Workflow configuration types and RDF extraction
//!
//! This module provides types for configuring CI workflows and extracting
//! workflow requirements from RDF ontologies using SPARQL queries.

use crate::Graph;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete workflow configuration extracted from RDF
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowConfig {
    /// Name of the workflow
    pub name: String,
    /// Workflow triggers (push, pull_request, schedule, etc.)
    pub triggers: Vec<WorkflowTrigger>,
    /// Jobs to run in the workflow
    pub jobs: Vec<JobConfig>,
    /// Global environment variables
    pub env: HashMap<String, String>,
    /// Optimization settings
    pub optimization: WorkflowOptimization,
}

/// Workflow trigger configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum WorkflowTrigger {
    /// Trigger on push to branches
    Push { branches: Vec<String> },
    /// Trigger on pull request
    PullRequest { branches: Vec<String> },
    /// Trigger on schedule (cron)
    Schedule { cron: String },
    /// Trigger on workflow dispatch
    WorkflowDispatch,
}

/// Configuration for a single job
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobConfig {
    /// Job identifier
    pub id: String,
    /// Human-readable job name
    pub name: String,
    /// Job dependencies (runs-on)
    pub runs_on: String,
    /// Steps to execute
    pub steps: Vec<StepConfig>,
    /// Matrix strategy (if any)
    pub matrix: Option<MatrixConfig>,
    /// Job-level environment variables
    pub env: HashMap<String, String>,
    /// Timeout in minutes
    pub timeout_minutes: u32,
}

/// Step within a job
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepConfig {
    /// Step name
    pub name: String,
    /// Action to use (uses) or command to run (run)
    pub action: StepAction,
    /// Step-level environment variables
    pub env: HashMap<String, String>,
}

/// Step action type
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum StepAction {
    /// Use a GitHub Action
    Uses { action: String, with: HashMap<String, String> },
    /// Run a shell command
    Run { command: String },
}

/// Matrix build strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatrixConfig {
    /// Matrix dimensions (e.g., otp_version, os)
    pub dimensions: HashMap<String, Vec<String>>,
    /// Include specific combinations
    pub include: Vec<HashMap<String, String>>,
    /// Exclude specific combinations
    pub exclude: Vec<HashMap<String, String>>,
}

/// Workflow optimization settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowOptimization {
    /// Enable dependency caching
    pub cache_dependencies: bool,
    /// Pre-pull Docker images
    pub prepull_images: bool,
    /// Parallel job execution
    pub parallel_jobs: bool,
    /// Maximum parallel jobs
    pub max_parallel: u32,
}

/// Cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheConfig {
    /// Cache key
    pub key: String,
    /// Paths to cache
    pub paths: Vec<String>,
    /// Restore keys (fallbacks)
    pub restore_keys: Vec<String>,
}

impl WorkflowConfig {
    /// Create a new workflow configuration with defaults
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            triggers: vec![
                WorkflowTrigger::Push {
                    branches: vec!["main".to_string(), "develop".to_string()],
                },
                WorkflowTrigger::PullRequest {
                    branches: vec!["main".to_string()],
                },
            ],
            jobs: Vec::new(),
            env: HashMap::new(),
            optimization: WorkflowOptimization::default(),
        }
    }

    /// Extract workflow configuration from RDF graph using SPARQL
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - SPARQL queries fail to execute
    /// - Required RDF properties are missing
    /// - RDF data is malformed
    pub fn from_graph(graph: &Graph) -> Result<Self> {
        let mut config = Self::new("Test with Containers".to_string());

        // Extract test suites
        let test_suites = Self::extract_test_suites(graph)?;

        // Create jobs from test suites
        for suite in test_suites {
            let job = Self::create_job_from_suite(&suite)?;
            config.jobs.push(job);
        }

        // Extract optimization settings
        config.optimization = Self::extract_optimization(graph)?;

        Ok(config)
    }

    /// Extract test suite configurations from RDF
    fn extract_test_suites(graph: &Graph) -> Result<Vec<TestSuite>> {
        let query = r#"
            PREFIX ggen: <http://ggen.example.org/>
            PREFIX test: <http://ggen.example.org/test/>

            SELECT ?suite ?testType ?timeout ?requires
            WHERE {
                ?suite a ggen:TestSuite .
                ?suite ggen:testType ?testType .
                OPTIONAL { ?suite ggen:timeout ?timeout }
                OPTIONAL { ?suite ggen:requires ?requires }
            }
        "#;

        let cached_result = graph.query_cached(query).map_err(|e| {
            Error::with_source(
                "Failed to query test suites from RDF",
                Box::new(e),
            )
        })?;

        let mut suites = Vec::new();

        // Extract solutions from CachedResult
        if let crate::graph::types::CachedResult::Solutions(rows) = cached_result {
            for row in rows {
                let test_type = row.get("testType")
                    .ok_or_else(|| Error::new("Missing testType in RDF"))?
                    .clone();

                let timeout = row.get("timeout")
                    .map(|s| s.clone())
                    .unwrap_or_else(|| "30s".to_string());

                let requires = row.get("requires").map(|s| s.clone());

                suites.push(TestSuite {
                    test_type,
                    timeout,
                    requires,
                });
            }
        }

        Ok(suites)
    }

    /// Create a job configuration from a test suite
    fn create_job_from_suite(suite: &TestSuite) -> Result<JobConfig> {
        let timeout_minutes = Self::parse_timeout(&suite.timeout)?;

        let mut steps = vec![
            StepConfig {
                name: "Checkout code".to_string(),
                action: StepAction::Uses {
                    action: "actions/checkout@v4".to_string(),
                    with: HashMap::new(),
                },
                env: HashMap::new(),
            },
        ];

        // Add cache step if needed
        if suite.requires.as_deref() == Some("cache") {
            steps.push(StepConfig {
                name: "Cache dependencies".to_string(),
                action: StepAction::Uses {
                    action: "actions/cache@v4".to_string(),
                    with: HashMap::from([
                        ("path".to_string(), "_build\ndeps".to_string()),
                        ("key".to_string(), "${{ runner.os }}-mix-${{ hashFiles('**/mix.lock') }}".to_string()),
                    ]),
                },
                env: HashMap::new(),
            });
        }

        // Add test execution step
        let test_command = match suite.test_type.as_str() {
            "unit" => "cargo make test-unit",
            "integration" => "cargo make test",
            "chaos" => "cargo make test -- chaos",
            "benchmark" => "cargo make bench",
            _ => "cargo make test",
        };

        steps.push(StepConfig {
            name: format!("Run {} tests", suite.test_type),
            action: StepAction::Run {
                command: test_command.to_string(),
            },
            env: HashMap::new(),
        });

        Ok(JobConfig {
            id: format!("{}_tests", suite.test_type),
            name: format!("{} Tests", suite.test_type),
            runs_on: "ubuntu-latest".to_string(),
            steps,
            matrix: None,
            env: HashMap::new(),
            timeout_minutes,
        })
    }

    /// Parse timeout string (e.g., "30s", "10m") to minutes
    fn parse_timeout(timeout: &str) -> Result<u32> {
        let timeout = timeout.trim();

        if let Some(seconds) = timeout.strip_suffix('s') {
            let secs: u32 = seconds.parse()
                .map_err(|_| Error::invalid_input(&format!("Invalid timeout seconds: {}", seconds)))?;
            Ok((secs + 59) / 60) // Round up to minutes
        } else if let Some(minutes) = timeout.strip_suffix('m') {
            minutes.parse()
                .map_err(|_| Error::invalid_input(&format!("Invalid timeout minutes: {}", minutes)))
        } else {
            Err(Error::invalid_input(&format!("Invalid timeout format: {}", timeout)))
        }
    }

    /// Extract optimization settings from RDF
    fn extract_optimization(graph: &Graph) -> Result<WorkflowOptimization> {
        let query = r#"
            PREFIX ggen: <http://ggen.example.org/>
            PREFIX opt: <http://ggen.example.org/optimization/>

            SELECT ?cacheDeps ?prepullImages ?parallelJobs ?maxParallel
            WHERE {
                OPTIONAL { opt:config ggen:cacheDependencies ?cacheDeps }
                OPTIONAL { opt:config ggen:prepullImages ?prepullImages }
                OPTIONAL { opt:config ggen:parallelJobs ?parallelJobs }
                OPTIONAL { opt:config ggen:maxParallel ?maxParallel }
            }
        "#;

        let cached_result = graph.query_cached(query).map_err(|e| {
            Error::with_source(
                "Failed to query optimization settings from RDF",
                Box::new(e),
            )
        })?;

        // Use defaults if no optimization config in RDF
        if let crate::graph::types::CachedResult::Solutions(rows) = cached_result {
            if rows.is_empty() {
                return Ok(WorkflowOptimization::default());
            }

            let row = &rows[0];

            Ok(WorkflowOptimization {
                cache_dependencies: row.get("cacheDeps")
                    .and_then(|v| v.parse::<bool>().ok())
                    .unwrap_or(true),
                prepull_images: row.get("prepullImages")
                    .and_then(|v| v.parse::<bool>().ok())
                    .unwrap_or(true),
                parallel_jobs: row.get("parallelJobs")
                    .and_then(|v| v.parse::<bool>().ok())
                    .unwrap_or(true),
                max_parallel: row.get("maxParallel")
                    .and_then(|v| v.parse::<u32>().ok())
                    .unwrap_or(8),
            })
        } else {
            Ok(WorkflowOptimization::default())
        }
    }
}

impl Default for WorkflowOptimization {
    fn default() -> Self {
        Self {
            cache_dependencies: true,
            prepull_images: true,
            parallel_jobs: true,
            max_parallel: 8,
        }
    }
}

/// Internal representation of a test suite
#[derive(Debug, Clone)]
struct TestSuite {
    test_type: String,
    timeout: String,
    requires: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_config_new() {
        // Arrange
        let name = "Test Workflow".to_string();

        // Act
        let config = WorkflowConfig::new(name.clone());

        // Assert
        assert_eq!(config.name, name);
        assert_eq!(config.triggers.len(), 2);
        assert!(config.jobs.is_empty());
    }

    #[test]
    fn test_parse_timeout_seconds() -> Result<()> {
        // Arrange
        let timeout = "90s";

        // Act
        let minutes = WorkflowConfig::parse_timeout(timeout)?;

        // Assert
        assert_eq!(minutes, 2); // 90s rounds up to 2 minutes
        Ok(())
    }

    #[test]
    fn test_parse_timeout_minutes() -> Result<()> {
        // Arrange
        let timeout = "15m";

        // Act
        let minutes = WorkflowConfig::parse_timeout(timeout)?;

        // Assert
        assert_eq!(minutes, 15);
        Ok(())
    }

    #[test]
    fn test_parse_timeout_invalid() {
        // Arrange
        let timeout = "invalid";

        // Act
        let result = WorkflowConfig::parse_timeout(timeout);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_workflow_optimization_default() {
        // Arrange & Act
        let opt = WorkflowOptimization::default();

        // Assert
        assert!(opt.cache_dependencies);
        assert!(opt.prepull_images);
        assert!(opt.parallel_jobs);
        assert_eq!(opt.max_parallel, 8);
    }
}
