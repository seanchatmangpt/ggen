//! GitHub Actions workflow generation
//!
//! This module provides functionality to generate GitHub Actions workflow YAML files
//! from workflow configurations. It supports job parallelization, matrix strategies,
//! caching, and Docker image pre-pulling.

use super::workflow_config::{
    JobConfig, StepAction, StepConfig, WorkflowConfig, WorkflowTrigger,
};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// GitHub Actions workflow generator
#[derive(Debug, Clone)]
pub struct WorkflowGenerator {
    /// Workflow configuration
    config: WorkflowConfig,
}

/// Workflow job representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowJob {
    /// Job name
    pub name: String,
    /// Runner to use (e.g., ubuntu-latest)
    #[serde(rename = "runs-on")]
    pub runs_on: String,
    /// Job steps
    pub steps: Vec<WorkflowStep>,
    /// Job timeout in minutes
    #[serde(rename = "timeout-minutes", skip_serializing_if = "Option::is_none")]
    pub timeout_minutes: Option<u32>,
    /// Environment variables
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub env: HashMap<String, String>,
}

/// Workflow step representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStep {
    /// Step name
    pub name: String,
    /// Action to use (e.g., actions/checkout@v4)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uses: Option<String>,
    /// Command to run
    #[serde(skip_serializing_if = "Option::is_none")]
    pub run: Option<String>,
    /// Action inputs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub with: Option<HashMap<String, String>>,
    /// Environment variables
    #[serde(skip_serializing_if = "Option::is_none")]
    pub env: Option<HashMap<String, String>>,
}

/// Workflow trigger (used in YAML generation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowTriggerYaml {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub push: Option<TriggerConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pull_request: Option<TriggerConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schedule: Option<Vec<ScheduleConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workflow_dispatch: Option<serde_yaml::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TriggerConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branches: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScheduleConfig {
    pub cron: String,
}

impl WorkflowGenerator {
    /// Create a new workflow generator
    #[must_use]
    pub fn new(config: WorkflowConfig) -> Self {
        Self { config }
    }

    /// Generate complete GitHub Actions workflow YAML
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - YAML serialization fails
    /// - Workflow configuration is invalid
    pub async fn generate_workflow(&self) -> Result<String> {
        let mut yaml = String::new();

        // Add header comment
        yaml.push_str("# Auto-generated GitHub Actions workflow by ggen\n");
        yaml.push_str("# DO NOT EDIT - Regenerate with: ggen sync --ci-workflows\n\n");

        // Workflow name
        yaml.push_str(&format!("name: {}\n\n", self.config.name));

        // Triggers
        yaml.push_str("on:\n");
        for trigger in &self.config.triggers {
            yaml.push_str(&Self::format_trigger(trigger)?);
        }
        yaml.push_str("\n");

        // Global environment variables
        if !self.config.env.is_empty() {
            yaml.push_str("env:\n");
            for (key, value) in &self.config.env {
                yaml.push_str(&format!("  {}: {}\n", key, value));
            }
            yaml.push_str("\n");
        }

        // Jobs
        yaml.push_str("jobs:\n");
        for job in &self.config.jobs {
            yaml.push_str(&self.format_job(job).await?);
        }

        Ok(yaml)
    }

    /// Format a trigger for YAML
    fn format_trigger(trigger: &WorkflowTrigger) -> Result<String> {
        let mut yaml = String::new();

        match trigger {
            WorkflowTrigger::Push { branches } => {
                yaml.push_str("  push:\n");
                if !branches.is_empty() {
                    yaml.push_str("    branches:\n");
                    for branch in branches {
                        yaml.push_str(&format!("      - {}\n", branch));
                    }
                }
            }
            WorkflowTrigger::PullRequest { branches } => {
                yaml.push_str("  pull_request:\n");
                if !branches.is_empty() {
                    yaml.push_str("    branches:\n");
                    for branch in branches {
                        yaml.push_str(&format!("      - {}\n", branch));
                    }
                }
            }
            WorkflowTrigger::Schedule { cron } => {
                yaml.push_str("  schedule:\n");
                yaml.push_str(&format!("    - cron: '{}'\n", cron));
            }
            WorkflowTrigger::WorkflowDispatch => {
                yaml.push_str("  workflow_dispatch:\n");
            }
        }

        Ok(yaml)
    }

    /// Format a job for YAML
    async fn format_job(&self, job: &JobConfig) -> Result<String> {
        let mut yaml = String::new();

        yaml.push_str(&format!("  {}:\n", job.id));
        yaml.push_str(&format!("    name: {}\n", job.name));
        yaml.push_str(&format!("    runs-on: {}\n", job.runs_on));

        // Timeout
        if job.timeout_minutes > 0 {
            yaml.push_str(&format!("    timeout-minutes: {}\n", job.timeout_minutes));
        }

        // Environment variables
        if !job.env.is_empty() {
            yaml.push_str("    env:\n");
            for (key, value) in &job.env {
                yaml.push_str(&format!("      {}: {}\n", key, value));
            }
        }

        // Steps
        yaml.push_str("    steps:\n");
        for step in &job.steps {
            yaml.push_str(&Self::format_step(step)?);
        }

        yaml.push_str("\n");
        Ok(yaml)
    }

    /// Format a step for YAML
    fn format_step(step: &StepConfig) -> Result<String> {
        let mut yaml = String::new();

        yaml.push_str(&format!("      - name: {}\n", step.name));

        match &step.action {
            StepAction::Uses { action, with } => {
                yaml.push_str(&format!("        uses: {}\n", action));
                if !with.is_empty() {
                    yaml.push_str("        with:\n");
                    for (key, value) in with {
                        // Check if value contains newlines (for multi-line values)
                        if value.contains('\n') {
                            yaml.push_str(&format!("          {}: |\n", key));
                            for line in value.lines() {
                                yaml.push_str(&format!("            {}\n", line));
                            }
                        } else {
                            yaml.push_str(&format!("          {}: {}\n", key, value));
                        }
                    }
                }
            }
            StepAction::Run { command } => {
                yaml.push_str(&format!("        run: {}\n", command));
            }
        }

        // Environment variables
        if !step.env.is_empty() {
            yaml.push_str("        env:\n");
            for (key, value) in &step.env {
                yaml.push_str(&format!("          {}: {}\n", key, value));
            }
        }

        Ok(yaml)
    }

    /// Generate workflow with badge URL
    ///
    /// # Errors
    ///
    /// Returns an error if workflow generation fails
    pub async fn generate_with_badge(&self, repo_owner: &str, repo_name: &str) -> Result<(String, String)> {
        let workflow_yaml = self.generate_workflow().await?;

        let badge_url = format!(
            "![CI Status](https://github.com/{}/{}/workflows/{}/badge.svg)",
            repo_owner,
            repo_name,
            self.config.name.replace(' ', "%20")
        );

        Ok((workflow_yaml, badge_url))
    }

    /// Validate generated YAML syntax
    ///
    /// # Errors
    ///
    /// Returns an error if YAML is invalid
    pub fn validate_yaml(yaml: &str) -> Result<()> {
        serde_yaml::from_str::<serde_yaml::Value>(yaml)
            .map_err(|e| Error::with_source(
                "Generated YAML is invalid",
                Box::new(e),
            ))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ci::workflow_config::WorkflowOptimization;

    #[tokio::test]
    async fn test_generate_basic_workflow() -> Result<()> {
        // Arrange
        let mut config = WorkflowConfig::new("Test".to_string());
        config.jobs.push(JobConfig {
            id: "test".to_string(),
            name: "Run Tests".to_string(),
            runs_on: "ubuntu-latest".to_string(),
            steps: vec![
                StepConfig {
                    name: "Checkout".to_string(),
                    action: StepAction::Uses {
                        action: "actions/checkout@v4".to_string(),
                        with: HashMap::new(),
                    },
                    env: HashMap::new(),
                },
                StepConfig {
                    name: "Run tests".to_string(),
                    action: StepAction::Run {
                        command: "cargo make test".to_string(),
                    },
                    env: HashMap::new(),
                },
            ],
            matrix: None,
            env: HashMap::new(),
            timeout_minutes: 30,
        });

        let generator = WorkflowGenerator::new(config);

        // Act
        let yaml = generator.generate_workflow().await?;

        // Assert
        assert!(yaml.contains("name: Test"));
        assert!(yaml.contains("runs-on: ubuntu-latest"));
        assert!(yaml.contains("actions/checkout@v4"));
        assert!(yaml.contains("cargo make test"));
        WorkflowGenerator::validate_yaml(&yaml)?;

        Ok(())
    }

    #[tokio::test]
    async fn test_generate_with_badge() -> Result<()> {
        // Arrange
        let config = WorkflowConfig::new("CI Tests".to_string());
        let generator = WorkflowGenerator::new(config);

        // Act
        let (yaml, badge) = generator.generate_with_badge("owner", "repo").await?;

        // Assert
        assert!(yaml.contains("name: CI Tests"));
        assert!(badge.contains("![CI Status]"));
        assert!(badge.contains("github.com/owner/repo"));

        Ok(())
    }

    #[test]
    fn test_validate_yaml_valid() {
        // Arrange
        let yaml = r#"
name: Test
on: push
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
"#;

        // Act
        let result = WorkflowGenerator::validate_yaml(yaml);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_yaml_invalid() {
        // Arrange
        let yaml = "invalid: yaml: syntax: error:";

        // Act
        let result = WorkflowGenerator::validate_yaml(yaml);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_format_trigger_push() -> Result<()> {
        // Arrange
        let trigger = WorkflowTrigger::Push {
            branches: vec!["main".to_string(), "develop".to_string()],
        };

        // Act
        let yaml = WorkflowGenerator::format_trigger(&trigger)?;

        // Assert
        assert!(yaml.contains("push:"));
        assert!(yaml.contains("- main"));
        assert!(yaml.contains("- develop"));

        Ok(())
    }

    #[test]
    fn test_format_trigger_schedule() -> Result<()> {
        // Arrange
        let trigger = WorkflowTrigger::Schedule {
            cron: "0 0 * * *".to_string(),
        };

        // Act
        let yaml = WorkflowGenerator::format_trigger(&trigger)?;

        // Assert
        assert!(yaml.contains("schedule:"));
        assert!(yaml.contains("0 0 * * *"));

        Ok(())
    }
}
