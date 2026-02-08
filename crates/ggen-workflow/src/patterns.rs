//! Workflow pattern implementations
//!
//! This module provides implementations for the four core workflow patterns:
//! Sequence, Parallel, Choice, and Sync. Each pattern implements the
//! WorkflowPattern trait for consistent execution and error handling.

use crate::error::{WorkflowError, WorkflowResult};
use crate::CONSTANTS;
use std::collections::HashMap;

/// Trait for all workflow patterns
pub trait WorkflowPattern {
    /// Execute the workflow pattern
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()>;

    /// Validate the pattern configuration
    fn validate(&self) -> WorkflowResult<()>;

    /// Get pattern name for logging and debugging
    fn name(&self) -> &'static str;
}

/// Context for workflow execution
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct WorkflowContext {
    /// Input data for the workflow
    pub input: HashMap<String, serde_json::Value>,
    /// Output data from completed steps
    pub output: HashMap<String, serde_json::Value>,
    /// Configuration parameters
    pub config: HashMap<String, serde_json::Value>,
    /// Step execution metadata
    pub metadata: ExecutionMetadata,
}

/// Metadata for workflow execution
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExecutionMetadata {
    /// Workflow ID
    pub workflow_id: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Execution trace
    pub trace: Vec<TraceEvent>,
}

/// Event trace for debugging
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TraceEvent {
    /// Step name
    pub step: String,
    /// Event type
    pub event_type: String,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Message
    pub message: String,
}

/// Sequence pattern - Linear step execution
pub struct Sequence {
    /// Steps in sequence
    pub steps: Vec<String>,
}

impl WorkflowPattern for Sequence {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        for (i, step) in self.steps.iter().enumerate() {
            // Execute step logic would go here
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Executing step {}", i + 1),
            });

            // Simulate step execution
            context.output.insert(format!("{}_result", step), serde_json::json!({
                "step": step,
                "index": i,
                "status": "completed"
            }));

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Completed step {}", i + 1),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation("Sequence pattern must have at least one step".to_string()));
        }
        if self.steps.len() > CONSTANTS.max_workflow_steps {
            return Err(WorkflowError::Validation(format!(
                "Sequence pattern exceeds maximum steps limit: {} > {}",
                self.steps.len(),
                CONSTANTS.max_workflow_steps
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "sequence"
    }
}

/// Parallel pattern - Concurrent execution
pub struct Parallel {
    /// Steps to execute in parallel
    pub steps: Vec<String>,
    /// Synchronization configuration
    pub sync_config: SyncConfig,
}

impl WorkflowPattern for Parallel {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        if self.steps.len() > CONSTANTS.max_parallel_tasks {
            return Err(WorkflowError::Validation(format!(
                "Parallel pattern exceeds maximum parallel tasks: {} > {}",
                self.steps.len(),
                CONSTANTS.max_parallel_tasks
            )));
        }

        // In a real implementation, this would spawn tasks concurrently
        for step in self.steps.iter() {
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Starting parallel step: {}", step),
            });

            // Simulate parallel execution
            context.output.insert(format!("{}_result", step), serde_json::json!({
                "step": step,
                "status": "completed",
                "parallel": true
            }));

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Completed parallel step: {}", step),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation("Parallel pattern must have at least one step".to_string()));
        }
        if self.steps.len() > CONSTANTS.max_parallel_tasks {
            return Err(WorkflowError::Validation(format!(
                "Parallel pattern exceeds maximum parallel tasks: {} > {}",
                self.steps.len(),
                CONSTANTS.max_parallel_tasks
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "parallel"
    }
}

/// Choice pattern - Conditional branching
pub struct Choice {
    /// Condition to evaluate
    pub condition: String,
    /// Branches for different outcomes
    pub branches: HashMap<String, Box<dyn WorkflowPattern>>,
}

impl WorkflowPattern for Choice {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        // Evaluate condition
        let selected_branch = match context.input.get(&self.condition) {
            Some(value) => {
                if value.is_boolean() && value.as_bool().unwrap_or(false) {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            None => {
                return Err(WorkflowError::Validation(format!(
                    "Condition '{}' not found in input",
                    self.condition
                )));
            }
        };

        // Execute selected branch
        if let Some(branch) = self.branches.get(&selected_branch) {
            context.metadata.trace.push(TraceEvent {
                step: "choice".to_string(),
                event_type: "selected".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Selected branch: {}", selected_branch),
            });

            branch.execute(context)?;
        } else {
            return Err(WorkflowError::Validation(format!(
                "No branch found for condition: {}",
                selected_branch
            )));
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.branches.is_empty() {
            return Err(WorkflowError::Validation("Choice pattern must have at least one branch".to_string()));
        }
        if self.branches.len() > 10 {
            return Err(WorkflowError::Validation("Choice pattern supports maximum 10 branches".to_string()));
        }

        for (branch_name, branch) in self.branches.iter() {
            branch.validate()?;

            // Ensure branch names are valid
            if !branch_name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return Err(WorkflowError::Validation(format!(
                    "Invalid branch name: '{}'. Only alphanumeric and underscore allowed",
                    branch_name
                )));
            }
        }

        Ok(())
    }

    fn name(&self) -> &'static str {
        "choice"
    }
}

/// Sync pattern - Barrier synchronization
pub struct Sync {
    /// Steps to synchronize
    pub steps: Vec<String>,
    /// Synchronization configuration
    pub sync_config: SyncConfig,
}

impl WorkflowPattern for Sync {
    fn execute(&self, context: &mut WorkflowContext) -> WorkflowResult<()> {
        // Execute all steps with barrier synchronization
        for step in self.steps.iter() {
            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "sync_start".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Synchronization point: {}", step),
            });

            // Simulate step execution with synchronization
            context.output.insert(format!("{}_sync_result", step), serde_json::json!({
                "step": step,
                "status": "synchronized",
                "barrier_reached": true
            }));

            context.metadata.trace.push(TraceEvent {
                step: step.clone(),
                event_type: "sync_complete".to_string(),
                timestamp: chrono::Utc::now(),
                message: format!("Synchronization completed: {}", step),
            });
        }

        Ok(())
    }

    fn validate(&self) -> WorkflowResult<()> {
        if self.steps.is_empty() {
            return Err(WorkflowError::Validation("Sync pattern must have at least one step".to_string()));
        }
        if self.steps.len() > CONSTANTS.max_parallel_tasks {
            return Err(WorkflowError::Validation(format!(
                "Sync pattern exceeds maximum parallel tasks: {} > {}",
                self.steps.len(),
                CONSTANTS.max_parallel_tasks
            )));
        }
        Ok(())
    }

    fn name(&self) -> &'static str {
        "sync"
    }
}

/// Synchronization configuration
#[derive(Debug, Clone)]
pub struct SyncConfig {
    /// Timeout in milliseconds
    pub timeout_ms: u64,
    /// Maximum retries
    pub max_retries: usize,
    /// Whether to fail fast
    pub fail_fast: bool,
}

impl Default for SyncConfig {
    fn default() -> Self {
        SyncConfig {
            timeout_ms: CONSTANTS.default_timeout_ms,
            max_retries: 3,
            fail_fast: false,
        }
    }
}

impl Default for WorkflowContext {
    fn default() -> Self {
        WorkflowContext {
            input: HashMap::new(),
            output: HashMap::new(),
            config: HashMap::new(),
            metadata: ExecutionMetadata {
                workflow_id: uuid::Uuid::new_v4().to_string(),
                timestamp: chrono::Utc::now(),
                trace: Vec::new(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sequence_pattern() {
        let sequence = Sequence {
            steps: vec!["step1".to_string(), "step2".to_string()],
        };

        assert_eq!(sequence.name(), "sequence");
        sequence.validate().unwrap();

        let mut context = WorkflowContext::default();
        sequence.execute(&mut context).unwrap();
        assert_eq!(context.output.len(), 2);
    }

    #[test]
    fn test_parallel_pattern() {
        let parallel = Parallel {
            steps: vec!["step1".to_string(), "step2".to_string()],
            sync_config: SyncConfig::default(),
        };

        assert_eq!(parallel.name(), "parallel");
        parallel.validate().unwrap();
    }

    #[test]
    fn test_choice_pattern() {
        let mut branches = HashMap::new();
        branches.insert("true".to_string(), Box::new(Sequence {
            steps: vec!["true_branch".to_string()],
        }) as Box<dyn WorkflowPattern>);
        branches.insert("false".to_string(), Box::new(Sequence {
            steps: vec!["false_branch".to_string()],
        }));

        let choice = Choice {
            condition: "test_condition".to_string(),
            branches,
        };

        assert_eq!(choice.name(), "choice");
        choice.validate().unwrap();
    }

    #[test]
    fn test_sync_pattern() {
        let sync = Sync {
            steps: vec!["step1".to_string(), "step2".to_string()],
            sync_config: SyncConfig::default(),
        };

        assert_eq!(sync.name(), "sync");
        sync.validate().unwrap();
    }

    #[test]
    fn test_workflow_context() {
        let context = WorkflowContext::default();
        assert!(!context.metadata.workflow_id.is_empty());
        assert!(context.metadata.trace.is_empty());
    }
}