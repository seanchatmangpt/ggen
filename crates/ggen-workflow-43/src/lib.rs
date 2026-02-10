//! Complete implementation of van der Aalst's 43 workflow patterns
//!
//! This crate provides a comprehensive workflow engine implementing all 43 patterns
//! from the Workflow Patterns Initiative by van der Aalst et al.
//!
//! # Pattern Categories
//! - Basic Control Flow (1-5)
//! - Advanced Branching and Synchronization (6-12)
//! - Multiple Instance Patterns (13-16)
//! - State-Based Patterns (17-22)
//! - Cancellation and Force Completion Patterns (23-29)
//! - Iteration Patterns (30-43)

#![deny(missing_docs)]
#![deny(unsafe_code)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

pub mod patterns;
pub mod executor;

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;
use uuid::Uuid;

/// Result type for workflow operations
pub type Result<T> = std::result::Result<T, WorkflowError>;

/// Workflow execution errors
#[derive(Error, Debug, Clone)]
pub enum WorkflowError {
    /// Pattern-specific execution error
    #[error("Pattern execution failed: {0}")]
    PatternExecutionFailed(String),

    /// Activity not found
    #[error("Activity not found: {0}")]
    ActivityNotFound(String),

    /// Invalid state transition
    #[error("Invalid state transition: {0}")]
    InvalidStateTransition(String),

    /// Synchronization error
    #[error("Synchronization error: {0}")]
    SynchronizationError(String),

    /// Cancellation error
    #[error("Cancellation error: {0}")]
    CancellationError(String),

    /// Multiple instance error
    #[error("Multiple instance error: {0}")]
    MultipleInstanceError(String),
}

/// Workflow activity identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActivityId(pub String);

impl ActivityId {
    /// Create a new activity ID
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Generate a random activity ID
    pub fn generate() -> Self {
        Self(Uuid::new_v4().to_string())
    }
}

/// Activity execution state
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActivityState {
    /// Activity is ready to execute
    Ready,
    /// Activity is currently executing
    Running,
    /// Activity completed successfully
    Completed,
    /// Activity failed
    Failed,
    /// Activity was cancelled
    Cancelled,
    /// Activity is suspended
    Suspended,
}

/// Activity execution context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActivityContext {
    /// Activity identifier
    pub id: ActivityId,
    /// Current state
    pub state: ActivityState,
    /// Input data
    pub input_data: HashMap<String, serde_json::Value>,
    /// Output data
    pub output_data: HashMap<String, serde_json::Value>,
    /// Execution timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl ActivityContext {
    /// Create a new activity context
    pub fn new(id: ActivityId) -> Self {
        Self {
            id,
            state: ActivityState::Ready,
            input_data: HashMap::new(),
            output_data: HashMap::new(),
            timestamp: chrono::Utc::now(),
        }
    }

    /// Set input data
    pub fn with_input(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.input_data.insert(key.into(), value);
        self
    }

    /// Set output data
    pub fn set_output(&mut self, key: impl Into<String>, value: serde_json::Value) {
        self.output_data.insert(key.into(), value);
    }

    /// Update state
    pub fn set_state(&mut self, state: ActivityState) {
        self.state = state;
        self.timestamp = chrono::Utc::now();
    }
}

/// Trait for executable workflow activities
#[async_trait]
pub trait Activity: Send + Sync {
    /// Execute the activity
    async fn execute(&self, context: &mut ActivityContext) -> Result<()>;

    /// Get activity identifier
    fn id(&self) -> &ActivityId;

    /// Check if activity can be cancelled
    fn can_cancel(&self) -> bool {
        true
    }

    /// Cancel the activity
    async fn cancel(&self, context: &mut ActivityContext) -> Result<()> {
        context.set_state(ActivityState::Cancelled);
        Ok(())
    }
}

/// Workflow pattern trait
#[async_trait]
pub trait WorkflowPattern: Send + Sync {
    /// Get pattern name
    fn name(&self) -> &str;

    /// Get pattern number (1-43)
    fn pattern_number(&self) -> u8;

    /// Execute the pattern
    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()>;

    /// Check if pattern supports cancellation
    fn supports_cancellation(&self) -> bool {
        false
    }
}

/// Main workflow engine
pub struct WorkflowEngine {
    /// Active activities
    pub activities: HashMap<ActivityId, Box<dyn Activity>>,
    /// Activity contexts
    pub contexts: HashMap<ActivityId, ActivityContext>,
    /// Pattern execution history
    pub execution_history: Vec<PatternExecution>,
}

/// Pattern execution record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternExecution {
    /// Pattern name
    pub pattern_name: String,
    /// Pattern number
    pub pattern_number: u8,
    /// Execution timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Activities involved
    pub activities: Vec<ActivityId>,
    /// Execution result
    pub result: ExecutionResult,
}

/// Execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionResult {
    /// Successful execution
    Success,
    /// Failed execution with error message
    Failed(String),
}

impl WorkflowEngine {
    /// Create a new workflow engine
    pub fn new() -> Self {
        Self {
            activities: HashMap::new(),
            contexts: HashMap::new(),
            execution_history: Vec::new(),
        }
    }

    /// Register an activity
    pub fn register_activity(&mut self, activity: Box<dyn Activity>) {
        let id = activity.id().clone();
        let context = ActivityContext::new(id.clone());
        self.activities.insert(id.clone(), activity);
        self.contexts.insert(id, context);
    }

    /// Get activity context
    pub fn get_context(&self, id: &ActivityId) -> Result<&ActivityContext> {
        self.contexts.get(id)
            .ok_or_else(|| WorkflowError::ActivityNotFound(id.0.clone()))
    }

    /// Get mutable activity context
    pub fn get_context_mut(&mut self, id: &ActivityId) -> Result<&mut ActivityContext> {
        self.contexts.get_mut(id)
            .ok_or_else(|| WorkflowError::ActivityNotFound(id.0.clone()))
    }

    /// Execute an activity
    pub async fn execute_activity(&mut self, id: &ActivityId) -> Result<()> {
        // Check activity exists
        if !self.activities.contains_key(id) {
            return Err(WorkflowError::ActivityNotFound(id.0.clone()));
        }

        // Set running state
        if let Some(context) = self.contexts.get_mut(id) {
            context.set_state(ActivityState::Running);
        }

        // Remove activity temporarily to avoid borrow conflicts
        let activity = self.activities.remove(id)
            .ok_or_else(|| WorkflowError::ActivityNotFound(id.0.clone()))?;

        // Get mutable context
        let context = self.contexts.get_mut(id)
            .ok_or_else(|| WorkflowError::ActivityNotFound(id.0.clone()))?;

        // Execute
        let result = activity.execute(context).await;

        // Put activity back
        self.activities.insert(id.clone(), activity);

        // Update state based on result
        match result {
            Ok(()) => {
                if let Some(ctx) = self.contexts.get_mut(id) {
                    ctx.set_state(ActivityState::Completed);
                }
                Ok(())
            }
            Err(e) => {
                if let Some(ctx) = self.contexts.get_mut(id) {
                    ctx.set_state(ActivityState::Failed);
                }
                Err(e)
            }
        }
    }

    /// Record pattern execution
    pub fn record_execution(&mut self, pattern: &dyn WorkflowPattern, activities: Vec<ActivityId>, result: ExecutionResult) {
        self.execution_history.push(PatternExecution {
            pattern_name: pattern.name().to_string(),
            pattern_number: pattern.pattern_number(),
            timestamp: chrono::Utc::now(),
            activities,
            result,
        });
    }
}

impl Default for WorkflowEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestActivity {
        id: ActivityId,
    }

    #[async_trait]
    impl Activity for TestActivity {
        async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
            context.set_output("result", serde_json::json!("success"));
            Ok(())
        }

        fn id(&self) -> &ActivityId {
            &self.id
        }
    }

    #[tokio::test]
    async fn test_workflow_engine_basic_execution() {
        let mut engine = WorkflowEngine::new();
        let activity_id = ActivityId::new("test-1");
        let activity = Box::new(TestActivity { id: activity_id.clone() });

        engine.register_activity(activity);

        let result = engine.execute_activity(&activity_id).await;
        assert!(result.is_ok());

        let context = engine.get_context(&activity_id);
        assert!(context.is_ok());
        let context = context.ok().unwrap();
        assert_eq!(context.state, ActivityState::Completed);
    }
}
