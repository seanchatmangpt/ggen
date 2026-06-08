//! KNHK V2 Type System - Foundational data structures for the workflow engine
//!
//! This crate defines all core types used throughout KNHK V2, including:
//! - Workflow and pattern definitions
//! - Execution state and events
//! - Error types
//! - Configuration types

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// A workflow pattern identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PatternId(pub u32);

impl PatternId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// A step in a workflow execution
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct StepId(pub String);

impl StepId {
    pub fn generate() -> Self {
        Self(Uuid::new_v4().to_string())
    }
}

/// Unique workflow execution identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExecutionId(pub Uuid);

impl ExecutionId {
    pub fn generate() -> Self {
        Self(Uuid::new_v4())
    }
}

/// Workflow definition - declarative specification of pattern flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowDef {
    pub id: String,
    pub name: String,
    pub version: String,
    pub patterns: Vec<PatternDef>,
    pub steps: Vec<WorkflowStep>,
}

/// Pattern definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternDef {
    pub id: PatternId,
    pub name: String,
    pub category: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

/// A single step in a workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStep {
    pub id: StepId,
    pub pattern_id: PatternId,
    pub inputs: HashMap<String, serde_json::Value>,
    pub outputs: Vec<String>,
}

/// Execution state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Workflow execution context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionContext {
    pub id: ExecutionId,
    pub workflow_id: String,
    pub state: ExecutionState,
    pub current_step: Option<StepId>,
    pub data: HashMap<String, serde_json::Value>,
}

impl ExecutionContext {
    pub fn new(workflow_id: String) -> Self {
        Self {
            id: ExecutionId::generate(),
            workflow_id,
            state: ExecutionState::Pending,
            current_step: None,
            data: HashMap::new(),
        }
    }
}

/// Execution event - immutable record of what happened
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub id: String,
    pub execution_id: ExecutionId,
    pub step_id: Option<StepId>,
    pub event_type: String,
    pub data: serde_json::Value,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl Event {
    pub fn new(execution_id: ExecutionId, event_type: String, data: serde_json::Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            execution_id,
            step_id: None,
            event_type,
            data,
            timestamp: chrono::Utc::now(),
        }
    }
}

/// Error type for KNHK operations
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Pattern not found: {0}")]
    PatternNotFound(String),

    #[error("Execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("State machine error: {0}")]
    StateError(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Other error: {0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
// Tests use unwrap() for clear failure messages; panics are intentional in test context.
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_execution_id_generate() {
        let id1 = ExecutionId::generate();
        let id2 = ExecutionId::generate();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_execution_context_new() {
        let ctx = ExecutionContext::new("test-workflow".to_string());
        assert_eq!(ctx.state, ExecutionState::Pending);
        assert!(ctx.current_step.is_none());
    }

    #[test]
    fn test_event_serialization() {
        let exec_id = ExecutionId::generate();
        let event = Event::new(
            exec_id,
            "test_event".to_string(),
            serde_json::json!({"key": "value"}),
        );

        let json = serde_json::to_string(&event).unwrap();
        let deserialized: Event = serde_json::from_str(&json).unwrap();

        assert_eq!(event.event_type, deserialized.event_type);
    }
}
