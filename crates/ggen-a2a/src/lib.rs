//! Agent-to-Agent Task State Machine Protocol
//!
//! Tasks are kanban cards that flow through a state machine.
//! Only terminal states (Completed/Failed) are allowed - no chat.

pub mod artifact;
pub mod state_machine;
pub mod transport;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

pub use artifact::{Artifact, ArtifactCollection, ArtifactContent, ArtifactError, ArtifactType};
pub use state_machine::{StateTransition, TaskStateMachine, TransitionError};
pub use transport::{Envelope, TaskMessage, Transport, TransportError};

/// Task state enum representing the kanban states
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskState {
    /// Task created but not started
    Created,
    /// Task currently being executed
    Running,
    /// Task blocked by external dependency
    Blocked,
    /// Task completed successfully (terminal state)
    Completed,
    /// Task failed with error (terminal state)
    Failed,
}

impl TaskState {
    /// Check if the state is terminal
    pub fn is_terminal(&self) -> bool {
        matches!(self, TaskState::Completed | TaskState::Failed)
    }

    /// Check if the state is active (not terminal)
    pub fn is_active(&self) -> bool {
        !self.is_terminal()
    }
}

/// Task represents a unit of work in the A2A protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Unique task identifier
    pub id: Uuid,
    /// Current state of the task
    pub state: TaskState,
    /// Task title/description
    pub title: String,
    /// Detailed task description
    pub description: Option<String>,
    /// Agent ID currently assigned to this task
    pub assigned_to: Option<String>,
    /// Agent ID that created this task
    pub created_by: String,
    /// Parent task ID for hierarchical tasks
    pub parent_id: Option<Uuid>,
    /// Task dependencies (must complete before this task)
    pub dependencies: Vec<Uuid>,
    /// Task artifacts (inputs and outputs)
    pub artifacts: HashMap<String, Artifact>,
    /// Task metadata
    pub metadata: HashMap<String, String>,
    /// Task creation timestamp
    pub created_at: DateTime<Utc>,
    /// Task last updated timestamp
    pub updated_at: DateTime<Utc>,
    /// Task completion timestamp
    pub completed_at: Option<DateTime<Utc>>,
    /// Failure reason if state is Failed
    pub failure_reason: Option<String>,
}

impl Task {
    /// Create a new task
    pub fn new(title: String, created_by: String) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4(),
            state: TaskState::Created,
            title,
            description: None,
            assigned_to: None,
            created_by,
            parent_id: None,
            dependencies: Vec::new(),
            artifacts: HashMap::new(),
            metadata: HashMap::new(),
            created_at: now,
            updated_at: now,
            completed_at: None,
            failure_reason: None,
        }
    }

    /// Set task description
    pub fn with_description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }

    /// Assign task to an agent
    pub fn with_assignment(mut self, agent_id: String) -> Self {
        self.assigned_to = Some(agent_id);
        self
    }

    /// Add a dependency
    pub fn with_dependency(mut self, dependency_id: Uuid) -> Self {
        self.dependencies.push(dependency_id);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Add an artifact
    pub fn with_artifact(mut self, name: String, artifact: Artifact) -> Self {
        self.artifacts.insert(name, artifact);
        self
    }

    /// Check if task is in terminal state
    pub fn is_terminal(&self) -> bool {
        self.state.is_terminal()
    }

    /// Check if task is active (not terminal)
    pub fn is_active(&self) -> bool {
        self.state.is_active()
    }

    /// Get task duration if completed
    pub fn duration(&self) -> Option<chrono::Duration> {
        self.completed_at
            .map(|completed| completed - self.created_at)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_creation() {
        let task = Task::new("Test Task".to_string(), "agent-1".to_string());
        assert_eq!(task.state, TaskState::Created);
        assert_eq!(task.title, "Test Task");
        assert_eq!(task.created_by, "agent-1");
        assert!(task.is_active());
        assert!(!task.is_terminal());
    }

    #[test]
    fn test_task_builder() {
        let task = Task::new("Test Task".to_string(), "agent-1".to_string())
            .with_description("Test description".to_string())
            .with_assignment("agent-2".to_string())
            .with_metadata("priority".to_string(), "high".to_string());

        assert_eq!(task.description, Some("Test description".to_string()));
        assert_eq!(task.assigned_to, Some("agent-2".to_string()));
        assert_eq!(task.metadata.get("priority"), Some(&"high".to_string()));
    }

    #[test]
    fn test_task_state_terminal() {
        assert!(!TaskState::Created.is_terminal());
        assert!(!TaskState::Running.is_terminal());
        assert!(!TaskState::Blocked.is_terminal());
        assert!(TaskState::Completed.is_terminal());
        assert!(TaskState::Failed.is_terminal());
    }

    #[test]
    fn test_task_with_dependency() {
        let dep_id = Uuid::new_v4();
        let task = Task::new("Test Task".to_string(), "agent-1".to_string())
            .with_dependency(dep_id);

        assert_eq!(task.dependencies.len(), 1);
        assert_eq!(task.dependencies[0], dep_id);
    }
}
