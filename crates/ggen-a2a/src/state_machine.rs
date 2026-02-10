//! Task state machine with transition logic and guards

use crate::{Task, TaskState};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// State transition errors
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum TransitionError {
    #[error("Invalid transition from {from:?} to {to:?}")]
    InvalidTransition { from: TaskState, to: TaskState },

    #[error("Task is in terminal state {state:?}")]
    TerminalState { state: TaskState },

    #[error("Task is not assigned to an agent")]
    NotAssigned,

    #[error("Task dependencies not met: {0}")]
    DependenciesNotMet(String),

    #[error("Guard condition failed: {0}")]
    GuardFailed(String),
}

/// State transition request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateTransition {
    /// Target state
    pub to_state: TaskState,
    /// Optional reason for transition
    pub reason: Option<String>,
    /// Agent requesting the transition
    pub agent_id: String,
}

impl StateTransition {
    /// Create a new state transition
    pub fn new(to_state: TaskState, agent_id: String) -> Self {
        Self {
            to_state,
            reason: None,
            agent_id,
        }
    }

    /// Set transition reason
    pub fn with_reason(mut self, reason: String) -> Self {
        self.reason = Some(reason);
        self
    }
}

/// State machine for task transitions
pub struct TaskStateMachine;

impl TaskStateMachine {
    /// Check if a transition is valid
    pub fn is_valid_transition(from: TaskState, to: TaskState) -> bool {
        match (from, to) {
            // From Created
            (TaskState::Created, TaskState::Running) => true,
            (TaskState::Created, TaskState::Failed) => true,

            // From Running
            (TaskState::Running, TaskState::Blocked) => true,
            (TaskState::Running, TaskState::Completed) => true,
            (TaskState::Running, TaskState::Failed) => true,

            // From Blocked
            (TaskState::Blocked, TaskState::Running) => true,
            (TaskState::Blocked, TaskState::Failed) => true,

            // Terminal states cannot transition
            (TaskState::Completed, _) => false,
            (TaskState::Failed, _) => false,

            // All other transitions invalid
            _ => false,
        }
    }

    /// Apply a state transition to a task
    pub fn transition(
        task: &mut Task,
        transition: StateTransition,
    ) -> Result<(), TransitionError> {
        // Guard: Check if task is already in terminal state
        if task.is_terminal() {
            return Err(TransitionError::TerminalState { state: task.state });
        }

        // Guard: Check if transition is valid
        if !Self::is_valid_transition(task.state, transition.to_state) {
            return Err(TransitionError::InvalidTransition {
                from: task.state,
                to: transition.to_state,
            });
        }

        // Guard: Check assignment for Running state
        if transition.to_state == TaskState::Running && task.assigned_to.is_none() {
            return Err(TransitionError::NotAssigned);
        }

        // Perform transition
        let _old_state = task.state;
        task.state = transition.to_state;
        task.updated_at = Utc::now();

        // Handle state-specific logic
        match transition.to_state {
            TaskState::Completed => {
                task.completed_at = Some(Utc::now());
            }
            TaskState::Failed => {
                task.completed_at = Some(Utc::now());
                task.failure_reason = transition.reason;
            }
            _ => {}
        }

        Ok(())
    }

    /// Get all possible next states for a given state
    pub fn possible_transitions(state: TaskState) -> Vec<TaskState> {
        match state {
            TaskState::Created => vec![TaskState::Running, TaskState::Failed],
            TaskState::Running => {
                vec![TaskState::Blocked, TaskState::Completed, TaskState::Failed]
            }
            TaskState::Blocked => vec![TaskState::Running, TaskState::Failed],
            TaskState::Completed | TaskState::Failed => vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_transitions() {
        // Created transitions
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Created,
            TaskState::Running
        ));
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Created,
            TaskState::Failed
        ));

        // Running transitions
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Running,
            TaskState::Blocked
        ));
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Running,
            TaskState::Completed
        ));
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Running,
            TaskState::Failed
        ));

        // Blocked transitions
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Blocked,
            TaskState::Running
        ));
        assert!(TaskStateMachine::is_valid_transition(
            TaskState::Blocked,
            TaskState::Failed
        ));
    }

    #[test]
    fn test_invalid_transitions() {
        // Cannot transition from terminal states
        assert!(!TaskStateMachine::is_valid_transition(
            TaskState::Completed,
            TaskState::Running
        ));
        assert!(!TaskStateMachine::is_valid_transition(
            TaskState::Failed,
            TaskState::Running
        ));

        // Invalid transitions
        assert!(!TaskStateMachine::is_valid_transition(
            TaskState::Created,
            TaskState::Blocked
        ));
        assert!(!TaskStateMachine::is_valid_transition(
            TaskState::Created,
            TaskState::Completed
        ));
    }

    #[test]
    fn test_transition_success() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string())
            .with_assignment("agent-1".to_string());

        let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
        let result = TaskStateMachine::transition(&mut task, transition);

        assert!(result.is_ok());
        assert_eq!(task.state, TaskState::Running);
    }

    #[test]
    fn test_transition_not_assigned() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string());

        let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
        let result = TaskStateMachine::transition(&mut task, transition);

        assert!(matches!(result, Err(TransitionError::NotAssigned)));
    }

    #[test]
    fn test_transition_invalid() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string());

        let transition = StateTransition::new(TaskState::Blocked, "agent-1".to_string());
        let result = TaskStateMachine::transition(&mut task, transition);

        assert!(matches!(
            result,
            Err(TransitionError::InvalidTransition { .. })
        ));
    }

    #[test]
    fn test_transition_terminal_state() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string())
            .with_assignment("agent-1".to_string());

        // Complete the task
        let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
        TaskStateMachine::transition(&mut task, transition).unwrap();

        let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
        TaskStateMachine::transition(&mut task, transition).unwrap();

        // Try to transition from terminal state
        let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
        let result = TaskStateMachine::transition(&mut task, transition);

        assert!(matches!(result, Err(TransitionError::TerminalState { .. })));
    }

    #[test]
    fn test_transition_with_reason() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string());

        let transition = StateTransition::new(TaskState::Failed, "agent-1".to_string())
            .with_reason("Connection timeout".to_string());

        TaskStateMachine::transition(&mut task, transition).unwrap();

        assert_eq!(task.state, TaskState::Failed);
        assert_eq!(
            task.failure_reason,
            Some("Connection timeout".to_string())
        );
        assert!(task.completed_at.is_some());
    }

    #[test]
    fn test_possible_transitions() {
        assert_eq!(
            TaskStateMachine::possible_transitions(TaskState::Created),
            vec![TaskState::Running, TaskState::Failed]
        );

        assert_eq!(
            TaskStateMachine::possible_transitions(TaskState::Running),
            vec![TaskState::Blocked, TaskState::Completed, TaskState::Failed]
        );

        assert_eq!(
            TaskStateMachine::possible_transitions(TaskState::Blocked),
            vec![TaskState::Running, TaskState::Failed]
        );

        assert_eq!(
            TaskStateMachine::possible_transitions(TaskState::Completed),
            Vec::<TaskState>::new()
        );

        assert_eq!(
            TaskStateMachine::possible_transitions(TaskState::Failed),
            Vec::<TaskState>::new()
        );
    }

    #[test]
    fn test_completed_transition_sets_timestamp() {
        let mut task = Task::new("Test".to_string(), "agent-1".to_string())
            .with_assignment("agent-1".to_string());

        let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
        TaskStateMachine::transition(&mut task, transition).unwrap();

        let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
        TaskStateMachine::transition(&mut task, transition).unwrap();

        assert!(task.completed_at.is_some());
        assert!(task.duration().is_some());
    }
}
