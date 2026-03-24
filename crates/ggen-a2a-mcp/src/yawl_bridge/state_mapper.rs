//! State mapping between YAWL and A2A task states
//!
//! Provides bidirectional mapping between YAWL workflow task states
//! and A2A protocol task/message states.

use a2a_generated::converged::message::MessageState;
use a2a_generated::task::TaskStatus;

/// Maps YAWL task states to A2A task states
pub struct YawlStateMapper;

impl YawlStateMapper {
    /// Map YAWL state string to A2A TaskStatus
    ///
    /// YAWL states: NotStarted, Executing, Suspended, Completed, Failed, Cancelled, Ready
    /// A2A states: Pending, Ready, Running, Completed, Failed, Cancelled
    pub fn to_a2a_task_status(yawl_state: &str) -> TaskStatus {
        match yawl_state {
            "NotStarted" => TaskStatus::Pending,
            "Executing" => TaskStatus::Running,
            "Suspended" => TaskStatus::Pending,
            "Completed" => TaskStatus::Completed,
            "Failed" => TaskStatus::Failed,
            "Cancelled" => TaskStatus::Cancelled,
            "Ready" => TaskStatus::Ready,
            _ => TaskStatus::Pending,
        }
    }

    /// Map A2A TaskStatus to YAWL state string
    ///
    /// A2A states: Pending, Ready, Running, Completed, Failed, Cancelled
    /// YAWL states: NotStarted, Executing, Suspended, Completed, Failed, Cancelled, Ready
    pub fn from_a2a_task_status(a2a_status: TaskStatus) -> &'static str {
        match a2a_status {
            TaskStatus::Pending => "NotStarted",
            TaskStatus::Ready => "Ready",
            TaskStatus::Running => "Executing",
            TaskStatus::Completed => "Completed",
            TaskStatus::Failed => "Failed",
            TaskStatus::Cancelled => "Cancelled",
        }
    }

    /// Map A2A TaskStatus to ConvergedMessage MessageState
    ///
    /// Maps task execution states to message lifecycle states
    pub fn to_message_state(a2a_status: TaskStatus) -> MessageState {
        match a2a_status {
            TaskStatus::Pending => MessageState::Created,
            TaskStatus::Ready => MessageState::Queued,
            TaskStatus::Running => MessageState::InTransit,
            TaskStatus::Completed => MessageState::Completed,
            TaskStatus::Failed | TaskStatus::Cancelled => MessageState::Failed,
        }
    }

    /// Check if a YAWL state transition is valid
    ///
    /// YAWL state transitions follow specific rules:
    /// - NotStarted -> Ready -> Executing -> Completed
    /// - Any state -> Failed (on error)
    /// - Any non-terminal state -> Cancelled
    pub fn is_valid_transition(from: &str, to: &str) -> bool {
        match (from, to) {
            // Valid forward transitions
            ("NotStarted", "Ready") => true,
            ("Ready", "Executing") => true,
            ("Executing", "Completed") => true,
            ("Executing", "Suspended") => true,
            ("Suspended", "Executing") => true,

            // Terminal states
            ("Completed", _) => false,
            ("Failed", _) => false,
            ("Cancelled", _) => false,

            // Error transitions from any non-terminal state
            (from, "Failed") if !matches!(from, "Completed" | "Failed" | "Cancelled") => true,
            (from, "Cancelled") if !matches!(from, "Completed" | "Failed" | "Cancelled") => true,

            // Same state is valid (no-op)
            (s1, s2) if s1 == s2 => true,

            // All other transitions are invalid
            _ => false,
        }
    }

    /// Get all valid next states for a given YAWL state
    pub fn valid_next_states(current: &str) -> Vec<&'static str> {
        match current {
            "NotStarted" => vec!["Ready", "Failed", "Cancelled"],
            "Ready" => vec!["Executing", "Failed", "Cancelled"],
            "Executing" => vec!["Suspended", "Completed", "Failed", "Cancelled"],
            "Suspended" => vec!["Executing", "Failed", "Cancelled"],
            "Completed" | "Failed" | "Cancelled" => vec![],
            _ => vec!["Ready", "Failed", "Cancelled"],
        }
    }

    /// Check if a YAWL state is a terminal state
    pub fn is_terminal(state: &str) -> bool {
        matches!(state, "Completed" | "Failed" | "Cancelled")
    }

    /// Check if a YAWL state is an active (non-terminal) state
    pub fn is_active(state: &str) -> bool {
        !Self::is_terminal(state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yawl_to_a2a_mapping() {
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("NotStarted"),
            TaskStatus::Pending
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("Executing"),
            TaskStatus::Running
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("Completed"),
            TaskStatus::Completed
        );
    }

    #[test]
    fn test_a2a_to_yawl_mapping() {
        assert_eq!(
            YawlStateMapper::from_a2a_task_status(TaskStatus::Pending),
            "NotStarted"
        );
        assert_eq!(
            YawlStateMapper::from_a2a_task_status(TaskStatus::Running),
            "Executing"
        );
        assert_eq!(
            YawlStateMapper::from_a2a_task_status(TaskStatus::Completed),
            "Completed"
        );
    }

    #[test]
    fn test_message_state_mapping() {
        assert_eq!(
            YawlStateMapper::to_message_state(TaskStatus::Running),
            MessageState::InTransit
        );
        assert_eq!(
            YawlStateMapper::to_message_state(TaskStatus::Completed),
            MessageState::Completed
        );
    }

    #[test]
    fn test_valid_transitions() {
        assert!(YawlStateMapper::is_valid_transition("NotStarted", "Ready"));
        assert!(YawlStateMapper::is_valid_transition("Ready", "Executing"));
        assert!(YawlStateMapper::is_valid_transition(
            "Executing",
            "Completed"
        ));
        assert!(!YawlStateMapper::is_valid_transition(
            "Completed",
            "Executing"
        ));
    }

    #[test]
    fn test_terminal_states() {
        assert!(YawlStateMapper::is_terminal("Completed"));
        assert!(YawlStateMapper::is_terminal("Failed"));
        assert!(YawlStateMapper::is_terminal("Cancelled"));
        assert!(!YawlStateMapper::is_terminal("Executing"));
        assert!(!YawlStateMapper::is_terminal("Ready"));
    }
}
