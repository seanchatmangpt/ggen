//! State mapping between YAWL and A2A task states
//!
//! Provides bidirectional mapping between YAWL workflow task states
//! and A2A protocol task/message states.

use a2a_generated::converged::message::MessageState;
// NOTE: Two distinct status enums are used here for different purposes:
//   - task::TaskStatus: Pending, Ready, Running, Completed, Failed, Cancelled
//     (used for A2A task lifecycle mapping)
//   - converged::message::MessageState: Created, Queued, InTransit, Delivered, Completed, Failed, Expired
//     (used for message lifecycle mapping within the converged model)
//
// State equivalence mapping:
//   YAWL NotStarted  -> TaskStatus::Pending      -> MessageState::Created
//   YAWL Ready       -> TaskStatus::Ready        -> MessageState::Queued
//   YAWL Executing   -> TaskStatus::Running       -> MessageState::InTransit
//   YAWL Completed   -> TaskStatus::Completed    -> MessageState::Completed
//   YAWL Failed      -> TaskStatus::Failed        -> MessageState::Failed
//   YAWL Cancelled   -> TaskStatus::Cancelled    -> MessageState::Failed
//   YAWL Suspended   -> TaskStatus::Pending      (no direct MessageState equivalent — maps to Created)
//
// No TaskStatus equivalent exists for MessageState::Delivered or MessageState::Expired.
use crate::otel_attrs;
use a2a_generated::task::TaskStatus;
use tracing::error;
use tracing::info;

/// Maps YAWL task states to A2A task states
pub struct YawlStateMapper;

impl YawlStateMapper {
    /// Map YAWL state string to A2A TaskStatus
    ///
    /// YAWL states (case-insensitive):
    ///   - disabled, notstarted, unstarted -> Pending
    ///   - enabled, active, executing, running -> Running
    ///   - suspended, ready -> Ready
    ///   - completed -> Completed
    ///   - failed, timeout -> Failed
    ///   - cancelled -> Cancelled
    ///   - inputwaiting, outputwaiting -> Pending
    ///
    /// A2A states: Pending, Ready, Running, Completed, Failed, Cancelled
    #[tracing::instrument(name = "ggen.yawl.task_execution", fields(
        otel_operation_name = "state_mapper.to_a2a_task_status",
        yawl_state_from = %yawl_state,
    ))]
    pub fn to_a2a_task_status(yawl_state: &str) -> TaskStatus {
        let result = match yawl_state.to_lowercase().as_str() {
            // Initial/inactive states
            "notstarted" | "unstarted" | "disabled" => TaskStatus::Pending,

            // Active execution states
            "executing" | "running" | "enabled" | "active" => TaskStatus::Running,

            // Waiting states
            "suspended" => TaskStatus::Pending,
            "ready" => TaskStatus::Ready,
            "inputwaiting" | "outputwaiting" => TaskStatus::Pending,

            // Terminal states
            "completed" => TaskStatus::Completed,
            "failed" | "timeout" => TaskStatus::Failed,
            "cancelled" => TaskStatus::Cancelled,

            // Unknown states default to Pending with error logging
            _ => {
                error!(unknown_state = %yawl_state, "Unknown YAWL state - defaulting to Pending");
                TaskStatus::Pending
            }
        };
        info!(from = yawl_state, to = ?result, "YAWL state mapped to A2A");
        tracing::Span::current().record(otel_attrs::YAWL_STATE_FROM, yawl_state);
        tracing::Span::current().record(otel_attrs::YAWL_STATE_TO, format!("{:?}", result));
        result
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
    #[tracing::instrument(name = "ggen.yawl.task_execution", fields(
        otel_operation_name = "state_mapper.is_valid_transition",
        yawl_state_from = from,
        yawl_state_to = to,
    ))]
    pub fn is_valid_transition(from: &str, to: &str) -> bool {
        let valid = match (from, to) {
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
        };
        if !valid {
            tracing::warn!(from, to, "Invalid YAWL state transition attempted");
        }
        info!(from, to, valid, "YAWL transition validated");
        tracing::Span::current().record(otel_attrs::YAWL_STATE_FROM, from);
        tracing::Span::current().record(otel_attrs::YAWL_STATE_TO, to);
        valid
    }

    /// Get all valid next states for a given YAWL state, including the current state
    /// (same-state transitions are valid no-ops per `is_valid_transition`).
    pub fn valid_next_states(current: &str) -> Vec<&'static str> {
        let next: Vec<&'static str> = match current {
            "NotStarted" => vec!["Ready", "Failed", "Cancelled"],
            "Ready" => vec!["Executing", "Failed", "Cancelled"],
            "Executing" => vec!["Suspended", "Completed", "Failed", "Cancelled"],
            "Suspended" => vec!["Executing", "Failed", "Cancelled"],
            "Completed" | "Failed" | "Cancelled" => vec![],
            _ => vec!["Ready", "Failed", "Cancelled"],
        };
        // Map the input to its &'static literal (all valid YAWL states are known at compile time).
        let current_static: &'static str = match current {
            "NotStarted" => "NotStarted",
            "Ready" => "Ready",
            "Executing" => "Executing",
            "Suspended" => "Suspended",
            "Completed" => "Completed",
            "Failed" => "Failed",
            "Cancelled" => "Cancelled",
            _other => {
                // Unknown state: fall back to input without prepending (no valid same-state transition).
                return next;
            }
        };
        let mut result = vec![current_static];
        result.extend(next);
        result
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

    // ========== NEW TESTS FOR BUG FIX ==========

    #[test]
    fn test_all_8_yawl_engine_states() {
        // Test all 8 actual YAWL engine states (lowercase)
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("disabled"),
            TaskStatus::Pending,
            "disabled should map to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("enabled"),
            TaskStatus::Running,
            "enabled should map to Running"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("active"),
            TaskStatus::Running,
            "active should map to Running"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("completed"),
            TaskStatus::Completed,
            "completed should map to Completed"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("cancelled"),
            TaskStatus::Cancelled,
            "cancelled should map to Cancelled"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("failed"),
            TaskStatus::Failed,
            "failed should map to Failed"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("timeout"),
            TaskStatus::Failed,
            "timeout should map to Failed"
        );
    }

    #[test]
    fn test_case_insensitive_matching() {
        // Test various case combinations
        let test_cases = vec![
            ("disabled", TaskStatus::Pending),
            ("DISABLED", TaskStatus::Pending),
            ("Disabled", TaskStatus::Pending),
            ("enabled", TaskStatus::Running),
            ("ENABLED", TaskStatus::Running),
            ("Enabled", TaskStatus::Running),
            ("active", TaskStatus::Running),
            ("ACTIVE", TaskStatus::Running),
            ("Active", TaskStatus::Running),
            ("completed", TaskStatus::Completed),
            ("COMPLETED", TaskStatus::Completed),
            ("Completed", TaskStatus::Completed),
            ("failed", TaskStatus::Failed),
            ("FAILED", TaskStatus::Failed),
            ("Failed", TaskStatus::Failed),
            ("cancelled", TaskStatus::Cancelled),
            ("CANCELLED", TaskStatus::Cancelled),
            ("Cancelled", TaskStatus::Cancelled),
        ];

        for (input, expected) in test_cases {
            assert_eq!(
                YawlStateMapper::to_a2a_task_status(input),
                expected,
                "State '{}' should map to {:?}",
                input,
                expected
            );
        }
    }

    #[test]
    fn test_additional_state_aliases() {
        // Test additional state aliases for completeness
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("notstarted"),
            TaskStatus::Pending,
            "notstarted should map to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("unstarted"),
            TaskStatus::Pending,
            "unstarted should map to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("suspended"),
            TaskStatus::Pending,
            "suspended should map to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("ready"),
            TaskStatus::Ready,
            "ready should map to Ready"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("executing"),
            TaskStatus::Running,
            "executing should map to Running"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("running"),
            TaskStatus::Running,
            "running should map to Running"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("inputwaiting"),
            TaskStatus::Pending,
            "inputwaiting should map to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("outputwaiting"),
            TaskStatus::Pending,
            "outputwaiting should map to Pending"
        );
    }

    #[test]
    fn test_unknown_state_defaults_to_pending() {
        // Test that unknown states default to Pending (no panic)
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("unknown"),
            TaskStatus::Pending,
            "unknown state should default to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("bogus"),
            TaskStatus::Pending,
            "bogus state should default to Pending"
        );
        assert_eq!(
            YawlStateMapper::to_a2a_task_status("xyz123"),
            TaskStatus::Pending,
            "xyz123 state should default to Pending"
        );
    }

    #[test]
    fn test_no_tasks_stuck_in_pending_after_fix() {
        // Verify that all 8 actual YAWL engine states are now mapped correctly
        // and NOT caught by the wildcard fallback
        let yawl_engine_states = vec![
            "disabled",
            "enabled",
            "active",
            "completed",
            "cancelled",
            "failed",
            "timeout",
        ];

        for state in yawl_engine_states {
            let status = YawlStateMapper::to_a2a_task_status(state);
            // All these states should be mapped to their correct status,
            // NOT stuck in Pending (which was the bug)
            match state {
                "disabled" => assert_eq!(status, TaskStatus::Pending),
                "enabled" | "active" => assert_eq!(status, TaskStatus::Running),
                "completed" => assert_eq!(status, TaskStatus::Completed),
                "cancelled" => assert_eq!(status, TaskStatus::Cancelled),
                "failed" | "timeout" => assert_eq!(status, TaskStatus::Failed),
                _ => panic!("Unexpected state: {}", state),
            }
        }
    }
}
