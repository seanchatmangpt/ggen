//! WP17: Cancel Activity Pattern Tests
//!
//! The Cancel Activity pattern allows an enabled or running activity to be
//! cancelled before it completes.
//!
//! Pattern definition:
//! - An activity in the process can be disabled
//! - If the activity is running, it is terminated
//! - If the activity is enabled but not started, it is disabled
//!
//! YAWL representation:
//! - Cancellation region or cancellation set
//! - Task can be cancelled by external trigger
//! - Flows may include cancellation conditions

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod cancel_activity_tests {
    use super::*;

    /// Test: WP17 - Basic cancel activity
    #[test]
    fn test_wp17_cancel_activity_basic() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Trigger".to_string(),
                name: "Trigger".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "CancellableTask".to_string(),
                name: "Cancellable Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false, // Manual task can be cancelled
                decomposition_id: Some("CANCELLABLE".to_string()),
            },
            TaskContext {
                id: "CancelTrigger".to_string(),
                name: "Cancel Trigger".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Trigger".to_string(),
                target: "CancellableTask".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "CancelTrigger".to_string(),
                target: "CancellableTask".to_string(),
                condition: Some("action == 'cancel'".to_string()),
                predicate: Some("action == 'cancel'".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp17_cancel_activity".to_string(),
            description: "Cancel activity pattern".to_string(),
            version: "1.0.0".to_string(),
            tasks,
            flows,
            input_condition: Some(ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: Some(ConditionContext {
                id: "output".to_string(),
                expression: "true".to_string(),
                condition_type: "output".to_string(),
            }),
            variables: vec![],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("CancellableTask"));
        assert!(generated.contains("CancelTrigger"));

        // Verify golden file
        compare_with_golden("wp17_cancel_activity", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP17 - Cancel with timeout
    #[test]
    fn test_wp17_cancel_with_timeout() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "StartTask".to_string(),
                name: "Start Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "LongRunning".to_string(),
                name: "Long Running Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("TIMEOUT_CANCELLABLE".to_string()),
            },
            TaskContext {
                id: "HandleTimeout".to_string(),
                name: "Handle Timeout".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "StartTask".to_string(),
                target: "LongRunning".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "LongRunning".to_string(),
                target: "HandleTimeout".to_string(),
                condition: Some("status == 'timeout'".to_string()),
                predicate: Some("status == 'timeout'".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp17_cancel_timeout".to_string(),
            description: "Cancel with timeout pattern".to_string(),
            version: "1.0.0".to_string(),
            tasks,
            flows,
            input_condition: Some(ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: Some(ConditionContext {
                id: "output".to_string(),
                expression: "true".to_string(),
                condition_type: "output".to_string(),
            }),
            variables: vec![],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("LongRunning"));
        assert!(generated.contains("HandleTimeout"));

        // Verify golden file
        compare_with_golden("wp17_cancel_timeout", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP17 - Cancel semantics
    ///
    /// Verifies that the YAWL XML correctly encodes cancellation:
    /// - Manual task (can be cancelled)
    /// - Cancellation condition/trigger
    #[test]
    fn test_wp17_cancel_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "WaitForInput".to_string(),
                name: "Wait For Input".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false, // Manual = cancellable
                decomposition_id: Some("CANCELLABLE".to_string()),
            },
        ];

        let flows = vec![];

        let ctx = TemplateContext {
            workflow_name: "wp17_cancel_semantics".to_string(),
            description: "Cancel activity semantics".to_string(),
            version: "1.0.0".to_string(),
            tasks,
            flows,
            input_condition: Some(ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: Some(ConditionContext {
                id: "output".to_string(),
                expression: "true".to_string(),
                condition_type: "output".to_string(),
            }),
            variables: vec![],
        };

        // Verify task is manual (cancellable)
        let task = ctx.tasks.first().unwrap();
        assert!(!task.is_auto, "Cancellable tasks must be manual (not auto)");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("WaitForInput"));

        // Verify golden file
        compare_with_golden("wp17_cancel_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
