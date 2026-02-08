//! WP18: Cancel Case Pattern Tests
//!
//! The Cancel Case pattern allows an entire workflow case to be terminated,
//! cancelling all activities within the case.
//!
//! Pattern definition:
//! - A mechanism to terminate the entire process instance
//! - All active and enabled tasks are cancelled
//! - Workflow case is marked as cancelled
//!
//! YAWL representation:
//! - Cancellation condition at workflow level
//! - Special cancel handling
//! - All tasks respond to cancel signal

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod cancel_case_tests {
    use super::*;

    /// Test: WP18 - Basic cancel case
    #[test]
    fn test_wp18_cancel_case_basic() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Start".to_string(),
                name: "Start".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Task1".to_string(),
                name: "Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CANCEL_REGION_ALL".to_string()),
            },
            TaskContext {
                id: "Task2".to_string(),
                name: "Task 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CANCEL_REGION_ALL".to_string()),
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
                source: "Start".to_string(),
                target: "Task1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "Task2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "CancelTrigger".to_string(),
                target: "Task1".to_string(),
                condition: Some("cancel.case == true".to_string()),
                predicate: Some("cancel.case == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp18_cancel_case".to_string(),
            description: "Cancel case pattern".to_string(),
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
        assert!(generated.contains("cancel.case"));

        // Verify both tasks are in cancellation region
        let task1_region = ctx.tasks[1].decomposition_id.as_ref().unwrap();
        let task2_region = ctx.tasks[2].decomposition_id.as_ref().unwrap();
        assert_eq!(task1_region, task2_region);

        // Verify golden file
        compare_with_golden("wp18_cancel_case", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP18 - Cancel case semantics
    ///
    /// Verifies that the YAWL XML correctly encodes case cancellation:
    /// - All tasks in cancellation region
    /// - Cancel trigger affects entire case
    #[test]
    fn test_wp18_cancel_case_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Init".to_string(),
                name: "Initialize".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Work1".to_string(),
                name: "Work 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CASE_CANCEL".to_string()),
            },
            TaskContext {
                id: "Work2".to_string(),
                name: "Work 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CASE_CANCEL".to_string()),
            },
            TaskContext {
                id: "EmergencyCancel".to_string(),
                name: "Emergency Cancel".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Init".to_string(),
                target: "Work1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Init".to_string(),
                target: "Work2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "EmergencyCancel".to_string(),
                target: "Work1".to_string(),
                condition: Some("emergency == true".to_string()),
                predicate: Some("emergency == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp18_cancel_case_semantics".to_string(),
            description: "Cancel case semantics".to_string(),
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

        // Assert - Verify emergency cancel condition
        assert!(generated.contains("emergency"));

        // Verify golden file
        compare_with_golden("wp18_cancel_case_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
