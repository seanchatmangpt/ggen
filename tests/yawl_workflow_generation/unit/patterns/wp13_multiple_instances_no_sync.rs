//! WP13: Multiple Instances Without Synchronization Pattern Tests
//!
//! The Multiple Instances Without Synchronization pattern creates multiple
//! instances that execute independently without waiting for each other.
//!
//! Pattern definition:
//! - A single activity is instantiated multiple times
//! - Instances execute independently
//! - No synchronization barrier: workflow continues immediately
//!
//! YAWL representation:
//! - Task with special MI behavior
//! - AND split to spawn instances
//! - XOR join (don't wait for all instances)

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod multiple_instances_no_sync_tests {
    use super::*;

    /// Test: WP13 - Multiple instances without synchronization
    #[test]
    fn test_wp13_mi_no_sync() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Spawn".to_string(),
                name: "Spawn Tasks".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "IndependentTask".to_string(),
                name: "Independent Task".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(), // No sync: XOR join
                is_auto: true,
                decomposition_id: Some("MI_NO_SYNC".to_string()),
            },
            TaskContext {
                id: "Continue".to_string(),
                name: "Continue Immediately".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Spawn".to_string(),
                target: "IndependentTask".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "IndependentTask".to_string(),
                target: "Continue".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp13_mi_no_sync".to_string(),
            description: "Multiple instances without synchronization".to_string(),
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

        // Assert - Verify XOR join (no sync)
        assert!(generated.contains("XOR"));

        // Verify golden file
        compare_with_golden("wp13_mi_no_sync", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP13 - Fire-and-forget pattern
    #[test]
    fn test_wp13_fire_and_forget() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "TriggerNotifications".to_string(),
                name: "Trigger Notifications".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "SendNotification".to_string(),
                name: "Send Notification".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: Some("MI_FIRE_FORGET".to_string()),
            },
            TaskContext {
                id: "Proceed".to_string(),
                name: "Proceed Without Waiting".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "TriggerNotifications".to_string(),
                target: "SendNotification".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "TriggerNotifications".to_string(),
                target: "Proceed".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp13_fire_forget".to_string(),
            description: "Fire and forget pattern".to_string(),
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
        assert!(generated.contains("SendNotification"));
        assert!(generated.contains("Proceed"));

        // Verify golden file
        compare_with_golden("wp13_fire_forget", &generated)
            .expect("Generated XML matches golden file");
    }
}
