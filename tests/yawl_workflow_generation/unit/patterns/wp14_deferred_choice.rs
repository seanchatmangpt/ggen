//! WP14: Deferred Choice Pattern Tests
//!
//! The Deferred Choice pattern represents a choice point where the alternative
//! branches are known at design time, but the actual choice is made at runtime
//! based on external events or conditions.
//!
//! Pattern definition:
//! - Multiple branches are available
//! - The choice is deferred until runtime based on which event occurs first
//! - Once a branch is chosen, others are cancelled
//!
//! YAWL representation:
//! - Task with deferred choice semantics
//! - Multiple conditional flows
//! - Event-based selection (not data-based)

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod deferred_choice_tests {
    use super::*;

    /// Test: WP14 - Basic deferred choice (wait for event A or B)
    #[test]
    fn test_wp14_deferred_choice_basic() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "WaitForEvent".to_string(),
                name: "Wait for Event".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false, // Manual task - waits for external trigger
                decomposition_id: Some("DEFERRED".to_string()),
            },
            TaskContext {
                id: "HandleEventA".to_string(),
                name: "Handle Event A".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "HandleEventB".to_string(),
                name: "Handle Event B".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "WaitForEvent".to_string(),
                target: "HandleEventA".to_string(),
                condition: Some("event.type == 'A'".to_string()),
                predicate: Some("event.type == 'A'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "WaitForEvent".to_string(),
                target: "HandleEventB".to_string(),
                condition: Some("event.type == 'B'".to_string()),
                predicate: Some("event.type == 'B'".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp14_deferred_basic".to_string(),
            description: "Deferred choice between events".to_string(),
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
        assert!(generated.contains("WaitForEvent"));
        assert!(generated.contains("HandleEventA"));
        assert!(generated.contains("HandleEventB"));

        // Verify golden file
        compare_with_golden("wp14_deferred_basic", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP14 - Timeout-based deferred choice
    #[test]
    fn test_wp14_deferred_timeout() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "WaitWithTimeout".to_string(),
                name: "Wait with Timeout".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("DEFERRED_TIMEOUT".to_string()),
            },
            TaskContext {
                id: "HandleResponse".to_string(),
                name: "Handle Response".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
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
                source: "WaitWithTimeout".to_string(),
                target: "HandleResponse".to_string(),
                condition: Some("response.received == true".to_string()),
                predicate: Some("response.received == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "WaitWithTimeout".to_string(),
                target: "HandleTimeout".to_string(),
                condition: Some("timed.out == true".to_string()),
                predicate: Some("timed.out == true".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp14_deferred_timeout".to_string(),
            description: "Deferred choice with timeout".to_string(),
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
        assert!(generated.contains("HandleResponse"));
        assert!(generated.contains("HandleTimeout"));

        // Verify golden file
        compare_with_golden("wp14_deferred_timeout", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP14 - Deferred choice semantics
    ///
    /// Verifies that the YAWL XML correctly encodes deferred choice:
    /// - Manual task at choice point
    /// - Multiple conditional outgoing flows
    /// - Event-driven selection
    #[test]
    fn test_wp14_deferred_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "ChoicePoint".to_string(),
                name: "Deferred Choice".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false, // Key: Manual = waits for external event
                decomposition_id: Some("DEFERRED".to_string()),
            },
            TaskContext {
                id: "Option1".to_string(),
                name: "Option 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Option2".to_string(),
                name: "Option 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "ChoicePoint".to_string(),
                target: "Option1".to_string(),
                condition: Some("selected == 'option1'".to_string()),
                predicate: Some("selected == 'option1'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "ChoicePoint".to_string(),
                target: "Option2".to_string(),
                condition: Some("selected == 'option2'".to_string()),
                predicate: Some("selected == 'option2'".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp14_deferred_semantics".to_string(),
            description: "Deferred choice semantics".to_string(),
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

        // Verify choice point is manual (not auto)
        let choice_task = ctx.tasks.first().unwrap();
        assert!(!choice_task.is_auto, "Deferred choice requires manual task");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify manual task marker
        assert!(!generated.contains("<starting/>") || generated.contains("isAuto=\"false\""));

        // Verify golden file
        compare_with_golden("wp14_deferred_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
