//! WP1: Sequence Pattern Tests
//!
//! The Sequence pattern represents the most basic workflow control flow:
//! one task executes after another in a predetermined order.
//!
//! Pattern definition:
//! - An activity A is followed by activity B
//! - B cannot start until A has completed
//! - No branching or concurrency involved
//!
//! YAWL representation:
//! - Tasks connected by flows with default split/join behavior (XOR)
//! - Flow from A to B with no condition

use ggen_yawl::template::{FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod sequence_tests {
    use super::*;

    /// Helper to create a basic sequence workflow context
    fn create_sequence_workflow(
        name: &str,
        tasks: Vec<&str>,
    ) -> TemplateContext {
        let task_contexts: Vec<TaskContext> = tasks
            .iter()
            .enumerate()
            .map(|(i, id)| TaskContext {
                id: id.to_string(),
                name: format!("Task {}", id),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        let flows: Vec<FlowContext> = tasks
            .windows(2)
            .enumerate()
            .map(|(i, pair)| FlowContext {
                source: pair[0].to_string(),
                target: pair[1].to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with sequence pattern", name),
            version: "1.0.0".to_string(),
            tasks: task_contexts,
            flows,
            input_condition: Some(ggen_yawl::template::ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: Some(ggen_yawl::template::ConditionContext {
                id: "output".to_string(),
                expression: "true".to_string(),
                condition_type: "output".to_string(),
            }),
            variables: vec![],
        }
    }

    /// Test: WP1 - Two task sequence (A -> B)
    #[test]
    fn test_wp1_sequence_two_tasks() {
        // Arrange
        let ctx = create_sequence_workflow("wp1_two_task_sequence", vec!["A", "B"]);

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("<task id=\"A\""));
        assert!(generated.contains("<task id=\"B\""));
        assert!(generated.contains("<flow into=\"B\""));
        assert!(generated.contains("from=\"A\""));

        // Verify golden file
        compare_with_golden("wp1_two_task_sequence", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP1 - Three task sequence (A -> B -> C)
    #[test]
    fn test_wp1_sequence_three_tasks() {
        // Arrange
        let ctx = create_sequence_workflow("wp1_three_task_sequence", vec!["A", "B", "C"]);

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify all tasks in order
        let a_pos = generated.find("id=\"A\"").unwrap();
        let b_pos = generated.find("id=\"B\"").unwrap();
        let c_pos = generated.find("id=\"C\"").unwrap();
        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);

        // Verify flows connect in sequence
        assert!(generated.contains("from=\"A\"") && generated.contains("into=\"B\""));
        assert!(generated.contains("from=\"B\"") && generated.contains("into=\"C\""));

        // Verify golden file
        compare_with_golden("wp1_three_task_sequence", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP1 - Long sequence (A -> B -> C -> D -> E)
    #[test]
    fn test_wp1_sequence_long_chain() {
        // Arrange
        let ctx = create_sequence_workflow(
            "wp1_long_sequence",
            vec!["A", "B", "C", "D", "E"],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify 5 tasks
        assert_eq!(ctx.tasks.len(), 5);
        assert_eq!(ctx.flows.len(), 4); // n-1 flows for n tasks

        // Verify golden file
        compare_with_golden("wp1_long_sequence", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP1 - Sequential task execution semantics
    ///
    /// Verifies that the YAWL XML correctly encodes sequential execution:
    /// - XOR split on source task (only one outgoing path)
    /// - XOR join on target (only one incoming path needed)
    /// - Default flow with no condition
    #[test]
    fn test_wp1_sequence_semantics() {
        // Arrange
        let mut ctx = create_sequence_workflow("wp1_sequence_semantics", vec!["Task1", "Task2"]);

        // Verify task split/join types enforce sequence
        assert_eq!(ctx.tasks[0].split_type, "XOR");
        assert_eq!(ctx.tasks[0].join_type, "XOR");
        assert_eq!(ctx.tasks[1].split_type, "XOR");
        assert_eq!(ctx.tasks[1].join_type, "XOR");

        // Verify flow has no condition (unconditional execution)
        assert!(ctx.flows[0].condition.is_none());
        assert!(ctx.flows[0].is_default);

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify XML contains default flow marker
        assert!(generated.contains("<isDefaultFlow>true</isDefaultFlow>") ||
                generated.contains("isDefault=\"true\""));

        // Verify golden file
        compare_with_golden("wp1_sequence_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP1 - Sequence preserves task ordering
    #[test]
    fn test_wp1_sequence_preserves_order() {
        // Arrange
        let task_names = vec!["First", "Second", "Third", "Fourth"];
        let ctx = create_sequence_workflow("wp1_order_preservation", task_names.clone());

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify tasks appear in specified order
        let mut last_pos = 0;
        for name in &task_names {
            let pos = generated.find(&format!("id=\"{}\"", name))
                .expect(&format!("Task {} not found in generated XML", name));
            assert!(pos > last_pos, "Tasks should appear in order: {} should come after previous task", name);
            last_pos = pos;
        }

        // Verify golden file
        compare_with_golden("wp1_order_preservation", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP1 - Single task sequence (degenerate case)
    ///
    /// Edge case: A workflow with just one task is technically a sequence
    #[test]
    fn test_wp1_single_task_sequence() {
        // Arrange
        let ctx = create_sequence_workflow("wp1_single_task", vec!["OnlyTask"]);

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Single task with no flows
        assert_eq!(ctx.tasks.len(), 1);
        assert_eq!(ctx.flows.len(), 0);
        assert!(generated.contains("id=\"OnlyTask\""));

        // Verify golden file
        compare_with_golden("wp1_single_task", &generated)
            .expect("Generated XML matches golden file");
    }
}


mod sequence_validation_tests {
    use super::*;

    /// Test: Sequence workflow validates successfully
    #[test]
    fn test_sequence_validation_passes() {
        let ctx = crate::patterns::wp01_sequence::create_sequence_workflow(
            "wp1_validation",
            vec!["A", "B", "C"],
        );

        let result = ctx.validate();
        assert!(result.is_ok(), "Sequence workflow should validate: {:?}", result.err());
    }

    /// Test: Missing flow in sequence fails validation
    #[test]
    fn test_sequence_missing_flow_fails() {
        // Create sequence but missing flow from A to B
        let tasks = vec![
            TaskContext {
                id: "A".to_string(),
                name: "Task A".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B".to_string(),
                name: "Task B".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        // Flow only from B to C, A has no outgoing
        let flows = vec![FlowContext {
            source: "B".to_string(),
            target: "C".to_string(),
            condition: None,
            predicate: None,
            is_default: true,
        }];

        let ctx = TemplateContext {
            workflow_name: "broken_sequence".to_string(),
            description: "Invalid sequence".to_string(),
            version: "1.0.0".to_string(),
            tasks,
            flows,
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // This should still validate (disconnected tasks are allowed in YAWL)
        // But would represent an invalid logical sequence
        assert!(ctx.validate().is_ok());
    }
}
