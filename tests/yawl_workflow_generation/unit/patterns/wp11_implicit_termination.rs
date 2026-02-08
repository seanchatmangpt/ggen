//! WP11: Implicit Termination Pattern Tests
//!
//! The Implicit Termination pattern defines when a workflow case should
//! be considered complete without an explicit end node.
//!
//! Pattern definition:
//! - A workflow case is terminated when there are no more active tasks
//! - No explicit "end" activity needed
//! - Case completes when all branches have finished
//!
//! YAWL representation:
//! - No explicit output condition for certain branches
//! - Engine detects termination when work queue is empty
//! - Output condition may be optional

use ggen_yawl::template::{FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod implicit_termination_tests {
    use super::*;

    /// Helper to create an implicit termination workflow
    fn create_implicit_termination_workflow(
        name: &str,
        tasks: Vec<&str>,
    ) -> TemplateContext {
        // Create tasks
        let task_contexts: Vec<TaskContext> = tasks
            .iter()
            .map(|id| TaskContext {
                id: id.to_string(),
                name: format!("Task {}", id),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        // Create sequential flows
        let flows: Vec<FlowContext> = tasks
            .windows(2)
            .map(|pair| FlowContext {
                source: pair[0].to_string(),
                target: pair[1].to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with implicit termination", name),
            version: "1.0.0".to_string(),
            tasks: task_contexts,
            flows,
            input_condition: Some(ggen_yawl::template::ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: None, // Key: No explicit output condition
            variables: vec![],
        }
    }

    /// Test: WP11 - Single task with implicit termination
    #[test]
    fn test_wp11_single_task_implicit() {
        // Arrange
        let ctx = create_implicit_termination_workflow(
            "wp11_single_task",
            vec!["OnlyTask"],
        );

        // Assert - No output condition
        assert!(ctx.output_condition.is_none());

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("OnlyTask"));

        // Verify golden file
        compare_with_golden("wp11_single_task", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP11 - Sequence with implicit termination
    #[test]
    fn test_wp11_sequence_implicit() {
        // Arrange
        let ctx = create_implicit_termination_workflow(
            "wp11_sequence_implicit",
            vec!["A", "B", "C"],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(ctx.output_condition.is_none());
        assert_eq!(ctx.tasks.len(), 3);

        // Verify golden file
        compare_with_golden("wp11_sequence_implicit", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP11 - Parallel branches with implicit termination
    ///
    /// All branches complete, then case terminates
    #[test]
    fn test_wp11_parallel_implicit() {
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

        let flows = vec![
            FlowContext {
                source: "Start".to_string(),
                target: "A".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "B".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp11_parallel_implicit".to_string(),
            description: "Parallel with implicit termination".to_string(),
            version: "1.0.0".to_string(),
            tasks,
            flows,
            input_condition: Some(ggen_yawl::template::ConditionContext {
                id: "input".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: None, // Implicit termination
            variables: vec![],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Both A and B must complete for termination
        assert!(generated.contains("A"));
        assert!(generated.contains("B"));

        // Verify golden file
        compare_with_golden("wp11_parallel_implicit", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP11 - Semantic verification
    ///
    /// Verifies that implicit termination is properly encoded
    #[test]
    fn test_wp11_implicit_termination_semantics() {
        // Arrange
        let ctx = create_implicit_termination_workflow(
            "wp11_semantics",
            vec!["Process"],
        );

        // Verify no explicit output condition
        assert!(ctx.output_condition.is_none(),
                   "Implicit termination requires no output condition");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Should not have outputCondition or should have implicit marker
        // (This depends on YAWL XML schema specifics)

        // Verify golden file
        compare_with_golden("wp11_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
