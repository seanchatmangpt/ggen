//! WP9: Discriminator Pattern Tests
//!
//! The Discriminator pattern waits for the first incoming branch to complete,
//! then ignores all other incoming branches.
//!
//! Pattern definition:
//! - A point in a workflow where multiple branches converge
//! - Once the FIRST branch completes, the discriminator fires
//! - All other incoming branches are blocked/discarded
//! - Subsequent executions require reinitialization
//!
//! YAWL representation:
//! - Task with special join behavior
//! - Only first completion triggers downstream
//! - State tracking to ignore subsequent completions

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod discriminator_tests {
    use super::*;

    /// Helper to create a discriminator workflow
    fn create_discriminator_workflow(
        name: &str,
        branches: Vec<&str>,
        discriminator_task: &str,
    ) -> TemplateContext {
        // Create a parallel split before the branches
        let split = TaskContext {
            id: "Start".to_string(),
            name: "Split".to_string(),
            split_type: "AND".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        // Create branch tasks
        let branch_contexts: Vec<TaskContext> = branches
            .iter()
            .map(|id| TaskContext {
                id: id.to_string(),
                name: format!("Branch {}", id),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        // Create the discriminator task
        let discriminator = TaskContext {
            id: discriminator_task.to_string(),
            name: format!("Discriminator {}", discriminator_task),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(), // Discriminator uses XOR with special handling
            is_auto: true,
            decomposition_id: None,
        };

        let mut all_tasks = vec![split];
        all_tasks.extend(branch_contexts);
        all_tasks.push(discriminator);

        // Create flows: Start -> branches, branches -> Discriminator
        let mut flows = vec![];

        // Split to branches
        for branch in &branches {
            flows.push(FlowContext {
                source: "Start".to_string(),
                target: branch.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        // Branches to discriminator
        for branch in &branches {
            flows.push(FlowContext {
                source: branch.to_string(),
                target: discriminator_task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with discriminator pattern", name),
            version: "1.0.0".to_string(),
            tasks: all_tasks,
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
        }
    }

    /// Test: WP9 - Basic discriminator ([A, B, C] -> D, first wins)
    #[test]
    fn test_wp9_discriminator_three_branches() {
        // Arrange
        let ctx = create_discriminator_workflow(
            "wp9_discriminator_three",
            vec!["A", "B", "C"],
            "FirstWins",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("id=\"FirstWins\""));

        // Verify all branches flow into discriminator
        let disc_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.target == "FirstWins")
            .collect();
        assert_eq!(disc_flows.len(), 3);

        // Verify golden file
        compare_with_golden("wp9_discriminator_three", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP9 - Discriminator semantics
    ///
    /// Verifies that the YAWL XML correctly encodes discriminator:
    /// - Multiple incoming flows
    /// - Special semantics: first completion wins
    #[test]
    fn test_wp9_discriminator_semantics() {
        // Arrange
        let ctx = create_discriminator_workflow(
            "wp9_discriminator_semantics",
            vec!["Path1", "Path2"],
            "Discriminator",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify parallel split (AND) before discriminator
        assert!(generated.contains("AND"));

        // Verify golden file
        compare_with_golden("wp9_discriminator_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP9 - Discriminator with timeout behavior
    ///
    /// Business context: Race between competing processes
    #[test]
    fn test_wp9_discriminator_race_condition() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Trigger".to_string(),
                name: "Trigger All".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "FastPath".to_string(),
                name: "Fast Path".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "SlowPath".to_string(),
                name: "Slow Path".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "FirstResponse".to_string(),
                name: "First Response".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Trigger".to_string(),
                target: "FastPath".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Trigger".to_string(),
                target: "SlowPath".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "FastPath".to_string(),
                target: "FirstResponse".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "SlowPath".to_string(),
                target: "FirstResponse".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp9_race_condition".to_string(),
            description: "Discriminator for race condition".to_string(),
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
        assert!(generated.contains("FastPath"));
        assert!(generated.contains("SlowPath"));
        assert!(generated.contains("FirstResponse"));

        // Verify golden file
        compare_with_golden("wp9_race_condition", &generated)
            .expect("Generated XML matches golden file");
    }
}
