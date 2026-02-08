//! WP5: Simple Merge (XOR-join) Pattern Tests
//!
//! The Simple Merge pattern merges multiple alternative branches into a
//! single thread of control. Only ONE incoming branch needs to complete.
//!
//! Pattern definition:
//! - A point in a workflow where multiple alternative branches converge
//!   into one single thread of control
//! - The successor activity is triggered when ANY ONE of the incoming
//!   branches completes
//! - Not to be confused with synchronization (AND-join)
//!
//! YAWL representation:
//! - Task with joinBehavior="XOR"
//! - Multiple flows from alternative tasks into the merge task
//! - First completion triggers the merge task

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod simple_merge_tests {
    use super::*;

    /// Helper to create a simple merge workflow
    fn create_simple_merge_workflow(
        name: &str,
        branches: Vec<&str>,
        merge_task: &str,
    ) -> TemplateContext {
        // Create a choice point before the branches
        let choice = TaskContext {
            id: "Choice".to_string(),
            name: "Choice Point".to_string(),
            split_type: "XOR".to_string(),
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

        // Create the merge task (with XOR join behavior)
        let merge = TaskContext {
            id: merge_task.to_string(),
            name: format!("Merge Point {}", merge_task),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(), // Key for simple merge
            is_auto: true,
            decomposition_id: None,
        };

        let mut all_tasks = vec![choice];
        all_tasks.extend(branch_contexts);
        all_tasks.push(merge);

        // Create flows: Choice -> branches, branches -> Merge
        let mut flows = vec![];

        // Choice to branches (with conditions)
        for (i, branch) in branches.iter().enumerate() {
            flows.push(FlowContext {
                source: "Choice".to_string(),
                target: branch.to_string(),
                condition: Some(format!("choice == {}", i + 1)),
                predicate: Some(format!("choice == {}", i + 1)),
                is_default: i == branches.len() - 1,
            });
        }

        // Branches to merge (no conditions - simple merge)
        for branch in &branches {
            flows.push(FlowContext {
                source: branch.to_string(),
                target: merge_task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with simple merge pattern", name),
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

    /// Test: WP5 - Basic simple merge (A -> [B|C] -> D)
    #[test]
    fn test_wp5_simple_merge_two_branches() {
        // Arrange
        let ctx = create_simple_merge_workflow(
            "wp5_merge_two_branches",
            vec!["B", "C"],
            "Merge",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify XOR join
        assert!(generated.contains("XOR"));

        // Verify flows into merge
        assert!(generated.contains("from=\"B\"") && generated.contains("into=\"Merge\""));
        assert!(generated.contains("from=\"C\"") && generated.contains("into=\"Merge\""));

        // Verify golden file
        compare_with_golden("wp5_merge_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP5 - Three-way simple merge (A -> [B|C|D] -> E)
    #[test]
    fn test_wp5_simple_merge_three_branches() {
        // Arrange
        let ctx = create_simple_merge_workflow(
            "wp5_merge_three_branches",
            vec!["Branch1", "Branch2", "Branch3"],
            "Converge",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 5); // 1 choice + 3 branches + 1 merge

        // Count flows into merge task
        let merge_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.target == "Converge")
            .collect();
        assert_eq!(merge_flows.len(), 3);

        // Verify golden file
        compare_with_golden("wp5_merge_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP5 - Simple merge semantics
    ///
    /// Verifies that the YAWL XML correctly encodes simple merge:
    /// - XOR join type on merge task
    /// - No conditions on incoming flows
    /// - First branch to complete triggers merge
    #[test]
    fn test_wp5_simple_merge_semantics() {
        // Arrange
        let ctx = create_simple_merge_workflow(
            "wp5_merge_semantics",
            vec!["PathA", "PathB"],
            "JoinPoint",
        );

        // Find the merge task
        let merge_task = ctx.tasks.iter()
            .find(|t| t.id == "JoinPoint")
            .expect("Merge task should exist");

        // Verify XOR join behavior
        assert_eq!(merge_task.join_type, "XOR",
                   "Simple merge requires XOR join type");

        // Verify incoming flows have no conditions
        let incoming_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.target == "JoinPoint")
            .collect();
        for flow in incoming_flows {
            assert!(flow.condition.is_none(),
                   "Simple merge incoming flows should have no conditions");
        }

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify XOR join in XML
        assert!(generated.contains("XOR"));

        // Verify golden file
        compare_with_golden("wp5_merge_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP5 - Exclusive choice followed by simple merge
    ///
    /// Common pattern: XOR-split -> alternative branches -> XOR-join
    #[test]
    fn test_wp5_xor_split_and_merge() {
        // Arrange
        // Choice -> [A, B] -> Merge -> End
        let tasks = vec![
            TaskContext {
                id: "Choice".to_string(),
                name: "Choice".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "A".to_string(),
                name: "Path A".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B".to_string(),
                name: "Path B".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Merge".to_string(),
                name: "Merge".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "End".to_string(),
                name: "End".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Choice".to_string(),
                target: "A".to_string(),
                condition: Some("path == 'A'".to_string()),
                predicate: Some("path == 'A'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Choice".to_string(),
                target: "B".to_string(),
                condition: Some("path == 'B'".to_string()),
                predicate: Some("path == 'B'".to_string()),
                is_default: true,
            },
            FlowContext {
                source: "A".to_string(),
                target: "Merge".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "B".to_string(),
                target: "Merge".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Merge".to_string(),
                target: "End".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp5_xor_split_merge".to_string(),
            description: "Exclusive choice and simple merge pattern".to_string(),
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

        // Assert - Verify complete pattern
        assert!(generated.contains("id=\"Choice\""));
        assert!(generated.contains("id=\"A\""));
        assert!(generated.contains("id=\"B\""));
        assert!(generated.contains("id=\"Merge\""));
        assert!(generated.contains("id=\"End\""));

        // Verify golden file
        compare_with_golden("wp5_xor_split_merge", &generated)
            .expect("Generated XML matches golden file");
    }
}
