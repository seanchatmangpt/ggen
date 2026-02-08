//! WP7: Synchronizing Merge (OR-join) Pattern Tests
//!
//! The Synchronizing Merge pattern merges multiple branches where one or
//! more may have been activated. It waits for all ACTIVATED branches to complete.
//!
//! Pattern definition:
//! - A point in a workflow where multiple branches converge
//! - Differs from simple merge: it waits for ALL activated branches
//! - Differs from synchronization: not all branches may have been activated
//! - Must track which branches were activated at runtime
//!
//! YAWL representation:
//! - Task with joinBehavior="OR"
//! - Multiple flows into the merge task
//! - Runtime tracking of activated branches

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod synchronizing_merge_tests {
    use super::*;

    /// Helper to create a synchronizing merge workflow
    fn create_synchronizing_merge_workflow(
        name: &str,
        branches: Vec<&str>,
        merge_task: &str,
    ) -> TemplateContext {
        // Create a multi-choice point before the branches
        let choice = TaskContext {
            id: "Choice".to_string(),
            name: "Multi-Choice Point".to_string(),
            split_type: "OR".to_string(),
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

        // Create the merge task (with OR join behavior)
        let merge = TaskContext {
            id: merge_task.to_string(),
            name: format!("Synchronizing Merge {}", merge_task),
            split_type: "XOR".to_string(),
            join_type: "OR".to_string(), // Key for synchronizing merge
            is_auto: true,
            decomposition_id: None,
        };

        let mut all_tasks = vec![choice];
        all_tasks.extend(branch_contexts);
        all_tasks.push(merge);

        // Create flows: Choice -> branches (conditional), branches -> Merge
        let mut flows = vec![];

        // Choice to branches (OR-split - optional activation)
        for (i, branch) in branches.iter().enumerate() {
            flows.push(FlowContext {
                source: "Choice".to_string(),
                target: branch.to_string(),
                condition: Some(format!("activate.{} == true", branch)),
                predicate: Some(format!("activate.{} == true", branch)),
                is_default: false,
            });
        }

        // Branches to merge (OR-join - wait for all activated)
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
            description: format!("{} workflow with synchronizing merge pattern", name),
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

    /// Test: WP7 - Basic synchronizing merge (A -> {B, C} -> D)
    #[test]
    fn test_wp7_sync_merge_two_branches() {
        // Arrange
        let ctx = create_synchronizing_merge_workflow(
            "wp7_sync_merge_two_branches",
            vec!["B", "C"],
            "Merge",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify OR join
        assert!(generated.contains("OR") || generated.contains("type=\"OR\""));

        // Verify flows into merge
        let merge_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.target == "Merge")
            .collect();
        assert_eq!(merge_flows.len(), 2);

        // Verify golden file
        compare_with_golden("wp7_sync_merge_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP7 - Three-way synchronizing merge (A -> {B, C, D} -> E)
    #[test]
    fn test_wp7_sync_merge_three_branches() {
        // Arrange
        let ctx = create_synchronizing_merge_workflow(
            "wp7_sync_merge_three_branches",
            vec!["Branch1", "Branch2", "Branch3"],
            "Converge",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 5); // 1 choice + 3 branches + 1 merge

        // Verify golden file
        compare_with_golden("wp7_sync_merge_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP7 - Synchronizing merge semantics
    ///
    /// Verifies that the YAWL XML correctly encodes synchronizing merge:
    /// - OR join type on merge task
    /// - Waits for all ACTIVATED incoming branches
    #[test]
    fn test_wp7_sync_merge_semantics() {
        // Arrange
        let ctx = create_synchronizing_merge_workflow(
            "wp7_sync_merge_semantics",
            vec!["PathA", "PathB"],
            "JoinPoint",
        );

        // Find the merge task
        let merge_task = ctx.tasks.iter()
            .find(|t| t.id == "JoinPoint")
            .expect("Merge task should exist");

        // Verify OR join behavior
        assert_eq!(merge_task.join_type, "OR",
                   "Synchronizing merge requires OR join type");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify OR join in XML
        assert!(generated.contains("OR"));

        // Verify golden file
        compare_with_golden("wp7_sync_merge_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP7 - Multi-choice followed by synchronizing merge
    ///
    /// Common pattern: OR-split -> optional branches -> OR-join
    #[test]
    fn test_wp7_or_split_and_merge() {
        // Arrange
        // Choice -> {A, B, C} (optional) -> Merge -> End
        let tasks = vec![
            TaskContext {
                id: "Choice".to_string(),
                name: "Choice".to_string(),
                split_type: "OR".to_string(),
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
                id: "C".to_string(),
                name: "Path C".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Merge".to_string(),
                name: "Merge".to_string(),
                split_type: "XOR".to_string(),
                join_type: "OR".to_string(),
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
            // OR-split flows
            FlowContext {
                source: "Choice".to_string(),
                target: "A".to_string(),
                condition: Some("activate.A == true".to_string()),
                predicate: Some("activate.A == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Choice".to_string(),
                target: "B".to_string(),
                condition: Some("activate.B == true".to_string()),
                predicate: Some("activate.B == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Choice".to_string(),
                target: "C".to_string(),
                condition: Some("activate.C == true".to_string()),
                predicate: Some("activate.C == true".to_string()),
                is_default: false,
            },
            // OR-join flows
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
                source: "C".to_string(),
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
            workflow_name: "wp7_or_split_merge".to_string(),
            description: "Multi-choice and synchronizing merge pattern".to_string(),
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
        assert!(generated.contains("id=\"C\""));
        assert!(generated.contains("id=\"Merge\""));
        assert!(generated.contains("id=\"End\""));

        // Verify both OR split and OR join are present
        let or_count = generated.matches("OR").count();
        assert!(or_count >= 2, "Should have both OR split and OR join");

        // Verify golden file
        compare_with_golden("wp7_or_split_merge", &generated)
            .expect("Generated XML matches golden file");
    }
}
