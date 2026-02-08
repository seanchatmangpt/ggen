//! WP2: Parallel Split (AND-split) Pattern Tests
//!
//! The Parallel Split pattern divides one thread of execution into multiple
//! parallel threads that execute concurrently.
//!
//! Pattern definition:
//! - A point in a workflow where a single thread of control splits into
//!   multiple threads which are executed in parallel
//! - All outgoing branches are activated simultaneously
//! - No conditions on the branches - all execute
//!
//! YAWL representation:
//! - Task with splitBehavior="AND"
//! - Multiple flows from the split task to each parallel task
//! - All flows are unconditional (isDefault=true)

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod parallel_split_tests {
    use super::*;

    /// Helper to create a parallel split workflow
    fn create_parallel_split_workflow(
        name: &str,
        split_task: &str,
        parallel_tasks: Vec<&str>,
    ) -> TemplateContext {
        // Create the split task (with AND split behavior)
        let split = TaskContext {
            id: split_task.to_string(),
            name: format!("Split Task {}", split_task),
            split_type: "AND".to_string(), // Key for parallel split
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        // Create parallel tasks
        let parallel_contexts: Vec<TaskContext> = parallel_tasks
            .iter()
            .map(|id| TaskContext {
                id: id.to_string(),
                name: format!("Parallel Task {}", id),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        let mut all_tasks = vec![split];
        all_tasks.extend(parallel_contexts);

        // Create flows from split to each parallel task
        let flows: Vec<FlowContext> = parallel_tasks
            .iter()
            .map(|target| FlowContext {
                source: split_task.to_string(),
                target: target.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with parallel split pattern", name),
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

    /// Test: WP2 - Basic parallel split (A -> [B, C])
    #[test]
    fn test_wp2_parallel_split_two_branches() {
        // Arrange
        let ctx = create_parallel_split_workflow(
            "wp2_parallel_two_branches",
            "A",
            vec!["B", "C"],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify AND split
        assert!(generated.contains("split=\"AND\"") || generated.contains("type=\"AND\""));
        assert!(generated.contains("<task id=\"B\""));
        assert!(generated.contains("<task id=\"C\""));

        // Verify both flows from A
        assert!(generated.contains("from=\"A\"") && generated.contains("into=\"B\""));
        assert!(generated.contains("from=\"A\"") && generated.contains("into=\"C\""));

        // Verify golden file
        compare_with_golden("wp2_parallel_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP2 - Three-way parallel split (A -> [B, C, D])
    #[test]
    fn test_wp2_parallel_split_three_branches() {
        // Arrange
        let ctx = create_parallel_split_workflow(
            "wp2_parallel_three_branches",
            "Start",
            vec!["Process1", "Process2", "Process3"],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 4); // 1 split + 3 parallel
        assert_eq!(ctx.flows.len(), 3); // 3 flows from split

        // Verify all parallel tasks are present
        for task in ["Process1", "Process2", "Process3"] {
            assert!(generated.contains(&format!("id=\"{}\"", task)));
        }

        // Verify golden file
        compare_with_golden("wp2_parallel_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP2 - Many parallel branches (A -> [B, C, D, E, F])
    #[test]
    fn test_wp2_parallel_split_many_branches() {
        // Arrange
        let parallel_tasks: Vec<&str> =
            vec!["T1", "T2", "T3", "T4", "T5"];
        let ctx = create_parallel_split_workflow(
            "wp2_parallel_many_branches",
            "SplitPoint",
            parallel_tasks.clone(),
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.flows.len(), 5);

        // Verify all flows originate from split task
        for flow in &ctx.flows {
            assert_eq!(flow.source, "SplitPoint");
        }

        // Verify golden file
        compare_with_golden("wp2_parallel_many_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP2 - Parallel split semantics
    ///
    /// Verifies that the YAWL XML correctly encodes parallel execution:
    /// - AND split type on source task
    /// - All flows are unconditional
    /// - All flows are marked as default
    #[test]
    fn test_wp2_parallel_split_semantics() {
        // Arrange
        let ctx = create_parallel_split_workflow(
            "wp2_parallel_semantics",
            "SplitTask",
            vec!["Branch1", "Branch2"],
        );

        // Verify split task has AND behavior
        let split_task = ctx.tasks.first().unwrap();
        assert_eq!(split_task.split_type, "AND");

        // Verify all flows are unconditional
        for flow in &ctx.flows {
            assert!(flow.condition.is_none(), "Parallel split flows should have no condition");
            assert!(flow.is_default, "Parallel split flows should be default");
        }

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify AND split in XML
        assert!(generated.contains("AND"));

        // Verify golden file
        compare_with_golden("wp2_parallel_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP2 - Parallel split followed by sequence
    ///
    /// Common pattern: Split into parallel branches, each branch does
    /// sequential processing
    #[test]
    fn test_wp2_parallel_split_with_sequential_branches() {
        // Arrange
        // A -> [B1, B2, B3] where each Bi continues to Bi2
        let tasks = vec![
            TaskContext {
                id: "A".to_string(),
                name: "Start".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B1".to_string(),
                name: "Branch 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B1_2".to_string(),
                name: "Branch 1 Step 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B2".to_string(),
                name: "Branch 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "B2_2".to_string(),
                name: "Branch 2 Step 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            // Parallel splits from A
            FlowContext {
                source: "A".to_string(),
                target: "B1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "A".to_string(),
                target: "B2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            // Sequential continuations
            FlowContext {
                source: "B1".to_string(),
                target: "B1_2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "B2".to_string(),
                target: "B2_2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp2_parallel_with_sequence".to_string(),
            description: "Parallel split with sequential branches".to_string(),
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

        // Assert - Verify structure
        assert!(generated.contains("id=\"A\""));
        assert!(generated.contains("id=\"B1\""));
        assert!(generated.contains("id=\"B2\""));
        assert!(generated.contains("id=\"B1_2\""));
        assert!(generated.contains("id=\"B2_2\""));

        // Verify golden file
        compare_with_golden("wp2_parallel_with_sequence", &generated)
            .expect("Generated XML matches golden file");
    }
}
