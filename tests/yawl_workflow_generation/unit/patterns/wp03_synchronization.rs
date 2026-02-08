//! WP3: Synchronization (AND-join) Pattern Tests
//!
//! The Synchronization pattern merges multiple parallel threads into a
//! single thread of control. All incoming threads must complete before
//! the next activity can start.
//!
//! Pattern definition:
//! - A point in a workflow where multiple parallel concurrent threads
//!   converge into one single thread of control
//! - The successor activity is only triggered when ALL incoming
//!   branches have completed
//!
//! YAWL representation:
//! - Task with joinBehavior="AND"
//! - Multiple flows into the sync task from parallel tasks
//! - Sync task waits for all incoming tokens

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod synchronization_tests {
    use super::*;

    /// Helper to create a synchronization workflow
    fn create_synchronization_workflow(
        name: &str,
        parallel_tasks: Vec<&str>,
        sync_task: &str,
    ) -> TemplateContext {
        // Create the split task (implicit start of parallel branches)
        let split_task = TaskContext {
            id: "Start".to_string(),
            name: "Start".to_string(),
            split_type: "AND".to_string(),
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

        // Create the sync task (with AND join behavior)
        let sync = TaskContext {
            id: sync_task.to_string(),
            name: format!("Synchronization Point {}", sync_task),
            split_type: "XOR".to_string(),
            join_type: "AND".to_string(), // Key for synchronization
            is_auto: true,
            decomposition_id: None,
        };

        let mut all_tasks = vec![split_task];
        all_tasks.extend(parallel_contexts);
        all_tasks.push(sync);

        // Create flows: Start -> parallel tasks, parallel tasks -> sync
        let mut flows = vec![];

        // Split flows
        for task in &parallel_tasks {
            flows.push(FlowContext {
                source: "Start".to_string(),
                target: task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        // Sync flows
        for task in &parallel_tasks {
            flows.push(FlowContext {
                source: task.to_string(),
                target: sync_task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with synchronization pattern", name),
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

    /// Test: WP3 - Basic synchronization ([A, B] -> C)
    #[test]
    fn test_wp3_synchronization_two_branches() {
        // Arrange
        let ctx = create_synchronization_workflow(
            "wp3_sync_two_branches",
            vec!["A", "B"],
            "Sync",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify AND join
        assert!(generated.contains("join=\"AND\"") || generated.contains("type=\"AND\""));

        // Verify flows into sync task
        assert!(generated.contains("from=\"A\"") && generated.contains("into=\"Sync\""));
        assert!(generated.contains("from=\"B\"") && generated.contains("into=\"Sync\""));

        // Verify golden file
        compare_with_golden("wp3_sync_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP3 - Three-way synchronization ([A, B, C] -> D)
    #[test]
    fn test_wp3_synchronization_three_branches() {
        // Arrange
        let ctx = create_synchronization_workflow(
            "wp3_sync_three_branches",
            vec!["Process1", "Process2", "Process3"],
            "Finalize",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 5); // 1 start + 3 parallel + 1 sync

        // Count flows into sync task
        let sync_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.target == "Finalize")
            .collect();
        assert_eq!(sync_flows.len(), 3);

        // Verify golden file
        compare_with_golden("wp3_sync_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP3 - Synchronization semantics
    ///
    /// Verifies that the YAWL XML correctly encodes synchronization:
    /// - AND join type on sync task
    /// - Multiple flows into sync from parallel tasks
    /// - Sync task waits for all incoming branches
    #[test]
    fn test_wp3_synchronization_semantics() {
        // Arrange
        let ctx = create_synchronization_workflow(
            "wp3_sync_semantics",
            vec!["Branch1", "Branch2"],
            "MergePoint",
        );

        // Find the sync task
        let sync_task = ctx.tasks.iter()
            .find(|t| t.id == "MergePoint")
            .expect("Sync task should exist");

        // Verify AND join behavior
        assert_eq!(sync_task.join_type, "AND",
                   "Sync task must have AND join type");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify AND join in XML
        assert!(generated.contains("AND"));

        // Verify golden file
        compare_with_golden("wp3_sync_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP3 - Parallel split followed by synchronization
    ///
    /// Common pattern: AND-split -> parallel tasks -> AND-join
    #[test]
    fn test_wp3_parallel_split_and_sync() {
        // Arrange
        // Start -> [A, B, C] -> Sync -> End
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
            TaskContext {
                id: "C".to_string(),
                name: "Task C".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Sync".to_string(),
                name: "Synchronize".to_string(),
                split_type: "XOR".to_string(),
                join_type: "AND".to_string(),
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
            FlowContext {
                source: "Start".to_string(),
                target: "C".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "A".to_string(),
                target: "Sync".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "B".to_string(),
                target: "Sync".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "C".to_string(),
                target: "Sync".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Sync".to_string(),
                target: "End".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp3_split_sync_pattern".to_string(),
            description: "Parallel split and synchronization pattern".to_string(),
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
        assert!(generated.contains("id=\"Start\""));
        assert!(generated.contains("id=\"A\""));
        assert!(generated.contains("id=\"B\""));
        assert!(generated.contains("id=\"C\""));
        assert!(generated.contains("id=\"Sync\""));
        assert!(generated.contains("id=\"End\""));

        // Verify both AND split and AND join are present
        let and_count = generated.matches("AND").count();
        assert!(and_count >= 2, "Should have both AND split and AND join");

        // Verify golden file
        compare_with_golden("wp3_split_sync_pattern", &generated)
            .expect("Generated XML matches golden file");
    }
}
