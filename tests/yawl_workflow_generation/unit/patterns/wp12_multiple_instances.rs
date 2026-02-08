//! WP12: Multiple Instances Pattern Tests
//!
//! The Multiple Instances pattern creates multiple instances of a task
//! that execute concurrently.
//!
//! Pattern definition:
//! - A single activity is instantiated multiple times
//! - Instances may execute in parallel or sequentially
//! - Number of instances can be fixed or data-dependent
//!
//! YAWL representation:
//! - Task with decompositionType="MultipleInstance"
//! - Instance count specified statically or dynamically
//! - Synchronization strategy for joining instances

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod multiple_instances_tests {
    use super::*;

    /// Helper to create a multiple instances workflow
    fn create_multiple_instances_workflow(
        name: &str,
        mi_task: &str,
        instance_count: u32,
    ) -> TemplateContext {
        // Create the task before multiple instance
        let before = TaskContext {
            id: "Before".to_string(),
            name: "Before MI".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        // Create the multiple instance task
        let mi = TaskContext {
            id: mi_task.to_string(),
            name: format!("Multiple Instance Task {}", mi_task),
            split_type: "AND".to_string(), // Spawn multiple instances
            join_type: "AND".to_string(), // Wait for all instances
            is_auto: true,
            decomposition_id: Some(format!("MI_{}", instance_count)),
        };

        // Create task after multiple instance
        let after = TaskContext {
            id: "After".to_string(),
            name: "After MI".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        let tasks = vec![before, mi, after];

        let flows = vec![
            FlowContext {
                source: "Before".to_string(),
                target: mi_task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: mi_task.to_string(),
                target: "After".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with multiple instances", name),
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
        }
    }

    /// Test: WP12 - Fixed number of instances (3 instances)
    #[test]
    fn test_wp12_fixed_instances() {
        // Arrange
        let ctx = create_multiple_instances_workflow(
            "wp12_fixed_instances",
            "ProcessItem",
            3,
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("ProcessItem"));

        // Verify decomposition_id indicates multiple instances
        let mi_task = ctx.tasks.iter()
            .find(|t| t.id == "ProcessItem")
            .unwrap();
        assert!(mi_task.decomposition_id.is_some());

        // Verify golden file
        compare_with_golden("wp12_fixed_instances", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP12 - Data-dependent instance count
    #[test]
    fn test_wp12_data_dependent_instances() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "DetermineCount".to_string(),
                name: "Determine Instance Count".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ProcessAll".to_string(),
                name: "Process All Items".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: Some("MI_DYNAMIC".to_string()),
            },
            TaskContext {
                id: "Continue".to_string(),
                name: "Continue".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "DetermineCount".to_string(),
                target: "ProcessAll".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "ProcessAll".to_string(),
                target: "Continue".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp12_data_dependent".to_string(),
            description: "Data-dependent multiple instances".to_string(),
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
            variables: vec![
                ggen_yawl::template::VariableContext {
                    name: "itemCount".to_string(),
                    var_type: "integer".to_string(),
                    default: Some("0".to_string()),
                    scope: "workflow".to_string(),
                },
            ],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("itemCount"));

        // Verify golden file
        compare_with_golden("wp12_data_dependent", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP12 - Multiple instances semantics
    ///
    /// Verifies that the YAWL XML correctly encodes multiple instances:
    /// - AND split to spawn instances
    /// - AND join to wait for all instances
    /// - Decomposition indicates MI behavior
    #[test]
    fn test_wp12_multiple_instances_semantics() {
        // Arrange
        let ctx = create_multiple_instances_workflow(
            "wp12_mi_semantics",
            "ParallelTask",
            5,
        );

        // Verify MI task split/join behavior
        let mi_task = ctx.tasks.iter()
            .find(|t| t.id == "ParallelTask")
            .expect("MI task should exist");

        // MI tasks typically use AND for spawning and joining
        assert_eq!(mi_task.split_type, "AND");
        assert_eq!(mi_task.join_type, "AND");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify AND in XML for MI semantics
        assert!(generated.contains("AND"));

        // Verify golden file
        compare_with_golden("wp12_mi_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP12 - Business context (Batch processing)
    #[test]
    fn test_wp12_batch_processing() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "ReceiveBatch".to_string(),
                name: "Receive Batch".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ProcessItem".to_string(),
                name: "Process Item".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: Some("MI_BATCH_SIZE".to_string()),
            },
            TaskContext {
                id: "AggregateResults".to_string(),
                name: "Aggregate Results".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "ReceiveBatch".to_string(),
                target: "ProcessItem".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "ProcessItem".to_string(),
                target: "AggregateResults".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp12_batch_processing".to_string(),
            description: "Batch processing with multiple instances".to_string(),
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
        assert!(generated.contains("ReceiveBatch"));
        assert!(generated.contains("ProcessItem"));
        assert!(generated.contains("AggregateResults"));

        // Verify golden file
        compare_with_golden("wp12_batch_processing", &generated)
            .expect("Generated XML matches golden file");
    }
}
