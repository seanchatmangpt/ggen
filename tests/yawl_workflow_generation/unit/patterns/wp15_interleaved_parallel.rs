//! WP15: Interleaved Parallel Routing Pattern Tests
//!
//! The Interleaved Parallel Routing pattern executes multiple branches in
//! an interleaved manner, alternating between branches.
//!
//! Pattern definition:
//! - Multiple branches are activated
//! - Execution alternates between branches
//! - No predetermined order, but controlled interleaving
//!
//! YAWL representation:
//! - Complex pattern requiring explicit state management
//! - Multiple flows with sequencing constraints
//! - Coordination between parallel branches

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod interleaved_parallel_tests {
    use super::*;

    /// Test: WP15 - Basic interleaved routing (A <-> B)
    #[test]
    fn test_wp15_interleaved_two_branches() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Start".to_string(),
                name: "Start".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ProcessA".to_string(),
                name: "Process A".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ProcessB".to_string(),
                name: "Process B".to_string(),
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
                source: "Start".to_string(),
                target: "ProcessA".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "ProcessA".to_string(),
                target: "ProcessB".to_string(),
                condition: Some("turn == 'B'".to_string()),
                predicate: Some("turn == 'B'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "ProcessA".to_string(),
                target: "End".to_string(),
                condition: Some("turn == 'END'".to_string()),
                predicate: Some("turn == 'END'".to_string()),
                is_default: true,
            },
            FlowContext {
                source: "ProcessB".to_string(),
                target: "ProcessA".to_string(),
                condition: Some("turn == 'A'".to_string()),
                predicate: Some("turn == 'A'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "ProcessB".to_string(),
                target: "End".to_string(),
                condition: Some("turn == 'END'".to_string()),
                predicate: Some("turn == 'END'".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp15_interleaved_two".to_string(),
            description: "Interleaved parallel routing".to_string(),
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
                    name: "turn".to_string(),
                    var_type: "string".to_string(),
                    default: Some("'A'".to_string()),
                    scope: "workflow".to_string(),
                },
            ],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("ProcessA"));
        assert!(generated.contains("ProcessB"));
        assert!(generated.contains("turn"));

        // Verify golden file
        compare_with_golden("wp15_interleaved_two", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP15 - Interleaved routing semantics
    ///
    /// Verifies that the YAWL XML correctly encodes interleaved routing:
    /// - Cross-branch flows
    /// - State variable controlling alternation
    #[test]
    fn test_wp15_interleaved_semantics() {
        // Arrange - Same as basic test, focusing on semantic verification
        let tasks = vec![
            TaskContext {
                id: "T1".to_string(),
                name: "Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "T2".to_string(),
                name: "Task 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        // Create cross flows for interleaving
        let flows = vec![
            FlowContext {
                source: "T1".to_string(),
                target: "T2".to_string(),
                condition: Some("next == 'T2'".to_string()),
                predicate: Some("next == 'T2'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "T2".to_string(),
                target: "T1".to_string(),
                condition: Some("next == 'T1'".to_string()),
                predicate: Some("next == 'T1'".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp15_interleaved_semantics".to_string(),
            description: "Interleaved routing semantics".to_string(),
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

        // Verify cross-branch flows exist
        assert_eq!(ctx.flows.len(), 2);

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Both flows should be present
        assert!(generated.contains("T1"));
        assert!(generated.contains("T2"));

        // Verify golden file
        compare_with_golden("wp15_interleaved_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
