//! WP16: Milestone Pattern Tests
//!
//! The Milestone pattern enables an activity only when a specific milestone
//! (state) has been reached in the workflow.
//!
//! Pattern definition:
//! - An activity can only execute if a milestone has been achieved
//! - The milestone represents a specific state in the process
//! - Activity is blocked until milestone is reached
//!
//! YAWL representation:
//! - Condition on flow guarding task execution
//! - Milestone variable or state check
//! - Enabling condition based on process state

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod milestone_tests {
    use super::*;

    /// Test: WP16 - Basic milestone (task requires milestone)
    #[test]
    fn test_wp16_milestone_basic() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "EstablishMilestone".to_string(),
                name: "Establish Milestone".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "OtherWork".to_string(),
                name: "Other Work".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "MilestoneDependent".to_string(),
                name: "Milestone Dependent Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "EstablishMilestone".to_string(),
                target: "OtherWork".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "EstablishMilestone".to_string(),
                target: "MilestoneDependent".to_string(),
                condition: Some("milestone.reached == true".to_string()),
                predicate: Some("milestone.reached == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp16_milestone_basic".to_string(),
            description: "Basic milestone pattern".to_string(),
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
                    name: "milestone.reached".to_string(),
                    var_type: "boolean".to_string(),
                    default: Some("false".to_string()),
                    scope: "workflow".to_string(),
                },
            ],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("milestone.reached"));

        // Verify golden file
        compare_with_golden("wp16_milestone_basic", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP16 - Multiple milestones
    #[test]
    fn test_wp16_multiple_milestones() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Milestone1".to_string(),
                name: "Milestone 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Milestone2".to_string(),
                name: "Milestone 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "FinalTask".to_string(),
                name: "Final Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Milestone1".to_string(),
                target: "Milestone2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Milestone2".to_string(),
                target: "FinalTask".to_string(),
                condition: Some("milestone1.reached == true && milestone2.reached == true".to_string()),
                predicate: Some("milestone1.reached == true && milestone2.reached == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp16_multiple_milestones".to_string(),
            description: "Multiple milestones pattern".to_string(),
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
                    name: "milestone1.reached".to_string(),
                    var_type: "boolean".to_string(),
                    default: Some("false".to_string()),
                    scope: "workflow".to_string(),
                },
                ggen_yawl::template::VariableContext {
                    name: "milestone2.reached".to_string(),
                    var_type: "boolean".to_string(),
                    default: Some("false".to_string()),
                    scope: "workflow".to_string(),
                },
            ],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify both milestone conditions
        assert!(generated.contains("milestone1"));
        assert!(generated.contains("milestone2"));

        // Verify golden file
        compare_with_golden("wp16_multiple_milestones", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP16 - Milestone semantics
    ///
    /// Verifies that the YAWL XML correctly encodes milestone:
    /// - Condition guards task execution
    /// - Milestone state variable
    #[test]
    fn test_wp16_milestone_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "SetMilestone".to_string(),
                name: "Set Milestone".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "GuardedTask".to_string(),
                name: "Guarded Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "SetMilestone".to_string(),
                target: "GuardedTask".to_string(),
                condition: Some("ms.approved == true".to_string()),
                predicate: Some("ms.approved == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp16_milestone_semantics".to_string(),
            description: "Milestone semantics".to_string(),
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

        // Verify guarding condition exists
        let flow = ctx.flows.first().unwrap();
        assert!(flow.condition.is_some());

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify condition is in XML
        assert!(generated.contains("ms.approved"));

        // Verify golden file
        compare_with_golden("wp16_milestone_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
