//! WP4: Exclusive Choice (XOR-split) Pattern Tests
//!
//! The Exclusive Choice pattern selects exactly one branch from multiple
//! alternatives based on a condition.
//!
//! Pattern definition:
//! - A point in a workflow where a single thread of control splits into
//!   multiple alternative branches
//! - Only ONE branch is selected based on data or process state
//! - The selected branch is immediately executed; others are skipped
//!
//! YAWL representation:
//! - Task with splitBehavior="XOR"
//! - Multiple flows with mutually exclusive conditions
//! - Each flow has a predicate expression

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod exclusive_choice_tests {
    use super::*;

    /// Helper to create an exclusive choice workflow
    fn create_exclusive_choice_workflow(
        name: &str,
        choice_task: &str,
        branches: Vec<(&str, &str)>, // (task_id, condition)
    ) -> TemplateContext {
        // Create the choice task (with XOR split behavior)
        let choice = TaskContext {
            id: choice_task.to_string(),
            name: format!("Choice Point {}", choice_task),
            split_type: "XOR".to_string(), // Key for exclusive choice
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        // Create branch tasks
        let branch_contexts: Vec<TaskContext> = branches
            .iter()
            .map(|(id, _)| TaskContext {
                id: id.to_string(),
                name: format!("Branch {}", id),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        let mut all_tasks = vec![choice];
        all_tasks.extend(branch_contexts);

        // Create conditional flows from choice to each branch
        let flows: Vec<FlowContext> = branches
            .iter()
            .enumerate()
            .map(|(i, (target, condition))| FlowContext {
                source: choice_task.to_string(),
                target: target.to_string(),
                condition: Some(condition.to_string()),
                predicate: Some(condition.to_string()),
                is_default: i == branches.len() - 1, // Last branch is default
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with exclusive choice pattern", name),
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

    /// Test: WP4 - Basic exclusive choice (A -> B | C)
    #[test]
    fn test_wp4_exclusive_choice_two_branches() {
        // Arrange
        let ctx = create_exclusive_choice_workflow(
            "wp4_xor_two_branches",
            "Review",
            vec![
                ("Approve", "approved == true"),
                ("Reject", "approved == false"),
            ],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify XOR split
        assert!(generated.contains("XOR") || generated.contains("type=\"XOR\""));

        // Verify both branches exist
        assert!(generated.contains("id=\"Approve\""));
        assert!(generated.contains("id=\"Reject\""));

        // Verify conditions are present
        assert!(generated.contains("approved == true") ||
                generated.contains("approved=true"));
        assert!(generated.contains("approved == false") ||
                generated.contains("approved=false"));

        // Verify golden file
        compare_with_golden("wp4_xor_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP4 - Three-way exclusive choice (A -> B | C | D)
    #[test]
    fn test_wp4_exclusive_choice_three_branches() {
        // Arrange
        let ctx = create_exclusive_choice_workflow(
            "wp4_xor_three_branches",
            "Router",
            vec![
                ("RouteA", "priority == 'high'"),
                ("RouteB", "priority == 'medium'"),
                ("RouteC", "priority == 'low'"),
            ],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 4); // 1 choice + 3 branches
        assert_eq!(ctx.flows.len(), 3);

        // Verify all flows have conditions
        for flow in &ctx.flows {
            assert!(flow.condition.is_some(),
                   "Exclusive choice flows must have conditions");
        }

        // Verify golden file
        compare_with_golden("wp4_xor_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP4 - Exclusive choice with default branch
    #[test]
    fn test_wp4_exclusive_choice_with_default() {
        // Arrange
        let ctx = create_exclusive_choice_workflow(
            "wp4_xor_with_default",
            "Decision",
            vec![
                ("ProcessFast", "urgent == true"),
                ("ProcessNormal", "urgent == false"),
            ],
        );

        // Verify one flow is marked as default
        let default_count = ctx.flows.iter()
            .filter(|f| f.is_default)
            .count();
        assert!(default_count >= 1, "Should have at least one default flow");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify default flow marker
        assert!(generated.contains("isDefault") ||
                generated.contains("default"));

        // Verify golden file
        compare_with_golden("wp4_xor_with_default", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP4 - Exclusive choice semantics
    ///
    /// Verifies that the YAWL XML correctly encodes exclusive choice:
    /// - XOR split type on choice task
    /// - Each flow has a condition/predicate
    /// - Only one branch can be taken
    #[test]
    fn test_wp4_exclusive_choice_semantics() {
        // Arrange
        let ctx = create_exclusive_choice_workflow(
            "wp4_xor_semantics",
            "ChoicePoint",
            vec![
                ("OptionA", "data.type == 'A'"),
                ("OptionB", "data.type == 'B'"),
                ("OptionC", "otherwise"), // Default
            ],
        );

        // Verify choice task has XOR split
        let choice_task = ctx.tasks.first().unwrap();
        assert_eq!(choice_task.split_type, "XOR",
                   "Exclusive choice requires XOR split type");

        // Verify all flows have predicates
        for flow in &ctx.flows {
            assert!(flow.predicate.is_some(),
                   "Exclusive choice flows must have predicates");
        }

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify predicates in XML
        assert!(generated.contains("<predicate>") ||
                generated.contains("predicate"));

        // Verify golden file
        compare_with_golden("wp4_xor_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP4 - Exclusive choice in business context (Approval workflow)
    #[test]
    fn test_wp4_exclusive_choice_approval_workflow() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "SubmitRequest".to_string(),
                name: "Submit Request".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "AutoApprove".to_string(),
                name: "Auto Approve".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ManagerReview".to_string(),
                name: "Manager Review".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "Reject".to_string(),
                name: "Reject".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "SubmitRequest".to_string(),
                target: "AutoApprove".to_string(),
                condition: Some("amount < 1000".to_string()),
                predicate: Some("amount < 1000".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "SubmitRequest".to_string(),
                target: "ManagerReview".to_string(),
                condition: Some("amount >= 1000 && amount < 10000".to_string()),
                predicate: Some("amount >= 1000 && amount < 10000".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "SubmitRequest".to_string(),
                target: "Reject".to_string(),
                condition: Some("amount >= 10000".to_string()),
                predicate: Some("amount >= 10000".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp4_approval_workflow".to_string(),
            description: "Approval workflow with exclusive choice".to_string(),
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

        // Assert - Verify business logic is encoded
        assert!(generated.contains("amount"));
        assert!(generated.contains("AutoApprove"));
        assert!(generated.contains("ManagerReview"));
        assert!(generated.contains("Reject"));

        // Verify golden file
        compare_with_golden("wp4_approval_workflow", &generated)
            .expect("Generated XML matches golden file");
    }
}
