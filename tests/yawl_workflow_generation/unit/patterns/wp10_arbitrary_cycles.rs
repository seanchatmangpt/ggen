//! WP10: Arbitrary Cycles Pattern Tests
//!
//! The Arbitrary Cycles pattern allows a workflow to loop back to a
//! previously executed activity.
//!
//! Pattern definition:
//! - A point in a workflow where a thread can return to a previous
//!   activity in the same thread
//! - Enables iteration, retry logic, and repetitive processing
//! - Must be well-structured to avoid infinite loops
//!
//! YAWL representation:
//! - Flow from a task back to an earlier task in the sequence
//! - Typically with a condition to control loop termination
//! - Cycle marker or predicate to indicate loop behavior

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod arbitrary_cycles_tests {
    use super::*;

    /// Helper to create an arbitrary cycle workflow
    fn create_cycle_workflow(
        name: &str,
        tasks: Vec<&str>,
        cycle_from: &str,
        cycle_to: &str,
        cycle_condition: &str,
    ) -> TemplateContext {
        // Create tasks in sequence
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

        // Create sequential flows plus cycle flow
        let mut flows = vec![];

        // Sequential flows
        for pair in tasks.windows(2) {
            flows.push(FlowContext {
                source: pair[0].to_string(),
                target: pair[1].to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            });
        }

        // Cycle flow
        flows.push(FlowContext {
            source: cycle_from.to_string(),
            target: cycle_to.to_string(),
            condition: Some(cycle_condition.to_string()),
            predicate: Some(cycle_condition.to_string()),
            is_default: false,
        });

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with arbitrary cycle pattern", name),
            version: "1.0.0".to_string(),
            tasks: task_contexts,
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

    /// Test: WP10 - Basic cycle (A -> B -> A)
    #[test]
    fn test_wp10_cycle_two_tasks() {
        // Arrange
        let ctx = create_cycle_workflow(
            "wp10_cycle_two",
            vec!["A", "B"],
            "B",
            "A",
            "retry == true",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("id=\"A\""));
        assert!(generated.contains("id=\"B\""));

        // Verify cycle flow exists
        let cycle_flows: Vec<_> = ctx.flows
            .iter()
            .filter(|f| f.source == "B" && f.target == "A")
            .collect();
        assert_eq!(cycle_flows.len(), 1);

        // Verify golden file
        compare_with_golden("wp10_cycle_two", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP10 - Loop with counter (A -> B -> C, C -> A while count < N)
    #[test]
    fn test_wp10_cycle_with_counter() {
        // Arrange
        let ctx = create_cycle_workflow(
            "wp10_cycle_counter",
            vec!["Initialize", "Process", "Check"],
            "Check",
            "Initialize",
            "count < max_iterations",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify loop condition
        assert!(generated.contains("count") || generated.contains("max_iterations"));

        // Verify golden file
        compare_with_golden("wp10_cycle_counter", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP10 - Retry pattern (A -> B, B -> A on failure)
    #[test]
    fn test_wp10_retry_pattern() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Attempt".to_string(),
                name: "Attempt Operation".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "CheckResult".to_string(),
                name: "Check Result".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Success".to_string(),
                name: "Success Path".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Attempt".to_string(),
                target: "CheckResult".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "CheckResult".to_string(),
                target: "Attempt".to_string(),
                condition: Some("failed == true".to_string()),
                predicate: Some("failed == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "CheckResult".to_string(),
                target: "Success".to_string(),
                condition: Some("failed == false".to_string()),
                predicate: Some("failed == false".to_string()),
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp10_retry_pattern".to_string(),
            description: "Retry until success pattern".to_string(),
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
        assert!(generated.contains("Attempt"));
        assert!(generated.contains("CheckResult"));
        assert!(generated.contains("Success"));

        // Verify retry flow exists
        assert!(generated.contains("failed"));

        // Verify golden file
        compare_with_golden("wp10_retry_pattern", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP10 - Do-while loop structure
    #[test]
    fn test_wp10_do_while_loop() {
        // Arrange
        // A -> B -> C -> B (do-while: B executes, then condition checked at C)
        let ctx = create_cycle_workflow(
            "wp10_do_while",
            vec!["A", "B", "C"],
            "C",
            "B",
            "continue == true",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 3);
        assert_eq!(ctx.flows.len(), 3); // 2 sequential + 1 cycle

        // Verify golden file
        compare_with_golden("wp10_do_while", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP10 - While loop structure
    #[test]
    fn test_wp10_while_loop() {
        // Arrange
        // A checks condition -> B (loop body) -> A
        let tasks = vec![
            TaskContext {
                id: "CheckCondition".to_string(),
                name: "Check Condition".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "LoopBody".to_string(),
                name: "Loop Body".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Exit".to_string(),
                name: "Exit Loop".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "CheckCondition".to_string(),
                target: "LoopBody".to_string(),
                condition: Some("condition == true".to_string()),
                predicate: Some("condition == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "CheckCondition".to_string(),
                target: "Exit".to_string(),
                condition: Some("condition == false".to_string()),
                predicate: Some("condition == false".to_string()),
                is_default: true,
            },
            FlowContext {
                source: "LoopBody".to_string(),
                target: "CheckCondition".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp10_while_loop".to_string(),
            description: "While loop pattern".to_string(),
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
        assert!(generated.contains("condition"));

        // Verify golden file
        compare_with_golden("wp10_while_loop", &generated)
            .expect("Generated XML matches golden file");
    }
}
