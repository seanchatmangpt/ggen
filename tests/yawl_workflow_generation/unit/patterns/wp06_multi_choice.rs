//! WP6: Multi-Choice (OR-split) Pattern Tests
//!
//! The Multi-Choice pattern splits execution into multiple branches where
//! one or more branches may be selected based on conditions.
//!
//! Pattern definition:
//! - A point in a workflow where a single thread of control splits into
//!   multiple concurrent threads
//! - At runtime, ONE or MORE branches are selected based on data/process state
//! - Different from parallel split: not all branches necessarily execute
//! - Different from exclusive choice: more than one branch can execute
//!
//! YAWL representation:
//! - Task with splitBehavior="OR"
//! - Multiple flows with independent conditions
//! - Runtime evaluation determines which branches activate

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod multi_choice_tests {
    use super::*;

    /// Helper to create a multi-choice workflow
    fn create_multi_choice_workflow(
        name: &str,
        choice_task: &str,
        branches: Vec<(&str, &str)>, // (task_id, condition)
    ) -> TemplateContext {
        // Create the choice task (with OR split behavior)
        let choice = TaskContext {
            id: choice_task.to_string(),
            name: format!("Multi-Choice Point {}", choice_task),
            split_type: "OR".to_string(), // Key for multi-choice
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        // Create branch tasks
        let branch_contexts: Vec<TaskContext> = branches
            .iter()
            .map(|(id, _)| TaskContext {
                id: id.to_string(),
                name: format!("Optional Branch {}", id),
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
            .map(|(target, condition)| FlowContext {
                source: choice_task.to_string(),
                target: target.to_string(),
                condition: Some(condition.to_string()),
                predicate: Some(condition.to_string()),
                is_default: false, // OR-split branches are conditional
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with multi-choice pattern", name),
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

    /// Test: WP6 - Basic multi-choice (A -> {B, C} where 1+ may execute)
    #[test]
    fn test_wp6_multi_choice_two_branches() {
        // Arrange
        let ctx = create_multi_choice_workflow(
            "wp6_or_two_branches",
            "Route",
            vec![
                ("NotifyAlice", "notify.alice == true"),
                ("NotifyBob", "notify.bob == true"),
            ],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify OR split
        assert!(generated.contains("OR") || generated.contains("type=\"OR\""));

        // Verify both optional branches exist
        assert!(generated.contains("id=\"NotifyAlice\""));
        assert!(generated.contains("id=\"NotifyBob\""));

        // Verify conditions for optional execution
        assert!(generated.contains("notify.alice"));
        assert!(generated.contains("notify.bob"));

        // Verify golden file
        compare_with_golden("wp6_or_two_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP6 - Three-way multi-choice (A -> {B, C, D})
    #[test]
    fn test_wp6_multi_choice_three_branches() {
        // Arrange
        let ctx = create_multi_choice_workflow(
            "wp6_or_three_branches",
            "Distribute",
            vec![
                ("ChannelA", "channel.contains('A')"),
                ("ChannelB", "channel.contains('B')"),
                ("ChannelC", "channel.contains('C')"),
            ],
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 4); // 1 choice + 3 branches

        // Verify all flows have conditions
        for flow in &ctx.flows {
            assert!(flow.condition.is_some(),
                   "Multi-choice flows should have conditions");
        }

        // Verify golden file
        compare_with_golden("wp6_or_three_branches", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP6 - Multi-choice semantics
    ///
    /// Verifies that the YAWL XML correctly encodes multi-choice:
    /// - OR split type on choice task
    /// - Each flow has a condition
    /// - Zero, one, or multiple branches may activate
    #[test]
    fn test_wp6_multi_choice_semantics() {
        // Arrange
        let ctx = create_multi_choice_workflow(
            "wp6_or_semantics",
            "Evaluate",
            vec![
                ("Option1", "score >= 90"),
                ("Option2", "score >= 70"),
                ("Option3", "score >= 50"),
            ],
        );

        // Verify choice task has OR split
        let choice_task = ctx.tasks.first().unwrap();
        assert_eq!(choice_task.split_type, "OR",
                   "Multi-choice requires OR split type");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify OR split in XML
        assert!(generated.contains("OR"));

        // Verify golden file
        compare_with_golden("wp6_or_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP6 - Multi-choice business context (Notification routing)
    #[test]
    fn test_wp6_multi_choice_notification_routing() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "TriggerEvent".to_string(),
                name: "Trigger Event".to_string(),
                split_type: "OR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "SendEmail".to_string(),
                name: "Send Email".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "SendSMS".to_string(),
                name: "Send SMS".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "SendPush".to_string(),
                name: "Send Push".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "LogNotification".to_string(),
                name: "Log Notification".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "TriggerEvent".to_string(),
                target: "SendEmail".to_string(),
                condition: Some("prefs.email == true".to_string()),
                predicate: Some("prefs.email == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "TriggerEvent".to_string(),
                target: "SendSMS".to_string(),
                condition: Some("prefs.sms == true".to_string()),
                predicate: Some("prefs.sms == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "TriggerEvent".to_string(),
                target: "SendPush".to_string(),
                condition: Some("prefs.push == true".to_string()),
                predicate: Some("prefs.push == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "TriggerEvent".to_string(),
                target: "LogNotification".to_string(),
                condition: Some("prefs.log == true".to_string()),
                predicate: Some("prefs.log == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp6_notification_routing".to_string(),
            description: "Multi-choice notification routing".to_string(),
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

        // Assert - Verify notification channels
        assert!(generated.contains("SendEmail"));
        assert!(generated.contains("SendSMS"));
        assert!(generated.contains("SendPush"));
        assert!(generated.contains("LogNotification"));

        // Verify golden file
        compare_with_golden("wp6_notification_routing", &generated)
            .expect("Generated XML matches golden file");
    }
}
