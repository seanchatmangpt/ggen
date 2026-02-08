//! WP19: Critical Section Pattern Tests
//!
//! The Critical Section pattern ensures that only one thread can execute
//! a specific activity or set of activities at a time.
//!
//! Pattern definition:
//! - Multiple threads may want to execute a critical section
//! - Only one thread is allowed to enter at a time
//! - Other threads wait or are blocked until the section is free
//!
//! YAWL representation:
//! - Mutual exclusion constraint
//! - Resource-based locking
//! - Queue for waiting threads

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod critical_section_tests {
    use super::*;

    /// Test: WP19 - Basic critical section
    #[test]
    fn test_wp19_critical_section_basic() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Thread1".to_string(),
                name: "Thread 1".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Thread2".to_string(),
                name: "Thread 2".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "CriticalSection".to_string(),
                name: "Critical Section".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: Some("MUTEX_RESOURCE".to_string()),
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Thread1".to_string(),
                target: "CriticalSection".to_string(),
                condition: Some("lock.available == true".to_string()),
                predicate: Some("lock.available == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Thread2".to_string(),
                target: "CriticalSection".to_string(),
                condition: Some("lock.available == true".to_string()),
                predicate: Some("lock.available == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp19_critical_section".to_string(),
            description: "Critical section pattern".to_string(),
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
                    name: "lock.available".to_string(),
                    var_type: "boolean".to_string(),
                    default: Some("true".to_string()),
                    scope: "workflow".to_string(),
                },
            ],
        };

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("CriticalSection"));
        assert!(generated.contains("lock.available"));

        // Verify both threads check same lock
        let flow1_cond = ctx.flows[0].condition.as_ref().unwrap();
        let flow2_cond = ctx.flows[1].condition.as_ref().unwrap();
        assert_eq!(flow1_cond, flow2_cond);

        // Verify golden file
        compare_with_golden("wp19_critical_section", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP19 - Critical section with queuing
    #[test]
    fn test_wp19_critical_section_queue() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Requester1".to_string(),
                name: "Requester 1".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Requester2".to_string(),
                name: "Requester 2".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "Requester3".to_string(),
                name: "Requester 3".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ProtectedResource".to_string(),
                name: "Protected Resource".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: Some("CRITICAL_MUTEX".to_string()),
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Requester1".to_string(),
                target: "ProtectedResource".to_string(),
                condition: Some("mutex.locked == false".to_string()),
                predicate: Some("mutex.locked == false".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Requester2".to_string(),
                target: "ProtectedResource".to_string(),
                condition: Some("mutex.locked == false".to_string()),
                predicate: Some("mutex.locked == false".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "Requester3".to_string(),
                target: "ProtectedResource".to_string(),
                condition: Some("mutex.locked == false".to_string()),
                predicate: Some("mutex.locked == false".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp19_critical_queue".to_string(),
            description: "Critical section with queuing".to_string(),
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

        // Assert - All requesters check same mutex
        for flow in &ctx.flows {
            assert!(flow.condition.as_ref().unwrap().contains("mutex"));
        }

        // Verify golden file
        compare_with_golden("wp19_critical_queue", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP19 - Critical section semantics
    ///
    /// Verifies that the YAWL XML correctly encodes critical section:
    /// - Lock condition on entry
    /// - Mutual exclusion through shared resource
    #[test]
    fn test_wp19_critical_section_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "Competitor".to_string(),
                name: "Competing Thread".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "ExclusiveZone".to_string(),
                name: "Exclusive Zone".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: Some("EXCLUSIVE_LOCK".to_string()),
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Competitor".to_string(),
                target: "ExclusiveZone".to_string(),
                condition: Some("lock.held == false".to_string()),
                predicate: Some("lock.held == false".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp19_critical_semantics".to_string(),
            description: "Critical section semantics".to_string(),
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

        // Verify lock condition exists
        let flow = ctx.flows.first().unwrap();
        assert!(flow.condition.is_some());

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify lock condition in XML
        assert!(generated.contains("lock.held"));

        // Verify golden file
        compare_with_golden("wp19_critical_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
