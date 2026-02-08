//! WP20: Cancel Region Pattern Tests
//!
//! The Cancel Region pattern allows a set of activities to be cancelled
//! together as a group, rather than cancelling the entire case.
//!
//! Pattern definition:
//! - A mechanism to cancel a subset of activities in a process
//! - The cancelled activities form a region
//! - Activities outside the region continue executing
//!
//! YAWL representation:
//! - Cancellation region identifier
//! - Tasks belong to specific cancellation regions
//! - Cancel trigger affects only tasks in that region

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod cancel_region_tests {
    use super::*;

    /// Test: WP20 - Basic cancel region
    #[test]
    fn test_wp20_cancel_region_basic() {
        // Arrange
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
                id: "RegionTask1".to_string(),
                name: "Region Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CANCEL_REGION_ALPHA".to_string()),
            },
            TaskContext {
                id: "RegionTask2".to_string(),
                name: "Region Task 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("CANCEL_REGION_ALPHA".to_string()),
            },
            TaskContext {
                id: "OutsideTask".to_string(),
                name: "Outside Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None, // Not in cancellation region
            },
            TaskContext {
                id: "CancelRegionTrigger".to_string(),
                name: "Cancel Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Start".to_string(),
                target: "RegionTask1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "RegionTask2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "OutsideTask".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "CancelRegionTrigger".to_string(),
                target: "RegionTask1".to_string(),
                condition: Some("cancel.region.alpha == true".to_string()),
                predicate: Some("cancel.region.alpha == true".to_string()),
                is_default: false,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp20_cancel_region".to_string(),
            description: "Cancel region pattern".to_string(),
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

        // Assert - Verify region tasks share cancellation region
        let task1_region = ctx.tasks[1].decomposition_id.as_ref().unwrap();
        let task2_region = ctx.tasks[2].decomposition_id.as_ref().unwrap();
        assert_eq!(task1_region, "CANCEL_REGION_ALPHA");
        assert_eq!(task2_region, "CANCEL_REGION_ALPHA");

        // Verify outside task is NOT in the cancellation region
        assert!(ctx.tasks[3].decomposition_id.is_none());

        // Verify golden file
        compare_with_golden("wp20_cancel_region", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP20 - Multiple cancellation regions
    #[test]
    fn test_wp20_multiple_cancel_regions() {
        // Arrange - Two independent cancellation regions
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
                id: "Alpha1".to_string(),
                name: "Alpha 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("REGION_ALPHA".to_string()),
            },
            TaskContext {
                id: "Beta1".to_string(),
                name: "Beta 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("REGION_BETA".to_string()),
            },
            TaskContext {
                id: "Independent".to_string(),
                name: "Independent Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![
            FlowContext {
                source: "Start".to_string(),
                target: "Alpha1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "Beta1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "Start".to_string(),
                target: "Independent".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ];

        let ctx = TemplateContext {
            workflow_name: "wp20_multiple_regions".to_string(),
            description: "Multiple cancellation regions".to_string(),
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

        // Assert - Verify two separate regions
        let regions: Vec<_> = ctx.tasks.iter()
            .filter_map(|t| t.decomposition_id.as_ref())
            .collect();
        assert!(regions.contains(&&"REGION_ALPHA".to_string()));
        assert!(regions.contains(&&"REGION_BETA".to_string()));

        // Verify golden file
        compare_with_golden("wp20_multiple_regions", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP20 - Cancel region semantics
    ///
    /// Verifies that the YAWL XML correctly encodes region cancellation:
    /// - Tasks grouped by cancellation region
    /// - Cancel affects only tasks in that region
    /// - Other tasks are unaffected
    #[test]
    fn test_wp20_cancel_region_semantics() {
        // Arrange
        let tasks = vec![
            TaskContext {
                id: "InRegion1".to_string(),
                name: "In Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("MY_REGION".to_string()),
            },
            TaskContext {
                id: "OutOfRegion".to_string(),
                name: "Out of Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![];

        let ctx = TemplateContext {
            workflow_name: "wp20_region_semantics".to_string(),
            description: "Cancel region semantics".to_string(),
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

        // Verify one task in region, one out
        assert!(ctx.tasks[0].decomposition_id.is_some());
        assert!(ctx.tasks[1].decomposition_id.is_none());

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Both tasks should be present
        assert!(generated.contains("InRegion1"));
        assert!(generated.contains("OutOfRegion"));

        // Verify golden file
        compare_with_golden("wp20_region_semantics", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP20 - Nested cancellation regions
    #[test]
    fn test_wp20_nested_cancel_regions() {
        // Arrange - A region can contain another region
        let tasks = vec![
            TaskContext {
                id: "OuterRegionTask".to_string(),
                name: "Outer Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("OUTER_REGION".to_string()),
            },
            TaskContext {
                id: "InnerRegionTask".to_string(),
                name: "Inner Region".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: Some("INNER_REGION".to_string()),
            },
            TaskContext {
                id: "UnaffectedTask".to_string(),
                name: "Unaffected".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ];

        let flows = vec![];

        let ctx = TemplateContext {
            workflow_name: "wp20_nested_regions".to_string(),
            description: "Nested cancellation regions".to_string(),
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

        // Assert - Verify separate regions
        let outer_region = ctx.tasks[0].decomposition_id.as_ref().unwrap();
        let inner_region = ctx.tasks[1].decomposition_id.as_ref().unwrap();
        assert_ne!(outer_region, inner_region);

        // Verify golden file
        compare_with_golden("wp20_nested_regions", &generated)
            .expect("Generated XML matches golden file");
    }
}
