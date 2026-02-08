//! WP8: Multi-Merge Pattern Tests
//!
//! The Multi-Merge pattern merges multiple branches without synchronization.
//! It's similar to Simple Merge but handles multiple incoming tokens.
//!
//! Pattern definition:
//! - A point in a workflow where multiple branches converge
//! - No synchronization barrier: incoming tokens pass through independently
//! - Different from discriminator: doesn't consume only one token
//! - Different from synchronizing merge: doesn't track activation
//!
//! YAWL representation:
//! - Task with joinBehavior="XOR" and special handling
//! - Multiple flows from different sources
//! - Each incoming token triggers a new instance

use ggen_yawl::template::{ConditionContext, FlowContext, TaskContext, TemplateContext};
use super::golden::compare_with_golden;


mod multi_merge_tests {
    use super::*;

    /// Helper to create a multi-merge workflow
    fn create_multi_merge_workflow(
        name: &str,
        sources: Vec<&str>,
        merge_task: &str,
    ) -> TemplateContext {
        // Create source tasks (could come from different contexts)
        let source_contexts: Vec<TaskContext> = sources
            .iter()
            .map(|id| TaskContext {
                id: id.to_string(),
                name: format!("Source {}", id),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            })
            .collect();

        // Create the merge task (XOR join for multi-merge behavior)
        let merge = TaskContext {
            id: merge_task.to_string(),
            name: format!("Multi-Merge {}", merge_task),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(), // Multi-merge uses XOR join semantics
            is_auto: true,
            decomposition_id: None,
        };

        let mut all_tasks = source_contexts;
        all_tasks.push(merge);

        // Create flows from each source to merge
        let flows: Vec<FlowContext> = sources
            .iter()
            .map(|source| FlowContext {
                source: source.to_string(),
                target: merge_task.to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            })
            .collect();

        TemplateContext {
            workflow_name: name.to_string(),
            description: format!("{} workflow with multi-merge pattern", name),
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

    /// Test: WP8 - Basic multi-merge ([A, B] -> C)
    #[test]
    fn test_wp8_multi_merge_two_sources() {
        // Arrange
        let ctx = create_multi_merge_workflow(
            "wp8_multi_merge_two",
            vec!["A", "B"],
            "Merge",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert!(generated.contains("id=\"A\""));
        assert!(generated.contains("id=\"B\""));
        assert!(generated.contains("id=\"Merge\""));

        // Verify both flows into merge
        assert!(generated.contains("from=\"A\"") && generated.contains("into=\"Merge\""));
        assert!(generated.contains("from=\"B\"") && generated.contains("into=\"Merge\""));

        // Verify golden file
        compare_with_golden("wp8_multi_merge_two", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP8 - Three-way multi-merge ([A, B, C] -> D)
    #[test]
    fn test_wp8_multi_merge_three_sources() {
        // Arrange
        let ctx = create_multi_merge_workflow(
            "wp8_multi_merge_three",
            vec!["Source1", "Source2", "Source3"],
            "Collector",
        );

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert
        assert_eq!(ctx.tasks.len(), 4); // 3 sources + 1 merge
        assert_eq!(ctx.flows.len(), 3);

        // Verify golden file
        compare_with_golden("wp8_multi_merge_three", &generated)
            .expect("Generated XML matches golden file");
    }

    /// Test: WP8 - Multi-merge semantics
    ///
    /// Verifies that the YAWL XML correctly encodes multi-merge:
    /// - XOR join type (pass-through)
    /// - No synchronization
    #[test]
    fn test_wp8_multi_merge_semantics() {
        // Arrange
        let ctx = create_multi_merge_workflow(
            "wp8_multi_merge_semantics",
            vec!["Input1", "Input2"],
            "PassThrough",
        );

        // Verify merge task has XOR join
        let merge_task = ctx.tasks.iter()
            .find(|t| t.id == "PassThrough")
            .expect("Merge task should exist");
        assert_eq!(merge_task.join_type, "XOR");

        // Act
        let generated = crate::patterns::generate_yawl_xml(&ctx)
            .expect("Failed to generate YAWL XML");

        // Assert - Verify XOR join in XML
        assert!(generated.contains("XOR"));

        // Verify golden file
        compare_with_golden("wp8_multi_merge_semantics", &generated)
            .expect("Generated XML matches golden file");
    }
}
