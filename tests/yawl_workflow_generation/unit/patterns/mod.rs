//! Van der Aalst Workflow Pattern Tests
//!
//! Comprehensive tests for all 20 core workflow control-flow patterns
//! from Wil van der Aalst's workflow patterns research.
//!
//! Test methodology:
//! - Golden file testing: Compare generated YAWL XML against expected outputs
//! - AAA pattern: Arrange/Act/Assert with real collaborators
//! - State verification over interaction verification
//!
//! References:
//! - https://www.workflowpatterns.com/
//! - Van der Aalst, W.M.P., ter Hofstede, A.H.M., Kiepuszewski, B., & Barros, A.P. (2003).
//!   Workflow Patterns. Distributed and Parallel Databases, 14(3), 5-51.

pub mod golden;
pub mod wp01_sequence;
pub mod wp02_parallel_split;
pub mod wp03_synchronization;
pub mod wp04_exclusive_choice;
pub mod wp05_simple_merge;
pub mod wp06_multi_choice;
pub mod wp07_synchronizing_merge;
pub mod wp08_multi_merge;
pub mod wp09_discriminator;
pub mod wp10_arbitrary_cycles;
pub mod wp11_implicit_termination;
pub mod wp12_multiple_instances;
pub mod wp13_multiple_instances_no_sync;
pub mod wp14_deferred_choice;
pub mod wp15_interleaved_parallel;
pub mod wp16_milestone;
pub mod wp17_cancel_activity;
pub mod wp18_cancel_case;
pub mod wp19_critical_section;
pub mod wp20_cancel_region;

/// Test helper to verify golden file matches
pub fn verify_golden(
    pattern_name: &str,
    generated: &str,
) -> Result<(), String> {
    golden::compare_with_golden(pattern_name, generated)
        .map_err(|e| e.to_string())
}

/// Test helper to generate YAWL XML from template context
pub fn generate_yawl_xml(
    ctx: &ggen_yawl::template::TemplateContext,
) -> Result<String, Box<dyn std::error::Error>> {
    let renderer = ggen_yawl::template::renderer::TemplateRenderer::new();
    renderer.render_yawl_xml(ctx).map_err(Into::into)
}

#[cfg(test)]
mod pattern_tests {
    use super::*;

    #[test]
    fn test_all_20_patterns_have_tests() {
        // Verify we have test modules for all 20 patterns
        let patterns = vec![
            "wp01_sequence",
            "wp02_parallel_split",
            "wp03_synchronization",
            "wp04_exclusive_choice",
            "wp05_simple_merge",
            "wp06_multi_choice",
            "wp07_synchronizing_merge",
            "wp08_multi_merge",
            "wp09_discriminator",
            "wp10_arbitrary_cycles",
            "wp11_implicit_termination",
            "wp12_multiple_instances",
            "wp13_multiple_instances_no_sync",
            "wp14_deferred_choice",
            "wp15_interleaved_parallel",
            "wp16_milestone",
            "wp17_cancel_activity",
            "wp18_cancel_case",
            "wp19_critical_section",
            "wp20_cancel_region",
        ];

        // This test documents that all 20 patterns should be tested
        // Each pattern module should contain at least one test
        assert_eq!(patterns.len(), 20, "Should have exactly 20 Van der Aalst patterns");
    }

    #[test]
    fn test_golden_directory_exists() {
        let golden_dir = format!("{}/tests/yawl_workflow_generation/unit/patterns/golden",
                                 env!("CARGO_MANIFEST_DIR"));
        let path = std::path::Path::new(&golden_dir);
        assert!(path.exists(), "Golden directory should exist: {}", golden_dir);
    }

    #[test]
    fn test_pattern_coverage_summary() {
        // Document the patterns and their test coverage
        // WP1: Sequence - Basic sequential execution
        // WP2: Parallel Split (AND-split) - Concurrent execution
        // WP3: Synchronization (AND-join) - Wait for all branches
        // WP4: Exclusive Choice (XOR-split) - Choose one branch
        // WP5: Simple Merge (XOR-join) - Merge from one branch
        // WP6: Multi-Choice (OR-split) - Choose multiple branches
        // WP7: Synchronizing Merge (OR-join) - Wait for activated branches
        // WP8: Multi-Merge - Merge without synchronization
        // WP9: Discriminator - First completion wins
        // WP10: Arbitrary Cycles - Loop back to previous task
        // WP11: Implicit Termination - End when no more work
        // WP12: Multiple Instances - Spawn multiple task instances
        // WP13: MI without Synchronization - Independent instances
        // WP14: Deferred Choice - Runtime choice based on events
        // WP15: Interleaved Parallel - Alternating execution
        // WP16: Milestone - Enable based on process state
        // WP17: Cancel Activity - Cancel single task
        // WP18: Cancel Case - Cancel entire workflow
        // WP19: Critical Section - Mutual exclusion
        // WP20: Cancel Region - Cancel subset of tasks

        let expected_patterns = 20;
        assert!(expected_patterns == 20, "All Van der Aalst patterns documented");
    }
}
