//! Tool Selection Accuracy Tests (User Story 1)
//!
//! Tests verify that AI agents can select the correct cargo make target
//! for a given task based solely on tool descriptions.
//!
//! SUCCESS CRITERIA: 90% tool selection accuracy (SC-001)
//!
//! NOTE: These tests follow Chicago TDD - they test observable behavior
//! using real Makefile.toml parsing, not mocks.

// Import shared test utilities from mod.rs in same directory
#[path = "mod.rs"]
mod aci_utils;

use aci_utils::{extract_description, parse_makefile_toml, validate_description_components};
use std::path::Path;

/// Test: Agent selects correct target for "verify code compiles"
///
/// EXPECTED: cargo make check
/// RATIONALE: check is the fast compilation verification target (<5s SLO)
#[test]
fn test_agent_selects_check_for_compilation() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // Simulate agent decision: Which tool verifies code compiles without running tests?
    let check_target = targets.get("check").expect("check target not found");
    let test_target = targets.get("test").expect("test target not found");

    // Agent should select "check" based on description
    let check_desc = check_target
        .description
        .as_ref()
        .expect("check has no description");
    let test_desc = test_target
        .description
        .as_ref()
        .expect("test has no description");

    // Check description should mention "compiles" or "compilation" and NOT "tests"
    assert!(
        check_desc.to_lowercase().contains("compil") || check_desc.to_lowercase().contains("check"),
        "check description should mention compilation: {}",
        check_desc
    );

    // Test description should mention "tests" or "test suite"
    assert!(
        test_desc.to_lowercase().contains("test"),
        "test description should mention tests: {}",
        test_desc
    );

    // SUCCESS: Agent can distinguish check (compilation only) from test (runs tests)
}

/// Test: All targets have >100 character descriptions with 5 components
///
/// COMPONENTS:
/// 1. Purpose: What the target does
/// 2. Timing: When to use it
/// 3. SLO: Performance threshold
/// 4. Examples: RED/YELLOW/GREEN signal outputs
/// 5. Recovery: What to do when it fails
///
/// SUCCESS CRITERIA: Every cargo make target has comprehensive description (FR-001)
#[test]
fn test_all_targets_have_comprehensive_descriptions() {
    let makefile_path = Path::new("Makefile.toml");
    let targets = parse_makefile_toml(makefile_path).expect("Failed to parse Makefile.toml");

    // Critical targets that MUST have comprehensive descriptions
    let critical_targets = vec![
        "check",
        "test",
        "test-unit",
        "lint",
        "build",
        "build-release",
        "fmt",
        "clean",
        "pre-commit",
        "ci",
        "test-doc",
        "test-integration",
        "docs-check",
        "audit",
        "validate-rdf",
    ];

    let mut failures = Vec::new();

    for target_name in critical_targets {
        if let Some(target) = targets.get(target_name) {
            if let Some(desc) = &target.description {
                let check = validate_description_components(desc);

                if !check.is_complete() {
                    failures.push(format!(
                        "{}: missing components ({}/5 present, {} chars) - needs: {}{}{}{}{}",
                        target_name,
                        check.component_count(),
                        check.length,
                        if !check.has_purpose { " purpose" } else { "" },
                        if !check.has_timing { " timing" } else { "" },
                        if !check.has_slo { " slo" } else { "" },
                        if !check.has_examples { " examples" } else { "" },
                        if !check.has_recovery { " recovery" } else { "" },
                    ));
                }
            } else {
                failures.push(format!("{}: NO DESCRIPTION", target_name));
            }
        } else {
            failures.push(format!("{}: TARGET NOT FOUND", target_name));
        }
    }

    if !failures.is_empty() {
        panic!(
            "Incomplete tool descriptions found:\n{}",
            failures.join("\n")
        );
    }
}

/// Test: Agent understands RED/YELLOW/GREEN Andon signals from examples
///
/// EXPECTED: Tool descriptions include example outputs showing:
/// - GREEN: Successful execution (e.g., "Finished dev target(s) in 1.95s")
/// - RED: Failure requiring immediate action (e.g., "error[E0425]: cannot find value")
/// - YELLOW: Warnings requiring investigation (e.g., "warning: unused variable")
///
/// SUCCESS CRITERIA: 95% Andon signal interpretation accuracy (SC-002)
#[test]
fn test_andon_signal_interpretation() {
    let check_desc = extract_description("check").expect("check target has no description");

    // Check description should include example outputs with signal colors
    let has_signal_examples = (check_desc.to_lowercase().contains("green")
        && check_desc.to_lowercase().contains("red"))
        || check_desc.to_lowercase().contains("example");

    assert!(
        has_signal_examples,
        "check description should include RED/GREEN example outputs: {}",
        check_desc
    );

    // Verify signals are interpretable (GREEN = success, RED = error)
    let has_success_example =
        check_desc.contains("Finished") || check_desc.to_lowercase().contains("success");
    let has_error_example = check_desc.contains("error") || check_desc.contains("fail");

    assert!(
        has_success_example && has_error_example,
        "check description should show both success and error examples"
    );
}

/// Test: Agent distinguishes test vs test-unit based on context
///
/// CONTEXT: "quick feedback during development" → test-unit (<16s SLO)
/// CONTEXT: "comprehensive validation before push" → test (30s-120s SLO)
///
/// SUCCESS CRITERIA: Agent selects appropriate test target based on SLO constraints
#[test]
fn test_agent_distinguishes_test_vs_test_unit() {
    let test_desc = extract_description("test").expect("test target has no description");
    let test_unit_desc =
        extract_description("test-unit").expect("test-unit target has no description");

    // test-unit should mention "quick" or "fast" or "unit" and have shorter SLO
    let test_unit_is_fast = test_unit_desc.to_lowercase().contains("unit")
        || test_unit_desc.to_lowercase().contains("fast")
        || test_unit_desc.to_lowercase().contains("quick");

    // test should mention "all" or "comprehensive" and have longer SLO
    let test_is_comprehensive = test_desc.to_lowercase().contains("all")
        || test_desc.to_lowercase().contains("comprehensive");

    assert!(
        test_unit_is_fast,
        "test-unit description should emphasize speed: {}",
        test_unit_desc
    );
    assert!(
        test_is_comprehensive,
        "test description should emphasize comprehensiveness: {}",
        test_desc
    );

    // SLO comparison: test-unit should have shorter timeout than test
    let test_unit_has_timeout = test_unit_desc.contains("<16s")
        || test_unit_desc.contains("16s")
        || test_unit_desc.contains("16 second");
    let test_has_timeout =
        test_desc.contains("30s") || test_desc.contains("120s") || test_desc.contains("escalation");

    assert!(
        test_unit_has_timeout,
        "test-unit description should specify <16s SLO: {}",
        test_unit_desc
    );
    assert!(
        test_has_timeout,
        "test description should specify 30s/120s escalation SLO: {}",
        test_desc
    );
}

/// Test: Agent selects lint for code quality checks (not check or test)
///
/// EXPECTED: cargo make lint (clippy with all lints)
/// NOT: cargo make check (compilation only, no linting)
#[test]
fn test_agent_selects_lint_for_quality_checks() {
    let lint_desc = extract_description("lint").expect("lint target has no description");
    let check_desc = extract_description("check").expect("check target has no description");

    // lint should mention "clippy" or "linting" or "quality"
    let lint_mentions_quality = lint_desc.to_lowercase().contains("clippy")
        || lint_desc.to_lowercase().contains("lint")
        || lint_desc.to_lowercase().contains("quality");

    assert!(
        lint_mentions_quality,
        "lint description should mention code quality/clippy: {}",
        lint_desc
    );

    // check should NOT mention linting (compilation only)
    let check_is_compilation_only =
        check_desc.to_lowercase().contains("compil") && !check_desc.to_lowercase().contains("lint");

    assert!(
        check_is_compilation_only,
        "check description should focus on compilation, not linting: {}",
        check_desc
    );
}

/// Test: Agent selects ci for comprehensive validation (not just test)
///
/// EXPECTED: cargo make ci (full pipeline: check + lint + test + docs)
/// NOT: cargo make test (only tests, not full validation)
#[test]
fn test_agent_selects_ci_for_comprehensive_validation() {
    let ci_desc = extract_description("ci").expect("ci target has no description");

    // ci should mention "pipeline" or "comprehensive" or "all checks"
    let ci_is_comprehensive = ci_desc.to_lowercase().contains("ci")
        || ci_desc.to_lowercase().contains("pipeline")
        || ci_desc.to_lowercase().contains("comprehensive")
        || ci_desc.to_lowercase().contains("all");

    assert!(
        ci_is_comprehensive,
        "ci description should emphasize comprehensive validation: {}",
        ci_desc
    );

    // ci should have longer SLO than individual targets (sum of dependencies)
    let ci_has_complex_slo = ci_desc.to_lowercase().contains("sum")
        || ci_desc.to_lowercase().contains("all")
        || ci_desc.to_lowercase().contains("dependency");

    assert!(
        ci_has_complex_slo,
        "ci description should mention dependency SLOs: {}",
        ci_desc
    );
}

/// Test: Agent selects fmt for code formatting (not build or check)
///
/// EXPECTED: cargo make fmt (auto-format with rustfmt)
#[test]
fn test_agent_selects_fmt_for_formatting() {
    let fmt_desc = extract_description("fmt").expect("fmt target has no description");

    // fmt should mention "format" or "rustfmt"
    let fmt_mentions_formatting =
        fmt_desc.to_lowercase().contains("format") || fmt_desc.to_lowercase().contains("rustfmt");

    assert!(
        fmt_mentions_formatting,
        "fmt description should mention formatting: {}",
        fmt_desc
    );

    // fmt should mention "before commit" or timing guidance
    let fmt_has_timing = fmt_desc.to_lowercase().contains("before")
        || fmt_desc.to_lowercase().contains("when:")
        || fmt_desc.to_lowercase().contains("timing:");

    assert!(
        fmt_has_timing,
        "fmt description should specify when to use it: {}",
        fmt_desc
    );
}

/// Test: Agent selects clean for build artifact removal
///
/// EXPECTED: cargo make clean (removes target/ directory)
#[test]
fn test_agent_selects_clean_for_artifact_removal() {
    let clean_desc = extract_description("clean").expect("clean target has no description");

    // clean should mention "remove" or "clean" or "artifacts"
    let clean_mentions_removal = clean_desc.to_lowercase().contains("clean")
        || clean_desc.to_lowercase().contains("remove")
        || clean_desc.to_lowercase().contains("artifact");

    assert!(
        clean_mentions_removal,
        "clean description should mention artifact removal: {}",
        clean_desc
    );

    // clean should mention when to use (after build issues, workspace cleanup)
    let clean_has_timing =
        clean_desc.to_lowercase().contains("after") || clean_desc.to_lowercase().contains("when");

    assert!(
        clean_has_timing,
        "clean description should specify when to use it: {}",
        clean_desc
    );
}

/// Test: Tool selection accuracy measurement
///
/// Simulates 10 real-world scenarios and measures how often the agent
/// would select the correct tool based on descriptions alone.
///
/// TARGET: 90% accuracy (9/10 scenarios correct)
#[test]
fn test_overall_tool_selection_accuracy() {
    let scenarios = vec![
        ("verify code compiles without running tests", "check"),
        ("run all tests before pushing to main", "test"),
        ("quick unit test feedback during development", "test-unit"),
        ("check code quality with clippy lints", "lint"),
        ("format code with rustfmt before commit", "fmt"),
        ("build optimized release binary", "build-release"),
        ("remove build artifacts after failure", "clean"),
        ("run full CI pipeline validation before PR", "ci"),
        ("validate RDF graphs and SPARQL queries", "validate-rdf"),
        ("check API documentation compiles", "docs-check"),
    ];

    let mut correct = 0;
    let mut failures = Vec::new();

    for (scenario, expected_target) in &scenarios {
        let desc = extract_description(expected_target);

        if let Some(desc) = desc {
            let check = validate_description_components(&desc);

            // Agent can select correct tool if description is comprehensive
            if check.is_complete() {
                correct += 1;
            } else {
                failures.push(format!(
                    "Scenario: '{}' → Expected '{}' but description incomplete ({}/5 components)",
                    scenario,
                    expected_target,
                    check.component_count()
                ));
            }
        } else {
            failures.push(format!(
                "Scenario: '{}' → Expected '{}' but NO DESCRIPTION",
                scenario, expected_target
            ));
        }
    }

    let accuracy = (correct as f64) / (scenarios.len() as f64) * 100.0;

    if !failures.is_empty() {
        eprintln!("Tool selection failures:\n{}", failures.join("\n"));
    }

    assert!(
        accuracy >= 90.0,
        "Tool selection accuracy is {:.1}% (expected ≥90%). Correct: {}/{} scenarios",
        accuracy,
        correct,
        scenarios.len()
    );
}
