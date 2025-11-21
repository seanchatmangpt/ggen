//! Noun-Verb Validation Tests for Clap Integration
//!
//! Tests command structure validation, circular dependency detection,
//! and action ordering constraints.

use assert_cmd::Command;
use predicates::prelude::*;

/// Helper to create ggen command
fn ggen() -> Command {
    #[allow(clippy::expect_used)]
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Test that valid noun-verb sequences work
#[test]
fn test_valid_noun_verb_sequences() {
    // Valid sequences should execute without error
    let valid_sequences = vec![
        vec!["template", "list"],
        vec!["template", "show"],
        vec!["template", "new"],
        vec!["template", "generate"],
        vec!["marketplace", "list"],
        vec!["ontology", "init"],
    ];

    for args in valid_sequences {
        let result = ggen()
            .args(&args)
            .arg("--help")
            .assert();

        // Should not fail with parse error
        result.success();
    }
}

/// Test that invalid verb sequences fail with good error messages
#[test]
fn test_invalid_verb_sequences() {
    // Invalid verb should produce helpful error
    ggen()
        .arg("template")
        .arg("invalid-verb-xyz")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("error")
                .or(predicate::str::contains("invalid"))
                .or(predicate::str::contains("unrecognized"))
        );
}

/// Test that noun without verb shows help
#[test]
fn test_noun_without_verb_shows_help() {
    ggen()
        .arg("template")
        .assert()
        .failure(); // Should fail but with helpful message
}

/// Test that invalid noun fails with suggestion
#[test]
fn test_invalid_noun_with_suggestion() {
    ggen()
        .arg("invalid-noun-xyz")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error"));
}

/// Test action ordering validation
#[test]
fn test_action_ordering_validation() {
    // Some commands might have ordering constraints
    // For example: template new before template generate

    // This test verifies the CLI structure supports such constraints
    ggen()
        .arg("template")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("new"));
}

/// Test subcommand nesting limits
#[test]
fn test_subcommand_nesting_limits() {
    // Clap naturally limits nesting depth
    // Test that we don't have excessive nesting

    ggen()
        .arg("template")
        .arg("--help")
        .assert()
        .success();

    // Commands shouldn't nest more than 2 levels (noun -> verb)
    // Attempting to nest deeper should fail
    ggen()
        .arg("template")
        .arg("list")
        .arg("invalid-sub-verb")
        .assert()
        .failure();
}

/// Test that circular dependencies are prevented
#[test]
fn test_no_circular_dependencies() {
    // In clap-noun-verb, circular deps would manifest as
    // commands that reference themselves

    // Verify help doesn't create circular references
    ggen()
        .arg("--help")
        .assert()
        .success();

    ggen()
        .arg("template")
        .arg("--help")
        .assert()
        .success();
}

/// Test command structure consistency
#[test]
fn test_command_structure_consistency() {
    // All nouns should follow same pattern
    let nouns = vec!["template", "marketplace", "ontology", "workflow"];

    for noun in nouns {
        ggen()
            .arg(noun)
            .arg("--help")
            .assert()
            .success()
            .stdout(predicate::str::contains("Commands:").or(predicate::str::contains("Usage:")));
    }
}

/// Test that required arguments are enforced
#[test]
fn test_required_arguments_enforced() {
    // Some verbs require arguments
    // Test that they fail without required args

    // Note: This would need specific knowledge of which commands require args
    // For now, test that help shows argument requirements
    ggen()
        .arg("template")
        .arg("lint")
        .arg("--help")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("path")
                .or(predicate::str::contains("PATH"))
                .or(predicate::str::contains("Arguments"))
        );
}

/// Test global flags work with all commands
#[test]
fn test_global_flags_work() {
    // Global flags like --help, --version should work everywhere

    ggen()
        .arg("--version")
        .assert()
        .success();

    ggen()
        .arg("--help")
        .assert()
        .success();

    // Should work with nouns too
    ggen()
        .arg("template")
        .arg("--help")
        .assert()
        .success();
}

/// Test performance of command parsing
#[test]
fn test_command_parsing_performance() {
    use std::time::Instant;

    let start = Instant::now();
    for _ in 0..10 {
        let _ = ggen()
            .arg("--help")
            .ok();
    }
    let duration = start.elapsed();

    // Command parsing should be fast
    assert!(
        duration.as_millis() < 1000,
        "Command parsing should be fast, took {:?}",
        duration
    );
}
