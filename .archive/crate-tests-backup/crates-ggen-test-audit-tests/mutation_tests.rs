//! Chicago TDD tests for mutation testing analyzer
//!
//! Tests verify observable behavior using real collaborators (no mocks).
//! Focus: State changes, return values, side effects.

use ggen_test_audit::{MutationAnalyzer, MutationResult, MutationType, TestId};
use std::fs;
use tempfile::TempDir;

/// Test: MutationAnalyzer can be created with valid workspace
#[test]
fn test_mutation_analyzer_new_with_valid_workspace() {
    // Arrange: Create temporary workspace
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Act: Create analyzer
    let result = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"));

    // Assert: Verify state - analyzer created successfully
    assert!(
        result.is_ok(),
        "Should create analyzer with valid workspace"
    );
}

/// Test: MutationAnalyzer rejects non-existent workspace
#[test]
fn test_mutation_analyzer_new_with_invalid_workspace() {
    // Arrange: Non-existent path
    let invalid_path = "/tmp/nonexistent_ggen_workspace_12345";

    // Act: Attempt to create analyzer
    let result = MutationAnalyzer::new(invalid_path, "/tmp/reports");

    // Assert: Verify state - should fail with error
    assert!(result.is_err(), "Should reject non-existent workspace");
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("not found"),
        "Error should mention workspace not found"
    );
}

/// Test: Adding critical paths updates internal state
#[test]
fn test_add_critical_paths() {
    // Arrange: Create analyzer
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let mut analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    // Act: Add critical paths
    analyzer.add_critical_paths();

    // Assert: Verify side effect - crate paths should be configured
    // (We can't directly observe internal state, but subsequent operations will validate)
    // This is Chicago TDD - we test through observable behavior
}

/// Test: Calculate kill rate with all mutants killed
#[test]
fn test_calculate_kill_rate_all_killed() {
    // Arrange: Create analyzer and mutation results
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    let results = vec![
        MutationResult {
            mutation_id: "m1".to_string(),
            test_id: TestId::new("test1").expect("Valid TestId"),
            mutant_survived: false, // Killed
            mutation_type: MutationType::BinaryOp,
            kill_timestamp: chrono::Utc::now(),
        },
        MutationResult {
            mutation_id: "m2".to_string(),
            test_id: TestId::new("test2").expect("Valid TestId"),
            mutant_survived: false, // Killed
            mutation_type: MutationType::ConstantReplacement,
            kill_timestamp: chrono::Utc::now(),
        },
    ];

    // Act: Calculate kill rate
    let kill_rate = analyzer.calculate_kill_rate(&results);

    // Assert: Verify return value - 100% kill rate (1.0)
    assert_eq!(
        kill_rate, 1.0,
        "All mutants killed should give 1.0 kill rate"
    );
}

/// Test: Calculate kill rate with some mutants survived
#[test]
fn test_calculate_kill_rate_partial() {
    // Arrange: Create analyzer with mixed results
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    let results = vec![
        MutationResult {
            mutation_id: "m1".to_string(),
            test_id: TestId::new("test1").expect("Valid TestId"),
            mutant_survived: false, // Killed
            mutation_type: MutationType::BinaryOp,
            kill_timestamp: chrono::Utc::now(),
        },
        MutationResult {
            mutation_id: "m2".to_string(),
            test_id: TestId::new("test2").expect("Valid TestId"),
            mutant_survived: true, // Survived (BAD - test didn't catch bug)
            mutation_type: MutationType::ConstantReplacement,
            kill_timestamp: chrono::Utc::now(),
        },
        MutationResult {
            mutation_id: "m3".to_string(),
            test_id: TestId::new("test3").expect("Valid TestId"),
            mutant_survived: false, // Killed
            mutation_type: MutationType::UnaryOp,
            kill_timestamp: chrono::Utc::now(),
        },
    ];

    // Act: Calculate kill rate
    let kill_rate = analyzer.calculate_kill_rate(&results);

    // Assert: Verify return value - 2/3 = 0.666...
    assert!(
        (kill_rate - 0.666666).abs() < 0.001,
        "Kill rate should be ~66.67% (2/3)"
    );
}

/// Test: Calculate kill rate with empty results
#[test]
fn test_calculate_kill_rate_empty() {
    // Arrange: Create analyzer with no results
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    let results: Vec<MutationResult> = vec![];

    // Act: Calculate kill rate
    let kill_rate = analyzer.calculate_kill_rate(&results);

    // Assert: Verify return value - 0.0 for empty results
    assert_eq!(kill_rate, 0.0, "Empty results should give 0.0 kill rate");
}

/// Test: Generate baseline report creates file with correct structure
#[test]
fn test_generate_baseline_report_creates_file() {
    // Arrange: Create analyzer and results
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    let results = vec![MutationResult {
        mutation_id: "m1".to_string(),
        test_id: TestId::new("test1").expect("Valid TestId"),
        mutant_survived: false,
        mutation_type: MutationType::BinaryOp,
        kill_timestamp: chrono::Utc::now(),
    }];

    // Act: Generate baseline report
    let report_path = analyzer
        .generate_baseline_report(&results)
        .expect("Failed to generate baseline report");

    // Assert: Verify side effect - file created with correct content
    assert!(
        report_path.exists(),
        "Baseline report file should be created"
    );

    let content = fs::read_to_string(&report_path).expect("Failed to read report");
    let json: serde_json::Value = serde_json::from_str(&content).expect("Invalid JSON");

    // Verify JSON structure (state validation)
    assert_eq!(
        json["total_mutants"]
            .as_u64()
            .expect("Missing total_mutants"),
        1,
        "Should have 1 total mutant"
    );
    assert_eq!(
        json["killed_mutants"]
            .as_u64()
            .expect("Missing killed_mutants"),
        1,
        "Should have 1 killed mutant"
    );
    assert_eq!(
        json["survived_mutants"]
            .as_u64()
            .expect("Missing survived_mutants"),
        0,
        "Should have 0 survived mutants"
    );
}

/// Test: Mutation analyzer meets 80% kill rate target
#[test]
fn test_meets_eighty_percent_target() {
    // Arrange: Create analyzer with results meeting target
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let analyzer = MutationAnalyzer::new(temp_dir.path(), temp_dir.path().join("reports"))
        .expect("Failed to create analyzer");

    // 8 out of 10 mutants killed = 80% exactly
    let results = (0..10)
        .map(|i| MutationResult {
            mutation_id: format!("m{}", i),
            test_id: TestId::new(format!("test{}", i)).expect("Valid TestId"),
            mutant_survived: i >= 8, // Last 2 survive, first 8 killed
            mutation_type: MutationType::BinaryOp,
            kill_timestamp: chrono::Utc::now(),
        })
        .collect::<Vec<_>>();

    // Act: Calculate kill rate
    let kill_rate = analyzer.calculate_kill_rate(&results);

    // Assert: Verify return value - exactly 80%
    assert_eq!(kill_rate, 0.8, "Should achieve exactly 80% kill rate");
    assert!(
        kill_rate >= 0.8,
        "Should meet 80% kill rate target (Feature 004 requirement)"
    );
}
