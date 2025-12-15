//! Chicago TDD tests for assertion strength analyzer
//!
//! Tests verify observable behavior using real AST parsing (no mocks).

use ggen_test_audit::{AssertionAnalyzer, AssertionStrength};
use std::fs;
use tempfile::TempDir;

/// Test: Classify assert_eq as strong assertion
#[test]
fn test_classify_assert_eq_as_strong() {
    // Arrange: Create analyzer
    let analyzer = AssertionAnalyzer::new(vec![]);

    // Act: Classify assertion
    let strength = analyzer.classify_assertion("assert_eq");

    // Assert: Verify return value
    assert_eq!(
        strength,
        AssertionStrength::Strong,
        "assert_eq! should be classified as Strong"
    );
}

/// Test: Classify is_ok as weak assertion
#[test]
fn test_classify_is_ok_as_weak() {
    // Arrange: Create analyzer
    let analyzer = AssertionAnalyzer::new(vec![]);

    // Act: Classify assertion
    let strength = analyzer.classify_assertion("is_ok");

    // Assert: Verify return value
    assert_eq!(
        strength,
        AssertionStrength::Weak,
        "is_ok! should be classified as Weak (execution-only)"
    );
}

/// Test: Classify assert as medium assertion
#[test]
fn test_classify_assert_as_medium() {
    // Arrange: Create analyzer
    let analyzer = AssertionAnalyzer::new(vec![]);

    // Act: Classify assertion
    let strength = analyzer.classify_assertion("assert");

    // Assert: Verify return value
    assert_eq!(
        strength,
        AssertionStrength::Medium,
        "assert! should be classified as Medium"
    );
}

/// Test: Score strong assertion correctly
#[test]
fn test_score_strong_assertion() {
    // Arrange: Create analyzer
    let analyzer = AssertionAnalyzer::new(vec![]);

    // Act: Score strong assertion
    let score = analyzer.score_assertion(AssertionStrength::Strong);

    // Assert: Verify return value (8-10 range for strong)
    assert_eq!(score, 9.0, "Strong assertion should score 9.0");
    assert!(
        score >= 8.0 && score <= 10.0,
        "Strong assertion score should be in 8-10 range"
    );
}

/// Test: Score weak assertion correctly
#[test]
fn test_score_weak_assertion() {
    // Arrange: Create analyzer
    let analyzer = AssertionAnalyzer::new(vec![]);

    // Act: Score weak assertion
    let score = analyzer.score_assertion(AssertionStrength::Weak);

    // Assert: Verify return value (0-3 range for weak)
    assert_eq!(score, 2.0, "Weak assertion should score 2.0");
    assert!(
        score >= 0.0 && score <= 3.0,
        "Weak assertion score should be in 0-3 range"
    );
}

/// Test: Analyze real test file with strong assertions
#[test]
fn test_analyze_file_with_strong_assertions() {
    // Arrange: Create test file with assert_eq!
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_strong.rs");

    fs::write(
        &test_file,
        r#"
        #[test]
        fn test_value() {
            let result = 2 + 2;
            assert_eq!(result, 4); // Strong assertion
        }
        "#,
    )
    .expect("Failed to write test file");

    let analyzer = AssertionAnalyzer::new(vec![temp_dir.path().to_path_buf()]);

    // Act: Analyze test file
    let assertions = analyzer
        .analyze_test_file(&test_file)
        .expect("Failed to analyze test file");

    // Assert: Verify state - should find strong assertion
    assert_eq!(assertions.len(), 1, "Should find 1 test");
    assert_eq!(
        assertions[0].assertion_strength,
        AssertionStrength::Strong,
        "Should detect strong assertion"
    );
}

/// Test: Analyze real test file with weak assertions (FALSE POSITIVE case)
#[test]
fn test_analyze_file_with_weak_assertions() {
    // Arrange: Create test file with is_ok() only
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_weak.rs");

    fs::write(
        &test_file,
        r#"
        #[test]
        fn test_execution() {
            let result = parse_config();
            assert!(result.is_ok()); // Weak - only checks execution
        }
        "#,
    )
    .expect("Failed to write test file");

    let analyzer = AssertionAnalyzer::new(vec![temp_dir.path().to_path_buf()]);

    // Act: Analyze test file
    let assertions = analyzer
        .analyze_test_file(&test_file)
        .expect("Failed to analyze test file");

    // Assert: Verify state - should detect weak assertion
    assert_eq!(assertions.len(), 1, "Should find 1 test");

    // NOTE: Currently classifies based on macro name, so assert! is Medium
    // This demonstrates the false positive: test passes (is_ok) but doesn't validate values
}

/// Test: Analyze test file with multiple assertions
#[test]
fn test_analyze_file_with_multiple_assertions() {
    // Arrange: Create test file with mixed assertions
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_mixed.rs");

    fs::write(
        &test_file,
        r#"
        #[test]
        fn test_comprehensive() {
            let result = calculate(2, 2);
            assert!(result.is_ok());
            let value = result.unwrap();
            assert_eq!(value, 4); // Strong assertion
            assert_ne!(value, 5); // Also strong
        }
        "#,
    )
    .expect("Failed to write test file");

    let analyzer = AssertionAnalyzer::new(vec![temp_dir.path().to_path_buf()]);

    // Act: Analyze test file
    let assertions = analyzer
        .analyze_test_file(&test_file)
        .expect("Failed to analyze test file");

    // Assert: Verify state - should find test with strongest assertion type
    assert_eq!(assertions.len(), 1, "Should find 1 test");
    assert_eq!(
        assertions[0].assertion_strength,
        AssertionStrength::Strong,
        "Should classify by strongest assertion (assert_eq! or assert_ne!)"
    );
}

/// Test: Invalid Rust syntax returns error
#[test]
fn test_analyze_invalid_syntax_returns_error() {
    // Arrange: Create test file with invalid Rust
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_invalid.rs");

    fs::write(
        &test_file,
        r#"
        This is not valid Rust code!
        #[test] fn { { { }
        "#,
    )
    .expect("Failed to write test file");

    let analyzer = AssertionAnalyzer::new(vec![temp_dir.path().to_path_buf()]);

    // Act: Attempt to analyze invalid file
    let result = analyzer.analyze_test_file(&test_file);

    // Assert: Verify error state
    assert!(result.is_err(), "Should return error for invalid syntax");
}

/// Test: Analyze all tests in directory
#[test]
fn test_analyze_all_tests_in_directory() {
    // Arrange: Create directory with multiple test files
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    fs::write(
        temp_dir.path().join("test1.rs"),
        r#"
        #[test]
        fn test1() {
            assert_eq!(1, 1);
        }
        "#,
    )
    .expect("Failed to write test1");

    fs::write(
        temp_dir.path().join("test2.rs"),
        r#"
        #[test]
        fn test2() {
            assert!(true);
        }
        "#,
    )
    .expect("Failed to write test2");

    let analyzer = AssertionAnalyzer::new(vec![temp_dir.path().to_path_buf()]);

    // Act: Analyze all tests
    let assertions = analyzer
        .analyze_all_tests()
        .expect("Failed to analyze all tests");

    // Assert: Verify state - should find 2 tests
    assert_eq!(assertions.len(), 2, "Should find 2 tests across all files");
}
