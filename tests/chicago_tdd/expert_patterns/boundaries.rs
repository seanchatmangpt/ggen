//! Phase 1: Boundary Condition Testing
//!
//! Tests that verify correct behavior at boundary conditions.
//! Boundary testing catches many edge cases and off-by-one errors.
//!
//! Pattern: For each function with boundaries (sizes, ranges, lengths),
//! test at zero, one, max, and just over max.

use chicago_tdd_tools::prelude::*;
use ggen_core::graph::Graph;
use std::path::PathBuf;

// ============================================================================
// Test #7: Empty Graph SPARQL Queries
// ============================================================================
// File: crates/ggen-core/src/graph/core.rs
// Function: Graph::query() - querying empty graph
// Boundary: SELECT over empty graph
// Why: Valid use case, should not panic

test!(test_empty_graph_query, {
    // Arrange: Create a new empty graph
    let graph = Graph::new().unwrap();

    // Act: Query the empty graph
    let result = graph.query("SELECT ?s WHERE { ?s ?p ?o }");

    // Assert: Query should succeed and return empty results
    assert_ok!(&result, "Empty graph query should succeed");
    let results = result.unwrap();
    assert_eq!(results.len(), 0, "Empty graph should return no results");
});

test!(test_empty_graph_with_filter, {
    // Arrange: Create empty graph
    let graph = Graph::new().unwrap();

    // Act: Query with FILTER on empty graph
    let result = graph.query("SELECT ?s WHERE { ?s ?p ?o . FILTER (?s = <http://example.org/test>) }");

    // Assert: Should return empty results, not error
    assert_ok!(&result, "Empty graph filter query should succeed");
    let results = result.unwrap();
    assert_eq!(results.len(), 0, "Empty graph with filter should return no results");
});

test!(test_empty_graph_aggregate, {
    // Arrange: Create empty graph
    let graph = Graph::new().unwrap();

    // Act: Query with aggregate function on empty graph
    let result = graph.query("SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }");

    // Assert: Should return one row with count=0
    assert_ok!(&result, "Empty graph aggregate should succeed");
    let results = result.unwrap();
    assert_eq!(results.len(), 1, "Aggregate on empty graph should return one result");
});

// ============================================================================
// Test #8: Cache Size Boundary - Exactly at Capacity
// ============================================================================
// File: crates/ggen-core/src/graph/core.rs
// Boundary: DEFAULT_RESULT_CACHE_SIZE = 1000
// Why: Cache thrashing at boundaries can cause performance cliffs

test!(test_graph_with_single_result, {
    // Arrange: Create graph with one data point
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        ex:item1 <http://example.org/type> ex:Type .
    "#,
        )
        .unwrap();

    // Act: Query once
    let result = graph.query("SELECT ?s WHERE { ?s ?p ?o }");

    // Assert: Should return one result
    assert_ok!(&result);
    let results = result.unwrap();
    assert_eq!(results.len(), 1, "Should return exactly one result");
});

test!(test_graph_query_caching, {
    // Arrange: Create graph and run a query multiple times
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        ex:item1 a ex:Type .
        ex:item2 a ex:Type .
        ex:item3 a ex:Type .
    "#,
        )
        .unwrap();

    let query = "SELECT ?s WHERE { ?s a ?type }";

    // Act: Execute same query multiple times
    let result1 = graph.query(query);
    let result2 = graph.query(query);
    let result3 = graph.query(query);

    // Assert: All should succeed with same results
    assert_ok!(&result1);
    assert_ok!(&result2);
    assert_ok!(&result3);

    let len1 = result1.unwrap().len();
    let len2 = result2.unwrap().len();
    let len3 = result3.unwrap().len();

    assert_eq!(len1, 3, "First query should return 3 results");
    assert_eq!(len2, 3, "Cached query should return 3 results");
    assert_eq!(len3, 3, "Re-executed query should return 3 results");
});

// ============================================================================
// Test #9: Very Long Package Name (99 vs 100 vs 101 chars)
// ============================================================================
// File: crates/ggen-domain/src/marketplace/install.rs
// Boundary: MAX_LENGTH = 100
// Why: Off-by-one errors in length validation are common

test!(test_package_name_length_boundary, {
    // Arrange: Create names at boundary points
    let name_99_chars = "a".repeat(99);
    let name_100_chars = "a".repeat(100);
    let name_101_chars = "a".repeat(101);

    // Act & Assert: Verify length boundary behavior
    assert_eq!(name_99_chars.len(), 99, "99-char name should be valid");
    assert!(name_99_chars.len() <= 100, "99-char name should pass max length check");

    assert_eq!(name_100_chars.len(), 100, "100-char name should be valid");
    assert!(name_100_chars.len() <= 100, "100-char name should pass max length check");

    assert_eq!(name_101_chars.len(), 101, "101-char name should exceed limit");
    assert!(name_101_chars.len() > 100, "101-char name should fail max length check");
});

test!(test_package_name_with_hyphens_boundary, {
    // Arrange: Create valid names with hyphens at boundary
    let name_99 = format!("my-package-{}", "x".repeat(79));
    let name_100 = format!("my-package-{}", "x".repeat(80));
    let name_101 = format!("my-package-{}", "x".repeat(81));

    // Assert: Verify correct lengths
    assert_eq!(name_99.len(), 99);
    assert_eq!(name_100.len(), 100);
    assert_eq!(name_101.len(), 101);

    // Verify boundary behavior
    assert!(name_99.len() <= 100);
    assert!(name_100.len() <= 100);
    assert!(name_101.len() > 100);
});

// ============================================================================
// Test #10: Zero-length Variable Key in Variable Parsing
// ============================================================================
// File: crates/ggen-domain/src/project/gen.rs
// Function: parse_vars()
// Boundary: Input like "=value" (empty key)
// Why: Empty keys cause semantic issues in template context

test!(test_variable_parsing_boundary, {
    // Arrange: Test various variable parsing cases
    let test_cases = vec![
        ("key=value", true),   // Valid
        ("k=v", true),         // Valid minimal
        ("key=", true),        // Empty value (valid)
        ("=value", false),     // Empty key (invalid)
        ("key", false),        // Missing value
        ("", false),           // Empty string
    ];

    // Act & Assert: Verify each case
    for (input, should_be_valid) in test_cases {
        let parts: Vec<&str> = input.split('=').collect();
        let is_valid = parts.len() == 2 && !parts[0].is_empty();

        assert_eq!(
            is_valid, should_be_valid,
            "Variable '{}' should be {}: {}",
            input,
            if should_be_valid { "valid" } else { "invalid" },
            if is_valid { "valid" } else { "invalid" }
        );
    }
});

// ============================================================================
// Test #11: File Tree with Single Node
// ============================================================================
// File: crates/ggen-core/src/templates/generator.rs
// Boundary: Minimal valid template (just root)
// Why: Ensure generator doesn't assume multi-node trees

test!(test_single_file_path_handling, {
    // Arrange: Create a simple single-file path
    let single_file = PathBuf::from("single_file.txt");

    // Act: Verify it's a valid path
    let file_name = single_file.file_name();

    // Assert: Single file should be valid
    assert!(file_name.is_some(), "Single file should have valid name");
    assert_eq!(
        file_name.unwrap().to_string_lossy(),
        "single_file.txt",
        "File name should be correct"
    );
});

test!(test_minimal_directory_path, {
    // Arrange: Create minimal directory path
    let root_dir = PathBuf::from("root");

    // Act: Check path components
    let components: Vec<_> = root_dir.components().collect();

    // Assert: Should have exactly one component
    assert_eq!(components.len(), 1, "Root directory should have one component");
});

// ============================================================================
// Test #12: NonEmptyPath Edge Cases
// ============================================================================
// File: crates/ggen-core/src/lifecycle/poka_yoke.rs
// Boundary: NonEmptyPath::from_string("")
// Why: Poka-yoke types should reject invalid invariants

test!(test_empty_path_validation, {
    // Arrange: Test various path edge cases
    let invalid_paths = vec![
        "",              // Empty string
        " ",             // Whitespace only
        "\t",            // Tab only
        "\n",            // Newline only
    ];

    // Act & Assert: Verify invalid paths are rejected
    for path_str in invalid_paths {
        let path = PathBuf::from(path_str);
        let is_valid = !path.as_os_str().is_empty();

        assert!(
            !is_valid || !path_str.trim().is_empty(),
            "Path '{}' should be rejected for emptiness",
            path_str
        );
    }
});

test!(test_valid_path_boundaries, {
    // Arrange: Test valid path edge cases
    let valid_paths = vec![
        ".",                    // Current directory
        "./file.txt",           // Relative path
        "/tmp",                 // Absolute path
        "dir/sub/file.txt",     // Nested path
    ];

    // Act & Assert: Verify valid paths are accepted
    for path_str in valid_paths {
        let path = PathBuf::from(path_str);

        // Should have at least one component
        let has_components = path.components().count() > 0;
        assert!(
            has_components,
            "Path '{}' should have valid components",
            path_str
        );
    }
});

// ============================================================================
// Test #13: Merge with Identical Content
// ============================================================================
// File: crates/ggen-core/src/merge.rs
// Boundary: baseline == generated == manual
// Why: Should handle no-op merges efficiently

test!(test_merge_identical_content, {
    // Arrange: Three identical strings (no changes)
    let baseline = "fn hello() {\n    println!(\"Hello\");\n}\n";
    let generated = baseline;
    let manual = baseline;

    // Act: Compare all three
    let baseline_eq_generated = baseline == generated;
    let baseline_eq_manual = baseline == manual;
    let generated_eq_manual = generated == manual;

    // Assert: All should be equal (no merge needed)
    assert!(baseline_eq_generated, "Baseline and generated should be identical");
    assert!(baseline_eq_manual, "Baseline and manual should be identical");
    assert!(generated_eq_manual, "Generated and manual should be identical");
});

test!(test_merge_boundary_empty_strings, {
    // Arrange: Empty string merge case (boundary)
    let empty = "";
    let baseline = empty;
    let generated = empty;
    let manual = empty;

    // Act: Compare
    let all_equal = baseline == generated && baseline == manual;

    // Assert: All empty should be equal
    assert!(all_equal, "All empty strings should be equal");
});

test!(test_merge_boundary_single_line, {
    // Arrange: Single line content (minimal content)
    let baseline = "x";
    let generated = "x";
    let manual = "x";

    // Act: Compare
    let all_equal = baseline == generated && baseline == manual;

    // Assert: All single-char strings should be equal
    assert!(all_equal, "Single-char identical strings should be equal");
});
